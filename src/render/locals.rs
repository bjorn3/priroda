use rustc_data_structures::indexed_vec::Idx;
use rustc::ty::{Ty, TyS, TypeVariants, TypeAndMut, layout::{LayoutOf, Size, Align}};
use rustc::mir;

use miri::{
    Frame,
    Value,
    PrimVal,
    Allocation,
    Pointer,
    MemoryPointer,
    Place,
};

use horrorshow::prelude::*;
use horrorshow::Template;

use EvalContext;

pub fn render_locals<'a, 'tcx: 'a>(ecx: &EvalContext<'a, 'tcx>, frame: Option<&Frame<'tcx, 'tcx>>) -> String {
    //               name    ty      alloc        val     style
    let locals: Vec<(String, String, Option<u64>, String, &str)> = frame.map_or(
        Vec::new(),
        |&Frame {
             instance,
             ref locals,
             ref mir,
             return_place: ref _return_place,
             ..
         }| {
            locals
                .iter()
                .enumerate()
                .map(|(id, &val)| {
                    let local_decl = &mir.local_decls[mir::Local::new(id)];
                    let name = local_decl
                        .name
                        .map(|n| n.as_str().to_string())
                        .unwrap_or_else(|| String::new());
                    let ty = ecx.monomorphize(local_decl.ty, instance.substs);
                    let (alloc, val, style) = match val.map(|value| print_value(ecx, ty, value)) {
                        Some(Ok((alloc, text))) => (alloc, text, ""),
                        Some(Err(())) => (None, format!("{:?} does not exist", val), ""),
                        None => (None, "&lt;uninit&gt;".to_owned(), "font-size: 0;"),
                    };
                    (name, ty.to_string(), alloc, val, style)
                })
                .collect()
        },
    );

    let (arg_count, var_count, tmp_count) = frame.map_or((0, 0, 0), |&Frame { ref mir, .. }| (
        mir.args_iter().count(),
        mir.vars_iter().count(),
        mir.temps_iter().count(),
    ));

    (html! {
        table(border="1") {
            tr {
                td(width="20px");
                th { : "id" }
                th { : "name" }
                th { : "alloc" }
                th { : "memory" }
                th { : "type" }
            }
            @ for (i, &(ref name, ref ty, alloc, ref text, ref style)) in locals.iter().enumerate() {
                tr(style=style) {
                    @if i == 0 {
                        th(rowspan=1) { span(class="vertical") { : "Return" } }
                    } else if i == 1 && arg_count != 0 {
                        th(rowspan=arg_count) { span(class="vertical") { : "Arguments" } }
                    } else if i == arg_count + 1 {
                        th(rowspan=var_count) { span(class="vertical") { : "Variables" } }
                    } else if i == var_count + arg_count + 1 {
                        th(rowspan=tmp_count) { span(class="vertical") { : "Temporaries" } }
                    }
                    td { : format!("_{}", i) }
                    td { : name }
                    @if let Some(alloc) = alloc {
                        td { : alloc.to_string() }
                    } else {
                        td;
                    }
                    td { : Raw(text) }
                    td { : ty }
                }
            }
        }
    }).into_string().unwrap()
}

pub fn print_primval<'a, 'tcx: 'a>(ecx: &EvalContext<'a, 'tcx>, ty: Option<Ty<'tcx>>, val: PrimVal) -> String {
    match val {
        PrimVal::Undef => "&lt;undef &gt;".to_string(),
        PrimVal::Ptr(ptr) => {
            let txt = format!("<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>", alloc = ptr.alloc_id.0, offset = ptr.offset.bytes());
            if let Some(ty) = ty {
                return print_adtdef(ecx, ptr, ty)
                    .map(|p| format!("{} ({})", p, txt))
                    .unwrap_or(txt);
            } else {
                return txt;
            }
        },
        PrimVal::Bytes(bytes) => {
            match ty {
                Some(&TyS { sty: TypeVariants::TyBool, ..}) => {
                    if bytes == 0 {
                        return "false (0)".to_string()
                    } else if bytes == 1 {
                        return "true (1)".to_string()
                    }
                }
                Some(&TyS { sty: TypeVariants::TyChar, ..}) => {
                    if bytes < ::std::u32::MAX as u128 {
                        let chr = ::std::char::from_u32(bytes as u32).unwrap();
                        if chr.is_ascii() {
                            return format!("'{}' (0x{:08X})", chr, bytes);
                        }
                    }
                }
                Some(&TyS { sty: TypeVariants::TyUint(_), ..}) => {
                    return format!("{0} (0x{0:08X})", bytes);
                }
                Some(&TyS { sty: TypeVariants::TyInt(_), ..}) => {
                    return format!("{0} (0x{0:08X})", bytes as i128);
                }
                Some(&TyS { sty: TypeVariants::TyFloat(float_ty), ..}) => {
                    use syntax::ast::FloatTy::*;
                    match float_ty {
                        F32 => {
                            if bytes < ::std::u32::MAX as u128 {
                                return format!("{} (0x{:08X})", <f32>::from_bits(bytes as u32), bytes as u32);
                            }
                        }
                        F64 => {
                            if bytes < ::std::u64::MAX as u128 {
                                return format!("{} (0x{:08X})", <f64>::from_bits(bytes as u64), bytes as u64);
                            }
                        }
                    }
                }
                _ => {},
            }
            bytes.to_string()
        },
    }
}

pub fn print_adtdef<'a, 'tcx: 'a>(ecx: &EvalContext<'a, 'tcx>, ptr: MemoryPointer, ty: Ty<'tcx>) -> Option<String> {
    match ty.sty {
        TypeVariants::TyAdt(adt_def, substs) => {
            println!("{:?} {:?} {:?}", ptr, ty, adt_def.variants);
            let layout = ecx.layout_of(ty).unwrap();
            let val = ecx.read_value(ptr.into(), Align::from_bytes(1, 1).ok()?, ty).ok()?;
            if adt_def.variants.len() == 1 {
                let mut pretty = format!("{:?} {{ ", ecx.tcx.absolute_item_path_str(adt_def.did).replace("<", "&lt;").replace(">", "&gt;"));
                for (i, adt_field) in adt_def.variants[0].fields.iter().enumerate() {
                    let (field_val, field_ty) = ecx.read_field(val, None, ::rustc::mir::Field::new(i), ty).ok()??;
                    pretty.push_str(&format!("{}, ", print_value(ecx, field_ty, field_val).ok()?.1));
                }
                pretty.push_str("}");
                return Some(pretty)
            }
        }
        _ => {},
    }
    None
}

pub fn print_value<'a, 'tcx: 'a>(ecx: &EvalContext<'a, 'tcx>, ty: Ty<'tcx>, val: Value) -> Result<(Option<u64>, String), ()> {
    let txt = match val {
        Value::ByRef(ptr, _align) => {
            let (alloc, txt, _len) = print_ptr(ecx, ptr)?;
            return Ok((
                alloc,
                print_adtdef(ecx, ptr.to_ptr().unwrap(), ty)
                    .map(|p| format!("{} ({})", p, txt))
                    .unwrap_or(txt)
            ));
        },
        Value::ByVal(primval) => print_primval(ecx, Some(ty), primval),
        Value::ByValPair(val, extra) => {
            match ty.sty {
                TypeVariants::TyRawPtr(TypeAndMut { ty: &TyS { sty: TypeVariants::TyStr, .. }, .. }) |
                TypeVariants::TyRef(_, &TyS { sty: TypeVariants::TyStr, .. }, _) => {
                    if let (PrimVal::Ptr(ptr), PrimVal::Bytes(extra)) = (val, extra) {
                        if let Ok(allocation) = ecx.memory.get(ptr.alloc_id) {
                            if (ptr.offset.bytes() as u128) < allocation.bytes.len() as u128 {
                                let bytes = &allocation.bytes[ptr.offset.bytes() as usize..];
                                let s = String::from_utf8_lossy(bytes);
                                return Ok((None, format!("\"{}\" ({}, {})", s, print_primval(ecx, None, val), extra)));
                            }
                        }
                    }
                }
                _ => {}
            }
            format!("{}, {}", print_primval(ecx, None, val), print_primval(ecx, None, extra))
        },
    };
    Ok((None, txt))
}

pub fn print_ptr(ecx: &EvalContext, ptr: Pointer) -> Result<(Option<u64>, String, u64), ()> {
    let ptr = ptr.to_ptr().map_err(|_| ())?;
    match (ecx.memory().get(ptr.alloc_id), ecx.memory().get_fn(ptr)) {
        (Ok(alloc), Err(_)) => {
            let s = print_alloc(ecx.memory().pointer_size().bytes(), ptr, alloc);
            Ok((Some(ptr.alloc_id.0), s, alloc.bytes.len() as u64))
        },
        (Err(_), Ok(_)) => {
            // FIXME: print function name
            Ok((None, "function pointer".to_string(), 16))
        },
        (Err(_), Err(_)) => Err(()),
        (Ok(_), Ok(_)) => unreachable!(),
    }
}

pub fn print_alloc(ptr_size: u64, ptr: MemoryPointer, alloc: &Allocation) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    let mut i = 0;
    while i < alloc.bytes.len() as u64 {
        if let Some(&reloc) = alloc.relocations.get(&Size::from_bytes(i)) {
            i += ptr_size;
            write!(&mut s,
                "<a style=\"text-decoration: none\" href=\"/ptr/{alloc}/{offset}\">┠{nil:─<wdt$}┨</a>",
                alloc = reloc.0,
                offset = ptr.offset.bytes(),
                nil = "",
                wdt = (ptr_size * 2 - 2) as usize,
            ).unwrap();
        } else {
            if alloc.undef_mask.is_range_defined(Size::from_bytes(i), Size::from_bytes(i + 1)) {
                write!(&mut s, "{:02x}", alloc.bytes[i as usize] as usize).unwrap();
            } else {
                let ub_chars = ['∅','∆','∇','∓','∞','⊙','⊠','⊘','⊗','⊛','⊝','⊡','⊠'];
                let c1 = (ptr.alloc_id.0 * 769 + i as u64 * 5689) as usize % ub_chars.len();
                let c2 = (ptr.alloc_id.0 * 997 + i as u64 * 7193) as usize % ub_chars.len();
                write!(&mut s, "<mark>{}{}</mark>", ub_chars[c1], ub_chars[c2]).unwrap();
            }
            i += 1;
        }
    }
    s
}
