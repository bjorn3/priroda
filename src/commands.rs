use promising_future::Promise;
use super::Page;
use super::Page::*;

use miri::{
    EvalContext,
    Frame,
    Pointer,
    Value,
    PrimVal,
};

use rustc::session::Session;
use rustc::ty::TyCtxt;
use rustc::mir;

use std::borrow::Cow;
use std::iter;

fn print_primval(val: PrimVal) -> String {
    use miri::PrimValKind::*;
    match val.kind {
        Ptr => format!("<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>", alloc = val.relocation.unwrap(), offset = val.bits),
        FnPtr => "function pointer".to_string(),
        U8 | U16 | U32 | U64 => val.bits.to_string(),
        I8 | I16 | I32 | I64 => (val.bits as i64).to_string(),
        F32 => val.to_f32().to_string(),
        F64 => val.to_f64().to_string(),
        Bool => val.try_as_bool().expect("bad bool primval").to_string(),
        Char => ::std::char::from_u32(val.bits as u32).expect("bad char primval").to_string(),
    }
}

fn print_value(ecx: &EvalContext, val: Value) -> Result<(Option<u64>, String, u64), ()> {
    let txt = match val {
        Value::ByRef(ptr) => return print_ptr(ecx, ptr),
        Value::ByVal(primval) => print_primval(primval),
        Value::ByValPair(val, extra) => format!("{}, {}", print_primval(val), print_primval(extra)),
    };
    Ok((None, txt, 0))
}

fn print_ptr(ecx: &EvalContext, ptr: Pointer) -> Result<(Option<u64>, String, u64), ()> {
    if ptr.points_to_zst() || ptr == Pointer::never_ptr() {
        return Ok((None, String::new(), 0));
    }
    match (ecx.memory().get(ptr.alloc_id), ecx.memory().get_fn(ptr.alloc_id)) {
        (Ok(alloc), Err(_)) => {
            use std::fmt::Write;
            let mut s = String::new();
            let mut i = 0;
            while i < alloc.bytes.len() as u64 {
                if let Some(&reloc) = alloc.relocations.get(&i) {
                    i += ecx.memory().pointer_size();
                    write!(&mut s,
                        "<a style=\"text-decoration: none\" href=\"/ptr/{alloc}/{offset}\">┠{nil:─<wdt$}┨</a>",
                        alloc = reloc,
                        offset = ptr.offset,
                        nil = "",
                        wdt = (ecx.memory().pointer_size() * 2 - 2) as usize,
                    ).unwrap();
                } else {
                    if alloc.undef_mask.is_range_defined(i, i + 1) {
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
