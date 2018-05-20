use rustc_data_structures::indexed_vec::Idx;
use rustc::hir::def_id::DefId;
use rustc::mir;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use {EvalContext, PrirodaContext};

pub enum ShouldContinue {
    Continue,
    Stop,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Breakpoint(pub DefId, pub mir::BasicBlock, pub usize);


#[derive(Default)]
pub struct BreakpointTree(HashMap<DefId, HashSet<Breakpoint>>);

impl BreakpointTree {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_breakpoint(&mut self, bp: Breakpoint) {
        self.0.entry(bp.0).or_insert(HashSet::new()).insert(bp);
    }

    pub fn remove_breakpoint(&mut self, bp: Breakpoint) -> bool{
        self.0.get_mut(&bp.0).map(|local|local.remove(&bp)).unwrap_or(false)
    }

    pub fn remove_all(&mut self) {
        self.0.clear();
    }

    pub fn for_def_id(&self, def_id: DefId) -> LocalBreakpoints {
        if let Some(bps) = self.0.get(&def_id) {
            LocalBreakpoints::SomeBps(bps)
        } else {
            LocalBreakpoints::NoBp
        }
    }

    pub fn is_at_breakpoint(&self, ecx: &EvalContext) -> bool {
        let frame = ecx.frame();
        self.for_def_id(frame.instance.def_id()).breakpoint_exists(frame.block, frame.stmt)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Breakpoint> {
        self.0.values().flat_map(|local| {
            local.iter()
        })
    }
}

#[derive(Copy, Clone)]
pub enum LocalBreakpoints<'a> {
    NoBp,
    SomeBps(&'a HashSet<Breakpoint>),
}

impl<'a> LocalBreakpoints<'a> {
    pub fn breakpoint_exists(&self, bb: mir::BasicBlock, stmt: usize) -> bool {
        match *self {
            LocalBreakpoints::NoBp => false,
            LocalBreakpoints::SomeBps(bps) => bps.iter().any(|bp| {
                bp.1 == bb && bp.2 == stmt
            })
        }
    }
}

pub fn step<F>(pcx: &mut PrirodaContext, continue_while: F) -> String
    where F: Fn(&EvalContext) -> ShouldContinue {
    let mut message = None;
    loop {
        if pcx.ecx.stack().len() <= 1 && is_ret(&pcx.ecx) {
            break;
        }
        match pcx.ecx.step() {
            Ok(true) => {
                *pcx.step_count += 1;
                ::watch::step_callback(pcx);

                if let Some(frame) = pcx.ecx.stack().last() {
                    let blck = &frame.mir.basic_blocks()[frame.block];
                    if frame.stmt != blck.statements.len() {
                        if ::should_hide_stmt(&blck.statements[frame.stmt]) && !pcx.bptree.is_at_breakpoint(&pcx.ecx) {
                            continue;
                        }
                    }
                }
                if let ShouldContinue::Stop = continue_while(&pcx.ecx) {
                    break;
                }
                if pcx.bptree.is_at_breakpoint(&pcx.ecx) {
                    break;
                }
            }
            Ok(false) => {
                message = Some("interpretation finished".to_string());
                break;
            }
            Err(e) => {
                message = Some(format!("{:?}", e));
                break;
            }
        }
    }
    message.unwrap_or_else(String::new)
}

pub fn is_ret(ecx: &EvalContext) -> bool {
    if let Some(stack) = ecx.stack().last() {
        let basic_block = &stack.mir.basic_blocks()[stack.block];

        match basic_block.terminator().kind {
            ::rustc::mir::TerminatorKind::Return => stack.stmt >= basic_block.statements.len(),
            _ => false,
        }
    } else {
        true
    }
}

pub fn load_breakpoints_from_file() -> BreakpointTree {
    use std::io::{BufRead, BufReader};
    use std::fs::File;
    match File::open("./.priroda_breakpoints") {
        Ok(file) => {
            let file = BufReader::new(file);
            let mut breakpoints = BreakpointTree::new();
            for line in file.lines() {
                let line = line.expect("Couldn't read breakpoint from file");
                if line.trim().is_empty() {
                    continue;
                }
                breakpoints.add_breakpoint(parse_breakpoint_from_url(&format!("/set/{}", line)).unwrap());
            }
            breakpoints
        }
        Err(e) => {
            eprintln!("Couldn't load breakpoint file ./.priroda_breakpoints: {:?}", e);
            BreakpointTree::new()
        }
    }
}

fn parse_breakpoint_from_url(s: &str) -> Result<Breakpoint, String> {
    use rustc::hir::def_id::{CrateNum, DefId, DefIndex, DefIndexAddressSpace};
    let regex = ::regex::Regex::new(r#"DefId\((\d+)/(0|1):(\d+) ~ [^\)]+\)@(\d+):(\d+)"#).unwrap();
    // my_command/DefId(1/0:14824 ~ mycrate::main)@1:3
    //                  ^ ^ ^                      ^ ^
    //                  | | |                      | statement
    //                  | | |                      BasicBlock
    //                  | | DefIndex::as_array_index()
    //                  | DefIndexAddressSpace
    //                  CrateNum

    let s = s.replace("%20", " ");
    let caps = regex.captures(&s).ok_or_else(||format!("Invalid breakpoint {}", s))?;

    // Parse DefId
    let crate_num = CrateNum::new(caps.get(1).unwrap().as_str().parse::<usize>().unwrap());
    let address_space = match caps.get(2).unwrap().as_str().parse::<u64>().unwrap() {
        0 => DefIndexAddressSpace::Low,
        1 => DefIndexAddressSpace::High,
        _ => return Err("address_space is not 0 or 1".to_string()),
    };
    let index = caps.get(3).unwrap().as_str().parse::<usize>().map_err(|_| "index is not a positive integer")?;
    let def_index = DefIndex::from_array_index(index, address_space);
    let def_id = DefId {
        krate: crate_num,
        index: def_index,
    };

    // Parse block and stmt
    let bb = mir::BasicBlock::new(caps.get(4).unwrap().as_str().parse::<usize>().map_err(|_| "block id is not a positive integer")?);
    let stmt = caps.get(5).unwrap().as_str().parse::<usize>().map_err(|_| "stmt id is not a positive integer")?;

    Ok(Breakpoint(def_id, bb, stmt))
}

pub mod step_routes {
    use ::action_route;
    use super::*;

    pub fn routes() -> Vec<::rocket::Route> {
        routes! [
            restart,
            single,
            single_back,
            next,
            return_,
            continue_,
        ]
    }

    action_route!(restart: "/restart", |pcx| {
        pcx.ecx = ::create_ecx(pcx.ecx.tcx.sess, pcx.ecx.tcx.tcx);
        "restarted".to_string()
    });

    action_route!(single: "/single", |pcx| {
        step(pcx, |_ecx| ShouldContinue::Stop)
    });

    action_route!(single_back: "/single_back", |pcx| {
        pcx.ecx = ::create_ecx(pcx.ecx.tcx.sess, pcx.ecx.tcx.tcx);
        if *pcx.step_count > 0 {
            *pcx.step_count -= 1;
            for _ in 0..*pcx.step_count {
                match pcx.ecx.step() {
                    Ok(true) => {}
                    res => return format!("Miri is not deterministic causing error {:?}", res),
                }
            }
            "stepped back".to_string()
        } else {
            "already at the start".to_string()
        }
    });

    action_route!(next: "/next", |pcx| {
        let frame = pcx.ecx.stack().len();
        let stmt = pcx.ecx.frame().stmt;
        let block = pcx.ecx.frame().block;
        step(pcx, |ecx| {
            if ecx.stack().len() <= frame && (block < ecx.frame().block || stmt < ecx.frame().stmt) {
                ShouldContinue::Stop
            } else {
                ShouldContinue::Continue
            }
        })
    });

    action_route!(return_: "/return", |pcx| {
        let frame = pcx.ecx.stack().len();
        step(pcx, |ecx| {
            if ecx.stack().len() <= frame && is_ret(&ecx) {
                ShouldContinue::Stop
            } else {
                ShouldContinue::Continue
            }
        })
    });

    action_route!(continue_: "/continue", |pcx| {
        step(pcx, |_ecx| ShouldContinue::Continue)
    });
}

pub mod bp_routes {
    use ::action_route;
    use super::*;
    use std::path::PathBuf;

    pub fn routes() -> Vec<::rocket::Route> {
        routes! [
            add_here,
            add,
            remove,
            remove_all,
        ]
    }

    action_route!(add_here: "/add_here", |pcx| {
        let frame = pcx.ecx.frame();
        pcx.bptree.add_breakpoint(Breakpoint(frame.instance.def_id(), frame.block, frame.stmt));
        format!("Breakpoint added for {:?}@{}:{}", frame.instance.def_id(), frame.block.index(), frame.stmt)
    });

    action_route!(add: "/add/<path..>", |pcx, path: PathBuf| {
        let path = path.to_string_lossy();
        let res = parse_breakpoint_from_url(&path);
        match res {
            Ok(breakpoint) => {
                pcx.bptree.add_breakpoint(breakpoint);
                format!("Breakpoint added for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2)
            }
            Err(e) => e,
        }
    });

    action_route!(remove: "/remove/<path..>", |pcx, path: PathBuf| {
        let path = path.to_string_lossy();
        let res = parse_breakpoint_from_url(&path);
        match res {
            Ok(breakpoint) => {
                if pcx.bptree.remove_breakpoint(breakpoint) {
                    format!("Breakpoint removed for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2)
                } else {
                    format!("No breakpoint for for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2)
                }
            }
            Err(e) => e,
        }
    });

    action_route!(remove_all: "/remove_all", |pcx| {
        pcx.bptree.remove_all();
        "All breakpoints removed".to_string()
    });
}
