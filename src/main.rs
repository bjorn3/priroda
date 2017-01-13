#![feature(rustc_private, custom_attribute)]
#![feature(pub_restricted)]
#![allow(unused_attributes)]

#[macro_use]
extern crate serde_derive;
extern crate serde_json;

extern crate getopts;
extern crate miri;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_trans;
extern crate rustc_data_structures;
extern crate env_logger;
extern crate log_settings;
#[macro_use]
extern crate log;
extern crate syntax;
extern crate hyper;
extern crate open;
extern crate promising_future;

use promising_future::future_promise;

use miri::{
    EvalContext,
    StackPopCleanup,
    Lvalue,
    Pointer,
    ResourceLimits,
};

use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls};
use rustc::ty::TyCtxt;

use std::sync::Mutex;
use hyper::server::{Server, Request, Response};
use hyper::header::{TransferEncoding, Encoding, ContentType};
use hyper::uri::RequestUri;

enum Page {
    Elm,
    Ico,
    Json(String),
}

use Page::*;

struct MiriCompilerCalls;

impl<'a> CompilerCalls<'a> for MiriCompilerCalls {
    fn build_controller(
        &mut self,
        _: &Session,
        _: &getopts::Matches
    ) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();

        control.after_analysis.callback = Box::new(|state| {
            state.session.abort_if_errors();
            let tcx = state.tcx.unwrap();

            let (node_id, span) = state.session.entry_fn.borrow().expect("no main or start function found");
            let def_id = tcx.map.local_def_id(node_id);
            debug!("found `main` function at: {:?}", span);

            let mir = tcx.item_mir(def_id);
            let def_id = tcx.map.local_def_id(node_id);
            let limits = ResourceLimits {
                memory_size: 100*1024*1024, // 100MB
                stack_limit: 100,
                step_limit: 1000_000,
            };
            let mut ecx = EvalContext::new(tcx, limits);
            let substs = tcx.intern_substs(&[]);

            ecx.push_stack_frame(
                def_id,
                mir.span,
                mir,
                substs,
                Lvalue::from_ptr(Pointer::zst_ptr()),
                StackPopCleanup::None,
                Vec::new(),
            ).unwrap();

            act(ecx, state.session, tcx);
        });

        control
    }
}

fn act<'a, 'tcx: 'a>(mut ecx: EvalContext<'a, 'tcx>, session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>) {
    // setup http server and similar
    let (sender, receiver) = std::sync::mpsc::channel();
    // weird hack to make sure the closure passed to the server doesn't consume the sender
    let sender = Mutex::new(sender);

    // setup server
    let addr = "localhost:54321";
    let server = Server::http(addr).expect("could not create http server");
    let addr = format!("http://{}", addr);
    if open::that(&addr).is_err() {
        println!("open {} in your browser", addr);
    };
    let handle = std::thread::spawn(|| {
        server.handle(move |req: Request, mut res: Response| {
            if let RequestUri::AbsolutePath(path) = req.uri {
                let (future, promise) = future_promise::<Page>();
                println!("got `{}`", path);
                sender.lock().unwrap().send((path, promise)).unwrap();
                match future.value() {
                    Some(Elm) => {
                        res.headers_mut().set(
                            TransferEncoding(vec![
                                Encoding::Gzip,
                            ])
                        );
                        res.headers_mut().set(ContentType::html());
                        //let elm = include_bytes!("../index.html");
                        use std::io::Read;
                        let elm: Result<Vec<u8>, _> = std::fs::File::open("index.html").unwrap().bytes().collect();
                        res.send(&elm.unwrap()).unwrap();
                    }
                    Some(Ico) => {
                        let ico = include_bytes!("../favicon.ico");
                        use hyper::mime::{Mime, TopLevel, SubLevel};
                        res.headers_mut().set(ContentType(Mime(TopLevel::Image, SubLevel::Ext("x-icon".to_string()), vec![])));
                        res.send(ico).unwrap()
                    },
                    Some(Json(text)) => {
                        res.headers_mut().set(ContentType::json());
                        res.send(text.as_bytes()).unwrap();
                    },
                    None => res.send(b"unable to process").unwrap(),
                }
            }
        }).expect("http server crashed");
    });

    // process commands
    for (path, promise) in receiver {
        println!("processing `{}`", path);
        assert_eq!(&path[..1], "/");
        let mut matches = path[1..].split('/');
        match matches.next() {
            Some("") | None => promise.set(Elm),
            Some("cmd") => process_cmd(&mut ecx, matches, promise),
            Some("favicon.ico") => promise.set(Ico),
            Some("frames") => dump_frames(&mut ecx, session, tcx, matches, promise),
            Some("frame") => dump_frame(&mut ecx, session, tcx, matches, promise),
            Some(cmd) => promise.set(Json(format!("\"unknown option: `{}`\"", cmd))),
        }
    }
    handle.join().unwrap();
}

fn dump_frames<'a, 'b, 'tcx: 'a, I: Iterator<Item=&'b str> + 'b>(ecx: &mut EvalContext<'a, 'tcx>, session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>, _paths: I, promise: promising_future::Promise<Page>) {
    let frames: Vec<_> = ecx.stack().iter().map(|frame| Frame {
        function: tcx.absolute_item_path_str(frame.def_id),
        span: session.codemap().span_to_string(frame.span),
    }).collect();
    promise.set(Json(serde_json::to_string(&frames).unwrap()));
}

fn json_error<T: ::std::fmt::Display>(promise: promising_future::Promise<Page>, t: T) {
    promise.set(Json(format!("\"{}\"", t)));
}

fn dump_frame<'a, 'b, 'tcx: 'a, I: Iterator<Item=&'b str> + 'b>(ecx: &mut EvalContext<'a, 'tcx>, session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>, mut paths: I, promise: promising_future::Promise<Page>) {
    let frame: usize = match paths.next().map(std::str::FromStr::from_str) {
        Some(Ok(val)) => val,
        Some(Err(_)) => {
            json_error(promise, "given stackframe is not a number");
            return;
        },
        None => {
            let frame = ecx.stack().len();
            if frame == 0 {
                json_error(promise, "no stack frame");
                return;
            }
            frame - 1
        },
    };
    if frame >= ecx.stack().len() {
        json_error(promise, format!("only {} stack frames available", ecx.stack().len()));
        return;
    }
    let frame = &ecx.stack()[frame];
    match paths.next() {
        None => {
            let frame = Frame {
                function: tcx.item_path_str(frame.def_id),
                span: session.codemap().span_to_string(frame.span),
            };
            promise.set(Json(serde_json::to_string(&frame).unwrap()));
        },
        Some("locals") => match paths.next() {
            None => {
                let locals = frame.mir.local_decls.iter();
                let locals: Vec<_> = locals.zip(&frame.locals).map(|(local, data)| {
                    Local {
                        name: local.name.as_ref().map(|symb| symb.as_str().to_string()),
                        type: local.ty.to_string(),
                        data: format!("{:?}", data),
                    }
                }).collect();
                promise.set(Json(serde_json::to_string(&locals).unwrap()));
            },
            Some(other) => json_error(promise, format!("local info: {}", other)),
        },
        Some(other) => json_error(promise, format!("invalid frame info: {}", other)),
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Local {
    name: Option<String>,
    type: String,
    data: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct Frame {
    function: String,
    span: String,
}

fn process_cmd<'a, 'b, 'tcx: 'a, I: Iterator<Item=&'b str> + 'b>(ecx: &mut EvalContext<'a, 'tcx>, mut paths: I, promise: promising_future::Promise<Page>) {
    match paths.next() {
        Some("step") => match ecx.step() {
            Ok(true) => promise.set(Json("true".to_owned())),
            Ok(false) => promise.set(Json("false".to_owned())),
            Err(e) => promise.set(Json(format!("\"{:?}\"", e))),
        },
        Some("next") => {
            let frame = ecx.stack().len();
            if frame == 0 {
                promise.set(Json("\"no stack frame\"".to_owned()));
                return;
            }
            let stmt = ecx.stack().last().unwrap().stmt;
            let block = ecx.stack().last().unwrap().block;
            loop {
                match ecx.step() {
                    Ok(true) => if ecx.stack().len() == frame && (block < ecx.stack().last().unwrap().block || stmt < ecx.stack().last().unwrap().stmt) {
                        promise.set(Json("true".to_owned()));
                    } else {
                        continue;
                    },
                    Ok(false) => promise.set(Json("false".to_owned())),
                    Err(e) => promise.set(Json(format!("\"{:?}\"", e))),
                }
                break;
            }
        },
        Some("return") => {
            let frame = ecx.stack().len();
            if frame == 0 {
                promise.set(Json("\"no stack frame\"".to_owned()));
                return;
            }
            fn is_ret(ecx: &EvalContext) -> bool {
                let stack = ecx.stack().last().unwrap();
                let basic_block = &stack.mir.basic_blocks()[stack.block];

                match basic_block.terminator().kind {
                    rustc::mir::TerminatorKind::Return => stack.stmt >= basic_block.statements.len(),
                    _ => false,
                }
            }
            loop {
                if ecx.stack().len() <= frame && is_ret(&ecx) {
                    promise.set(Json("true".to_owned()));
                    break;
                }
                match ecx.step() {
                    Ok(true) => continue,
                    Ok(false) => promise.set(Json("false".to_owned())),
                    Err(e) => promise.set(Json(format!("\"{:?}\"", e))),
                }
                break;
            }
        }
        Some("continue") => {
            loop {
                match ecx.step() {
                    Ok(true) => continue,
                    Ok(false) => promise.set(Json("false".to_owned())),
                    Err(e) => promise.set(Json(format!("\"{:?}\"", e))),
                }
                break;
            }
        },
        Some(cmd) => promise.set(Json(format!("\"unknown command: `{}`\"", cmd))),
        None => promise.set(Json("\"no command issued\"".to_owned())),
    }
}


fn find_sysroot() -> String {
    // Taken from https://github.com/Manishearth/rust-clippy/pull/911.
    let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
    let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
    match (home, toolchain) {
        (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
        _ => option_env!("RUST_SYSROOT")
            .expect("need to specify RUST_SYSROOT env var or use rustup or multirust")
            .to_owned(),
    }
}

fn main() {
    init_logger();
    let mut args: Vec<String> = std::env::args().collect();

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);
        args.push(find_sysroot());
    }

    rustc_driver::run_compiler(&args, &mut MiriCompilerCalls, None, None);
}

fn init_logger() {
    const NSPACES: usize = 40;
    let format = |record: &log::LogRecord| {
        // prepend spaces to indent the final string
        let indentation = log_settings::settings().indentation;
        format!("{lvl}:{module}{depth:2}{indent:<indentation$} {text}",
            lvl = record.level(),
            module = record.location().module_path(),
            depth = indentation / NSPACES,
            indentation = indentation % NSPACES,
            indent = "",
            text = record.args())
    };

    let mut builder = env_logger::LogBuilder::new();
    builder.format(format).filter(None, log::LogLevelFilter::Info);

    if std::env::var("MIRI_LOG").is_ok() {
        builder.parse(&std::env::var("MIRI_LOG").unwrap());
    }

    builder.init().unwrap();
}
