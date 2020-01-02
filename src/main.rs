#![feature(
    rustc_private,
    decl_macro,
    plugin,
    try_blocks,
    proc_macro_hygiene
)]
#![feature(never_type)]
#![allow(unused_attributes)]
#![recursion_limit = "5000"]

extern crate syntax;
extern crate rustc;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_mir;

extern crate lazy_static;
extern crate regex;
#[macro_use]
extern crate rental;
extern crate miri;
#[macro_use]
extern crate rocket;

extern crate env_logger;
extern crate log;
extern crate log_settings;

extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

extern crate open;
extern crate promising_future;
extern crate syntect;
#[macro_use]
extern crate horrorshow;
extern crate cgraph;

mod render;
mod step;
mod watch;

use std::ops::FnOnce;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use rustc::hir::def_id::LOCAL_CRATE;
use rustc::mir;
use rustc::ty::TyCtxt;
use rustc_driver::Compilation;
use rustc_interface::interface;

use promising_future::future_promise;
use rocket::response::content::*;
use rocket::response::status::BadRequest;
use rocket::response::NamedFile;
use rocket::State;

use miri::AllocId;

use crate::step::BreakpointTree;

fn should_hide_stmt(stmt: &mir::Statement) -> bool {
    use rustc::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Nop => true,
        _ => false,
    }
}

type InterpCx<'tcx> = miri::InterpCx<'tcx, 'tcx, miri::Evaluator<'tcx>>;

pub struct PrirodaContext<'a, 'tcx: 'a> {
    ecx: InterpCx<'tcx>,
    step_count: &'a mut u128,
    traces: watch::Traces<'tcx>,
    config: &'a mut Config,
}

impl<'a, 'tcx: 'a> PrirodaContext<'a, 'tcx> {
    fn restart(&mut self) {
        self.ecx = create_ecx(self.ecx.tcx.tcx);
        *self.step_count = 0;
        self.traces.clear(); // Cleanup all traces
    }
}

#[derive(Deserialize)]
pub struct Config {
    #[serde(default = "true_bool")]
    auto_refresh: bool,
    #[serde(default = "default_theme")]
    theme: String,
    #[serde(default)]
    bptree: BreakpointTree,
}

fn true_bool() -> bool {
    true
}
fn default_theme() -> String {
    "default".to_string()
}

impl Default for Config {
    fn default() -> Self {
        ::std::fs::File::open("config.json")
            .map(|f| serde_json::from_reader(f).unwrap())
            .unwrap_or(Config {
                auto_refresh: true,
                theme: "default".to_string(),
                bptree: step::BreakpointTree::default(),
            })
    }
}

type RResult<T> = Result<T, Html<String>>;

fn create_ecx<'tcx>(tcx: TyCtxt<'tcx>) -> InterpCx<'tcx> {
    let (main_id, _) = tcx
        .entry_fn(LOCAL_CRATE)
        .expect("no main or start function found");

    miri::create_ecx(
        tcx,
        main_id,
        miri::MiriConfig {
            args: vec![],
            communicate: true,
            excluded_env_vars: vec![],
            ignore_leaks: true,
            seed: None,
            tracked_pointer_tag: None,
            validate: true,
        },
    )
    .unwrap()
    .0
}

#[derive(Clone)]
pub struct PrirodaSender(
    Arc<Mutex<::std::sync::mpsc::Sender<Box<dyn FnOnce(&mut PrirodaContext) + Send>>>>,
);

impl PrirodaSender {
    fn do_work<'r, T, F>(&self, f: F) -> Result<T, Html<String>>
    where
        T: rocket::response::Responder<'r> + Send + 'static,
        F: FnOnce(&mut PrirodaContext) -> T + Send + 'static,
    {
        let (future, promise) = future_promise();
        let sender = self.0.lock().unwrap_or_else(|err| err.into_inner());
        match sender.send(Box::new(move |pcx: &mut PrirodaContext| {
            promise.set(f(pcx));
        })) {
            Ok(()) => match future.value() {
                Some(val) => Ok(val),
                None => Err(Html(
                    "<center><h1>Miri crashed please go to <a href='/'>index</a></h1></center>"
                        .to_string(),
                )),
            },
            Err(_) => Err(Html(
                "<center><h1>Miri crashed too often. Please restart priroda.</h1></center>"
                    .to_string(),
            )),
        }
    }
}

macro action_route($name:ident : $route:expr, |$pcx:ident $(,$arg:ident : $arg_ty:ty)*| $body:block) {
    #[get($route)]
    pub fn $name(
        sender: rocket::State<crate::PrirodaSender>
        $(,$arg:$arg_ty)*
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |$pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), (||$body)())
        })
    }
}

macro view_route($name:ident : $route:expr, |$pcx:ident $(,$arg:ident : $arg_ty:ty)*| $body:block) {
    #[get($route)]
    pub fn $name(
        sender: rocket::State<crate::PrirodaSender>
        $(,$arg:$arg_ty)*
    ) -> crate::RResult<Html<String>> {
        sender.do_work(move |pcx| {
            let $pcx = &*pcx;
            (||$body)()
        })
    }
}

#[get("/please_panic")]
#[allow(unreachable_code)]
fn please_panic(sender: State<PrirodaSender>) -> RResult<()> {
    sender.do_work(|_pcx| {
        panic!("You requested a panic");
    })
}

#[cfg(not(feature = "static_resources"))]
#[get("/resources/<path..>")]
fn resources(path: PathBuf) -> Result<NamedFile, std::io::Error> {
    let mut res_path = PathBuf::from("./resources/");
    res_path.push(path);
    NamedFile::open(res_path)
}

#[cfg(feature = "static_resources")]
#[get("/resources/<path..>")]
fn resources(path: PathBuf) -> Result<Content<&'static str>, std::io::Error> {
    use rocket::http::ContentType;
    use std::io::{Error, ErrorKind};
    match path.as_os_str().to_str() {
        Some("svg-pan-zoom.js") => Ok(Content(
            ContentType::JavaScript,
            include_str!("../resources/svg-pan-zoom.js"),
        )),
        Some("zoom_mir.js") => Ok(Content(
            ContentType::JavaScript,
            include_str!("../resources/zoom_mir.js"),
        )),
        Some("style-default.css") => Ok(Content(
            ContentType::CSS,
            include_str!("../resources/style-default.css"),
        )),
        Some("positioning.css") => Ok(Content(
            ContentType::CSS,
            include_str!("../resources/positioning.css"),
        )),
        _ => Err(Error::new(ErrorKind::InvalidInput, "Unknown resource")),
    }
}

#[get("/step_count")]
fn step_count(sender: State<PrirodaSender>) -> RResult<String> {
    sender.do_work(|pcx| format!("{}", pcx.step_count))
}

fn server(sender: PrirodaSender) {
    use rocket::config::Value;
    rocket::ignite()
        .manage(sender)
        .mount("/", routes![please_panic, resources, step_count])
        .mount("/", render::routes::routes())
        .mount("/breakpoints", step::bp_routes::routes())
        .mount("/step", step::step_routes::routes())
        .mount("/watch", watch::routes())
        .attach(rocket::fairing::AdHoc::on_launch(
            "Priroda, because code has no privacy rights",
            |rocket| {
                let config = rocket.config();
                if config.extras.get("spawn_browser") == Some(&Value::Boolean(true)) {
                    let addr = format!("http://{}:{}", config.address, config.port);
                    if open::that(&addr).is_err() {
                        println!("open {} in your browser", addr);
                    }
                }
            },
        ))
        .launch();
}

struct MessageStream {
    stream: std::net::TcpStream,
}

impl MessageStream {
    fn send(&mut self, msg: &[u8]) -> std::io::Result<()> {
        use std::io::Write;
        write!(self.stream, "{}:", msg.len())?;
        self.stream.write_all(msg)
    }

    fn recv_raw(&mut self) -> std::io::Result<Vec<u8>> {
        use std::io::Read;
        let mut req_len = 0usize;
        loop {
            let mut buf = [0; 1];
            if self.stream.read(&mut buf)? != 1 {
                panic!();
            }
            if &buf == b":" {
                break;
            } else {
                assert!(buf[0] >= b'0' && buf[0] <= b'9');
                req_len *= 10;
                req_len += (buf[0] - b'0') as usize;
            }
        }

        let mut buf = vec![0; req_len];
        self.stream.read_exact(&mut buf)?;
        Ok(buf)
    }

    fn recv(&mut self) -> std::io::Result<RequestMessage> {
        Ok(serde_json::from_slice(&*self.recv_raw()?).unwrap())
    }
}

#[derive(Debug, Deserialize)]
struct RequestMessage {
    to: String,
    #[serde(rename = "type")]
    type_: String,
    #[serde(flatten)]
    rest: std::collections::HashMap<String, serde_json::Value>,
}

fn ff_dbg_server(sender: PrirodaSender) {
    use std::io::Write;
    std::thread::spawn(move || {
        let listener = std::net::TcpListener::bind("127.0.0.1:6081").unwrap();
        println!("Listening");
        for stream in listener.incoming() {
            let mut stream = MessageStream {
                stream: stream.unwrap(),
            };

            /*let mut server = MessageStream {
                stream: std::net::TcpStream::connect("127.0.0.1:6080").unwrap(),
            };

            loop {
                let msg = server.recv_raw().unwrap();
                println!("<-{}", String::from_utf8_lossy(&msg));
                stream.send(&*msg).unwrap();

                let msg = stream.recv_raw().unwrap();
                println!("->{}", String::from_utf8_lossy(&msg));
                server.send(&*msg).unwrap();
            }*/

            println!("Accepted");

            let msg = r#"{"applicationType":"browser","from":"root","testConnectionPrefix":"server1.conn9.","traits":{"bulk":true,"heapSnapshots":true,"perfActorVersion":1,"sources":true,"watchpoints":true,"webConsoleCommands":true}}"#;

            stream.send(msg.as_bytes()).unwrap();

            loop {
                let msg = stream.recv().unwrap();
                println!("{:?}", msg);
                match &*msg.type_ {
                    "getRoot" => {
                        stream.send(br#"{"actorRegistryActor":"server1.conn9.actorRegistryActor2","deviceActor":"server1.conn9.deviceActor4","from":"root","heapSnapshotFileActor":"server1.conn9.heapSnapshotFileActor5","perfActor":"server1.conn9.perfActor6","preferenceActor":"server1.conn9.preferenceActor1"}"#).unwrap();
                    }
                    "getDescription" => {
                        match &*msg.to {
                            "server1.conn9.deviceActor4" => {
                                // FIXME remove as much as possible
                                stream.send(br#"{"from":"server1.conn9.deviceActor4","value":{"appbuildid":"20191227034945","appid":"{ec8030f7-c20a-464f-9b0e-13a3a9e97384}","apptype":"priroda","arch":"miri","brandName":"Priroda, because code has no privacy rights","canDebugServiceWorkers":false,"channel":"aurora","geckobuildid":"20191227034945","geckoversion":"72.0","isParentInterceptEnabled":false,"platformbuildid":"20191227034945","platformversion":"72.0","processor":"x86_64","profile":"ff-devtools-dbg-test","useragent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0","vendor":"Mozilla","version":"72.0"}}"#).unwrap();
                            }
                            _ => println!("Unknown actor"),
                        }
                    }
                    "getBoolPref" => {
                        match &*msg.to {
                            "server1.conn9.preferenceActor1" => {}
                            _ => println!("Wrong actor to send getBoolPref to"),
                        }
                        match msg.rest["value"].as_str().unwrap() {
                            "devtools.debugger.prompt-connection" | "browser.privatebrowsing.autostart" | "dom.serviceWorkers.enabled" => stream.send(br#"{"from":"server1.conn9.preferenceActor1","value":false}"#).unwrap(),
                            _ => println!("Unknown pref"),
                        }
                    }
                    "listAddons" => {
                        match &*msg.to {
                            "root" => stream.send(br#"{"addons":[],"from":"root"}"#).unwrap(),
                            _ => println!("Wrong actor to send listAddons to"),
                        }
                    }
                    "listTabs" => {
                        stream.send(br#"{
                            "actorRegistryActor":"server1.conn9.actorRegistryActor2",
                            "addonsActor":"server1.conn9.addonsActor3",
                            "deviceActor":"server1.conn9.deviceActor4",
                            "from":"root",
                            "heapSnapshotFileActor":"server1.conn9.heapSnapshotFileActor5",
                            "perfActor":"server1.conn9.perfActor6",
                            "preferenceActor":"server1.conn9.preferenceActor1",
                            "selected":0,
                            "tabs":[
                                {
                                    "accessibilityActor":"server1.conn9.child26/accessibilityActor14",
                                    "actor":"server1.conn9.child26/frameTarget1",
                                    "animationsActor":"server1.conn9.child26/animationsActor11",
                                    "browsingContextID":4,
                                    "changesActor":"server1.conn9.child26/changesActor16",
                                    "consoleActor":"server1.conn9.child26/consoleActor2",
                                    "cssPropertiesActor":"server1.conn9.child26/cssPropertiesActor9",
                                    "emulationActor":"server1.conn9.child26/emulationActor12",
                                    "favicon":null,
                                    "framerateActor":"server1.conn9.child26/framerateActor7",
                                    "inspectorActor":"server1.conn9.child26/inspectorActor3",
                                    "manifestActor":"server1.conn9.child26/manifestActor18",
                                    "memoryActor":"server1.conn9.child26/memoryActor6",
                                    "outerWindowID":6,
                                    "performanceActor":"server1.conn9.child26/performanceActor10",
                                    "reflowActor":"server1.conn9.child26/reflowActor8",
                                    "screenshotActor":"server1.conn9.child26/screenshotActor15",
                                    "storageActor":"server1.conn9.child26/storageActor5",
                                    "styleSheetsActor":"server1.conn9.child26/styleSheetsActor4",
                                    "title":"Nieuw tabblad",
                                    "traits":{"isBrowsingContext":true},
                                    "url":"about:home",
                                    "webExtensionInspectedWindowActor":"server1.conn9.child26/webExtensionInspectedWindowActor13",
                                    "webSocketActor":"server1.conn9.child26/webSocketActor17"
                                }
                            ]
                        }"#).unwrap();
                    }
                    "getTab" => {
                        match &*msg.to {
                            "root" => {
                                match msg.rest["outerWindowID"].as_u64().unwrap() {
                                    6 => {
                                        //stream.send(br#"{"type":"frameUpdate","frames":[{"id":6,"url":"https://github.com/","title":"The world's leading software development platform | GitHub"}],"from":"server1.conn39.child26/frameTarget1"}"#).unwrap();
                                        stream.send(br#"{"tab":{"actor":"server1.conn39.child26/frameTarget1","browsingContextID":4,"traits":{"isBrowsingContext":true},"title":"The world's leading software development platform | GitHub","url":"https://github.com/","outerWindowID":6,"consoleActor":"server1.conn39.child26/consoleActor2","inspectorActor":"server1.conn39.child26/inspectorActor3","styleSheetsActor":"server1.conn39.child26/styleSheetsActor4","storageActor":"server1.conn39.child26/storageActor5","memoryActor":"server1.conn39.child26/memoryActor6","framerateActor":"server1.conn39.child26/framerateActor7","reflowActor":"server1.conn39.child26/reflowActor8","cssPropertiesActor":"server1.conn39.child26/cssPropertiesActor9","performanceActor":"server1.conn39.child26/performanceActor10","animationsActor":"server1.conn39.child26/animationsActor11","emulationActor":"server1.conn39.child26/emulationActor12","webExtensionInspectedWindowActor":"server1.conn39.child26/webExtensionInspectedWindowActor13","accessibilityActor":"server1.conn39.child26/accessibilityActor14","screenshotActor":"server1.conn39.child26/screenshotActor15","changesActor":"server1.conn39.child26/changesActor16","webSocketActor":"server1.conn39.child26/webSocketActor17","manifestActor":"server1.conn39.child26/manifestActor18"},"from":"root"}"#).unwrap();
                                    }
                                    _ => println!("Unknown outerWindowID")
                                }
                            }
                            _ => println!("Wrong actor to send getTab to"),
                        }
                    }
                    "getProcess" => {
                        match &*msg.to {
                            "root" => {
                                match msg.rest["id"].as_u64().unwrap() {
                                    0 => stream.send(br#"{"form":{"actor":"server1.conn9.processDescriptor28","id":0,"isParent":true},"from":"root"}"#).unwrap(),
                                    _ => println!("Unknown process id"),
                                }
                            }
                            _ => println!("Wrong actor to send listProcess to"),
                        }
                    }
                    "listProcesses" => {
                        match &*msg.to {
                            "root" => {
                                stream.send(br#"{"from":"root","processes":[{"actor":"server1.conn9.processDescriptor28","id":0,"isParent":true}]}"#).unwrap();
                            }
                            _ => println!("Wrong actor to send listProcess to"),
                        }
                    }
                    "listWorkers" => {
                        stream.send(br#"{"from":"root","workers":[]}"#).unwrap();
                    }
                    "listServiceWorkerRegistrations" => {
                        stream.send(br#"{"from":"root","registrations":[]}"#).unwrap();
                    }
                    "attach" => {
                        match &*msg.to {
                            "server1.conn39.child26/frameTarget1" => {
                                stream.send(br#"{"type":"tabAttached","threadActor":"server1.conn39.thread53","cacheDisabled":false,"javascriptEnabled":true,traits:{},"from":"server1.conn39.child26/frameTarget1"}"#).unwrap();
                            }
                            _ => println!("Unknown actor"),
                        }
                    }
                    _ => println!("Unknown message"),
                }
            }

            // recv 30:{"type":"getRoot","to":"root"}
            // send 311:{"preferenceActor":"server1.conn2.preferenceActor1","actorRegistryActor":"server1.conn2.actorRegistryActor2","addonsActor":"server1.conn2.addonsActor3","deviceActor":"server1.conn2.deviceActor4","heapSnapshotFileActor":"server1.conn2.heapSnapshotFileActor5","perfActor":"server1.conn2.perfActor6","from":"root"}

            // send 59:{"to":"server1.conn2.deviceActor4","type":"getDescription"}
            // send 824:{"value":{"appid":"{ec8030f7-c20a-464f-9b0e-13a3a9e97384}","apptype":"firefox","vendor":"Mozilla","name":"Firefox","version":"72.0","appbuildid":"20191227034945","platformbuildid":"20191227034945","geckobuildid":"20191227034945","platformversion":"72.0","geckoversion":"72.0","locale":"nl","endianness":"LE","hostname":"iMac","os":"Darwin","platform":"Darwin","hardware":"unknown","deviceName":null,"arch":"x86_64","processor":"x86_64","compiler":"gcc3","profile":"ff-devtools-dbg-test","channel":"aurora","dpi":109,"useragent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0","width":2560,"height":1440,"physicalWidth":2560,"physicalHeight":1440,"brandName":"Firefox Developer Edition","canDebugServiceWorkers":false,"isParentInterceptEnabled":false},"from":"server1.conn5.deviceActor4"}
        }
    });
}

// Copied from miri/bin/miri.rs
fn find_sysroot() -> String {
    if let Ok(sysroot) = std::env::var("MIRI_SYSROOT") {
        return sysroot;
    }

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

    // setup http server and similar
    let (sender, receiver) = std::sync::mpsc::channel();
    let sender = PrirodaSender(Arc::new(Mutex::new(sender)));
    let step_count = Arc::new(Mutex::new(0));
    let config = Arc::new(Mutex::new(Config::default()));

    let handle = std::thread::spawn(move || {
        let args = Arc::new(args);
        let receiver = Arc::new(Mutex::new(receiver));
        for i in 0..5 {
            if i != 0 {
                println!(
                    "\n============== Miri crashed - restart try {} ==============\n",
                    i
                );
            }
            let step_count = step_count.clone();
            let config = config.clone();
            let receiver = receiver.clone();
            let args = args.clone();
            // Ignore result to restart in case of a crash
            let _ = std::thread::spawn(move || {
                let _ = rustc_driver::catch_fatal_errors(move || {
                    struct PrirodaCompilerCalls {
                        step_count: Arc<Mutex<u128>>,
                        config: Arc<Mutex<Config>>,
                        receiver: Arc<
                            Mutex<
                                std::sync::mpsc::Receiver<
                                    Box<dyn FnOnce(&mut PrirodaContext) + Send>,
                                >,
                            >,
                        >,
                    }

                    impl rustc_driver::Callbacks for PrirodaCompilerCalls {
                        fn after_analysis<'tcx>(&mut self, compiler: &interface::Compiler, queries: &'tcx rustc_interface::Queries<'tcx>) -> Compilation {
                            compiler.session().abort_if_errors();

                            queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
                                let mut step_count = self
                                    .step_count
                                    .lock()
                                    .unwrap_or_else(|err| err.into_inner());
                                let mut config =
                                    self.config.lock().unwrap_or_else(|err| err.into_inner());

                                let mut pcx = PrirodaContext {
                                    ecx: create_ecx(tcx),
                                    step_count: &mut *step_count,
                                    traces: watch::Traces::new(),
                                    config: &mut *config,
                                };

                                // Step to the position where miri crashed if it crashed
                                for _ in 0..*pcx.step_count {
                                    match pcx.ecx.step() {
                                        Ok(true) => {}
                                        res => panic!(
                                            "Miri is not deterministic causing error {:?}",
                                            res
                                        ),
                                    }
                                }

                                // Just ignore poisoning by panicking
                                let receiver =
                                    self.receiver.lock().unwrap_or_else(|err| err.into_inner());

                                // process commands
                                for command in receiver.iter() {
                                    command(&mut pcx);
                                }
                            });

                            compiler.session().abort_if_errors();

                            Compilation::Stop
                        }
                    }

                    rustc_driver::run_compiler(
                        &*args,
                        &mut PrirodaCompilerCalls {
                            step_count,
                            config,
                            receiver,
                        },
                        None,
                        None,
                    )
                });
            })
            .join();
            std::thread::sleep(std::time::Duration::from_millis(200));
        }
        println!("\n============== Miri crashed too often. Aborting ==============\n");
    });
    ff_dbg_server(sender.clone());
    server(sender);
    handle.join().unwrap();
}

fn init_logger() {
    const NSPACES: usize = 40;
    let format = |_fmt: &mut _, record: &log::Record| {
        // prepend spaces to indent the final string
        let indentation = log_settings::settings().indentation;
        println!(
            "{lvl}:{module}{depth:2}{indent:<indentation$} {text}",
            lvl = record.level(),
            module = record.module_path().unwrap_or(""),
            depth = indentation / NSPACES,
            indentation = indentation % NSPACES,
            indent = "",
            text = record.args()
        );
        Ok(())
    };

    let mut builder = env_logger::Builder::new();
    builder.format(format).filter(None, log::LevelFilter::Info);

    if std::env::var("MIRI_LOG").is_ok() {
        builder.parse(&std::env::var("MIRI_LOG").unwrap());
    }

    builder.init();
}
