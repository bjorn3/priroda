use std::cell::{Cell, RefCell};
use std::rc::Rc;

use rustc::hir::def_id::LOCAL_CRATE;
use rustc::middle::privacy::AccessLevels;
use rustc_metadata::creader::CrateLoader;
use rustc_metadata::cstore::CStore;
use rustc_resolve::Resolver;

use rustc_data_structures::fx::{FxHashMap, FxHashSet};

use rustdoc::clean::{self, Clean};
use rustdoc::core::DocContext;
use rustdoc::externalfiles::ExternalHtml;
use rustdoc::visit_ast::RustdocVisitor;

use PrirodaContext;

pub fn init(pcx: &PrirodaContext) {
    let tcx = pcx.ecx.tcx.tcx;
    let crate_name = tcx.crate_name(LOCAL_CRATE);
    let codegen_backend = ::rustc_driver::get_codegen_backend(tcx.sess);
    let cstore = Rc::new(CStore::new(codegen_backend.metadata_loader()));
    let mut crate_loader = CrateLoader::new(tcx.sess, &*cstore, &crate_name.as_str());
    let resolver_arenas = Resolver::arenas();
    let resolver = RefCell::new(Resolver::new(
        tcx.sess,
        &*cstore,
        &pcx.ast_crate,
        &crate_name.as_str(),
        ::rustc_resolve::MakeGlobMap::Yes,
        &mut crate_loader,
        &resolver_arenas,
    ));

    let access_levels = AccessLevels { map: FxHashMap() };

    let ctxt = DocContext {
        tcx,
        resolver: &resolver,
        crate_name: Some(crate_name.to_string()),
        cstore: cstore.clone(),
        access_levels: RefCell::new(access_levels),

        populated_all_crate_impls: Cell::new(false),
        external_traits: Default::default(),
        active_extern_traits: Default::default(),
        renderinfo: Default::default(),
        ty_substs: Default::default(),
        lt_substs: Default::default(),
        impl_trait_bounds: Default::default(),
        mod_ids: Default::default(),
        send_trait: if crate_name == "core" {
            clean::path_to_def_local(&tcx, &["marker", "Send"])
        } else {
            clean::path_to_def(&tcx, &["core", "marker", "Send"])
        },
        fake_def_ids: RefCell::new(FxHashMap()),
        all_fake_def_ids: RefCell::new(FxHashSet()),
        generated_synthetics: RefCell::new(FxHashSet()),
    };

    let krate = {
        let mut v = RustdocVisitor::new(&ctxt);
        v.visit(tcx.hir.krate());
        v.clean(&ctxt)
    };

    let external_html = ExternalHtml {
        in_header: String::new(),
        before_content: String::new(),
        after_content: String::new(),
    };
    let sort_modules_alphabetically = true;
    let enable_minification = true;

    ::rustdoc::html::render::run(
        krate,
        &external_html,
        None,
        ::std::path::PathBuf::from("target/priroda_generated_docs"),
        String::new(),
        vec![
            String::from("collapse-docs"),
            String::from("unindent-comments"),
        ]
            .into_iter()
            .collect(),
        None,
        ctxt.renderinfo.into_inner(),
        sort_modules_alphabetically,
        vec![],
        enable_minification,
    ).expect("failed to generate documentation");
}
