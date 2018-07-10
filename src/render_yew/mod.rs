use yew::prelude::*;

type Context = ();

struct MainWindow {}

enum MainWindowMsg {
    
}

impl Component<Context> for MainWindow {
    type Message = MainWindowMsg;
    type Properties = ();

    fn create(_: (), _: &mut Env<Context, Self>) -> Self {
        MainWindow {}
    }

    fn update(&mut self, msg: Self::Message, _: &mut Env<Context, Self>) -> ShouldRender {
        match msg {}
    }
}

impl Renderable<Context, MainWindow> for MainWindow {
    fn view(&self) -> Html<Context, Self> {
        /*let is_active_stack_frame = match display_frame {
            Some(n) => n == pcx.ecx.stack().len() - 1,
            None => true,
        };
        let frame = display_frame
            .and_then(|frame| pcx.ecx.stack().get(frame))
            .or_else(|| pcx.ecx.stack().last());
        let stack: Vec<(String, String, String)> = pcx
            .ecx
            .stack()
            .iter()
            .map(|&Frame { instance, span, .. }| {
                (
                    if pcx
                        .ecx
                        .tcx
                        .def_key(instance.def_id())
                        .disambiguated_data
                        .data == DefPathData::ClosureExpr
                    {
                        "inside call to closure".to_string()
                    } else {
                        instance.to_string()
                    },
                    pcx.ecx.tcx.sess.codemap().span_to_string(span),
                    format!("{:?}", instance.def_id()),
                )
            })
            .collect();
        let rendered_breakpoints: Vec<String> = pcx
            .config
            .bptree
            .iter()
            .map(|&Breakpoint(def_id, bb, stmt)| format!("{:?}@{}:{}", def_id, bb.index(), stmt))
            .collect();
        use rustc_data_structures::indexed_vec::Idx;
        let rendered_locals = locals::render_locals(&pcx.ecx, frame);

        #[cfg(feature = "render_source")]
        let rendered_source = source::render_source(pcx.ecx.tcx.tcx, frame);
        #[cfg(not(feature = "render_source"))]
        let rendered_source = String::new();

        let mir_graph = frame.map(|frame| {
            graphviz::render_html(frame, pcx.config.bptree.for_def_id(frame.instance.def_id()))
        });

        let filename = pcx
            .ecx
            .tcx
            .sess
            .local_crate_source_file
            .as_ref()
            .map(|f| f.display().to_string())
            .unwrap_or_else(|| "no file name".to_string());*/

        let command_html = if is_active_stack_frame {
            html! {
                <>
                    <a href="/step/single",><div title="Execute next MIR statement/terminator",>{ "Step" }</a>
                    <a href="/step/next",><div title="Run until after the next MIR statement/terminator",>{ "Next" }</a>
                    <a href="/step/return",><div title="Run until the function returns",>{ "Return" }</a>
                    <a href="/step/single_back",><div title="Execute previous MIR statement/terminator (restarts and steps till one stmt before the current stmt)",>{ "Step back (slow)" }</a>
                    <a href="/step/continue",><div title="Run until termination or breakpoint",>{ "Continue" }</a>
                    <a href="/step/restart",><div title="Abort execution and restart",>{ "Restart" }</a>
                    <a href="/breakpoints/add_here",><div title="Add breakpoint at current location",>{ "Add breakpoint here" }</a>
                    <a href="/breakpoints/remove_all",><div title="Remove all breakpoints",>{ "Remove all breakpoints" }</a>
                </>
            }
        } else {
            html! {
                <a href="/",><div title="Go to active stack frame",>{ "Go back to active stack frame" }</a>
            }
        };
        html! {
            <>
                <div id="left",>
                    <div id="commands",>
                        { command_html }
                    </div>
                    <div id="messages",>
                        <p>{ message }</p>
                    </div>
                    <div id="mir",>
                        { Raw(mir_graph.unwrap_or_else(|| "no current function".to_string())) }
                    </div>
                </div>
                <div id="right",>
                    <div>
                        { format!("Step count: {}", pcx.step_count) }
                    </div>
                    <div id="stack",>
                        <table border="1",>
                            {
                                for stack.iter().enumerate().rev().map(|(i, &(ref s, ref span, ref def_id))| {
                                    html! {
                                        <tr>
                                            {
                                                if i == display_frame.unwrap_or(stack.len() - 1) {
                                                    html! { <td>{ Raw("&#8594;") }</td> }
                                                } else {
                                                    html! { <td></td> }
                                                }
                                            }
                                            <td>{ s }</td>
                                            <td>{ span }</td>
                                            <td>{ def_id }</td>
                                            @ if i == display_frame.unwrap_or(stack.len() - 1) { td; } else { td { a(href=format!("/frame/{}", i)) { : "View" } } }
                                        </tr>
                                    }
                                })
                            }
                        </table>
                    </div>
                    <div id="breakpoints",>
                        { "Breakpoints:" } //<br />
                        <table border="1",>
                            /*@ for bp in rendered_breakpoints {
                                <tr>
                                    <td>{ &bp }</td>
                                    <td><a href=format!("/breakpoints/remove/{}", bp)>remove</td>
                                </tr>
                            }*/
                        </table>
                    </div>
                    <div id="locals",>
                        { Raw(rendered_locals) }
                    </div>
                    <div id="source",>
                        { rendered_source }
                    </div>
                </div>
            </>
        }
    }
}