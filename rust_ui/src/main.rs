mod ffi;
mod components;

use yew::prelude::*;
use components::ReactorPanel;

#[function_component(App)]
fn app() -> Html {
    html! { <ReactorPanel /> }
}

fn main() {
    yew::start_app::<App>();
}
