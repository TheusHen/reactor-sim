use yew::prelude::*;
use crate::ffi::*;

#[function_component(ReactorPanel)]
pub fn reactor_panel() -> Html {
    let state = use_state(|| unsafe {
        reactor_initialize();
        reactor_get_state()
    });

    let update_state = {
        let state = state.clone();
        Callback::from(move |_| {
            unsafe { reactor_step(); }
            let new_state = unsafe { reactor_get_state() };
            state.set(new_state);
        })
    };

    let set_pump = {
        let state = state.clone();
        Callback::from(move |val: f64| {
            unsafe { reactor_set_pump_speed(val as f32); }
            let s = unsafe { reactor_get_state() };
            state.set(s);
        })
    };

    let inject_fault = {
        let state = state.clone();
        Callback::from(move |id: i32| {
            unsafe { reactor_inject_fault(id); }
            let s = unsafe { reactor_get_state() };
            state.set(s);
        })
    };

    let rs = *state;

    html! {
        <div style="margin:2rem">
            <h1>{ "Simulador de Falhas Nucleares" }</h1>
            <p>{ format!("Temperatura: {:.1}°C", rs.core_temperature) }</p>
            <p>{ format!("Pressão: {:.1} bar", rs.pressure) }</p>
            <p>{ format!("Vazão do Refrigerante: {:.1}", rs.coolant_flow) }</p>
            <p>{ format!("Bomba: {}", if rs.pump_on { "Ligada" } else { "Desligada" }) }</p>
            <p>{ format!("Válvula: {:.0}%", rs.valve_position*100.) }</p>
            <p>{ format!("Barras de Controle: {:.0}%", rs.control_rods*100.) }</p>
            if rs.alert_flag {
                <div style="color:red">
                    <strong>{ "ALERTA: " }{ rs.fault_message() }</strong>
                </div>
            }
            <div style="margin:1rem 0">
                <label>{ "Velocidade da bomba: " }
                    <input type="range" min="0" max="1" step="0.01"
                        value={rs.coolant_flow.to_string()}
                        oninput={set_pump.reform(|e: InputEvent| {
                            let val = e.target_unchecked_into::<web_sys::HtmlInputElement>().value().parse().unwrap_or(0.0);
                            val
                        })} />
                </label>
            </div>
            <button onclick={update_state}>{ "Avançar Simulação" }</button>
            <button onclick={inject_fault.reform(|_| 1)}>{ "Falha: Bomba" }</button>
            <button onclick={inject_fault.reform(|_| 2)}>{ "Falha: Válvula" }</button>
            <button onclick={inject_fault.reform(|_| 3)}>{ "Falha: Barras de Controle" }</button>
        </div>
    }
}
