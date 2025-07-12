# Simulador de Falhas Nucleares

Um serious game/simulador de reator nuclear para treinamento, estudo e diversão, usando Ada (núcleo físico), Rust (FFI e UI), e WASM (frontend interativo).

## Estrutura

```
reactor-sim/
├── ada_core/     # Núcleo físico/simulação (Ada)
├── rust_ui/      # FFI + UI WASM (Rust, yew)
├── wasm_build/   # Frontend pronto para deploy (static)
└── README.md
```

## Como funciona

1. **Ada** modela o reator, expõe funções via C ABI.
2. **Rust** (FFI) chama Ada, expõe para o frontend.
3. **Rust (yew)** UI interativa, renderiza em WASM, chama FFI para alterar/ler estado.
4. **Frontend** mostra painel, gráficos, controles, alertas.

## Build Local

**Pré-requisitos:**
- Rust (`rustup`)
- Ada (GNAT)
- wasm-pack (`cargo install wasm-pack`)
- cbindgen (para headers do FFI)
- Python3 (opcional, para scripts)

**1. Compilar Ada como lib compartilhada:**
```sh
cd ada_core/src
gnatmake -fPIC -shared -o ../build/libreactor.so reactor.adb
# Gera ../build/libreactor.so
```

**2. Gerar bindings FFI (opcional):**
```sh
cbindgen --lang c --output rust_ui/reactor_ffi.h
```

**3. Compilar Frontend (Rust→WASM):**
```sh
cd rust_ui
wasm-pack build --target web --out-dir ../wasm_build/pkg
```

**4. Rodar local:**
```sh
cd wasm_build
python3 -m http.server 8080
# Acesse http://localhost:8080
```

## Extensões sugeridas

- Novos cenários: diferentes tipos de reator.
- Gráficos em tempo real (plotters).
- Exportação de logs/CSV.
- Procedimentos de mitigação interativos.
- Modo multiplayer (um usuário ativa falhas, outro mitiga).

---

**Este projeto é um ponto de partida para estudos de Engenharia Aerospacial, aprendendo Ada e Rust.**