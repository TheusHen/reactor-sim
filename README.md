# Nuclear Failure Simulator in the Terminal (CLI Game)

A serious game in the terminal simulating a nuclear reactor, with physics in Ada and a Rust interface.

**This project was developed using Rust and Ada to deepen and broaden my skillset, especially geared towards applications in Aerospace Engineering.** - **[More about my journey](https://www.theushen.me)**

## Structure

```
reactor-sim-terminal/
├── ada_core/      # Physics/simulation core in Ada
├── rust_cli/      # Terminal interface and FFI bridge in Rust
├── Makefile       # Simplified build
└── README.md
```

## How to Run

### Prerequisites

- [Rust](https://rustup.rs/)
- [GNAT Ada Compiler](https://www.adacore.com/download)
- `gprbuild` (to compile Ada)
- `make` (Linux/macOS)

### Build

1. Compile everything:
   ```
   make
   ```

2. Run the simulator:
   ```
   LD_LIBRARY_PATH=./ada_core/build ./rust_cli/target/release/reactor_sim_cli
   ```

> If you encounter FFI linking issues, see the tips at the end of this file.

## Components

- **ada_core/**: Reactor physics, state control, failures.
- **rust_cli/**: Dynamic CLI, interaction, logs, animations, FFI bridge.

## Features

- Real-time reactor panel.
- Failure injection via keyboard.
- Exportable logs in CSV.
- Terminal animations and alerts.
- Customizable scenarios and failures.

## FFI Build Tips

- On Linux, Ada generates a `.so` and `.h` header for Rust to consume.
- On Windows, you may need to adjust the `Makefile` and `Cargo.toml` for DLL.
- Check paths in the build files.