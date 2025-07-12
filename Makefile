all:
	cd ada_core && make
	cd rust_cli && cargo build --release
