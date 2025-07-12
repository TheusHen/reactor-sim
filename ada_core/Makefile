all:
	gprbuild -Pada_core.gpr
	gcc -c -fPIC src/ffi_c.c -o build/ffi_c.o
	gcc -shared -o build/libreactor.so obj/physics.o obj/faults.o obj/reactor.o build/ffi_c.o
