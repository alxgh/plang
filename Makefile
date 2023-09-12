build:
	zig build

run: build
	./zig-out/bin/plang

exec: build
	./zig-out/bin/plang main.p