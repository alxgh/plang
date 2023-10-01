build:
	zig build

run: build
	./zig-out/bin/plang

FILE ?= main.p

exec: build
	./zig-out/bin/plang $(FILE)