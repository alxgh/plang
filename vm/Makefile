build:
	zig build

run: build
	./zig-out/bin/vm

FILE ?= main.p

exec: build
	./zig-out/bin/vm $(FILE)