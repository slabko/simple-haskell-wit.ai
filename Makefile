all: build run

build:
	stack build 

test:
	stack test

run: build
	stack exec cli
