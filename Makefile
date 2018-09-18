default: build run

build:
	stack build

run:
	$$(stack exec -- which duplicates)
