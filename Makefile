
all:
	stack build

repl:
	stack repl

lint:
	hlint .

run:
	stack run