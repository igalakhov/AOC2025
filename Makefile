SOURCES = src/**

aoc: aoc.mlb $(SOURCES)
	mlton aoc.mlb

fmt: aoc.mlb $(SOURCES)
	smlfmt --force aoc.mlb
