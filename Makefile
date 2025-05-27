SOURCE_FILES := data-debruijn.cabal $(shell git ls-files '*.hs')

.PHONY: bench
bench: data/bench-space-fast.csv data/bench-space-safe.csv data/bench-time-fast.csv data/bench-time-safe.csv

data/bench-space-fast.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=-safe >$@)

data/bench-space-safe.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=+safe >$@)

data/bench-time-fast.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe -- --csv=$@

data/bench-time-safe.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe -- --csv=$@
