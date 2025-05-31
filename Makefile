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

.PHONY: bench-time-thinTh-samples1
bench-time-thinTh-samples1: data/bench-time-fast-integer-thinTh-samples1.csv data/bench-time-fast-word-thinTh-samples1.csv data/bench-time-safe-thinTh-samples1.csv

data/bench-time-fast-integer-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples1.csv

data/bench-time-fast-word-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples1.csv

data/bench-time-safe-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples1.csv

.PHONY: bench-time-thinTh-samples1-cyberglot
bench-time-thinTh-samples1-cyberglot: data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv data/bench-time-fast-word-thinTh-samples1-cyberglot.csv data/bench-time-safe-thinTh-samples1-cyberglot.csv

data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv

data/bench-time-fast-word-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples1-cyberglot.csv

data/bench-time-safe-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples1-cyberglot.csv
