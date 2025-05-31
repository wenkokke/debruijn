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

################################################################################
# Benchmark: Thinning Thinnings
################################################################################

################################################################################
# Benchmark: Thinning Thinnings -- Samples 1

.PHONY: bench-time-thinTh-samples1
bench-time-thinTh-samples1: data/bench-time-fast-integer-thinTh-samples1.csv data/bench-time-fast-word-thinTh-samples1.csv data/bench-time-safe-thinTh-samples1.csv

data/bench-time-fast-integer-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples1.csv

data/bench-time-fast-word-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples1.csv

data/bench-time-safe-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples1.csv

################################################################################
# Benchmark: Thinning Thinnings -- Samples 1 -- Cyberglot

.PHONY: bench-time-thinTh-samples1-cyberglot
bench-time-thinTh-samples1-cyberglot: data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv data/bench-time-fast-word-thinTh-samples1-cyberglot.csv data/bench-time-safe-thinTh-samples1-cyberglot.csv

data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv

data/bench-time-fast-word-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples1-cyberglot.csv

data/bench-time-safe-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples1-cyberglot.csv

################################################################################
# Benchmark: Thinning Thinnings -- Samples 2

.PHONY: bench-time-thinTh-samples2
bench-time-thinTh-samples2: data/bench-time-fast-integer-thinTh-samples2.csv data/bench-time-fast-word-thinTh-samples2.csv data/bench-time-safe-thinTh-samples2.csv

data/bench-time-fast-integer-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples2.csv

data/bench-time-fast-word-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples2.csv

data/bench-time-safe-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples2.csv

################################################################################
# Benchmark: Thinning Thinnings -- Samples 2 -- Cyberglot

.PHONY: bench-time-thinTh-samples2-cyberglot
bench-time-thinTh-samples2-cyberglot: data/bench-time-fast-integer-thinTh-samples2-cyberglot.csv data/bench-time-fast-word-thinTh-samples2-cyberglot.csv data/bench-time-safe-thinTh-samples2-cyberglot.csv

data/bench-time-fast-integer-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples2-cyberglot.csv

data/bench-time-fast-word-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples2-cyberglot.csv

data/bench-time-safe-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples2-cyberglot.csv

################################################################################
# Benchmark: Thinning Thinnings -- Samples 3

.PHONY: bench-time-thinTh-samples3
bench-time-thinTh-samples3: data/bench-time-fast-integer-thinTh-samples3.csv data/bench-time-fast-word-thinTh-samples3.csv data/bench-time-safe-thinTh-samples3.csv

data/bench-time-fast-integer-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples3.csv

data/bench-time-fast-word-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples3.csv

data/bench-time-safe-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples3.csv

################################################################################
# Benchmark: Thinning Thinnings -- Samples 3 -- Cyberglot

.PHONY: bench-time-thinTh-samples3-cyberglot
bench-time-thinTh-samples3-cyberglot: data/bench-time-fast-integer-thinTh-samples3-cyberglot.csv data/bench-time-fast-word-thinTh-samples3-cyberglot.csv data/bench-time-safe-thinTh-samples3-cyberglot.csv

data/bench-time-fast-integer-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-integer-thinTh-samples3-cyberglot.csv

data/bench-time-fast-word-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-fast-word-thinTh-samples3-cyberglot.csv

data/bench-time-safe-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-thinning-as-word -- 'Bench.Data.DeBruijn.Thinning/' --csv data/bench-time-safe-thinTh-samples3-cyberglot.csv
