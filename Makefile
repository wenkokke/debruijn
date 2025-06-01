SOURCE_FILES := data-debruijn.cabal $(shell git ls-files '*.hs')

################################################################################
# Benchmark: Indexes
################################################################################

################################################################################
# Benchmark: Indexes -- Space

.PHONY: bench-space-ix
bench-space-ix: \
	data/bench-space-fast-int-ix.csv \
	data/bench-space-fast-word8-ix.csv \
	data/bench-space-safe-ix.csv

data/bench-space-fast-int-ix.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=-safe --flags=-snat-as-word8 --flags=-ix-as-word8 >$@)

data/bench-space-fast-word8-ix.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=-safe --flags=+snat-as-word8 --flags=+ix-as-word8 >$@)

data/bench-space-safe-ix.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=+safe --flags=-snat-as-word8 --flags=-ix-as-word8 >$@)

################################################################################
# Benchmark: Indexes -- Time

.PHONY: bench-time-ix
bench-time-ix: \
	data/bench-time-fast-int-ix.csv \
	data/bench-time-fast-word8-ix.csv \
	data/bench-time-safe-ix.csv

data/bench-time-fast-int-ix.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-snat-as-word8 --flags=-ix-as-word8 -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-fast-word8-ix.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+snat-as-word8 --flags=+ix-as-word8 -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-safe-ix.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-snat-as-word8 --flags=-ix-as-word8 -- 'Bench.Data.DeBruijn.Index/' --csv=$@

################################################################################
# Benchmark: Indexes -- Space -- Cyberglot

.PHONY: bench-space-ix-cyberglot
bench-space-ix-cyberglot: \
	data/bench-space-fast-int-ix-cyberglot.csv \
	data/bench-space-fast-word8-ix-cyberglot.csv \
	data/bench-space-safe-ix-cyberglot.csv

data/bench-space-fast-int-ix-cyberglot.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=-safe --flags=-snat-as-word8 --flags=-ix-as-word8 >$@)

data/bench-space-fast-word8-ix-cyberglot.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=-safe --flags=+snat-as-word8 --flags=+ix-as-word8 >$@)

data/bench-space-safe-ix-cyberglot.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags=+safe --flags=-snat-as-word8 --flags=-ix-as-word8 >$@)

################################################################################
# Benchmark: Indexes -- Time -- Cyberglot

.PHONY: bench-time-ix-cyberglot
bench-time-ix-cyberglot: \
	data/bench-time-fast-int-ix-cyberglot.csv \
	data/bench-time-fast-word8-ix-cyberglot.csv \
	data/bench-time-safe-ix-cyberglot.csv

data/bench-time-fast-int-ix-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-snat-as-word8 --flags=-ix-as-word8 -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-fast-word8-ix-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+snat-as-word8 --flags=+ix-as-word8 -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-safe-ix-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-snat-as-word8 --flags=-ix-as-word8 -- 'Bench.Data.DeBruijn.Index/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings
################################################################################

.PHONY: bench-time-fast-bitvec-thinTh-cyberglot
bench-time-fast-bitvec-thinTh-cyberglot: \
	data/bench-time-fast-bitvec-thinTh-samples1-cyberglot.csv
	data/bench-time-fast-bitvec-thinTh-samples2-cyberglot.csv
	data/bench-time-fast-bitvec-thinTh-samples3-cyberglot.csv

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 1

.PHONY: bench-time-thinTh-samples1
bench-time-thinTh-samples1: \
	data/bench-time-fast-bitvec-thinTh-samples1.csv \
	data/bench-time-fast-integer-thinTh-samples1.csv \
	data/bench-time-fast-word-thinTh-samples1.csv \
	data/bench-time-safe-thinTh-samples1.csv

data/bench-time-fast-bitvec-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=+th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-integer-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-word-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 2

.PHONY: bench-time-thinTh-samples2
bench-time-thinTh-samples2: \
	data/bench-time-fast-bitvec-thinTh-samples2.csv \
	data/bench-time-fast-integer-thinTh-samples2.csv \
	data/bench-time-fast-word-thinTh-samples2.csv \
	data/bench-time-safe-thinTh-samples2.csv

data/bench-time-fast-bitvec-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=+th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-integer-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-word-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 3

.PHONY: bench-time-thinTh-samples3
bench-time-thinTh-samples3: \
	data/bench-time-fast-bitvec-thinTh-samples3.csv \
	data/bench-time-fast-integer-thinTh-samples3.csv \
	data/bench-time-fast-word-thinTh-samples3.csv \
	data/bench-time-safe-thinTh-samples3.csv

data/bench-time-fast-bitvec-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=+th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-integer-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-word-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 1 -- Cyberglot

.PHONY: bench-time-thinTh-samples1-cyberglot
bench-time-thinTh-samples1-cyberglot: \
	data/bench-time-fast-bitvec-thinTh-samples1-cyberglot.csv \
	data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv \
	data/bench-time-fast-word-thinTh-samples1-cyberglot.csv \
	data/bench-time-safe-thinTh-samples1-cyberglot.csv

data/bench-time-fast-bitvec-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=+th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-integer-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-word-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 2 -- Cyberglot

.PHONY: bench-time-thinTh-samples2-cyberglot
bench-time-thinTh-samples2-cyberglot: \
	data/bench-time-fast-bitvec-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-integer-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-word-thinTh-samples2-cyberglot.csv \
	data/bench-time-safe-thinTh-samples2-cyberglot.csv

data/bench-time-fast-bitvec-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=+th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-integer-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-word-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 3 -- Cyberglot

.PHONY: bench-time-thinTh-samples3-cyberglot
bench-time-thinTh-samples3-cyberglot: \
	data/bench-time-fast-bitvec-thinTh-samples3-cyberglot.csv \
	data/bench-time-fast-integer-thinTh-samples3-cyberglot.csv \
	data/bench-time-fast-word-thinTh-samples3-cyberglot.csv \
	data/bench-time-safe-thinTh-samples3-cyberglot.csv

data/bench-time-fast-bitvec-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=+th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-integer-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-word-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=-safe --flags=+th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags=+safe --flags=-th-as-word64 --flags=-th-as-bitvec -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@
