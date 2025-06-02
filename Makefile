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
	time (cabal run bench-space -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 -safe' >$@)

data/bench-space-fast-word8-ix.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags='-snat-as-int +snat-as-word8 -ix-as-int +ix-as-word8 -safe' >$@)

data/bench-space-safe-ix.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 +safe' >$@)

################################################################################
# Benchmark: Indexes -- Time

.PHONY: bench-time-ix
bench-time-ix: \
	data/bench-time-fast-ix-as-int.csv \
	data/bench-time-fast-th-as-word8-ix.csv \
	data/bench-time-safe-ix.csv

data/bench-time-fast-ix-as-int.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 -safe' -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-fast-th-as-word8-ix.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-snat-as-int +snat-as-word8 -ix-as-int +ix-as-word8 -safe' -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-safe-ix.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 +safe' -- 'Bench.Data.DeBruijn.Index/' --csv=$@

################################################################################
# Benchmark: Indexes -- Space -- Cyberglot

.PHONY: bench-space-ix-cyberglot
bench-space-ix-cyberglot: \
	data/bench-space-fast-int-ix-cyberglot.csv \
	data/bench-space-fast-word8-ix-cyberglot.csv \
	data/bench-space-safe-ix-cyberglot.csv

data/bench-space-fast-int-ix-cyberglot.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 -safe' >$@)

data/bench-space-fast-word8-ix-cyberglot.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags='-snat-as-int +snat-as-word8 -ix-as-int +ix-as-word8 -safe' >$@)

data/bench-space-safe-ix-cyberglot.csv: $(SOURCE_FILES)
	time (cabal run bench-space -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 +safe' >$@)

################################################################################
# Benchmark: Indexes -- Time -- Cyberglot

.PHONY: bench-time-ix-cyberglot
bench-time-ix-cyberglot: \
	data/bench-time-fast-ix-as-int-cyberglot.csv \
	data/bench-time-fast-ix-as-word8-cyberglot.csv \
	data/bench-time-safe-ix-cyberglot.csv

data/bench-time-fast-ix-as-int-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 -safe' -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-fast-ix-as-word8-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-snat-as-int +snat-as-word8 -ix-as-int +ix-as-word8 -safe' -- 'Bench.Data.DeBruijn.Index/' --csv=$@

data/bench-time-safe-ix-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+snat-as-int -snat-as-word8 +ix-as-int -ix-as-word8 +safe' -- 'Bench.Data.DeBruijn.Index/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings
################################################################################

.PHONY: bench-time-fast-natural-thinTh-cyberglot
bench-time-fast-natural-thinTh-cyberglot: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-th-as-bitvec-thinTh-samples3-cyberglot.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples1-cyberglot.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples3-cyberglot.csv

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 1

.PHONY: bench-time-thinTh-samples1
bench-time-thinTh-samples1: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples1.csv \
	data/bench-time-fast-th-as-integer-thinTh-samples1.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples1.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples1.csv \
	data/bench-time-safe-th-thinTh-samples1.csv

data/bench-time-fast-th-as-bitvec-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec +th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer +th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural +th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-th-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 +safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 2

.PHONY: bench-time-thinTh-samples2
bench-time-thinTh-samples2: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples2.csv \
	data/bench-time-fast-th-as-integer-thinTh-samples2.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples2.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples2.csv \
	data/bench-time-safe-th-thinTh-samples2.csv

data/bench-time-fast-th-as-bitvec-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec +th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer +th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural +th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-th-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 +safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 3

.PHONY: bench-time-thinTh-samples3
bench-time-thinTh-samples3: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples3.csv \
	data/bench-time-fast-th-as-integer-thinTh-samples3.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples3.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples3.csv \
	data/bench-time-safe-th-thinTh-samples3.csv

data/bench-time-fast-th-as-bitvec-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec +th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer +th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural +th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-th-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 +safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 1 -- cyberglot

.PHONY: bench-time-thinTh-samples1-cyberglot
bench-time-thinTh-samples1-cyberglot: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples1-cyberglot.csv \
	data/bench-time-fast-th-as-integer-thinTh-samples1-cyberglot.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples1-cyberglot.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples1-cyberglot.csv \
	data/bench-time-safe-th-thinTh-samples1-cyberglot.csv

data/bench-time-fast-th-as-bitvec-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec +th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer +th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural +th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-th-thinTh-samples1-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 +safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 2 -- cyberglot

.PHONY: bench-time-thinTh-samples2-cyberglot
bench-time-thinTh-samples2-cyberglot: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-th-as-integer-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples2-cyberglot.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples2-cyberglot.csv \
	data/bench-time-safe-th-thinTh-samples2-cyberglot.csv

data/bench-time-fast-th-as-bitvec-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec +th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer +th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural +th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-th-thinTh-samples2-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 +safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings -- Time -- Samples 3 -- cyberglot

.PHONY: bench-time-thinTh-samples3-cyberglot
bench-time-thinTh-samples3-cyberglot: \
	data/bench-time-fast-th-as-bitvec-thinTh-samples3-cyberglot.csv \
	data/bench-time-fast-th-as-integer-thinTh-samples3-cyberglot.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples3-cyberglot.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples3-cyberglot.csv \
	data/bench-time-safe-th-thinTh-samples3-cyberglot.csv

data/bench-time-fast-th-as-bitvec-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='+th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec +th-as-integer -th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer +th-as-natural -th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural +th-as-word64 -safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@

data/bench-time-safe-th-thinTh-samples3-cyberglot.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 --flags='-th-as-bitvec -th-as-integer -th-as-natural -th-as-word64 +safe' -- 'Bench.Data.DeBruijn.Thinning/' --csv=$@
