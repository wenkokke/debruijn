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
	time cabal run bench-space -v0 -- 'Data.DeBruijn.Index' --csv=$@

data/bench-space-fast-word8-ix.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -f+snat-as-word8 -f+ix-as-word8 -- 'Data.DeBruijn.Index' --csv=$@

data/bench-space-safe-ix.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -f+safe -- 'Data.DeBruijn.Index' --csv=$@

################################################################################
# Benchmark: Indexes -- Time

.PHONY: bench-time-ix
bench-time-ix: \
	data/bench-time-fast-ix-as-int.csv \
	data/bench-time-fast-ix-as-word8.csv \
	data/bench-time-safe-ix.csv

data/bench-time-fast-ix-as-int.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -- 'Data.DeBruijn.Index' --csv=$@

data/bench-time-fast-ix-as-word8.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+snat-as-word8 -f+ix-as-word8 -- 'Data.DeBruijn.Index' --csv=$@

data/bench-time-safe-ix.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+safe -- 'Data.DeBruijn.Index' --csv=$@

################################################################################
# Benchmark: Thinning Thinnings
################################################################################

.PHONY: bench-time-thinTh-vera
bench-time-thinTh-vera: \
	data/bench-time-fast-th-as-word64-thinTh-samples1.csv \
	data/bench-time-safe-th-thinTh-samples1.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples2.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples2.csv \
	data/bench-time-safe-th-thinTh-samples2.csv \
	data/bench-time-fast-th-as-natural-thinTh-samples3.csv \
	data/bench-time-fast-th-as-word64-thinTh-samples3.csv \
	data/bench-time-safe-th-thinTh-samples3.csv \

################################################################################
# Benchmark: Thinning Thinnings -- Space

.PHONY: bench-space-th
bench-space-th: \
	data/bench-space-fast-th-as-bitvec-thinTh.csv \
	data/bench-space-fast-th-as-integer-thinTh.csv \
	data/bench-space-fast-th-as-natural-thinTh.csv \
	data/bench-space-fast-th-as-word64-thinTh.csv \
	data/bench-space-safe-th-thinTh.csv

data/bench-space-fast-th-as-bitvec-thinTh.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -f+th-as-bitvec -- 'Data.DeBruijn.Thinning' --csv=$@

data/bench-space-fast-th-as-integer-thinTh.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -f+th-as-integer -- 'Data.DeBruijn.Thinning' --csv=$@

data/bench-space-fast-th-as-natural-thinTh.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -- 'Data.DeBruijn.Thinning' --csv=$@

data/bench-space-fast-th-as-word64-thinTh.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -f+th-as-word64 -- 'Data.DeBruijn.Thinning' --csv=$@

data/bench-space-safe-th-thinTh.csv: $(SOURCE_FILES)
	time cabal run bench-space -v0 -f+safe -- 'Data.DeBruijn.Thinning' --csv=$@

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
	time cabal run bench-time -v0 -f+th-as-bitvec -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+th-as-integer -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+th-as-word64 -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-safe-th-thinTh-samples1.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+safe -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

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
	time cabal run bench-time -v0 -f+th-as-bitvec -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+th-as-integer -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+th-as-word64 -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-safe-th-thinTh-samples2.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+safe -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

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
	time cabal run bench-time -v0 -f+th-as-bitvec -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-integer-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+th-as-integer -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-natural-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-fast-th-as-word64-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+th-as-word64 -- 'Data.DeBruijn.Thinning/thin/' --csv=$@

data/bench-time-safe-th-thinTh-samples3.csv: $(SOURCE_FILES)
	time cabal run bench-time -v0 -f+safe -- 'Data.DeBruijn.Thinning/thin/' --csv=$@
