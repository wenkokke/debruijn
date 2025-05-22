################################################################################
# Default: Build
################################################################################

.PHONY: build
build: index.pdf

index.pdf: index.tex
	@SKIP_CUS_DEP=1 latexmk -pvc- index.tex

index.tex: index.tex.lhs
	@lhs2tex index.tex.lhs -o index.tex

index.tex.lhs: index.md assets/preamble.fmt assets/preamble.tex assets/templates/acmart.tex assets/defaults.yaml
	@pandoc -d assets/defaults.yaml index.md -o index.tex.lhs

################################################################################
# Watch
################################################################################

.PHONY: watch
watch:
	@latexmk

################################################################################
# Test
################################################################################

.PHONY: test
test: index.md
	@cabal build hin

################################################################################
# Cleanup
################################################################################

.PHONY: clean
clean:
	@latexmk -C
