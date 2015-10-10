.PHONY: all
.SUFFIXES:

GHC = ghc
GHCFLAGS = --make -O2

all: scripts/bin/browsers

scripts/bin/browsers: scripts/bin/browsers.hs
	$(GHC) $(GHCFLAGS) $<

clean:
	rm -fv scripts/bin/*.o scripts/bin/*.hi
