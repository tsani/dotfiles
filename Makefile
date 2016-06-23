.PHONY: all
.SUFFIXES:

GHC = ghc
GHCFLAGS = --make -O2

all: scripts/bin/browsers scripts/bin/writewatch vimproc ssh

scripts/bin/browsers: scripts/bin/browsers.hs
	$(GHC) $(GHCFLAGS) $<

scripts/bin/writewatch: scripts/bin/writewatch.hs
	$(GHC) $(GHCFLAGS) $<

clean:
	rm -fv scripts/bin/*.o scripts/bin/*.hi

.PHONY: vimproc
vimproc:
	make -C vim/.vim/bundle/vimproc.vim

.PHONY: ssh
ssh:
	make -C ssh
