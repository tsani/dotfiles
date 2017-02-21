.SUFFIXES:

GHC = ghc
GHCFLAGS = --make -O2
NOTIFY_SFX_DIR=dunst/.local/share/pb-notify
NOTIFY_SFX_PATH=$(NOTIFY_SFX_DIR)/notify-sfx.wav
NOTIFY_SFX_URL=https://files.jerrington.me/notify-sfx.wav

.PHONY: all
all: vimproc ssh $(NOTIFY_SFX_PATH)

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

$(NOTIFY_SFX_PATH):
	mkdir -p "$(NOTIFY_SFX_DIR)"
	wget "$(NOTIFY_SFX_URL)" -O $@
