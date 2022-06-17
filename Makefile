compile: info-variable-pitch.elc

%.elc: %.el
	emacs --batch -f batch-byte-compile $<

.PHONY: compile
