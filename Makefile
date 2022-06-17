.cask: Cask info-variable-pitch.el
	cask install

compile: .cask
	cask build

.PHONY: compile
