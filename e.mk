# Compatibility shim for the former monolithic installer.
#
# New code belongs in the platform scripts under a/os/.  Existing commands such
# as `make -f e.mk install-a` continue to work while callers migrate.

DARWIN_INSTALLER := ./a/os/install.sh
UBUNTU_INSTALLER := ./a/os/linux/ubuntu/install.sh
FEDORA_INSTALLER := ./a/os/linux/fedora/install.sh
PLATFORM_INSTALLER := ./a/os/install.sh

.PHONY: all install Darwin Linux kws deps ldeps python-deps tree-sitter-src syncthing \
	configure build codesign vterm-module install-a site-lisp-sync userdir \
	local-bin brew-bin launch system brew-check brew-install help

all:
	$(PLATFORM_INSTALLER) all

install:
	$(PLATFORM_INSTALLER) install

Darwin:
	$(DARWIN_INSTALLER) all

Linux:
	$(UBUNTU_INSTALLER) all

kws:
	$(FEDORA_INSTALLER) all

deps:
	$(PLATFORM_INSTALLER) deps

ldeps:
	$(UBUNTU_INSTALLER) deps

python-deps:
	$(PLATFORM_INSTALLER) python-deps

tree-sitter-src:
	$(PLATFORM_INSTALLER) tree-sitter

syncthing:
	$(PLATFORM_INSTALLER) syncthing

configure:
	$(PLATFORM_INSTALLER) configure

build:
	$(PLATFORM_INSTALLER) build

codesign:
	$(DARWIN_INSTALLER) codesign

vterm-module:
	$(PLATFORM_INSTALLER) vterm

install-a site-lisp-sync userdir:
	$(PLATFORM_INSTALLER) site-lisp

local-bin:
	$(UBUNTU_INSTALLER) link

brew-bin:
	$(DARWIN_INSTALLER) link

launch:
	$(DARWIN_INSTALLER) service

system:
	$(UBUNTU_INSTALLER) service

brew-check:
	$(DARWIN_INSTALLER) doctor

brew-install:
	$(DARWIN_INSTALLER) brew-install

help:
	$(PLATFORM_INSTALLER) --help
