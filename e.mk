# Compatibility shim for the former monolithic installer.
#
# New code belongs in the platform scripts under a/os/.  Existing commands such
# as `make -f e.mk install-a` continue to work while callers migrate.

DARWIN_INSTALLER := ./a/os/install.sh
UBUNTU_INSTALLER := ./a/os/linux/ubuntu/install.sh
FEDORA_INSTALLER := ./a/os/linux/fedora/install.sh
PLATFORM_INSTALLER := ./a/os/install.sh

ZNODE ?= 0
ifeq ($(filter $(ZNODE),0 1),)
$(error ZNODE must be 0 or 1)
endif
export ZNODE

.PHONY: all install Darwin Linux kws deps ldeps python-deps tree-sitter-src syncthing \
	syncthing-service configure build codesign vterm-module install-a site-lisp-sync userdir \
	launch system brew-check brew-install kterm z-macos z-ios help

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

syncthing-service:
	$(PLATFORM_INSTALLER) syncthing-service

configure:
	$(PLATFORM_INSTALLER) configure

build:
	$(PLATFORM_INSTALLER) build

codesign:
	$(DARWIN_INSTALLER) codesign

kterm:
	$(PLATFORM_INSTALLER) kterm

z-macos:
	$(PLATFORM_INSTALLER) z-macos

z-ios:
	$(PLATFORM_INSTALLER) z-ios

vterm-module:
	$(PLATFORM_INSTALLER) vterm

install-a site-lisp-sync userdir:
	$(PLATFORM_INSTALLER) site-lisp

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
	@printf '%s\n' \
	  '' \
	  'Optional build flags:' \
	  '  ZNODE=1         Build this Emacs as a z fleet node (default: 0)' \
	  '' \
	  'Apple product targets (Darwin only):' \
	  '  kterm          Build, HZ-sign, and install kterm' \
	  '  z-macos        Build, HZ-sign, and install z/mac' \
	  '  z-ios          Test, HZ-sign, and deploy z/iOS'
