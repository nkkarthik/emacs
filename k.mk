# Makefile for building Emacs.app on macOS
OS := $(shell uname)
EMACS_PREFIX ?= $(HOME)/.local/emacs
JOBS ?= $(shell sysclt -n hw.ncpu 2>/dev/null || nproc)
BREW := $(shell command -v brew 2>/dev/null || echo /opt/homebrew/bin/brew)

.PHONY: all Darwin ubuntu deps configure build install

all: $(OS)
Darwin: deps configure build install

deps: brew-check
	@echo "🔧 Installing dependencies with Homebrew..."
	$(BREW) install autoconf automake texinfo pkg-config \
		gnutls libjpeg libpng librsvg libtiff libxpm \
		ncurses mailutils libxml2 jansson sqlite imagemagick tree-sitter \
		gcc libgccjit

brew-check:
	@echo "🔍 Checking if Homebrew is available..."
	@if ! command -v brew >/dev/null; then \
		if [ -x "/opt/homebrew/bin/brew" ]; then \
			echo "✅ Found brew at /opt/homebrew/bin/brew"; \
		elif [ -x "/usr/local/bin/brew" ]; then \
			echo "✅ Found brew at /usr/local/bin/brew"; \
		else \
			$(MAKE) brew-install; \
		fi \
	else \
		echo "✅ Brew already in PATH"; \
	fi

brew-install:
	@echo "🍺 Installing Homebrew..."
	@/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"


configure:
	@echo "⚙️ Running autogen.sh and configure..."
	./autogen.sh
	./configure --with-ns \
	            --with-modules \
	            --with-json \
	            --with-tree-sitter \
	            --with-native-compilation \
	            --with-sqlite3 \
	            --prefix=$(EMACS_PREFIX)

build:
	@echo "🔨 Building Emacs.app with $(EMACS_CORES) cores..."
	make -j$(EMACS_CORES)
	@echo "✅ Emacs.app built"


# Common Ubuntu build target (with GTK GUI + SQLite)
ubuntu:
	sudo apt-get update && \
	sudo apt-get install -y \
		autoconf automake build-essential texinfo libgtk-3-dev libjansson-dev \
		libgnutls28-dev libgccjit-10-dev pkg-config libsqlite3-dev && \
	./autogen.sh && \
	./configure --with-x-toolkit=gtk3 --with-json --with-modules \
	            --with-native-compilation --with-sqlite3 && \
	make -j$(JOBS)
	@echo "✅ Emacs.gtk built"

	# sudo make install

install:
	sudo ln -snf $(CURDIR)/src/emacs /opt/homebrew/bin/emacs
	@echo "✅ $(CURDIR)/src/emacs => /opt/homebrew/bin/emacs"

#	sudo ln -snf $(CURDIR)/src/emacs /usr/local/bin/emacs
#	@echo "✅ $(CURDIR)/src/emacs => /usr/local/bin/emacs"
