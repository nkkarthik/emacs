# Makefile for building Emacs.app on macOS
OS := $(shell uname)
EMACS_PREFIX ?= $(HOME)/.local/emacs
JOBS ?= $(shell sysclt -n hw.ncpu 2>/dev/null || nproc)
BREW := $(shell command -v brew 2>/dev/null || echo /opt/homebrew/bin/brew)

.PHONY: all Darwin Linux deps configure build userdir


all: $(OS)


Darwin: deps configure build userdir

userdir:
	mkdir -p $(HOME)/.emacs.d/
	ln -snf $(CURDIR)/init.el $(HOME)/.emacs.d/init.el
	ln -snf $(CURDIR)/early-init.el $(HOME)/.emacs.d/early-init.el


deps: brew-check
	@echo "🔧 Installing dependencies with Homebrew..."
	$(BREW) install autoconf automake texinfo pkg-config \
		gnutls libjpeg libpng librsvg libtiff libxpm \
		ncurses mailutils libxml2 jansson sqlite imagemagick tree-sitter \
		gcc libgccjit cmake libtool

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
	PKG_CONFIG_PATH="/opt/homebrew/opt/gcc/lib/pkgconfig" \
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
	make install
	@echo "✅ Emacs.app install"


# Ubuntu build target (with GTK GUI + SQLite)
Linux: ldeps
	./autogen.sh
	LDFLAGS="-L/usr/lib/gcc/x86_64-linux-gnu/13" \
	CPPFLAGS="-I/usr/lib/gcc/x86_64-linux-gnu/13/include" \
	./configure --with-x-toolkit=gtk3 --with-json --with-modules \
	            --with-native-compilation --with-sqlite3 && \
	make -j$(JOBS)
	@echo "✅ emacs-gtk built"


ldeps:
	sudo apt update && \
	sudo apt install -y \
		autoconf automake build-essential \
		texinfo libgtk-3-dev libjansson-dev libncurses-dev \
		libgnutls28-dev pkg-config \
		libsqlite3-dev libgccjit-13-dev \
		libxpm-dev libgif-dev libjpeg-dev libpng-dev \
		libtool libtool-bin

.PHONY: local-bin brew-bin
local-bin:
	sudo ln -snf $(CURDIR)/src/emacs /usr/local/bin/emacs
	@echo "✅ $(CURDIR)/src/emacs => /usr/local/bin/emacs"
	sudo ln -snf $(CURDIR)/lib-src/emacsclient /usr/local/bin/emacsclient
	@echo "✅ $(CURDIR)/lib-src/emacsclient => /usr/local/bin/emacsclient"
brew-bin:
	sudo ln -snf $(CURDIR)/src/emacs /opt/homebrew/bin/emacs
	@echo "✅ $(CURDIR)/src/emacs => /opt/homebrew/bin/emacs"
	sudo ln -snf $(CURDIR)/lib-src/emacsclient /opt/homebrew/bin/emacsclient
	@echo "✅ $(CURDIR)/lib-src/emacsclient => /opt/homebrew/bin/emacsclient"

.PHONY: launch
launch:
	mkdir -p $(HOME)/Library/LaunchAgents/
	cp $(CURDIR)/e.plist $(HOME)/Library/LaunchAgents/
	@echo "🚀 Starting Emacs daemon..."
	@launchctl bootstrap gui/$$(id -u) ~/Library/LaunchAgents/e.plist 2>/dev/null || true
	@launchctl enable gui/$$(id -u)/local.emacs.daemon
	@sleep 2
	@echo "✅ Emacs daemon started!"
	@echo "💡 Test with: emacsclient -c"

.PHONY: system
system:
	mkdir -p $(HOME)/.config/systemd/user/
	cp $(CURDIR)/e.service $(HOME)/.config/systemd/user/e.service
	@echo "🚀 Starting Emacs daemon..."
	# Start and enable on login
	systemctl --user enable e.service
	systemctl --user start e.service
	sudo loginctl enable-linger knannuru


# to start the daemon on boot without user login
#sudo loginctl enable-linger $USER


# Stop and disable
#systemctl --user stop emacs.service
#systemctl --user disable emacs.service

# Restart
#systemctl --user restart emacs.service

# Check status
#systemctl --user status emacs.service
