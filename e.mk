# Makefile for building Emacs.app on macOS
OS := $(shell uname)
EMACS_PREFIX ?= $(HOME)/.local/emacs
JOBS ?= $(shell sysctl -n hw.ncpu 2>/dev/null || nproc)
BREW := $(shell command -v brew 2>/dev/null || echo /opt/homebrew/bin/brew)

.PHONY: all Darwin Linux deps python-deps configure build codesign


all: $(OS)


Darwin: deps python-deps tree-sitter-src configure build


deps: brew-check
	@echo "🔧 Installing dependencies with Homebrew..."
	@# tree-sitter is intentionally omitted: it is built from the
	@# x/tree-sitter submodule by the tree-sitter-src target so both
	@# Darwin and Linux pin to the same version (currently v0.26.9).
	@# Run `brew uninstall tree-sitter` if you previously had it.
	$(BREW) install autoconf automake texinfo pkg-config \
		gnutls libjpeg libpng librsvg libtiff libxpm \
		ncurses mailutils libxml2 jansson sqlite imagemagick \
		gcc libgccjit cmake libtool libvterm \
		go node llvm rust-analyzer
	@echo "🔧 Installing language servers via go and npm..."
	@# gopls lands in $(GOBIN) (default $$HOME/go/bin); npm globals
	@# go to brew's node prefix (user-writable, no sudo needed).
	@# vscode-langservers-extracted bundles HTML, CSS, JSON, ESLint
	@# language servers in one package.
	go install golang.org/x/tools/gopls@latest
	npm install -g typescript-language-server typescript \
	               vscode-langservers-extracted \
	               dockerfile-language-server-nodejs

python-deps:
	@echo "🐍 Installing a/llm Python dependencies..."
	pip3 install -r a/llm/requirements.txt

brew-check:
	@echo "🔍 Checking if Homebrew is available..."
	@if command -v brew >/dev/null; then \
		echo "✅ Brew already in PATH"; \
	elif [ -x "/opt/homebrew/bin/brew" ]; then \
		echo "✅ Found brew at /opt/homebrew/bin/brew"; \
	elif [ -x "/usr/local/bin/brew" ]; then \
		echo "✅ Found brew at /usr/local/bin/brew"; \
	else \
		echo "❌ Homebrew not found."; \
		echo "   Install it yourself, then re-run this build."; \
		echo "   The 'brew-install' target shows the official one-liner:"; \
		echo "     make -f e.mk brew-install"; \
		exit 1; \
	fi

# Prints the official Homebrew install command for the user to run
# manually.  Intentionally does not pipe curl into bash on the user's
# behalf — installing Homebrew is a one-time choice that should be
# made knowingly.
brew-install:
	@echo "Run this yourself to install Homebrew:"
	@echo '  /bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'


configure:
	@echo "⚙️ Running autogen.sh and configure..."
	./autogen.sh
	PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/opt/homebrew/opt/gcc/lib/pkgconfig" \
	./configure --with-ns \
	            --with-modules \
	            --with-json \
	            --with-tree-sitter \
	            --with-sqlite3 \
	            --prefix=$(EMACS_PREFIX)

build:
	@echo "🔨 Building Emacs.app with $(JOBS) cores..."
	make -j$(JOBS)
	@echo "✅ Emacs.app built"
	make install
	@echo "✅ Emacs.app installed"
	$(MAKE) -f e.mk install-a
	@echo "✅ a/ packages installed into site-lisp/"
	$(MAKE) -f e.mk codesign


# Force-resign locally-built binaries with a real ad-hoc signature.
# macOS 26+ kills launchd-spawned binaries that carry only the default
# linker-signed signature (Launch Constraint Violation → SIGKILL
# "Code Signature Invalid").  Re-signing with `codesign --sign -`
# replaces the linker-signed marker with a real adhoc signature so the
# `em` / `ec` LaunchAgents (which reach these via brew-bin/local-bin
# symlinks at /opt/homebrew/bin/{emacs,emacsclient}) can run.
#
# Only the build-tree binaries are signed here — signing the Emacs.app
# bundle's binaries is a different exercise (codesign recurses into
# the bundle and trips on unsigned helpers like libexec/rcs2log), and
# the current LaunchAgents don't reference them.
codesign:
	@if [ "$(OS)" != "Darwin" ]; then \
		echo "ℹ️  codesign: not Darwin, skipping"; exit 0; \
	fi
	@echo "🔏 Code-signing emacs and emacsclient..."
	@for f in \
		$(CURDIR)/src/emacs \
		$(CURDIR)/lib-src/emacsclient; do \
		if [ -f "$$f" ]; then \
			codesign --force --sign - "$$f" \
				&& echo "✅ signed $$f" \
				|| { echo "❌ codesign failed: $$f"; exit 1; }; \
		fi; \
	done


# Copy personal elisp packages from a/ into the installed Emacs.app's
# site-lisp/ tree.  Excludes git-crypt/git metadata and CLAUDE.md so
# only package code lands in the bundle.  a/daemon/ holds launchd/systemd
# unit files (not elisp), so it is excluded too.
NS_APPRESDIR := $(CURDIR)/nextstep/Emacs.app/Contents/Resources
.PHONY: install-a
install-a: vterm-module
	@test -d $(NS_APPRESDIR)/site-lisp || { echo "❌ $(NS_APPRESDIR)/site-lisp not found — run 'make install' first"; exit 1; }
	@# Remove stale per-package build/ trees left by earlier installs
	@# (before --exclude='build/' was added below).  They're harmless
	@# functionally but pollute load-path via subdirs.el discovery.
	@rm -rf $(NS_APPRESDIR)/site-lisp/emacs-libvterm/build
	rsync -a \
		--exclude='.git*' \
		--exclude='CLAUDE.md' \
		--exclude='*.org' \
		--exclude='*~' \
		--exclude='*.db' --exclude='*.db-shm' --exclude='*.db-wal' \
		--exclude='*.elc' \
		--exclude='*-tests.el' \
		--exclude='daemon/' \
		--exclude='build/' \
		$(CURDIR)/a/ $(NS_APPRESDIR)/site-lisp/
	@echo "✅ rsync'd a/ -> $(NS_APPRESDIR)/site-lisp/ (excluded data/docs/tests/.elc)"
	@# Expose the bundle's site-lisp to the uninstalled src/emacs binary
	@# (and any daemon launched from it via brew-bin/local-bin) by
	@# symlinking it adjacent to lisp/ at the build root.  Without this
	@# symlink, src/emacs computes a bogus relative "Contents/Resources/
	@# site-lisp" load-path entry and never finds the bundled packages.
	@ln -snf $(NS_APPRESDIR)/site-lisp $(CURDIR)/site-lisp
	@echo "✅ symlinked $(CURDIR)/site-lisp -> $(NS_APPRESDIR)/site-lisp"


# Build the emacs-libvterm dynamic module via its CMake setup.  The
# source lives at x/emacs-libvterm (submodule); the resulting
# vterm-module.so lands next to vterm.el inside the submodule, and
# install-a rsyncs both into site-lisp/.  Prefers the system
# libvterm (installed by deps/ldeps); falls back to fetching +
# compiling the vendored libvterm if absent.
VTERM_DIR := $(CURDIR)/x/emacs-libvterm
.PHONY: vterm-module
vterm-module:
	@echo "🔨 Building emacs-libvterm vterm-module..."
	@# Drop any stale CMakeCache.txt whose recorded source dir no longer
	@# matches this checkout (happens when a/emacs-libvterm/build/ was
	@# populated from a different clone path).
	@if [ -f $(VTERM_DIR)/build/CMakeCache.txt ] && \
		! grep -q "CMAKE_HOME_DIRECTORY:INTERNAL=$(VTERM_DIR)$$" \
			$(VTERM_DIR)/build/CMakeCache.txt; then \
		echo "🧹 Stale CMakeCache detected — wiping $(VTERM_DIR)/build"; \
		rm -rf $(VTERM_DIR)/build; \
	fi
	mkdir -p $(VTERM_DIR)/build
	cd $(VTERM_DIR)/build && cmake .. && $(MAKE)
	@test -f $(VTERM_DIR)/vterm-module.so \
		&& echo "✅ vterm-module built: $(VTERM_DIR)/vterm-module.so" \
		|| { echo "❌ vterm-module.so missing after build"; exit 1; }


# Ubuntu build target (with GTK GUI + SQLite). libtree-sitter is
# built from the x/tree-sitter submodule rather than apt, because
# Ubuntu 24's libtree-sitter-dev (0.20.8-2) caps Emacs at ABI 14
# and several grammars (tree-sitter-python v0.25.0+, etc.) now
# require ABI 15.
Linux: ldeps python-deps tree-sitter-src vterm-module
	./autogen.sh
	LDFLAGS="-L/usr/lib/gcc/x86_64-linux-gnu/13" \
	CPPFLAGS="-I/usr/lib/gcc/x86_64-linux-gnu/13/include" \
	PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$$PKG_CONFIG_PATH" \
	LD_LIBRARY_PATH="/usr/local/lib:$$LD_LIBRARY_PATH" \
	./configure --with-x-toolkit=gtk3 --with-json --with-modules \
	            --with-native-compilation --with-sqlite3 \
	            --with-tree-sitter && \
	make -j$(JOBS)
	@echo "✅ emacs-gtk built"

#	make install
#	@echo "✅ emacs-gtk installed"


# Build and install libtree-sitter from the x/tree-sitter submodule
# into /usr/local. Used by Linux to bypass the Ubuntu apt ceiling on
# libtree-sitter-dev. Idempotent — re-runs the build and install,
# which is cheap (single static + shared lib).
TREE_SITTER_SRC := $(CURDIR)/x/tree-sitter
.PHONY: tree-sitter-src
tree-sitter-src:
	@# Submodules have a .git FILE (not directory) pointing into the parent
	@# repo's .git/modules tree, so test for Makefile presence instead.
	@test -e $(TREE_SITTER_SRC)/Makefile || { \
		echo "❌ $(TREE_SITTER_SRC) missing — run 'git submodule update --init x/tree-sitter'"; \
		exit 1; \
	}
	@echo "🔨 Building libtree-sitter from $(TREE_SITTER_SRC)..."
	cd $(TREE_SITTER_SRC) && make
	cd $(TREE_SITTER_SRC) && sudo make install PREFIX=/usr/local
	@# ldconfig is Linux-only; macOS resolves /usr/local/lib via the
	@# default dyld fallback search path.
	@command -v ldconfig >/dev/null && sudo ldconfig /usr/local/lib || true
	@echo "✅ libtree-sitter installed from source into /usr/local"


ldeps:
	sudo apt update && \
	sudo apt install -y \
		autoconf automake build-essential cmake \
		texinfo libgtk-3-dev libjansson-dev libncurses-dev \
		libgnutls28-dev pkg-config \
		libsqlite3-dev libgccjit-13-dev \
		libxpm-dev libgif-dev libjpeg-dev libpng-dev \
		libtool libtool-bin libsystemd-dev \
		libvterm-dev \
		clangd golang-go nodejs
	@echo "✅ apt deps installed"
	@# rust-analyzer is not in Ubuntu noble's apt repos. Pull the
	@# upstream static binary into ~/.local/bin (already on the
	@# daemon's PATH). If you have rustup installed,
	@# `rustup component add rust-analyzer` is the alternative.
	@echo "🔧 Installing rust-analyzer release binary..."
	mkdir -p $$HOME/.local/bin
	curl -fsSL https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz \
	  | gunzip -c - > $$HOME/.local/bin/rust-analyzer
	chmod +x $$HOME/.local/bin/rust-analyzer
	@echo "🔧 Installing gopls..."
	@# gopls into $$HOME/go/bin (already on the daemon's PATH).
	go install golang.org/x/tools/gopls@latest
	@echo "🔧 Installing npm-based language servers (sudo npm -g)..."
	@# nodesource's nodejs provides npm at /usr/bin/npm with prefix
	@# /usr, so sudo is required. If you switch npm to a user prefix
	@# (npm config set prefix $$HOME/.npm-global), drop sudo.
	@# vscode-langservers-extracted bundles HTML, CSS, JSON, ESLint
	@# language servers in one package.
	sudo npm install -g typescript-language-server typescript \
	                    vscode-langservers-extracted \
	                    dockerfile-language-server-nodejs
	@echo "✅ ldeps installed"

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
	cp $(CURDIR)/a/daemon/e.plist $(CURDIR)/a/daemon/ec.plist $(HOME)/Library/LaunchAgents/
	@echo "🚀 Restarting Emacs daemon and client LaunchAgents..."
	@launchctl bootout gui/$$(id -u) $(HOME)/Library/LaunchAgents/ec.plist 2>/dev/null || true
	@launchctl bootout gui/$$(id -u) $(HOME)/Library/LaunchAgents/e.plist 2>/dev/null || true
	@launchctl bootstrap gui/$$(id -u) $(HOME)/Library/LaunchAgents/e.plist
	@launchctl bootstrap gui/$$(id -u) $(HOME)/Library/LaunchAgents/ec.plist
	@launchctl enable gui/$$(id -u)/em
	@launchctl enable gui/$$(id -u)/ec
	@sleep 2
	@echo "✅ Emacs daemon and client LaunchAgents started"
	@echo "💡 Test with: emacsclient -c"


.PHONY: system
system:
	mkdir -p $(HOME)/.config/systemd/user/
	cp $(CURDIR)/a/daemon/e.service $(HOME)/.config/systemd/user/e.service
	@echo "🔄 Reloading systemd user units..."
	systemctl --user daemon-reload
	systemctl --user enable e.service
	@# restart (not start) so edits to e.service — env vars in particular —
	@# take effect when the daemon is already running. The previous `start`
	@# was a no-op when active, which silently dropped env-var changes.
	systemctl --user restart e.service
	sudo loginctl enable-linger $$(id -un)
	@echo "🚀 Emacs daemon (re)started"


# to start the daemon on boot without user login
#sudo loginctl enable-linger $USER


# Stop and disable
#systemctl --user stop emacs.service
#systemctl --user disable emacs.service

# Restart
#systemctl --user restart emacs.service

# Check status
#systemctl --user status emacs.service


TREE_SITTER_DIR := /tmp/tree-sitter
kws:
	sudo dnf update -y && \
	sudo dnf install -y \
		autoconf automake make gcc gcc-c++ \
		texinfo ncurses-devel jansson-devel \
		gnutls-devel pkgconf-pkg-config \
		sqlite-devel libgccjit-devel \
		libtool systemd-devel
	@echo "✅ kws deps installed"
	rm -rf $(TREE_SITTER_DIR)
	git clone --depth=1 https://github.com/tree-sitter/tree-sitter.git $(TREE_SITTER_DIR)
	cd $(TREE_SITTER_DIR) && make
	cd $(TREE_SITTER_DIR) && sudo make install
	PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH" \
	LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
	sudo ldconfig /usr/local/lib
	@echo "✅ kws tree sitter installed"
	./autogen.sh
	PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH" \
	LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
	./configure --without-x --with-json --with-modules \
	            --with-native-compilation --with-sqlite3
	make -j$(nproc)
	@echo "✅ kws emacs built"
