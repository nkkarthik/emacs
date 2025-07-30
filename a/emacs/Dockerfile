# emacs build
#   https://www.perplexity.ai/search/emacs-31-build-from-source-on-OEcW6DZBTzqJy9f0L2MtDQ
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC
RUN apt update && apt upgrade -y && apt install -y sbcl

# Update system and install essential build tools
RUN apt install -y \
    build-essential \
    git gosu \
    autoconf \
    automake \
    autotools-dev \
    texinfo \
    pkg-config \
    curl \
    wget \
    ca-certificates \
    xvfb x11-utils dbus-x11 \
    python3 python3-pip python3-venv python3-dev \
    nodejs npm 

# Enable source repositories (Ubuntu 24.04+ syntax)
# to get build-dep emacs
RUN sed -i 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/ubuntu.sources \
    && apt update

# Install Emacs build dependencies
RUN apt build-dep -y emacs && \
    apt install -y \
    libgccjit0 \
    libgccjit-12-dev \
    libjansson4 \
    libjansson-dev \
    libtree-sitter-dev \
    gnutls-bin \
    gcc-12 \
    g++-12

# Install comprehensive feature dependencies
RUN apt install -y \
    libgtk-3-dev \
    libxpm-dev \
    libjpeg-dev \
    libgif-dev \
    libtiff-dev \
    libpng-dev \
    libgnutls28-dev \
    libncurses-dev \
    libxml2-dev \
    libmagick++-dev \
    imagemagick \
    libwebkit2gtk-4.1-dev \
    libcairo2-dev \
    libpango1.0-dev \
    libharfbuzz-dev \
    librsvg2-dev \
    libsqlite3-dev \
    libxft-dev \
    libxft2 \
    libx11-dev \
    libxrandr-dev \
    libxi-dev \
    libxinerama-dev \
    libxcursor-dev \
    libxss-dev \
    libxtst-dev \
    libxv-dev \
    libxaw7-dev \
    mailutils

# Install EAF system dependencies
RUN apt install -y \
    python3-pyqt5 \
    python3-pyqt5.qtwebengine \
    python3-pyqt5.qtquick \
    python3-pyqt5.qtmultimedia \
    qtbase5-dev \
    qtchooser \
    qt5-qmake \
    qtbase5-dev-tools \
    libnss3-dev

# Install official Xpra for remote display
RUN wget -O /usr/share/keyrings/xpra.asc https://xpra.org/xpra-2023.asc && \
    echo "Types: deb\nURIs: https://xpra.org\nSuites: noble\nComponents: main\nSigned-By: /usr/share/keyrings/xpra.asc\nArchitectures: amd64 arm64" > /etc/apt/sources.list.d/xpra.sources && \
    apt update && \
    apt install -y xpra xpra-html5


# Set compiler versions for native compilation
ENV CC=/usr/bin/gcc-12
ENV CXX=/usr/bin/g++-12

# Create build directory
WORKDIR /build

# Clone Emacs source code (master branch = version 31)
RUN git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git emacs-31

WORKDIR /build/emacs-31

# Generate configure script
RUN ./autogen.sh

# Configure with comprehensive feature set
RUN ./configure \
    --prefix=/usr/local \
    --with-x \
    --with-x-toolkit=gtk3 \
    --without-xwidgets \
    --with-cairo \
    --with-imagemagick \
    --with-rsvg \
    --with-jpeg \
    --with-gif \
    --with-png \
    --with-tiff \
    --with-xpm \
    --with-gnutls \
    --with-xml2 \
    --with-json \
    --with-mailutils \
    --with-tree-sitter \
    --with-native-compilation=aot \
    --with-threads \
    --with-modules \
    --with-harfbuzz \
    --with-sqlite3 \
    --without-compress-install \
    --enable-checking=yes,glyphs \
    CFLAGS="-O2 -march=native" \
    CC=gcc-12

# Build and Install Emacs
RUN make -j$(nproc) bootstrap && \
    make install

# Install EAF (Emacs Application Framework)
# WORKDIR /opt
# RUN git clone --depth 1 https://github.com/emacs-eaf/emacs-application-framework.git eaf

# Install EAF Python dependencies
# WORKDIR /opt/eaf
# RUN python3 -m pip install --break-system-packages -r requirements.txt


# Create Xpra configuration
#RUN mkdir -p /etc/xpra && echo "bind-tcp=0.0.0.0:1111" > /etc/xpra/xpra.conf 




# Add Docker's
RUN install -m 0755 -d /etc/apt/keyrings && \
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc && \
    chmod a+r /etc/apt/keyrings/docker.asc

# Add Docker repository to APT sources
RUN echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
    $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
    tee /etc/apt/sources.list.d/docker.list > /dev/null


RUN apt update && apt install -y \
    sudo fish direnv \
    docker-ce-cli docker-compose-plugin docker-buildx-plugin \
    golang-go \
    apt-transport-https software-properties-common \
    cmake \
    libtool libtool-bin \
    sqlite3 libsqlite3-dev \
    fonts-firacode

# fish shell
RUN echo /usr/bin/fish >> /etc/shells

RUN go install github.com/sorenisanerd/gotty@latest



# knannuru
RUN groupmod -n knannuru ubuntu && \
    usermod -l knannuru -d /home/knannuru -m -c knannuru ubuntu && \
    chsh -s /usr/bin/fish knannuru && \
    mkdir -p /home/knannuru && \
    chown -R knannuru:knannuru /home/knannuru && \
    echo 'knannuru ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

#    useradd -m -u 1000 -g 1000 -G 1000 -s /usr/bin/fish knannuru && \
# groupmod --gid $USER_GID $USERNAME \
    # && usermod --uid $USER_UID --gid $USER_GID $USERNAME \
    # && chown -R $USER_UID:$USER_GID /home/$USERNAME
# echo 'knannuru ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers


# PRE-INSTALL STRAIGHT.EL AND PACKAGES DURING BUILD
#    --eval "(setq user-emacs-directory \"/home/knannuru/.emacs.d/\")" \

COPY lisp/deps.el /home/knannuru/.emacs.d/lisp/deps.el
RUN mkdir -p /home/knannuru/.cache/emacs/eln && \
    mkdir -p /home/knannuru/.cache/straight && \
    chown -R knannuru:knannuru /home/knannuru/.cache 
USER knannuru
RUN ls -ld /home/knannuru/.cache && \
    ls -l /home/knannuru/.cache && \
    HOME=/home/knannuru emacs --batch \
    --load /home/knannuru/.emacs.d/lisp/deps.el \
    --eval "(message \"All packages installed and cached!\")" \
    --kill
USER root


#RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
#    sbcl --disable-debugger --load quicklisp.lisp \
#    	 --eval '(quicklisp-quickstart:install)' \
#	 --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
#	 --quit

RUN apt install -y lsof

COPY sbclrc /home/knannuru/.sbclrc
       
COPY . /emacs
RUN chmod +x /emacs/emacs.sh
CMD ["/emacs/emacs.sh"]
EXPOSE 1111