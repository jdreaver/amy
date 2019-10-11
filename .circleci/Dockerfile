# Container for running the Haskell build

FROM ubuntu:18.04

# Install apt dependencies
RUN \
  apt-get update && \
  apt-get install -y \
    build-essential \
    curl \
    git \
    libelf-dev \
    libgmp3-dev \
    libtinfo-dev \
    locales \
    moreutils \
    pkg-config \
    python-yaml \
    wget && \
  rm -rf /var/lib/apt/lists/*

# Install LLVM
RUN \
  curl https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
  echo "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-9 main" > /etc/apt/sources.list.d/llvm.list && \
  echo "deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-9 main" >> /etc/apt/sources.list.d/llvm.list && \
  apt-get update && \
  apt-get install -y --allow-unauthenticated \
    clang-9 \
    llvm-9-dev && \
  ln -s /usr/bin/clang-9 /usr/bin/clang && \
  rm -rf /var/lib/apt/lists/*

# Install Boehm garbage collector
RUN \
  apt-get update && \
  apt-get install -y \
    libgc-dev && \
  rm -rf /var/lib/apt/lists/*

# Install stack
ENV STACK_VERSION 2.1.3
RUN \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar -xzv -C /tmp && \
  mv /tmp/stack-$STACK_VERSION-linux-x86_64/stack /usr/local/bin/stack && \
  rm -rf /tmp/*

# Use en_US.UTF-8 locale to fix decoding errors. It isn't here by default so we
# use locale-gen.
RUN locale-gen en_US.UTF-8
ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'
