FROM opensuse/leap:15.1

WORKDIR /app

# Due to Docker's caching model, only append RUNs to the end of this
# during "production".
# Minmizie RUNs before a new "release".

RUN zypper --non-interactive install nasm git-core libcrypto43 libssl45 binutils make gcc libglib-2_0-0 libgmodule-2_0-0
RUN ln -s /usr/lib64/libssl.so.1.1 /usr/lib64/libssl.so
RUN ln -s /usr/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so

RUN git clone --single-branch --depth 1 --branch v7.8 https://github.com/racket/racket
WORKDIR /app/racket
RUN make PKGS="base" PREFIX="/usr/" unix-style

RUN raco pkg install --auto compiler-lib plai-lib https://github.com/cpsc411/cpsc411-pub.git?path=cpsc411-lib#2020w2

WORKDIR /app/workspace