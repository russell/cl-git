VERSION 0.6
FROM debian:stable
WORKDIR /cl-git
ENV ROSWELL_DIST=https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh

libgit2:
    ARG --required LIBGIT2_VERSION
    RUN apt-get update && apt-get install -y curl cmake gcc libssl-dev libcurl4-gnutls-dev libssh2-1-dev
    RUN curl -L https://github.com/libgit2/libgit2/archive/v${LIBGIT2_VERSION}.tar.gz > v${LIBGIT2_VERSION}.tar.gz
    RUN tar -zxf v${LIBGIT2_VERSION}.tar.gz
    RUN mkdir libgit-build
    RUN cd libgit-build && \
        cmake -DBUILD_TESTS=OFF -DBUILD_CLAR=OFF -DCMAKE_VERBOSE_MAKEFILE=ON ../libgit2-${LIBGIT2_VERSION} && \
        cmake --build . && \
        cmake --build . --target install
    RUN ldconfig

deps:
    FROM +libgit2
    ENV CI=true
    ENV PATH="~/.roswell/bin:$PATH"
    RUN apt-get update && apt-get install -y curl make bzip2 libcurl3-gnutls zlib1g-dev lsof build-essential chrpath install-info texinfo file
    RUN curl -L $ROSWELL_DIST | sh

build:
    FROM +deps
    COPY cl-git.asd version.lisp-expr run-tests.lisp ./src/ ./tests/ /root/.roswell/local-projects/cl-git/
    COPY src /root/.roswell/local-projects/cl-git/src
    COPY tests /root/.roswell/local-projects/cl-git/tests

test-libgit2-sbcl:
    FROM +build
    RUN ros install sbcl-bin
    RUN ros use sbcl-bin

test-libgit2-ecl:
    FROM +build
    RUN apt update && apt install -y libgmp-dev
    RUN ros install ecl/21.2.1
    RUN ros use ecl

test-libgit2-ccl:
    FROM +build
    RUN ros install ccl-bin/1.12
    RUN ros use ccl-bin

test-libgit2-clasp:
    FROM +build
    RUN apt update && apt install -y libelf1 libllvm9 libgmpxx4ldbl locales-all
    RUN ros install clasp-bin
    RUN ros use clasp-bin

test-libgit2-0.27-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=0.27.10
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-0.28-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=0.28.5
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.0-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=1.0.1
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.1-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=1.1.1
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.2-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=1.2.0
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.3-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=1.3.2
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.4-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=1.4.4
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.5-sbcl:
    FROM +test-libgit2-sbcl --LIBGIT2_VERSION=1.5.0
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.5-ecl:
    FROM +test-libgit2-ecl --LIBGIT2_VERSION=1.5.0
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.5-ccl:
    FROM +test-libgit2-ccl --LIBGIT2_VERSION=1.5.0
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp

test-libgit2-1.5-clasp:
    FROM +test-libgit2-clasp --LIBGIT2_VERSION=1.5.0
    RUN /root/.roswell/local-projects/cl-git/run-tests.lisp
