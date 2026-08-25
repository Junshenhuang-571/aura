#!/bin/sh
# Aura build script (no fpm required)
set -e
cd "$(dirname "$0")"
mkdir -p build
FC=${FC:-gfortran}
CC=${CC:-gcc}

$FC -c -std=f2018 -O2 -Wall -Jbuild src/aura_ansi.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild src/aura_config.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild src/aura_session.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild src/aura_pty.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild src/aura_llm.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild src/aura_gui.f90
$CC -c -O2 csrc/aura_pty_bridge.c
$CC -c -O2 csrc/aura_llm_stub.c

$FC -Ibuild -Jbuild -o build/aura app/main.f90 \
    aura_ansi.o aura_config.o aura_session.o aura_pty.o aura_llm.o aura_gui.o \
    aura_pty_bridge.o aura_llm_stub.o

# tests
$FC -Ibuild -Jbuild -o build/aura_test test/test_aura_ansi.f90 \
    aura_ansi.o aura_config.o aura_session.o aura_pty.o aura_llm.o \
    aura_pty_bridge.o aura_llm_stub.o
$FC -Ibuild -Jbuild -o build/e2e_final test/e2e_final.f90 \
    aura_ansi.o aura_config.o aura_session.o aura_pty.o aura_llm.o aura_gui.o \
    aura_pty_bridge.o aura_llm_stub.o

echo "Built: build/aura  build/aura_test  build/e2e_final"
