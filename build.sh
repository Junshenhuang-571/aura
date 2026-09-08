#!/bin/sh
# Aura build script — phase B
set -e
cd "$(dirname "$0")"
mkdir -p build
FC=${FC:-gfortran}
CC=${CC:-gcc}

# --- native LLM engine (pure Fortran, zero deps) ---
$FC -c -std=f2018 -O2 -Wall -Jbuild -ffree-line-length-none -fmax-stack-var-size=1024 csrc/llm_f90_src/weight_module.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -ffree-line-length-none -fmax-stack-var-size=1024 csrc/llama2_mod.f90

$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_ansi.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_theme.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_config.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_session.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_workspace.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_pty.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_llm.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_keys.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_render.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_ai.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 src/aura_gui.f90
$FC -c -std=f2018 -O2 -Wall -Jbuild -fmax-stack-var-size=1024 -ffree-line-length-none src/aura_workbench.f90
$CC -c -O2 csrc/aura_pty_bridge.c
$CC -c -O2 csrc/aura_console_bridge.c
$CC -c -O2 csrc/aura_net.c
# aura_llm_stub.c removed: native backend is implemented in src/aura_llm.f90
# (bind(C) entry points aura_llm_available / aura_llm_generate).

$FC -static -Ibuild -Jbuild -fmax-stack-var-size=1024 -o build/aura app/main.f90 weight_module.o llama2_mod.o \
    aura_ansi.o aura_theme.o aura_workspace.o aura_config.o aura_session.o aura_pty.o aura_keys.o \
    aura_render.o aura_ai.o aura_llm.o aura_gui.o \
    aura_workbench.o aura_pty_bridge.o aura_console_bridge.o aura_net.o -lws2_32 -Wl,--stack,67108864

# tests
$FC -Ibuild -Jbuild -fmax-stack-var-size=1024 -o build/aura_test test/test_aura_ansi.f90 aura_ansi.o aura_theme.o aura_workspace.o \
    aura_config.o aura_session.o aura_pty.o aura_llm.o aura_pty_bridge.o \
    weight_module.o llama2_mod.o
$FC -Ibuild -Jbuild -fmax-stack-var-size=1024 -o build/e2e_final test/e2e_final.f90 aura_ansi.o aura_theme.o aura_workspace.o aura_config.o \
    aura_session.o aura_pty.o aura_llm.o aura_gui.o aura_pty_bridge.o \
    weight_module.o llama2_mod.o

echo "Built: build/aura  build/aura_test  build/e2e_final"
