#!/bin/bash

FUZZER_DIR=${1}
OTP_DIR=${2}
OUT=${3}
N=${4}


set -exo pipefail

# To update erlfuzz, update this to a later commit hash, branch or tag
ERLFUZZ_VERSION=c9364609b8944c71c8e6184abd8793477772862b

# Install Rust non-interactively
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain nightly
PATH=$HOME/.cargo/bin:$PATH
sudo apt-get install parallel

mkdir "../${FUZZER_DIR}"
cd "../${FUZZER_DIR}"
git clone https://github.com/WhatsApp/erlfuzz.git
cd erlfuzz
git checkout ${ERLFUZZ_VERSION}

# Be permissive when building erlfuzz - we don't need it to we warning-free for
# erlfuzz to be useful
RUSTFLAGS=-Awarnings $HOME/.cargo/bin/cargo build --release

mkdir -p out-erl
mkdir -p out-erlc-opts
mkdir -p out-jit
mkdir -p interesting-erl
mkdir -p interesting-erlc-opts
mkdir -p interesting-jit
mkdir -p minimized-erl
mkdir -p minimized-erlc-opts
mkdir -p minimized-jit

echo "Fuzzing erl"
echo "Generating ${N} test cases"

seq ${N} | parallel --line-buffer "./target/release/erlfuzz fuzz-and-reduce -c ./run_erl_once.sh --tmp-directory out-erl --interesting-directory interesting-erl --minimized-directory minimized-erl test{}"
seq ${N} | parallel --line-buffer "./target/release/erlfuzz --deterministic --wrapper printing fuzz-and-reduce -c ./verify_erlc_opts.sh --tmp-directory out-erlc-opts --interesting-directory interesting-erlc-opts --minimized-directory minimized-erlc-opts test{}"
seq ${N} | parallel --line-buffer "./target/release/erlfuzz --deterministic --wrapper printing fuzz-and-reduce -c ./verify_erl_jit.sh --tmp-directory out-jit --interesting-directory interesting-jit --minimized-directory minimized-jit test{}"

echo "Fuzzing complete. Collating results."

mv minimized-erl "${OUT}"/minimized-erl
mv minimized-erlc-opts "${OUT}"/minimized-erlc-opts
mv minimized-jit "${OUT}"/minimized-jit

echo "Results written to: ${OUT}/minimized-erl, "${OUT}"/minimized-erlc-opts and "${OUT}"/minimized-jit"
