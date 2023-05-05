#!/bin/sh

COMMIT_TIME="$(git log --pretty=format:'%at' -n 1)"

FUZZER_DIR=${1}
OTP_DIR=${2}
SEED=${3:-${COMMIT_TIME}}


set -euxo pipefail

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

mkdir ${FUZZER_DIR}
cd ${FUZZER_DIR}
git clone https://github.com/WhatsApp/erlfuzz.git
cd erlfuzz

cargo build --release

mkdir -p out
mkdir -p interesting
mkdir -p minimized

N=100000

echo "Fuzzing erl"
echo "Using seed: ${SEED}"
echo "Generating ${N} test cases"

seq ${N} | parallel --line-buffer "./target/release/erlfuzz fuzz -c ./run_erl_once.sh --tmp-directory out --interesting-directory interesting --minimized-directory minimized --seed ${SEED} test{}"


echo "Fuzzing complete"
