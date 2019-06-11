#!/bin/sh

export PATH="$HOME/.cargo/bin:$PATH"
export ROCKET_ENV=production
export ROCKET_PORT=$PORT
rustup run $(cat rust-toolchain) target/release/priroda example.rs --sysroot $(rustc +nightly --print sysroot)
