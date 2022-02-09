#!/usr/bin/env bash
# tiny script to run the bot repeatedly
while true; do
    # if there's an error here we need to print it somehow?
    # i think a better idea would be to have the rust code run `git pull && cargo build`
    # forward any errors to the user, and then have the shell `cargo run`
    # we also should forward panics but that's a different ~~disco~~ issue
    git pull
    cargo run
done