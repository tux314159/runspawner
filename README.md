# Pheidippides
This is a small little container coordinator that uses systemd-nspawn
to launch mini-instances of the same base container.

## Architecture
The main program is written in Haskell; it copies a container template
to a temporary location and launches it, then accepts commands to run in
it by communicating over a pipe with a small little C server inside.

## Building
Build the main Haskell executeable with `stack`. The small C server in
`sherver/` can be built with `make -C sherver`; the resulting binary
int `sherver/sherver` must be copied to `/bin/sherver` in the container.

## Usage
The container base image must have an unpriviledged user with UID 1000
and an unpriveiledged group with GID 1000. The main executable must be
started as root.
