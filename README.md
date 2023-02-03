# Pheidippides
This is a small little container coordinator that uses systemd-nspawn
to launch mini-instances of the same base container.

## Architecture
The bulk of this is written in Haskell; the main library function,
withContainer, copies a container template to a temporary location
and launches it, then accepts commands to run in it by communicating
over a pipe with a small little C server inside. The toy executable
is just a small program to demonstrate the core library's usage. The
(unfinished) server does server stuff.

## Building
Build the main Haskell executeable with `stack`. The small C server in
`sherver/` can be built with `make -C sherver`; the resulting binary
in `sherver/sherver` must be copied to `/bin/sherver` in the container.

## Usage
The container base image must have an unpriviledged user with UID 1000
and an unpriveiledged group with GID 1000. The executables must be
started as root.
