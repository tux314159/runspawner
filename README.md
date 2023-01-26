# Pheidippides
This is a small little container coordinator that uses systemd-nspawn.

## Architecture
The main program is written in Haskell; it copies a container template
to a temporary location and launches it, then accepts commands to run in
it by communicating over a pipe with a small little C server inside.
