# A pretty printer for Logipedia

A quick translator from a
[Logipedia](https://github.com/deducteam/logipedia) `ppterm` to a latex
string.

## What is this all about
This repository contains pretty printers for
[Logipedia](https://github.com/deducteam/logipedia.git), an encyclopedia of
proofs powered by [Dedukti](https://github.com/deducteam/dedukti.git).

This encyclopedia can be web browsed.  To obtain web pages, files checked by
Dedukti are transformed to `json` files.

Those json files contain mainly terms, represented according to a particular
schema drafted
[here](https://raw.githubusercontent.com/Deducteam/Logipedia/json_export/schemas/ppterm.json).

As nobody like to read json, a _pretty printer_ converts one of these terms to a
human readable one, or at least into a term that can be fed to something that
can do the job.

A pretty printer has to be act as a step in a pipeline, so its mission is to
transform a json term from standard input to another one on standard output.

There are two implementations, one relatively basic in Swi-Prolog, and another
one in Guile Scheme.

## Guile scheme
See [scheme readme](file:scheme/README.md).

## Prolog

### Requirements
- [swipl](https://www.swi-prolog.org) 8.0
  
### Building the binary
`make` yields a `logipp` binary.

### Usage
The usage is the same for any implementation: it takes a json ppterm from stdin
and outputs the conversion to stdout.
``` sh
./logipp < <term>.json
```
or
``` sh
cat <file>.json | ./logipp
```

### Examples
```sh
./to_latex.pl < tests/nat_le.json
\left(Π x: nat.nat, \left(Π y: nat.nat, Prop\right)\right)
./to_latex.pl < tests/omega.json
\left(λ x, \left(x\, x\right)\right)
```
