# A pretty printer for Logipedia

A quick translator from a
[Logipedia](https://github.com/deducteam/logipedia) `ppterm` to a latex
string.

There are two implementations, one in Swi-Prolog, and the other in Guile Scheme.

## Requirements
- [swipl](https://www.swi-prolog.org) 8.0

or
- [guile scheme](https://www.gnu.org/software/guile) 2.2
- [guile json](https://savannah.nongnu.org/projects/guile-json/)
  (which is available in debian repositories)
  
## Building the binary
### Prolog
`make prolog` yields a `logipp` binary.

## Usage
The usage is the same for any implementation: it takes a json ppterm from stdin
and outputs the conversion to stdout.
```sh
./logipp < <term>.json
```
or
```sh
cat <file>.json | ./logipp
```

### Scheme

``` sh
guile to_latex.scm < <term>.json
```

## Examples
```sh
./to_latex.pl tests/nat_le.json
\left(Π x: nat.nat, \left(Π y: nat.nat, Prop\right)\right)
./to_latex.pl tests/omega.json
\left(λ x, \left(x\, x\right)\right)
```

## Input
The program takes as input Logipedia ppterms as specified in the
`term` definition of schema
(https://raw.githubusercontent.com/Deducteam/Logipedia/json_export/schemas/ppterm.json).
