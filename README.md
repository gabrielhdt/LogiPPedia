# A pretty printer for Logipedia

A quick translator from a
[Logipedia](https://github.com/deducteam/logipedia) `ppterm` to a latex
string.  *This is designed to be a prototype only*.

## Requirements
- [swipl](https://www.swi-prolog.org)

## Usage
```
./to_latex.pl <term>.json
```
or
```
make
./logipp <term>.json
```

## Examples
```
$ ./to_latex.pl tests/nat_le.json
\left(Π x: nat.nat, \left(Π y: nat.nat, Prop\right)\right)
$ ./to_latex.pl tests/omega.json
\left(λ x, \left(x\, x\right)\right)
```

## Input
The program takes as input Logipedia ppterms as specified in the
`term` definition of schema
(https://raw.githubusercontent.com/Deducteam/Logipedia/json_export/schemas/ppterm.json).
