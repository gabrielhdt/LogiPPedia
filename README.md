# A pretty printer for logipedia

A quick translator from a
[Logipedia](https://github.com/deducteam/logipedia) `ppterm` to a latex
string.  *This is designed to be a prototype only*.

## Requirements
- swipl

## Usage
```
./ppterm.pl <term>.json
```
or
```
make
./logipp <term>.json
```

## Examples
```
$ ./ppterm.pl tests/nat_le.json
\left(Π x: \left(nat.nat\, \right), \left(Π y: \left(nat.nat\,
\right), \left(Prop\, \right)\right)\right)
```
