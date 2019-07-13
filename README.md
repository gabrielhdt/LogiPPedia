# Ppterm to LaTeX

A quick translator from a
[Logipedia](https://github.com/deducteam/logipedia) to a latex
string.

## Usage
```
./ppterm.pl <term>.json
```

## Examples
```
$ ./ppterm.pl tests/nat_le.json
\left(Π x: \left(nat.nat\, \right), \left(Π y: \left(nat.nat\,
\right), \left(Prop\, \right)\right)\right)
```
