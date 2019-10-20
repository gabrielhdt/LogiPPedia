# A Guile scheme pretty printer for Logipedia

## Requirements
- [guile scheme](https://www.gnu.org/software/guile) 2.2
- [guile json](https://savannah.nongnu.org/projects/guile-json/)
  (which is available in debian repositories) 3.2
  
## Installation
`make install` will install the scheme script `logipp-latex` to
`/usr/local/bin`.

## Usage
With `f.json` containing a single ppterm,
``` sh
logipp-latex < term.json
```
outputs a latex string to stdout.
