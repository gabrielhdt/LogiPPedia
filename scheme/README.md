# A Guile scheme pretty printer for Logipedia

## Requirements
- [guile scheme](https://www.gnu.org/software/guile) 2.2
- [guile json](https://savannah.nongnu.org/projects/guile-json/)
  (which is available in debian repositories) 3.2
  
## Installation
`make install` will install the scheme script `logipp-latex` to
`/usr/local/bin`.  Guile modules are install to the site-dir.

## Usage
With `f.json` containing a single ppterm,
``` sh
logipp-latex < f.json
```
outputs a latex string to stdout.

### URI conversion
Logipedia uses URIs for constant, such as `sttfa:sttfa/eta.cst`.  As they are
not really readable, one can supply an (scheme) association list from URIs to
symbols, such as
```scheme
(( "sttfa:sttfa/eta.cst"   . "η" )
 ( "sttfa:div_mod/mod.cst" . "≡" ))
```
If this association list is in a file, say `uriconv.scm`, it can be used with
the `--uriconv` command line parameter,
```sh
<input json> | logipp-latex --uriconv uriconv.scm
```
