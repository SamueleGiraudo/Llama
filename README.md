# Llama
`||__ /\/\ /\/\`

A minimalistic logic programming language.

Copyright (C) 2024--2024 [Samuele Giraudo](https://igm.univ-mlv.fr/~giraudo/) -
`giraudo.samuele@uqam.ca` -


## Quick overview
TODO


## First examples
TODO


## Versions
Here is the [changelog](Versions.md) of the different versions.


### Dependencies
The following programs or libraries are needed:

+ `pkg-config`
+ `make`
+ `ocaml` (Version `>= 5.2.0`. An inferior but not too old version may be suitable.)
+ `opam`
+ `ocamlbuild` (Available by `opam install ocamlbuild`.)
+ `ocamlfind` (Available by `opam install ocamlfind`.)
+ `menhir` (Available by `opam install menhir`.)


### Building
Here are the required steps to build the interpreter `Llama`:

1. Clone the repository somewhere by running
   `git clone https://github.com/SamueleGiraudo/Llama.git`.

2. Install all dependencies (see the section above).

3. Build the project by running `make noassert`.

This creates an executable `llama`. The following sections explain how to use it.


## User guide
This [page](Help.md) contains the description of the Llama language.

Llama program files must have `.llm` as extension. The main command is

```
./Llama [--help] [--version] --file PATH [--verbose L] [--max-depth D] [--max-cardinality C]
```

where

+ `--help` prints the short help.
+ `--version` prints the version and other information.
+ `--file PATH` sets `PATH` as the path to the Llama program to consider.
+ `--verbose L` enables the verbose mode at level L from 0 (nothing) to 2 (full). By
default, the level is 1.
+ `--max-depth D` sets `D` as the maximal search depth in the resolution.
+ `--max-cardinality C` sets `C` as the maximal number of computed solutions.

### Standard library
The [standard library](Stdlib) contains some useful definitions.

### Examples
The directory [Lab](Lab) contains some examples (awaiting full documentation!).

### Documentation of the standard library
TODO


## Miscellaneous
To get the syntax highlighting in the text editor `vim` for the Llama language, put the
file [llm.vim](Vim/syntax/llm.vim) at `~/.vim/syntax/llm.vim` and the file
[llm.vim](Vim/ftdetect/llm.vim) at `~/.vim/fdetect/llm.vim`.


## Theoretical aspects
TODO


### Bibliography
TODO

