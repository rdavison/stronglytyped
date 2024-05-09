# StronglyTyped Keyboard Layout Analyzer and Generator (WIP)

## Developer Setup

Make sure you have opam installed. Then you can:

```
$ opam switch create . 5.0.0
$ opam install . --deps-only
$ dune exec bin/main.exe
```

You can also build the code in watch mode with:

```
$ dune build @all -w
```

## Usage

This project is not currently very user friendly. If you want to hack on it, then usually I do the following:
```
eval $(opam env)
dune exec bin/main.exe gen shai.txt.sexp
```

This will load `shai.txt.sexp` as the corpus data and start generating layouts. By default this will try to generate a keyboard with an ansi layout and only two layers. Some files which might be relevant for tweaking layout generation params are: cli.ml, score.ml, stats.ml, and layout.ml.