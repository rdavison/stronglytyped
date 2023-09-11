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
