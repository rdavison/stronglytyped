# pspsps

## Developer Setup

Make sure you have nvm installed. Then you can:

```
$ nvm use
$ make install-deps
$ make
```

The default target for `make` will build in watch mode, so any changes will get automatically recompiled. To run the generator:

```
$ npx nsy dune exec bin/main.exe
```

If you want to pass args to `main.exe`, you can do so like this:

```
$ npx nsy dune exec bin/main.exe -- foo --bar
```

Args must be passed in after the double dash: `--`