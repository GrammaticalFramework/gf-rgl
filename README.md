![GF Logo](http://www.grammaticalframework.org/doc/Logos/gf1.svg)

# GF Resource Grammar Library (RGL)

The GF Resource Grammar Library is the standard library for Grammatical Framework. It covers the morphology and basic syntax of over 30 languages.

For more about the RGL, see the [synopsis page](http://www.grammaticalframework.org/lib/doc/synopsis.html).

## Choose your build method

There are 3 ways to build and install the RGL:

- Haskell script `Make.hs`
- Shell script `Make.sh` (does not require Haskell)
- Windows batch file `Make.bat` (does not require Haskell)

## Install locations

The install scripts will try to determine where to copy the compiled RGL modules.
It will look for, in this order:
- the `--dest=` flag (see below)
- the `GF_LIB_PATH` environment variable
- the file `../gf-core/DATA_DIR` (relative to this directory). This only works if you have the `gf-core` and `gf-rgl` repositories in the same top-level directory **and** you have already compiled GF from source.
(This is considered a bit hacky and will probably disappear in the future).

## Haskell script: `Make.hs`

This build method gives you most options.
You will need Haskell installed on your system.

### Basic

If you have `Make` installed and don't care about advanced settings,
you can compile the RGL and install it to the default location with:

```
make install
```

This is the same as `make build` followed by `make copy`.
There is also `make clean` available.

### Advanced

For more fine-grained control over the build process, you can run the build script directly:

```
runghc Make.hs ...
```

Where `...` is one of:
```
build   [CMD] [MODE] [--langs=[+|-]LANG,LANG,...] [--gf=...]
copy    [--dest=...]
install [CMD] [MODE] [--langs=[+|-]LANG,LANG,...] [--gf=...] [--dest=...]
clean
```

- `CMD` is one of:
`prelude`,
`all`,
`lang`,
`api`,
`compat`,
`pgf`,
`parse`
(default is `all`)
- `MODE` is one of:
`present`,
`alltenses`
(default is both)
- You can _override_ the default language list with `--langs=...`
- You can _add_ languages to the default list with `--langs=+...`
- You can _remove_ languages from the default list with `langs=-...`
- `LANG` is a 3-letter language code, e.g. `Eng`, `Swe` etc.
- The path to GF installed on your system can be specified via the `gf` flag (default is that the `gf` executable is in the global system path).
- The `to` flag can be used to manually specify where the compiled RGL modules should be copied/installed. This is the same place as `GF_LIB_PATH`.

## Shell script: `Make.sh`

This method is provided as an alternative for those who don't have Haskell installed. Simply run the script to build the entire RGL and install in the default location:

You can pass the following flags:
- `--dest=...` to manually specify the install location
- `--gf=...` to specify the path to the `gf` executable, if not available on the system path

## Windows batch file: `Make.bat`

This method is provided as an alternative for Windows users who don't have Haskell installed.

**COMING SOON**

## About this repository

On 2018-07-25, the monolithic [GF repository](https://github.com/GrammaticalFramework/GF)
was split in two:

1. [gf-core](https://github.com/GrammaticalFramework/gf-core) —  the GF compiler, shell and runtimes
2. [gf-rgl](https://github.com/GrammaticalFramework/gf-rgl) — the resource grammar library

The former repository is now archived and no longer updated.
The split was performed using [this script](https://github.com/GrammaticalFramework/GF/blob/30ae1b5a5f73513ac5825ca6712186ef8afe9fd4/split/run.sh)
and the output of that script is [here](https://gist.github.com/johnjcamilleri/a6c43ff61f15a9657b457ac94ab7db61).
