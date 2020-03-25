![GF Logo](http://www.grammaticalframework.org/doc/Logos/gf1.svg)

# GF Resource Grammar Library (RGL)

[![Build Status](https://travis-ci.org/GrammaticalFramework/gf-rgl.svg?branch=master)](https://travis-ci.org/GrammaticalFramework/gf-rgl)

The GF Resource Grammar Library is the standard library for Grammatical Framework. It covers the morphology and basic syntax of over 30 languages.

For more about the RGL, see the [synopsis page](http://www.grammaticalframework.org/lib/doc/synopsis/).

## Choose your build method

There are 3 ways to build and install the RGL:

- Haskell script `Setup.hs`
- Shell script `Setup.sh` (does not require Haskell)
- Windows batch file `Setup.bat` (does not require Haskell)

## Install locations

The install scripts will try to determine where to copy the compiled RGL modules.
It will look for, in this order:
- the `--dest=` flag (see below)
- the `GF_LIB_PATH` environment variable
- the file `../gf-core/DATA_DIR` (relative to this directory). This only works if you have the `gf-core` and `gf-rgl` repositories in the same top-level directory **and** you have already compiled GF from source.
(This is considered a bit hacky and will probably disappear in the future).

## Language config

A list of all languages and their properties is maintained centrally in [`languages.csv`](languages.csv).
This file should be kept up-to-date and all build methods should read this config file.
**If you see something wrong, please report/fix it.**

| #  | Column        | Description                              | Default |
|:---|:--------------|:-----------------------------------------|:-------:|
| 1  | Code          | 3-letter language code, e.g. `Eng`       |    -    |
| 2  | Name          | language name in English, e.g. `English` |    -    |
| 3  | Directory     | folder name under `src`, e.g. `english`  |    -    |
| 4  | Functor       | functor name (not used)                  |    -    |
| 5  | Unlexer       | unlexer (not used)                       |    -    |
| 6  | Present       | language is marked with `--# notpresent` |    n    |
| 7  | All           | compile `All` module                     |    y    |
| 8  | Try           | compile `Try` module                     |    y    |
| 9  | Symbolic      | compile `Symbolic` module                |    y    |
| 10 | Compatibility | complile `Compatibility` module          |    n    |
| 11 | Synopsis      | include language in the RGL synopsis     |    n    |

If default is `y` then anything other than `n`, including the empty string, is treated as true (and vice versa when default is `n`).

## Haskell script: `Setup.hs`

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
runghc Setup.hs ...
```

Where `...` is one of:
```
build   [CMDS] [MODE] [--langs=[+|-]LANG,LANG,...] [--gf=...] [--verbose|-v]
copy    [MODE] [--dest=...]
install [CMDS] [MODE] [--langs=[+|-]LANG,LANG,...] [--gf=...] [--dest=...] [--verbose|-v]
clean
```

- `CMDS` is one or more of:
`prelude`,
`all`,
`lang`,
`api`,
`compat`,
or an explicit module name (e.g. `ExtraEng.gf`. You don't need to specify to language subdirectory, but there is a restriction that the module must exist in a **direct** subdirectory of `src`).
If ommitted, the default command is `prelude all`.
- `MODE` is one of:
`present`,
`alltenses`
(default is both).
- `LANG` is a 3-letter language code, e.g. `Eng`, `Swe` etc.
- You can _override_ the default language list with `--langs=...`
- ~~You can _add_ languages to the default list with `--langs=+...`~~
- You can _remove_ languages from the default list with `langs=-...`
- The path to GF installed on your system can be specified via the `--gf` flag (default is that the `gf` executable is in the global system path).
- The `--dest` flag can be used to manually specify where the compiled RGL modules should be copied/installed. This is the same place as `GF_LIB_PATH`.

## Shell script: `Setup.sh`

This method is provided as an alternative for those who don't have Haskell installed.
Simply run the script to build the entire RGL and install in the default location.

You can pass the following flags:
- `--dest=...` to manually specify the install location
- `--gf=...` to specify the path to the `gf` executable, if not available on the system path
- `--verbose` or `-v` to show a list of files being built (errors will always be shown)

## Windows batch file: `Setup.bat`

This method is provided as an alternative for Windows users who don't have Haskell or Bash installed.

It is supposed to be a port of `Setup.sh` and works in largely the same way.
In particular, it accepts the same flags (in the same format) as described above.

However it currently tries to build all modules for all languages and doesn't consider the details of which modules should be compiled for each language (specified in `languages.csv`)

## About this repository

On 2018-07-25, the monolithic [GF repository](https://github.com/GrammaticalFramework/GF)
was split in two:

1. [gf-core](https://github.com/GrammaticalFramework/gf-core) — the GF compiler, shell and runtimes
2. [gf-rgl](https://github.com/GrammaticalFramework/gf-rgl) — the resource grammar library

The former repository is now archived and no longer updated.
The split was performed using [this script](https://github.com/GrammaticalFramework/GF/blob/30ae1b5a5f73513ac5825ca6712186ef8afe9fd4/split/run.sh)
and the output of that script is [here](https://gist.github.com/johnjcamilleri/a6c43ff61f15a9657b457ac94ab7db61).
