# Czech RGL regression tests

From the RGL checkout, run:

    sh tests/czech/check.sh

The script uses `gf` from `PATH` when `GF` is unset or empty. To select another
executable, including a path containing spaces, use:

    GF='/path/to/gf' sh tests/czech/check.sh

`GF` names one executable, not a command with flags. Generated files go into
a temporary directory removed on exit. No application grammar is required.

`regressions.gfs` checks morphology and grammatical composition through the
source API, grouped by feature. Its expected output is in `regressions.out`.

The runner also builds `AllCze`, `TryCze` and `SymbolicCze` into a fresh
directory, then compiles `CzeTests.gf` using only that distribution.
Typed declarations check every public V2/V3 paradigm overload. The runner
requires the consumer's `.gfo`, since GF can report errors and still exit
successfully.
