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
`markup.gfs` separately checks clitic movement through fronting, embedding
and coordination, including discontinuous marked constituents. It also checks
that NP predetermination preserves markup scope when inserting before a modifier.
Source computation exposes `Predef.BIND` and `Predef.SOFT_BIND` markers;
PGF linearization handles them as token joining and punctuation.

The runner also builds `AllCze`, `TryCze` and `SymbolicCze` into a fresh
directory, then compiles `CzeTests.gf` using only that distribution.
Typed declarations check every public V2/V3 paradigm overload. The runner
requires the consumer's `.gfo`, since GF can report errors and still exit
successfully.

`CzeRoundTrip` and `roundtrip.tsv` check PGF generation and parsing, including
joined negation, polarity recovery and polite/plural ambiguity.
The abstract fragments are acyclic: parsing must return the complete
expected set of trees, without truncation or reliance on enumeration order.
Repeated strings record distinct intended analyses. These small fragments
do not establish parsing coverage for the full Czech RGL.

Preserve the empty `Pol.s` constituent: selecting a verb form with `pol.p`
alone does not recover polarity. For example, parsing `nečti ji` should give:

    UttImpSg PNeg (ImpVP (ComplSlash (SlashV2a read_V2) (UsePron she_Pron)))

Removing the constituent leaves `?1` in place of `PNeg`, even though the
negative spelling is recognized.

Prefer treebank coverage where practical. Keep source checks for paradigms
and distinctions outside those fragments; do not repeat treebank examples
unless the source API adds a separate contract. These tests cover present
clauses, infinitives and imperatives, not the unimplemented RGL tenses or
anteriority.

Coordinated-subject person/number agreement and subordinate-clause punctuation
also retain inherited gaps. The finite fragments do not establish coverage
of those constructions.
