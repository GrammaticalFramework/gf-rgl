--# -path=.:../slovak:../common:../abstract:../prelude

resource SymbolicHrv = Symbolic with
  (Symbol = SymbolHrv),
  (Grammar = GrammarHrv)
  ** open MissingHrv in {}
