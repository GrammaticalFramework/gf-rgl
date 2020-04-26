--# -path=.:../slovak:../common:../abstract:../prelude

resource SymbolicSlo = Symbolic with
  (Symbol = SymbolSlo),
  (Grammar = GrammarSlo)
  ** open MissingSlo in {}
