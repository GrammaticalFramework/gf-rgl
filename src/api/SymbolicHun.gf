--# -path=.:../hungarian:../common:../abstract:../prelude

resource SymbolicHun = Symbolic with
  (Symbol = SymbolHun),
  (Grammar = GrammarHun) ** open MissingHun in {} ;
