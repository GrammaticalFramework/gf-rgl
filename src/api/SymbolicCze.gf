--# -path=.:../czech:../common:../abstract:../prelude

resource SymbolicCze = Symbolic with
  (Symbol = SymbolCze),
  (Grammar = GrammarCze)
  ** open MissingCze in {}
