--# -path=.:../korean:../common:../abstract:../prelude

resource SymbolicKor = Symbolic with
  (Symbol = SymbolKor),
  (Grammar = GrammarKor) ** open MissingKor in {} ;
