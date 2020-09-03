--# -path=.:../malay:../common:../abstract:../prelude

resource SymbolicMay = Symbolic with
  (Symbol = SymbolMay),
  (Grammar = GrammarMay) ** open MissingMay in {} ;
