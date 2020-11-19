--# -path=.:../latin:../common:../abstract:../prelude

resource SymbolicLat = Symbolic with 
  (Symbol = SymbolLat),
  (Grammar = GrammarLat) ** open MissingLat in {} ;
