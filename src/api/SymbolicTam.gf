--# -path=.:../tamil:../common:../abstract:../prelude

resource SymbolicTam = Symbolic with
  (Symbol = SymbolTam),
  (Grammar = GrammarTam) ** open MissingTam in {} ;
}
