--# -path=.:../somali:../common:../abstract:../prelude

resource SymbolicSom = Symbolic with
  (Symbol = SymbolSom),
  (Grammar = GrammarSom) ** open MissingSom in {} ;
