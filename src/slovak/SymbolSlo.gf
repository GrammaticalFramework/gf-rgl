--# -path=.:../abstract:../common:../prelude

concrete SymbolSlo of Symbol = CatSlo ** open Prelude, ResSlo in {

lincat
  Symb = {s : Str} ;
lin
  MkSymb s = s ;
  SymbPN s = lin PN {s = \\_ => s.s ; g = Neutr} ;

}
