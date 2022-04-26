--# -path=.:../abstract:../common:../prelude

concrete SymbolCze of Symbol = CatCze ** open Prelude, ResCze in {

lincat
  Symb = {s : Str} ;
lin
  MkSymb s = s ;
  SymbPN s = lin PN {s = \\_ => s.s ; g = Neutr} ;

}
