--# -path=.:../abstract:../common:../prelude

concrete SymbolHrv of Symbol = CatHrv ** open Prelude, ResHrv in {

lincat
  Symb = {s : Str} ;
lin
  MkSymb s = s ;
  SymbPN s = lin PN {s = \\_ => s.s ; g = Neutr} ;

}
