--# -path=.:../abstract:../common:../prelude

concrete SymbolHrv of Symbol = CatHrv ** open Prelude, ResHrv in {

lincat
  Symb = {s : Str} ;
lin
  MkSymb s = s ;
  SymbPN s = lin PN {s = \\_ => s.s ; g = Neutr} ;

  IntPN s = lin PN {s = \\_ => s.s ; g = Neutr} ;

  SymbNum s = lin Card {s = \\_,_ => s.s ; size = NS_20_} ; --- size

}
