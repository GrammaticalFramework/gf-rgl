--# -path=.:../abstract:../common:../prelude

concrete SymbolEst of Symbol = CatEst ** open Prelude, NounEst, ResEst in {

lin
  SymbPN i = {s = \\c => i.s} ; --- c
  IntPN i  = {s = \\c => i.s} ; --- c
  FloatPN i  = {s = \\c => i.s} ; --- c
  NumPN i  = {s = \\c => i.s!Sg!Nom } ; --- c

  CNIntNP cn i = let np : NP = NounEst.MassNP cn in np ** {
    postmod = np.postmod ++ i.s ;
    } ;
  CNSymbNP det cn xs = let np : NP = NounEst.DetCN det cn in np ** {
    postmod = np.postmod ++ xs.s ;
    } ;
  CNNumNP cn i = let np : NP = NounEst.MassNP cn in np ** {
    postmod = np.postmod ++ i.s ! Sg ! Nom ;
    } ;

  SymbS sy = sy ;

  SymbNum n = {s = \\_,_ => n.s ; isNum = True ; n = Pl} ;
  SymbOrd n = {s = \\_ => glue n.s "."} ;

lincat

  Symb, [Symb] = SS ;

lin

  MkSymb s = s ;

  BaseSymb = infixSS "ja" ;
  ConsSymb = infixSS "," ;

}

