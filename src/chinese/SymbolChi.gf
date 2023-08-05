--# -path=.:../abstract:../common:../prelude

concrete SymbolChi of Symbol = CatChi ** open Prelude, ResChi in {

  flags coding = utf8;

 lin
  SymbPN i = i ;
  IntPN i  = i ;
  FloatPN i = i ;
  NumPN i = i ;
  CNIntNP cn i = {
    s = cn.s ++ i.s ;
    det = cn.c
    } ;
  CNSymbNP det cn xs = {det = det.s ; s = cn.s ++ xs.s} ; ----
  CNNumNP cn i = {
    s = cn.s ++ i.s ;
    det = cn.c
    } ;

  SymbS sy = simpleS sy.s ;
  SymbNum sy = sy ;
  SymbOrd sy = sy ;

lincat

  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "" ;
  ConsSymb = infixSS "" ;

}
