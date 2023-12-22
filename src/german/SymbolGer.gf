--# -path=.:../abstract:../common:../prelude

concrete SymbolGer of Symbol = CatGer ** open Prelude, ResGer in {

lin
  SymbPN i = {s = \\c => i.s ; g = Neutr ; n = Sg} ; --- c
  IntPN i  = {s = \\c => i.s ; g = Neutr ; n = Sg} ; --- c
  FloatPN i  = {s = \\c => i.s ; g = Neutr ; n = Sg} ; --- c
  NumPN i  = {s = \\c => i.s ! APred ; g = Neutr ; n = Sg} ; --- c  -- HL

  CNIntNP cn i = {
    s = \\_,c => cn.s ! Weak ! Sg ! Nom ++ i.s ;
    a = agrP3 Sg ;
    w = WLight ;
    ext,rc = []
    } ;
  CNSymbNP det cn xs = let g = cn.g in {
    s = \\b,c => det.s ! b ! g ! c ++ cn.s ! adjfCase det.a c ! det.n ! c ++ xs.s ; 
    a = agrP3 det.n ;
    w = WLight ;
    ext,rc = []
    } ;
  CNNumNP cn i = {
    s = \\b,c => case b of {True => [] ; False => artDef ! (GSg cn.g) ! c}
                 ++ cn.s ! Weak ! Sg ! Nom ++ i.s ! AMod (GSg Neutr) c ;
    a = agrgP3 cn.g Sg ;  -- HL 27.9.2023
    w = WDefArt ;         -- im Haus 14
    ext,rc = []
    } ;

  SymbS sy = {s = \\_ => sy.s} ;

  SymbNum n = {s = \\_ => n.s ; n = Pl ; isNum = True} ;
  SymbOrd n = {s = \\_ => glue n.s "."} ;


lincat 

  Symb, [Symb] = SS ;

lin

  MkSymb s = s ;

  BaseSymb = infixSS "und" ;
  ConsSymb = infixSS "," ;

}

