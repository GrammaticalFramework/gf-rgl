--# -path=.:../abstract:../common:../prelude

concrete SymbolGer of Symbol = CatGer ** open Prelude, ResGer in {

lin
  SymbPN i = {s = \\c => i.s ; g = Neutr} ; --- c
  IntPN i  = {s = \\c => i.s ; g = Neutr} ; --- c
  FloatPN i  = {s = \\c => i.s ; g = Neutr} ; --- c
  NumPN i  = {s = i.s ! Neutr ; g = Neutr} ; --- c

  CNIntNP cn i = {
    s = \\c => cn.s ! Weak ! Sg ! Nom ++ i.s ;
    a = agrP3 Sg ;
    -- isPron = False ;
    -- isLight = True ; 
    w = WLight ;
    ext,rc = [] -- added
    } ;
  CNSymbNP det cn xs = let g = cn.g in {
    s = \\c => det.s ! g ! c ++ 
               (let k = (prepC c).c in cn.s !  adjfCase det.a k ! det.n ! k) ++ xs.s ; 
    a = agrP3 det.n ;
    -- isPron = False ;
    -- isLight = True ; 
    w = WLight ;
    ext,rc = [] -- added
    } ;
  CNNumNP cn i = {
    s = \\c => artDefContr (GSg cn.g) c ++ cn.s ! Weak ! Sg ! Nom ++ i.s ! Neutr ! (prepC c).c ;
    a = agrP3 Sg ;
    -- isPron = False ;
    -- isLight = True ; 
    w = WLight ;
    ext,rc = [] -- added
    } ;

  SymbS sy = {s = \\_ => sy.s} ;

  SymbNum n = {s = \\_,_ => n.s ; n = Pl ; isNum = True} ;
  SymbOrd n = {s = \\_   => glue n.s "."} ;


lincat 

  Symb, [Symb] = SS ;

lin

  MkSymb s = s ;

  BaseSymb = infixSS "und" ;
  ConsSymb = infixSS "," ;

}

