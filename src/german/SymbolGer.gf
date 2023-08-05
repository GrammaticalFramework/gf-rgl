--# -path=.:../abstract:../common:../prelude

concrete SymbolGer of Symbol = CatGer ** open Prelude, ResGer in {

lin
  SymbPN i = {s = \\c => i.s ; g = Neutr ; n = Sg} ; --- c
  IntPN i  = {s = \\c => i.s ; g = Neutr ; n = Sg} ; --- c
  FloatPN i  = {s = \\c => i.s ; g = Neutr ; n = Sg} ; --- c
  NumPN i  = {s = i.s ! Neutr ; g = Neutr ; n = Sg} ; --- c

  CNIntNP cn i = {
    s = \\b,c => cn.s ! Weak ! Sg ! Nom ++ i.s ;
    a = agrP3 Sg ;
    -- isPron = False ;
    -- isLight = True ; 
    w = WLight ;
    ext,rc = [] -- added
    } ;
  CNSymbNP det cn xs = let g = cn.g in {
    s = \\b,c => det.s ! b ! g ! c ++ 
--               (let k = (prepC c).c in cn.s !  adjfCase det.a k ! det.n ! k) ++ xs.s ; 
               (let k = c in cn.s !  adjfCase det.a k ! det.n ! k) ++ xs.s ; 
    a = agrP3 det.n ;
    -- isPron = False ;
    -- isLight = True ; 
    w = WLight ;
    ext,rc = [] -- added
    } ;
  CNNumNP cn i = {
--    s = \\c => artDefContr (GSg cn.g) c ++ cn.s ! Weak ! Sg ! Nom ++ i.s ! Neutr ! c ;
    s = \\_,c => artDef ! (GSg cn.g) ! c ++ cn.s ! Weak ! Sg ! Nom ++ i.s ! Neutr ! c ; -- HL 8/22 ad hoc
    a = agrP3 Sg ;
    w = WLight ;
    ext,rc = []
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

