--# -path=.:../abstract:../common

concrete SymbolTur of Symbol = CatTur ** open Prelude, ResTur, HarmonyTur in {

lin
  SymbPN i = {s,gen = \\_,_ => i.s ; h = {vow=I_Har; con=SCon Soft}} ;  -- just a placeholder, probably wrong

{- TODO!
  IntPN i  = {s = addGenitiveS i.s ; g = Neutr} ;
  FloatPN i = {s = addGenitiveS i.s ; g = Neutr} ;
  NumPN i = {s = i.s ; g = Neutr} ;
  CNIntNP cn i = {
    s = \\c => cn.s ! Sg ! Nom ++ (addGenitiveS i.s) ! c ;
    a = agrgP3 Sg cn.g
    } ;
  CNSymbNP det cn xs = {
    s = \\c => det.s ++ cn.s ! det.n ! Nom ++ (addGenitiveS xs.s) ! c ; 
    a = agrgP3 det.n cn.g
    } ;
  CNNumNP cn i = {
    s = \\c => cn.s ! Sg ! Nom ++ i.s ! c ;
    a = agrgP3 Sg cn.g
    } ;

  SymbS sy = sy ; 

  SymbNum sy = { s = addGenitiveS sy.s ; n = Pl ; hasCard = True } ;
  SymbOrd sy = { s = \\c => sy.s ++ (regGenitiveS "th")!c} ;
-}

lincat 

  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

--  BaseSymb = infixSS "and" ;
  ConsSymb = infixSS "," ;


}
