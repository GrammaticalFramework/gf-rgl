--# -path=.:../abstract:../common

concrete SymbolLat of Symbol = CatLat ** open Prelude, ResLat, ParadigmsLat, TenseX in {

lin
  SymbPN i = {s = \\n,c => i.s ; g = Neutr} ; --- c
  IntPN i  = {s = \\n,c => i.s ; g = Neutr} ; --- c
  FloatPN i = {s = \\n,c => i.s ; g = Neutr} ; --- c
  NumPN i = {s = \\n,c => i.s ! Neutr ! c; g = Neutr} ; --- c
--  CNIntNP cn i = {
--    s = \\c => (cn.s ! Sg ! Nom ++ i.s) ;
--    a = agrgP3 Sg cn.g
--    } ;
--  CNSymbNP det cn xs = {
--    s = \\c => det.s ++ cn.s ! det.n ! c ++ xs.s ; 
--    a = agrgP3 det.n cn.g
--    } ;
--  CNNumNP cn i = {
--    s = \\c => (cn.s ! Sg ! Nom ++ i.s) ;
--    a = agrgP3 Sg cn.g
--    } ;
--
  SymbS sy = { s = \\_ => sy.s ; neg = \\_ => "" ; o = \\_ => "" ; p = PPos ; sadv = "" ; t = TPres ; v = \\_ => "" } ;
--
  --  SymbNum sy = {s = sy.s ; n = Pl ; hasCard = True} ;
  SymbOrd sy = { s = \\g,n,c => sy.s } ; -- does not inflect properly

lincat 
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

--  BaseSymb = infixSS "and" ;
--  ConsSymb = infixSS "," ;
}
