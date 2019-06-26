--# -path=.:../abstract:../common

concrete SymbolLat of Symbol = CatLat ** open Prelude, ResLat, ParadigmsLat, TenseX in {

lin
  SymbPN i = {s = \\n,c => i.s ; g = Neutr} ; --- c
  IntPN i  = {s = \\n,c => i.s ; g = Neutr} ; --- c
  FloatPN i = {s = \\n,c => i.s ; g = Neutr} ; --- c
  NumPN i = {s = \\n,c => i.s ! Neutr ! c; g = Neutr} ; --- c
  CNIntNP cn i = {
    s = \\c => (cn.s ! Sg ! Nom ++ i.s) ;
    g = cn.g ;
    n = Sg ;
    adv = [] ;
    det = { s = \\_,_ => [] ; n = Sg ; sp = \\_,_ => [] } ;
    p = P3 ;
    postap = { s = \\_ => [] } ;
    preap = { s = \\_ => [] } ;
    } ;
  CNSymbNP det cn xs = {
    s = \\c => (cn.s ! Sg ! Nom ++ xs.s ) ;
    g = cn.g ;
    n = det.n ;
    adv = [] ;
    det = det ;
    p = P3 ;
    postap = { s = \\_ => [] } ;
    preap = { s = \\_ => [] } ;
    } ;
--    s = \\c => det.s ++ cn.s ! det.n ! c ++ xs.s ; 
--    a = agrgP3 det.n cn.g
--    } ;
  --    } ;
  CNNumNP cn i = {
    s = \\c => (cn.s ! Sg ! Nom ++ i.s ! cn.g ! Nom ) ;
    g = cn.g ;
    n = Sg ;
    adv = [] ;
    det = { s = \\_,_ => [] ; n = Sg ; sp = \\_,_ => [] } ;
    p = P3 ;
    postap = { s = \\_ => [] } ;
    preap = { s = \\_ => [] } ;
    } ;
--
  SymbS sy = { s = \\_ => sy.s ; neg = \\_ => "" ; o = \\_ => "" ; p = PPos ; sadv = "" ; t = TPres ; v = \\_,_ => "" } ;
--
  SymbNum sy = {s = \\_,_ => sy.s ; n = Pl } ;
  SymbOrd sy = { s = \\g,n,c => sy.s } ; -- does not inflect properly

lincat 
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "et" ;
  ConsSymb = infixSS "et" ;
}
