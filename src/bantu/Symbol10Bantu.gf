incomplete concrete SymbolBantu of Symbol = 
  CatBantu ** open Prelude, CommonBantu, ResBantu in {

lin
  SymbPN i = {s = i.s ; g = Masc} ;
  IntPN i  = {s = i.s ; g = Masc} ;
  FloatPN i  = {s = i.s ; g = Masc} ;
  NumPN i  = {s = i.s!Masc ; g = Masc} ;

  CNIntNP cn i = heavyNP {
    s = \\c => prepCase c ++ cn.s ! Sg ++ i.s ;
    a = agrP3 cn.g Sg ;
    hasClit = False
    } ;
  CNSymbNP det cn xs = let g = cn.g in heavyNP {
    s = \\c => det.s ! g ! c ++ cn.s ! det.n ++ xs.s ; 
    a = agrP3 g det.n ;
    hasClit = False
    } ;
  CNNumNP cn i = heavyNP {
    s = \\c => artDef False cn.g Sg c ++ cn.s ! Sg ++ i.s ! Masc ;
    a = agrP3 cn.g Sg ;
    hasClit = False
    } ;
  SymbS sy = {s = \\_ => sy.s} ;

  SymbNum n = {s = \\_ => n.s ; isNum = True ; n = Pl} ;
  SymbOrd n = {s = \\_ => n.s ++ BIND ++ "º"} ; -- feminine variant ª, also variants 1.º and 1.ª

lincat 

  Symb, [Symb] = SS ;

lin

  MkSymb s = s ;

  BaseSymb = infixSS "et" ; ----
  ConsSymb = infixSS "," ;

}
