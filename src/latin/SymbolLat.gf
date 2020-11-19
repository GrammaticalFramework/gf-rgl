--# -path=.:../abstract:../common

concrete SymbolLat of Symbol = CatLat ** open Prelude, ResLat, ParadigmsLat, TenseX in {

  lin
    -- SymbPN : Symb -> PN ;
    SymbPN i = {s = \\c => i.s ; g = Neutr ; n = Sg } ; --- c
    -- IntPN : Int -> PN ;
    IntPN i  = {s = \\c => i.s ; g = Neutr ; n = Sg } ; --- c
    -- FloatPN : Float -> PN ;
    FloatPN i = {s = \\c => i.s ; g = Neutr ; n = Sg } ; --- c
    -- NumPN : Num -> PN ;
    NumPN i = {s = \\c => i.s ! Neutr ! c; g = Neutr ; n = Pl } ; --- c
    -- CNIntNP : CN -> Int -> NP ;
    CNIntNP cn i = {
      s = \\_,c => (cn.s ! Sg ! Nom ++ i.s) ;
      g = cn.g ;
      n = Sg ;
      adv = [] ;
      det = { s , sp = \\_ => [] ; n = Sg } ;
      p = P3 ;
      preap , postap = { s = \\_ => [] } ;
      } ;
    --CNSymbNP : CN -> Symb -> NP ;
    CNSymbNP det cn xs = {
      s = \\_,c => (cn.s ! Sg ! Nom ++ xs.s ) ;
      g = cn.g ;
      n = det.n ;
      adv = [] ;
      det = { s = det.s ! cn.g ; sp = det.sp ! cn.g } ;
      p = P3 ;
      preap , postap = { s = \\_ => [] } ;
      } ;
--    s = \\c => det.s ++ cn.s ! det.n ! c ++ xs.s ; 
--    a = agrgP3 det.n cn.g
--    } ;
    --    } ;
    
    -- CNNumNP : CN -> Num -> NP ;
    CNNumNP cn i = {
      s = \\_,c => (cn.s ! Sg ! Nom ++ i.s ! cn.g ! Nom ) ;
      g = cn.g ;
      n = Sg ;
      adv = [] ;
      det = { s , sp = \\_ => [] ; n = Sg };
      p = P3 ;
      preap , postap = { s = \\_ => [] } ;
      } ;
--
    -- SymbS : Symb -> S ;
    SymbS sy = { s = \\_ => sy.s ; o , neg = \\_ => "" ; p = PPos ; sadv = "" ; t = TPres ; v = \\_ => "" ; compl = "" ; det = { s , sp = \\_ => [] ; n = Sg } } ;

    -- SymbNum : Symb -> Num 
    SymbNum sy = {s = \\_,_ => sy.s ; n = Pl } ;
    -- SymbOrd : Symb -> Ord
    SymbOrd sy = { s = \\g,n,c => sy.s } ; -- does not inflect properly

lincat 
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "et" ;
  ConsSymb = infixSS "et" ;
}
