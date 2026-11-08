concrete NumeralNor of Numeral = CatNor [Numeral,Digits,Decimal] ** open MorphoNor, Prelude in {
  flags coding=utf8 ;

lincat 
  Digit = {s : DForm => CardOrd => Str} ;
  Sub10 = {s : DForm => CardOrd => Str ; n : Number} ;
  Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 =
          {s :          CardOrd => Str ; n : Number} ;

lin 
  num x = x ;

  n2 = mkTal "to"   "tolv"    "tjue"   "andre"   "tolfte" ;
  n3 = mkTal "tre"  "tretten" "tretti" "tredje"  "trettende" ;
  n4 = mkTal "fire" "fjorten" "førti"  "fjerde"  "fjortende" ;
  n5 = mkTal "fem"  "femten"  "femti"  "femte"   "femtende" ;
  n6 = mkTal "seks" "seksten" "seksti" "sjette"  "sextende" ;
  n7 = mkTal "sju"  "sytten"  "sytti"  "syvende" "syttende" ;
  n8 = mkTal "åtte" "atten"   "åtti"   "åttende" "attende" ;
  n9 = mkTal "ni"   "nitten"  "nitti"  "niende"  "nittende" ;

  pot01 = {
    s = \\f => table {
          NCard g => case g of {NNeutr => "ett" ; NUtr _ => "en"} ; ---- ei ?
          _ => "første"
          } ; 
    n = Sg
    } ;
  pot0 d = {s = \\f,g => d.s ! f ! g ; n = Pl} ;
  pot110 = numPl (cardReg "ti") ;
  pot111 = numPl (cardOrd "elleve" "ellevte") ;
  pot1to19 d = numPl (d.s ! ton) ;
  pot0as1 n = {s = n.s ! ental ; n = n.n} ;
  pot1 d = numPl (d.s ! tiotal) ;
  pot1plus d e = {s = \\g => d.s ! tiotal ! invNum ++ e.s ! ental ! g ; n = Pl} ;
  pot1as2 n = n ;
  pot21 = numPl (cardOrd "hundre" "hundrede") ;
  pot2 d = 
    numPl (\\g => d.s ! ental ! invNum ++ cardOrd "hundre" "hundrede" ! g) ;
  pot2plus d e = 
    {s = \\g => d.s ! ental ! invNum ++ "hundre" ++ "og" ++ e.s ! g ; n = Pl} ;
  pot2as3 n = n ;
  pot31 = numPl (cardOrd "tusen" "tusende") ;
  pot3 n = 
    numPl (\\g => n.s ! invNum ++ cardOrd "tusen" "tusende" ! g) ;
  pot3plus n m = 
    {s = \\g => n.s ! invNum ++ "tusen" ++ "og" ++ m.s ! g ; n = Pl} ;

  pot3as4 n = n ;

  pot41 = numPl (cardOrd "en million" "millionte") ;
  pot4 n = numPl (\\g => n.s ! NCard masc ++
    cardOrd (case n.n of {Sg => "million" ; Pl => "millioner"}) "millionte" ! g) ;
  pot4plus n m = {
    s = \\g => n.s ! NCard masc ++ case n.n of {Sg => "million" ; Pl => "millioner"} ++ m.s ! g ;
    n = Pl
    } ;
  pot4decimal d = numPl (\\g => d.s ! NCard masc ++ cardOrd "millioner" "millionte" ! g) ;
  pot4as5 n = n ;

  pot51 = numPl (cardOrd "en milliard" "milliardte") ;
  pot5 n = numPl (\\g => n.s ! NCard masc ++
    cardOrd (case n.n of {Sg => "milliard" ; Pl => "milliarder"}) "milliardte" ! g) ;
  pot5plus n m = {
    s = \\g => n.s ! NCard masc ++ case n.n of {Sg => "milliard" ; Pl => "milliarder"} ++ m.s ! g ;
    n = Pl
    } ;
  pot5decimal d = numPl (\\g => d.s ! NCard masc ++ cardOrd "milliarder" "milliardte" ! g) ;

-- Numerals from sequences of digits.

  lincat 
    Dig = TDigit ;

  lin
    IDig d = d ; 

    IIDig d i = {
      s = \\o => d.s ! NCard neutrum ++ BIND ++ i.s ! o ;
      n = Pl
    } ;

    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1:e" Sg ;
    D_2 = mk2Dig "2" "2:e" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

    PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {
      s = \\o => "-" ++ BIND ++ d.s ! o ;
      n = Pl ;
      hasDot=False
      } ;
    IFrac d i = {
      s = \\o => d.s ! NCard neutrum ++
                 if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
                 i.s ! o ;
      n = Pl ;
      hasDot=True
      } ;

  oper
    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
    mkDig : Str -> TDigit = \c -> mk2Dig c (c + ":e") ;

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard _ => c ; NOrd _ => o} ;
      n = n
      } ;

    TDigit = {
      n : Number ;
      s : CardOrd => Str
    } ;

}

