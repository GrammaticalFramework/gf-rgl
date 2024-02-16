--# -path=.:../abstract

concrete NumeralSwe of Numeral = CatSwe [Numeral,Digits,Decimal] ** open ResSwe, MorphoSwe, Prelude in {
  flags coding=utf8 ;

lincat 
  Digit = {s : DForm => CardOrd => Str} ;
  Sub10 = {s : DForm => CardOrd => Str ; n : Number} ;
  Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 = 
          {s :          CardOrd => Str ; n : Number} ;

lin 
  num x = x ;

  n2 = mkTal  "två"  "tolv"    "tjugo"   "andra"   "tolfte" ;
  n3 = mkTal  "tre"  "tretton" "trettio" "tredje"  "trettonde" ;
  n4 = mkTal  "fyra" "fjorton" "fyrtio"  "fjärde"  "fjortonde" ;
  n5 = mkTal  "fem"  "femton"  "femtio"  "femte"   "femtonde" ;
  n6 = mkTal  "sex"  "sexton"  "sextio"  "sjätte"  "sextonde" ;
  n7 = mkTal  "sju"  "sjutton" "sjuttio" "sjunde"  "sjuttonde" ;
  n8 = mkTal  "åtta" "arton"   "åttio"   "åttonde" "artonde" ;
  n9 = mkTal  "nio"  "nitton"  "nittio"  "nionde"  "nittonde" ;

  pot01 = {
    s = \\f => table {
          NCard g => case g of {Neutr => "ett" ; _ => "en"} ;
          _ => "första"
          } ; 
    n = Sg
    } ;
  pot0 d = {s = \\f,g => d.s ! f ! g ; n = Pl} ;
  pot0as1 n = {s = n.s ! ental ; n = n.n} ;

  pot110 = numPl (cardReg "tio") ;
  pot111 = numPl (cardOrd "elva" "elfte") ;
  pot1to19 d = numPl (d.s ! ton) ;
  pot1 d = numPl (d.s ! tiotal) ;
  pot1plus d e = {s = \\g => d.s ! tiotal ! invNum ++ BIND ++ e.s ! ental ! g ; n = Pl} ;
  pot1as2 n = n ;

  pot21 = numPl (cardOrd "hundra" "hundrade") ;
  pot2 d = 
    numPl (\\g => d.s ! ental ! invNum ++ BIND ++ cardOrd "hundra" "hundrade" ! g) ;
  pot2plus d e = 
    {s = \\g => d.s ! ental ! invNum ++ BIND ++ "hundra" ++ e.s ! g ; n = Pl} ;
  pot2as3 n = n ;

  pot31 = numPl (cardOrd "tusen" "tusende") ;
  pot3 n = 
    numPl (\\g => n.s ! invNum ++ cardOrd "tusen" "tusende" ! g) ;
  pot3plus n m = 
    {s = \\g => n.s ! invNum ++ BIND ++ "tusen" ++ m.s ! g ; n = Pl} ;
  pot3as4 n = n ;

  pot41 = numPl (cardOrd "en miljon" "miljonde") ;
  pot4 n =
    numPl (\\g => n.s ! NCard Utr ++
                  cardOrd (case n.n of {
                             Sg => "miljon" ;
                             Pl => "miljoner"
                           })
                          "miljonte" ! g) ;
  pot4plus n m =
    {s = \\g => n.s ! NCard Utr ++ case n.n of {
                             Sg => "miljon" ;
                             Pl => "miljoner"
                           } ++ m.s ! g ; n = Pl} ;
  pot4as5 n = n ;
  pot4decimal d = 
    numPl (\\g => d.s ! NCard Utr ++ cardOrd "miljoner" "miljonte" ! g) ;

  pot51 = numPl (cardOrd "en miljard" "miljarde") ;
  pot5 n =
    numPl (\\g => n.s ! NCard Utr ++
                  cardOrd (case n.n of {
                             Sg => "miljard" ;
                             Pl => "miljarder"
                           })
                          "miljarde" ! g) ;
  pot5plus n m =
    {s = \\g => n.s ! NCard Utr ++ "miljard" ++ m.s ! g ; n = Pl} ;
  pot5decimal d = 
    numPl (\\g => d.s ! NCard Utr ++ cardOrd "miljarder" "miljarde" ! g) ;

  lincat
    Dig = TDigit ;

  lin
    IDig d = d ; 

    IIDig d i = {
      s = \\o => d.s ! NCard neutrum ++ BIND ++ i.s ! o ;
      n = Pl
    } ;

    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1:a" Sg ;
    D_2 = mk2Dig "2" "2:a" ;
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
                 i.s ! o;
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

