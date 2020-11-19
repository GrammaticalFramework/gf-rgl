concrete NumeralNno of Numeral = CatNno [Numeral,Digits] ** open MorphoNno, Prelude in {
  flags coding=utf8 ;

lincat
  Digit = {s : DForm => CardOrd => Str} ;
  Sub10 = {s : DForm => CardOrd => Str ; n : Number} ;
  Sub100, Sub1000, Sub1000000 =
          {s :          CardOrd => Str ; n : Number} ;

lin
  num x = x ;

  n2 = mkTal "to"   "tolv"    "tjue"   "andre"   "tolvte" ;
  n3 = mkTal "tre"  "tretten" "tretti" "tredje"  "trettande" ;
  n4 = mkTal "fire" "fjorten" "førti"  "fjerde"  "fjortande" ;
  n5 = mkTal "fem"  "femten"  "femti"  "femte"   "femtande" ;
  n6 = mkTal "seks" "seksten" "seksti" "sjette"  "sekstande" ;
  n7 = mkTal "sju"  "sytten"  "sytti"  "sjunde" "syttande" ;
  n8 = mkTal "åtte" "atten"   "åtti"   "åttande" "attande" ;
  n9 = mkTal "ni"   "nitten"  "nitti"  "niande"  "nittande" ;

  pot01 = {
    s = \\f => table {
          NCard g => case g of {NNeutr => "eit" ; NUtr _ => "ein"} ; ---- ei ?
          _ => "første"
          } ;
    n = Sg
    } ;
  pot0 d = {s = \\f,g => d.s ! f ! g ; n = Pl} ;
  pot110 = numPl (cardReg "ti") ;
  pot111 = numPl (cardOrd "elve" "elfte") ;
  pot1to19 d = numPl (d.s ! ton) ;
  pot0as1 n = {s = n.s ! ental ; n = n.n} ;
  pot1 d = numPl (d.s ! tiotal) ;
  pot1plus d e = {s = \\g => d.s ! tiotal ! invNum ++ e.s ! ental ! g ; n = Pl} ;
  pot1as2 n = n ;
  pot2 d =
    numPl (\\g => d.s ! ental ! invNum ++ cardOrd "hundre" "hundrede" ! g) ;
  pot2plus d e =
    {s = \\g => d.s ! ental ! invNum ++ "hundre" ++ "og" ++ e.s ! g ; n = Pl} ;
  pot2as3 n = n ;
  pot3 n =
    numPl (\\g => n.s ! invNum ++ cardOrd "tusen" "tusende" ! g) ;
  pot3plus n m =
    {s = \\g => n.s ! invNum ++ "tusen" ++ "og" ++ m.s ! g ; n = Pl} ;

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
