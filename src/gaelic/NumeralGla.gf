concrete NumeralGla of Numeral = CatGla [Numeral,Digits,Decimal] **
  open Prelude, ResGla in {

lincat
  Digit, Sub10, Sub100, Sub1000, Sub1000000,
  Sub1000000000, Sub1000000000000, Dig = LinNumeral ;

lin
  num x = x ;
  n2 = mkNumeral "dhà" "dàrna" ;
  n3 = mkNumeral "trì" "treas" ;
  n4 = mkNumeral "ceithir" "ceathramh" ;
  n5 = mkNumeral "còig" "còigeamh" ;
  n6 = mkNumeral "sia" "siathamh" ;
  n7 = mkNumeral "seachd" "seachdamh" ;
  n8 = mkNumeral "ochd" "ochdamh" ;
  n9 = mkNumeral "naoi" "naoidheamh" ;
  pot01 = one ;
  pot0 d = d ;
  pot0as1 n = n ;
  pot110 = mkNumeral "deich" "deicheamh" ;
  pot111 = mkNumeral "aon deug" "aonamh deug" ;
  pot1to19 d = join "deug" d ;
  pot1 d = join "deich" d ;
  pot1plus d e = plus (join "deich" d) e ;
  pot1as2 n = n ;
  pot21 = mkNumeral "ceud" "ceudamh" ;
  pot2 d = join "ceud" d ;
  pot2plus d e = plus (join "ceud" d) e ;
  pot2as3 n = n ;
  pot31 = mkNumeral "mìle" "mìleamh" ;
  pot3 d = join "mìle" d ;
  pot3plus d e = plus (join "mìle" d) e ;
  pot3as4 n = n ;
  pot3decimal d = mkNumeral (d.s ++ "mìle") (d.s ++ "mìleamh") ;
  pot41 = mkNumeral "millean" "milleanamh" ;
  pot4 d = join "millean" d ;
  pot4plus d e = plus (join "millean" d) e ;
  pot4as5 n = n ;
  pot4decimal d = mkNumeral (d.s ++ "millean") (d.s ++ "milleanamh") ;
  pot51 = mkNumeral "billean" "billeanamh" ;
  pot5 d = join "billean" d ;
  pot5plus d e = plus (join "billean" d) e ;
  pot5decimal d = mkNumeral (d.s ++ "billean") (d.s ++ "billeanamh") ;

  IDig d = d ;
  IIDig d ds = {
    s = table {NCard => d.s ! NCard ++ BIND ++ ds.s ! NCard ; NOrd => d.s ! NCard ++ BIND ++ ds.s ! NOrd} ;
    n = Pl
    } ;
  D_0 = digit "0" ; D_1 = digit "1" ; D_2 = digit "2" ; D_3 = digit "3" ;
  D_4 = digit "4" ; D_5 = digit "5" ; D_6 = digit "6" ; D_7 = digit "7" ;
  D_8 = digit "8" ; D_9 = digit "9" ;
  PosDecimal ds = {s = ds.s ! NCard} ;
  NegDecimal ds = {s = "-" ++ BIND ++ ds.s ! NCard} ;
  IFrac d x = {s = d.s ++ "." ++ x.s ! NCard} ;

oper
  one : LinNumeral = {s = table {NCard => "aon" ; NOrd => "ciad"} ; n = Sg} ;
  digit : Str -> LinNumeral = \x -> {s = table {NCard => x ; NOrd => x} ; n = Pl} ;
  join : Str -> LinNumeral -> LinNumeral = \unit,n -> {
    s = table {NCard => n.s ! NCard ++ unit ; NOrd => n.s ! NCard ++ unit} ; n = Pl
    } ;
  plus : LinNumeral -> LinNumeral -> LinNumeral = \x,y -> {
    s = table {NCard => x.s ! NCard ++ "'s" ++ y.s ! NCard ;
               NOrd => x.s ! NCard ++ "'s" ++ y.s ! NOrd} ;
    n = Pl
    } ;
}
