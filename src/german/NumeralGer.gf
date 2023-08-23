concrete NumeralGer of Numeral = CatGer [Numeral,Digits,Decimal] ** open MorphoGer, Prelude in {

flags optimize = all_subs ;
    coding=utf8 ;

lincat 
  Digit = {s : DForm => CardOrd => Str} ;
  Sub10 = {s : DForm => CardOrd => Str ; n : Number} ;
  Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 = 
          {s :          CardOrd => Str ; n : Number} ;

lin 
  num x = x ;

  n2 = mkDigit  "zwei"  "zwölf"   "zwanzig"   "zweite" ;
  n3 = mkDigit  "drei"  "dreizehn" "dreissig" "dritte" ;
  n4 = regDigit  "vier" ;
  n5 = regDigit  "fünf" ;
  n6 = regDigit  "sechs" ;
  n7 = mkDigit  "sieben"  "siebzehn" "siebzig" "siebte" ;
  n8 = mkDigit  "acht" "achzehn"   "achzig"   "achte" ;
  n9 = regDigit  "neun" ;

  pot01 = {
    s = \\f => table {
          NCard g c => "ein" + pronEnding ! GSg g ! c ;
          NOrd af => (regA "erst").s ! Posit ! af
          } ; 
    n = Sg
    } ;
  pot0 d = {s = \\f,g => d.s ! f ! g ; n = Pl} ;
  pot0as1 n = {s = n.s ! DUnit; n = n.n } ;

  pot110 = {s = cardReg "zehn"; n = Pl} ;
  pot111 = {s = cardReg "elf"; n = Pl} ;
  pot1to19 d = {s = d.s ! DTeen; n = Pl} ;
  pot1 d = {s = d.s ! DTen; n = Pl} ;
  pot1plus d e = {s = \\g => 
    e.s ! DUnit ! invNum ++ BIND ++ "und" ++ BIND ++ d.s ! DTen ! g; n = Pl} ;
  pot1as2 n = n ;

  pot2 d = {s = \\g => 
    multiple (d.s ! DUnit) d.n ++ cardOrd "hundert" "hunderte" ! g ; n = Pl} ;
  pot2plus d e = {s = \\g => 
    multiple (d.s ! DUnit) d.n ++ "hundert" ++ BIND ++ e.s ! g ; n = Pl} ;
  pot2as3 n = n ;

  pot3 n = {s = \\g => 
    multiple n.s n.n ++ cardOrd "tausend" "tausendte" ! g ; n = Pl} ; ----
  pot3plus n m = {s = \\g => 
    multiple n.s n.n ++ "tausend" ++ m.s ! g ; n = Pl} ;
  pot3as4 n = n ;
  pot3decimal d = {s = \\g =>
    d.s ! invNum ++ cardOrd "tausend" "tausendte" ! g ; n = Pl} ; ----

  pot4as5 n = n ;
  pot4decimal d = {s = \\g =>
    d.s ! invNum ++ cardOrd "Millionen" "Millionte" ! g ; n = Pl} ; ----

  pot51 = {s = \\g => "einer Milliarde"; n = Pl} ;  -- KA: case inflection missing

oper
  multiple : (CardOrd => Str) -> Number -> Str = \d,n -> 
    case n of {Sg => [] ; _ => d ! invNum ++ BIND} ;

--------------------

  lincat 
    Dig = TDigit ;

  lin
    IDig d = d ; 

    IIDig d i = {
      s = \\o => d.s ! invNum ++ BIND ++ i.s ! o ;
      n = Pl
    } ;

    ---- TODO: case endings of ordinals
    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1e" Sg ;
    D_2 = mk2Dig "2" "2e" ;
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
      s=\\o=>d.s ! invNum ++
             if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
             i.s ! o;
      n = Pl;
      hasDot=True
    } ;

  oper
    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
    mkDig : Str -> TDigit = \c -> mk2Dig c (c + "e") ;

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard _ _ => c ; NOrd _ => o} ;
      n = n
      } ;

    TDigit = {
      n : Number ;
      s : CardOrd => Str
    } ;

}
