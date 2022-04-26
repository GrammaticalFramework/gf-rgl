concrete NumeralGus of Numeral = CatGus [Numeral,Digits] ** open Prelude, 
DiffGus,MorphoGus in {

lincat 
  Digit = {s : DForm => CardOrd => Gender => Str} ;
  Sub10 = {s : DForm => CardOrd => Gender => Str ; n : Number} ;
  Sub100     = {s : CardOrd => Gender => Str ; n : Number} ;
  Sub1000    = {s : CardOrd => Gender => Str ; n : Number} ;
  Sub1000000 = {s : CardOrd => Gender => Str ; n : Number} ;

lin num x = x ;
lin n2 = mkNum2 "bere"   "kabere" ;
lin n3 = mkNum3 "ato"  "gatatu" ;
lin n4 = mkNum4 "ne"   "kane" ;
lin n5 = mkNum5 "tano"  "gatano" ;
lin n6 = mkNum6 "tano" "mo";
lin n7 = mkNum7 "tano" "bere";
lin n8 = mkNum8 "tano" "tato";
lin n9 = regNum "kianda" ;

lin pot01 = mkNum1 "mo"  "tang'ani" ** {n = Sg} ;
lin pot0 d = d ** {n = Pl} ;
lin pot110 = regCardOrd "ikomi" ** {n = Pl} ;
lin pot111 = regCardone "ikomi na" "mo" ** {n = Pl} ; -- creat another function to be gender specific
lin pot1to19 d = {s = d.s ! teen} ** {n = Pl} ;
lin pot0as1 n = {s = n.s ! unit}  ** {n = n.n} ;
lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
lin pot1plus d e = { s = table {
      NCard => \\g => d.s ! ten ! NCard ! g ++ "na"++ e.s ! unit ! NCard ! g  ;
      NOrd => \\g =>Ordprefix g++ d.s ! ten ! NCard ! g ++ "na"++ e.s ! unit ! NCard ! g } ;
                  n = Pl} ;
lin pot1as2 n = n ;
lin pot2 d = {s = d.s ! hund} ** {n = Pl} ; 
lin pot2plus d e = {s = table {
      NCard => \\g => d.s ! hund ! NCard ! g ++  "na" ++ e.s !NCard ! g ;
      NOrd => \\g =>Ordprefix g++ d.s ! hund ! NCard ! g ++  "na" ++ e.s ! NCard ! g } ;
                   n = Pl} ;
 lin pot2as3 n = n ;
lin pot3 n = { s = table {
      NCard => \\g => mkCard NCard "chilibu" ! g ++ n.s ! NCard ! g ;
      NOrd => \\g =>Ordprefix g++ mkCard NCard "chilibu" ! g ++ n.s ! NCard ! g } ;
              n = Pl} ;
lin pot3plus n m = { s = table {
      NCard => \\g => "chilibu" ++ n.s ! NCard !g ++  m.s ! NCard ! g ;
      NOrd => \\g =>Ordprefix g++ "chilibu" ++ n.s ! NCard !g ++  m.s ! NCard ! g} ;
                 n = Pl} ;

-- numerals as sequences of digits0'

  lincat 
    Dig = TDigit ;

  lin
    IDig d = d ; 

    IIDig d i = {
      --s = \\o,g => d.s ! NCard ! g   ++ i.s ! o ! g ;
      s = table {NCard => \\g => d.s! NCard ! g ++ BIND  ++ i.s ! NCard  ! g ;
                 NOrd => \\g => d.s! NOrd! g ++ BIND  ++ i.s !NCard! g } ;
           n = Pl 
    } ;

    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1" Sg ;
    D_2 = mkDig "2" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

  oper
    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
    mkDig : Str -> TDigit = \c -> mk2Dig c (c ) ;

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard => \\g => c ; NOrd => \\g =>Ordprefix g ++ o} ; 
            n = n} ;

    TDigit = {
      n : Number ;
      s : CardOrd => Gender => Str
    } ;
}
