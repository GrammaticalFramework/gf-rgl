concrete NumeralLat of Numeral = CatLat ** open ResLat,ParadigmsLat in {
  lincat 
    Digit      = Numeral ;
    Sub10      = Numeral ;
    Sub100     = Numeral ;
    Sub1000    = Numeral ;
    Sub1000000 = Numeral ;

  lin
    num x = x ;
    n2 = lin Numeral ( mkNum "duo" "secundus" ) ;
    n3 = lin Numeral ( mkNum "tres" "tertius" ) ;
    n4 = lin Numeral ( mkNum "quattuor"  "quartus" ) ;
    n5 = lin Numeral ( mkNum "quinque"  "quintus" ) ;
    n6 = lin Numeral ( mkNum "sex" "sextus" ) ;
    n7 = lin Numeral ( mkNum "septem" "septimus" ) ;
    n8 = lin Numeral ( mkNum "octo" "ocatvus" ) ;
    n9 = lin Numeral ( mkNum "novem" "nonus" ) ;

  lin pot01 = lin Numeral ( mkNum "unus" "primus" ) ;
  lin pot0 d = d ;
--lin pot110 = regCardOrd "ten" ** {n = Pl} ;
--lin pot111 = regCardOrd "eleven" ** {n = Pl} ;
--lin pot1to19 d = {s = d.s ! teen} ** {n = Pl} ;
  lin pot0as1 n = n ;
--lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
--lin pot1plus d e = {
--   s = \\c => d.s ! ten ! NCard ++ "-" ++ e.s ! unit ! c ; n = Pl} ;
      pot1as2 n = n ;
--lin pot2 d = {s = \\c => d.s ! unit ! NCard ++ mkCard c "hundred"}  ** {n = Pl} ;
--lin pot2plus d e = {
--  s = \\c => d.s ! unit ! NCard ++ "hundred" ++ "and" ++ e.s ! c ; n = Pl} ;
      pot2as3 n = n ;
--lin pot3 n = {
--  s = \\c => n.s ! NCard ++ mkCard c "thousand" ; n = Pl} ;
--lin pot3plus n m = {
--  s = \\c => n.s ! NCard ++ "thousand" ++ m.s ! c ; n = Pl} ;
--
-- numerals as sequences of digits

  lincat 
    Dig = TDigit ;

  lin
    IDig d = {s = d.s ! one; unit = ten} ;

    IIDig d i = {
      s = d.s ! i.unit ++ i.s ;
      unit = inc i.unit
    } ;

    D_0 = mkDig ""     ""     ""     ""       ""       "" ;
    D_1 = mkDig "I"    "X"    "C"    "M"      "(X)"    "(C)" ;
    D_2 = mkDig "II"   "XX"   "CC"   "MM"     "(XX)"   "(CC)" ;
    D_3 = mkDig "III"  "XXX"  "CCC"  "MMM"    "(XXX)"  "(CCC)" ;
    D_4 = mkDig "IV"   "XL"   "CD"   "(IV)"   "(XL)"   "(CD)" ;
    D_5 = mkDig "V"    "L"    "D"    "(V)"    "(L)"    "(D)" ;
    D_6 = mkDig "VI"   "LX"   "DC"   "(VI)"   "(LX)"   "(DC)" ;
    D_7 = mkDig "VII"  "LXX"  "DCC"  "(VII)"  "(LXX)"  "(DCC)" ;
    D_8 = mkDig "VIII" "LXXX" "DCCC" "(VIII)" "(LXXX)" "(DCCC)" ;
    D_9 = mkDig "IX"   "XC"   "CM"   "(IX)"   "(XC)"   "(CM)" ;

  oper
    TDigit = {
      s : Unit => Str
    } ;

    mkDig : Str -> Str -> Str -> Str -> Str -> Str -> TDigit = 
      \one,ten,hundred,thousand,ten_thousand,hundred_thousand -> {
          s = table Unit [one;ten;hundred;thousand;ten_thousand;hundred_thousand]
        } ;
        
    inc : Unit -> Unit = \u ->
      case u of {
        one              => ten ;
        ten              => hundred ;
        hundred          => thousand ;
        thousand         => ten_thousand ;
        ten_thousand     => hundred_thousand ;
        hundred_thousand => hundred_thousand
      } ;
}
