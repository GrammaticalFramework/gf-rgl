concrete NumeralTel of Numeral = CatTel [Numeral,Digits] ** open ResTel in {
  lincat
    Digit, Sub10, Sub100, Sub1000, Sub1000000,
    Sub1000000000, Sub1000000000000 = {s : Str ; n : Number} ;
    Dig = {s : Str} ;

  lin
    num x = x ;
    n2 = {s = "రెండు" ; n = Pl} ;
    n3 = {s = "మూడు" ; n = Pl} ;
    n4 = {s = "నాలుగు" ; n = Pl} ;
    n5 = {s = "ఐదు" ; n = Pl} ;
    n6 = {s = "ఆరు" ; n = Pl} ;
    n7 = {s = "ఏడు" ; n = Pl} ;
    n8 = {s = "ఎనిమిది" ; n = Pl} ;
    n9 = {s = "తొమ్మిది" ; n = Pl} ;

    pot01 = {s = "ఒకటి" ; n = Sg} ;
    pot0 d = d ;
    pot0as1 x = x ;
    pot110 = {s = "పది" ; n = Pl} ;
    pot111 = {s = "పదకొండు" ; n = Pl} ;
    pot1to19 d = {s = "పది" ++ d.s ; n = Pl} ;
    pot1 d = {s = d.s ++ "పది" ; n = Pl} ;
    pot1plus d x = {s = d.s ++ "పది" ++ x.s ; n = Pl} ;
    pot1as2 x = x ;
    pot21 = {s = "వంద" ; n = Pl} ;
    pot2 x = {s = x.s ++ "వందలు" ; n = Pl} ;
    pot2plus x y = {s = x.s ++ "వందలు" ++ y.s ; n = Pl} ;
    pot2as3 x = x ;
    pot31 = {s = "వెయ్యి" ; n = Pl} ;
    pot3 x = {s = x.s ++ "వేలు" ; n = Pl} ;
    pot3plus x y = {s = x.s ++ "వేలు" ++ y.s ; n = Pl} ;
    pot3as4 x = x ;
    pot3decimal x = {s = x.s ++ "వేలు" ; n = Pl} ;
    pot41 = {s = "పది లక్షలు" ; n = Pl} ;
    pot4 x = {s = x.s ++ "పది లక్షలు" ; n = Pl} ;
    pot4plus x y = {s = x.s ++ "పది లక్షలు" ++ y.s ; n = Pl} ;
    pot4as5 x = x ;
    pot4decimal x = {s = x.s ++ "పది లక్షలు" ; n = Pl} ;
    pot51 = {s = "వంద కోట్లు" ; n = Pl} ;
    pot5 x = {s = x.s ++ "వంద కోట్లు" ; n = Pl} ;
    pot5plus x y = {s = x.s ++ "వంద కోట్లు" ++ y.s ; n = Pl} ;
    pot5decimal x = {s = x.s ++ "వంద కోట్లు" ; n = Pl} ;

    IDig d = {s = d.s ; n = Pl} ;
    IIDig d ds = {s = d.s ++ ds.s ; n = Pl} ;
    D_0 = {s = "0"} ; D_1 = {s = "1"} ; D_2 = {s = "2"} ;
    D_3 = {s = "3"} ; D_4 = {s = "4"} ; D_5 = {s = "5"} ;
    D_6 = {s = "6"} ; D_7 = {s = "7"} ; D_8 = {s = "8"} ; D_9 = {s = "9"} ;
    PosDecimal ds = ds ;
    NegDecimal ds = {s = "-" ++ ds.s ; n = ds.n} ;
    IFrac dec d = {s = dec.s ++ "." ++ d.s ; n = dec.n} ;
--
--lincat
--  Digit = {s : DForm => CardOrd => Str} ;
--  Sub10 = {s : DForm => CardOrd => Str ; n : Number} ;
--  Sub100     = {s : CardOrd => Str ; n : Number} ;
--  Sub1000    = {s : CardOrd => Str ; n : Number} ;
--  Sub1000000 = {s : CardOrd => Str ; n : Number} ;
--
--lin num x = x ;
--lin n2 = let two = mkNum "two"   "twelve"   "twenty" "second" in
--         {s = \\f,c => case <f,c> of {
--             <teen,NOrd> => "twelfth" ;
--             _ => two.s ! f ! c
--             }
--         } ;
--
--lin n3 = mkNum "three" "thirteen" "thirty" "third" ;
--lin n4 = mkNum "four"  "fourteen" "forty" "fourth" ;
--lin n5 = mkNum "five"  "fifteen"  "fifty" "fifth" ;
--lin n6 = regNum "six" ;
--lin n7 = regNum "seven" ;
--lin n8 = mkNum "eight" "eighteen" "eighty" "eighth" ;
--lin n9 = mkNum "nine" "nineteen" "ninety" "ninth" ;
--
--lin pot01 = mkNum "one" "eleven" "ten" "first" ** {n = Sg} ;
--lin pot0 d = d ** {n = Pl} ;
--lin pot110 = regCardOrd "ten" ** {n = Pl} ;
--lin pot111 = regCardOrd "eleven" ** {n = Pl} ;
--lin pot1to19 d = {s = d.s ! teen} ** {n = Pl} ;
--lin pot0as1 n = {s = n.s ! unit}  ** {n = n.n} ;
--lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
--lin pot1plus d e = {
--   s = \\c => d.s ! ten ! NCard ++ "-" ++ e.s ! unit ! c ; n = Pl} ;
--lin pot1as2 n = n ;
--lin pot2 d = {s = \\c => d.s ! unit ! NCard ++ mkCard c "hundred"}  ** {n = Pl} ;
--lin pot2plus d e = {
--  s = \\c => d.s ! unit ! NCard ++ "hundred" ++ "and" ++ e.s ! c ; n = Pl} ;
--lin pot2as3 n = n ;
--lin pot3 n = {
--  s = \\c => n.s ! NCard ++ mkCard c "thousand" ; n = Pl} ;
--lin pot3plus n m = {
--  s = \\c => n.s ! NCard ++ "thousand" ++ m.s ! c ; n = Pl} ;
--
---- numerals as sequences of digits
--
--  lincat
--    Dig = TDigit ;
--
--  lin
--    IDig d = d ** {tail = T1} ;
--
--    IIDig d i = {
--      s = \\o => d.s ! NCard ++ commaIf i.tail ++ i.s ! o ;
--      n = Pl ;
--      tail = inc i.tail
--    } ;
--
--    D_0 = mkDig "0" ;
--    D_1 = mk3Dig "1" "1st" Sg ;
--    D_2 = mk2Dig "2" "2nd" ;
--    D_3 = mk2Dig "3" "3rd" ;
--    D_4 = mkDig "4" ;
--    D_5 = mkDig "5" ;
--    D_6 = mkDig "6" ;
--    D_7 = mkDig "7" ;
--    D_8 = mkDig "8" ;
--    D_9 = mkDig "9" ;
--
--  oper
--    commaIf : DTail -> Str = \t -> case t of {
--      T3 => BIND++","++BIND ;
--      _  => BIND
--      } ;
--
--    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
--    mkDig : Str -> TDigit = \c -> mk2Dig c (c + "th") ;
--
--    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
--      s = table {NCard => c ; NOrd => o} ;
--      n = n
--      } ;
--
--    TDigit = {
--      n : Number ;
--      s : CardOrd => Str
--    } ;
--
}
