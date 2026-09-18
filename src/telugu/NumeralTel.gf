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
}
