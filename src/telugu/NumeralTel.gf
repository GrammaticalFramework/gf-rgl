concrete NumeralTel of Numeral = CatTel [Numeral,Digits] ** open ResTel in {
  lincat
    Digit, Sub10, Sub100, Sub1000, Sub1000000,
    Sub1000000000, Sub1000000000000 = {s : Gender => Str ; n : Number} ;
    Dig = {s : Str} ;

  lin
    num x = x ;
    n2 = {s = \\_ => "రెండు" ; n = Pl} ;
    n3 = {s = \\_ => "మూడు" ; n = Pl} ;
    n4 = {s = \\_ => "నాలుగు" ; n = Pl} ;
    n5 = {s = \\_ => "ఐదు" ; n = Pl} ;
    n6 = {s = \\_ => "ఆరు" ; n = Pl} ;
    n7 = {s = \\_ => "ఏడు" ; n = Pl} ;
    n8 = {s = \\_ => "ఎనిమిది" ; n = Pl} ;
    n9 = {s = \\_ => "తొమ్మిది" ; n = Pl} ;

    pot01 = {
      s = table {Masc => "ఒకడు" ; Fem => "ఒకతె" ; Neutr => "ఒక"} ;
      n = Sg
      } ;
    pot0 d = d ;
    pot0as1 x = x ;
    pot110 = {s = \\_ => "పది" ; n = Pl} ;
    pot111 = {s = \\_ => "పదకొండు" ; n = Pl} ;
    pot1to19 d = {s = \\_ => ("పది" ++ d.s ! Neutr) ; n = Pl} ;
    pot1 d = {s = \\_ => (d.s ! Neutr ++ "పది") ; n = Pl} ;
    pot1plus d x = {s = \\_ => (d.s ! Neutr ++ "పది" ++ x.s ! Neutr) ; n = Pl} ;
    pot1as2 x = x ;
    pot21 = {s = \\_ => "వంద" ; n = Pl} ;
    pot2 x = {s = \\_ => (x.s ! Neutr ++ "వందలు") ; n = Pl} ;
    pot2plus x y = {s = \\_ => (x.s ! Neutr ++ "వందలు" ++ y.s ! Neutr) ; n = Pl} ;
    pot2as3 x = x ;
    pot31 = {s = \\_ => "వెయ్యి" ; n = Pl} ;
    pot3 x = {s = \\_ => (x.s ! Neutr ++ "వేలు") ; n = Pl} ;
    pot3plus x y = {s = \\_ => (x.s ! Neutr ++ "వేలు" ++ y.s ! Neutr) ; n = Pl} ;
    pot3as4 x = x ;
    pot3decimal x = {s = \\_ => (x.s ++ "వేలు") ; n = Pl} ;
    pot41 = {s = \\_ => "పది లక్షలు" ; n = Pl} ;
    pot4 x = {s = \\_ => (x.s ! Neutr ++ "పది లక్షలు") ; n = Pl} ;
    pot4plus x y = {s = \\_ => (x.s ! Neutr ++ "పది లక్షలు" ++ y.s ! Neutr) ; n = Pl} ;
    pot4as5 x = x ;
    pot4decimal x = {s = \\_ => (x.s ++ "పది లక్షలు") ; n = Pl} ;
    pot51 = {s = \\_ => "వంద కోట్లు" ; n = Pl} ;
    pot5 x = {s = \\_ => (x.s ! Neutr ++ "వంద కోట్లు") ; n = Pl} ;
    pot5plus x y = {s = \\_ => (x.s ! Neutr ++ "వంద కోట్లు" ++ y.s ! Neutr) ; n = Pl} ;
    pot5decimal x = {s = \\_ => (x.s ++ "వంద కోట్లు") ; n = Pl} ;

    IDig d = {s = d.s ; n = Pl} ;
    IIDig d ds = {s = d.s ++ ds.s ; n = Pl} ;
    D_0 = {s = "0"} ; D_1 = {s = "1"} ; D_2 = {s = "2"} ;
    D_3 = {s = "3"} ; D_4 = {s = "4"} ; D_5 = {s = "5"} ;
    D_6 = {s = "6"} ; D_7 = {s = "7"} ; D_8 = {s = "8"} ; D_9 = {s = "9"} ;
    PosDecimal ds = ds ;
    NegDecimal ds = {s = "-" ++ ds.s ; n = ds.n} ;
    IFrac dec d = {s = dec.s ++ "." ++ d.s ; n = dec.n} ;
}
