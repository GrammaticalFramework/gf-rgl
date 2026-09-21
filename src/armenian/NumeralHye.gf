--# -path=.:../abstract:../common:../prelude

concrete NumeralHye of Numeral = CatHye [Numeral,Digits,Decimal] **
  open Prelude, ResHye in {

  flags coding = utf8 ;

  param DForm = Unit | Teen | Ten ;

  lincat
    Digit = {s : DForm => CardOrd => Str; compoundOrd : DForm => Str} ;
    Sub10 = {s : DForm => CardOrd => Str; compoundOrd : DForm => Str; n : Number} ;
    Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 = LinNum ;

  lin
    num n = {s = n.s; n = n.n} ;

    n2 = mkDigit "երկու" "տասներկու" "քսան" "երկրորդ" ;
    n3 = mkDigit "երեք" "տասներեք" "երեսուն" "երրորդ" ;
    n4 = mkDigit "չորս" "տասնչորս" "քառասուն" "չորրորդ" ;
    n5 = mkDigit "հինգ" "տասնհինգ" "հիսուն" "հինգերորդ" ;
    n6 = mkDigit "վեց" "տասնվեց" "վաթսուն" "վեցերորդ" ;
    n7 = mkDigit "յոթ" "տասնյոթ" "յոթանասուն" "յոթերորդ" ;
    n8 = mkDigit "ութ" "տասնութ" "ութսուն" "ութերորդ" ;
    n9 = mkDigit "ինը" "տասնինը" "իննսուն" "իններորդ" ;

    pot01 = mkDigit "մեկ" "տասնմեկ" "տասը" "առաջին" ** {
      compoundOrd = table {
        Unit => "մեկերորդ" ;
        Teen => "տասնմեկերորդ" ;
        Ten => "տասներորդ"
        } ;
      n = Sg
      } ;
    pot0 d = d ** {n = Pl} ;

    pot110 = mkNum "տասը" "տասներորդ" Pl ** {attr = "տաս"} ;
    pot111 = regNum "տասնմեկ" Pl ;
    pot1to19 d = {
      s = d.s ! Teen ;
      compoundOrd = d.compoundOrd ! Teen ;
      attr = d.s ! Teen ! NCard ;
      n = Pl
      } ;
    pot0as1 n = {
      s = n.s ! Unit ;
      compoundOrd = n.compoundOrd ! Unit ;
      attr = n.s ! Unit ! NCard ;
      n = n.n
      } ;
    pot1 d = {
      s = d.s ! Ten ;
      compoundOrd = d.compoundOrd ! Ten ;
      attr = d.s ! Ten ! NCard ;
      n = Pl
      } ;
    pot1plus d e = regNum
      (d.s ! Ten ! NCard ++ BIND ++ e.s ! Unit ! NCard) Pl ;
    pot1as2 n = n ;

    pot21 = mkNum "հարյուր" "հարյուրերորդ" Pl ;
    pot2 d = scaleNum (d.s ! Unit ! NCard) "հարյուր" Pl ;
    pot2plus d e = plusNum
      (d.s ! Unit ! NCard ++ "հարյուր") e Pl ;
    pot2as3 n = n ;

    pot31 = mkNum "հազար" "հազարերորդ" Pl ;
    pot3 n = scaleNum n.attr "հազար" Pl ;
    pot3plus n m = plusNum (n.attr ++ "հազար") m Pl ;
    pot3as4 n = n ;
    pot3decimal n = decimalScale n "հազար" ;

    pot41 = mkNum "միլիոն" "միլիոներորդ" Pl ;
    pot4 n = scaleNum n.attr "միլիոն" Pl ;
    pot4plus n m = plusNum (n.attr ++ "միլիոն") m Pl ;
    pot4as5 n = n ;
    pot4decimal n = decimalScale n "միլիոն" ;

    pot51 = mkNum "միլիարդ" "միլիարդերորդ" Pl ;
    pot5 n = scaleNum n.attr "միլիարդ" Pl ;
    pot5plus n m = plusNum (n.attr ++ "միլիարդ") m Pl ;
    pot5decimal n = decimalScale n "միլիարդ" ;

  lincat Dig = {s : CardOrd => Str; n : Number} ;

  lin
    IDig d = d ** {tail = T1} ;
    IIDig d ds = {
      s = table {
        NCard => glue (d.s ! NCard) (ds.s ! NCard) ;
        NOrd => glue (glue (d.s ! NCard) (ds.s ! NCard)) "-րդ"
        } ;
      n = Pl ;
      tail = inc ds.tail
      } ;

    D_0 = mkDig "0" ;
    D_1 = mkDig1 ;
    D_2 = mkDig "2" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

    PosDecimal d = d ** {hasDot = False} ;
    NegDecimal d = {
      s = \\o => "-" ++ BIND ++ d.s ! o ;
      n = Pl ;
      hasDot = False
      } ;
    IFrac d i = {
      s = \\o => d.s ! NCard ++
        if_then_Str d.hasDot BIND (BIND ++ "." ++ BIND) ++
        i.s ! o ;
      n = Pl ;
      hasDot = True
      } ;

  oper
    LinNum : Type = {
      s : CardOrd => Str ;
      compoundOrd : Str ;
      attr : Str ;
      n : Number
      } ;

    mkNum : Str -> Str -> Number -> LinNum = \card,ord,n -> {
      s = table {NCard => card; NOrd => ord} ;
      compoundOrd = ord ;
      attr = card ;
      n = n
      } ;

    regNum : Str -> Number -> LinNum = \card,n ->
      mkNum card (glue card "երորդ") n ;

    mkDigit : Str -> Str -> Str -> Str ->
      {s : DForm => CardOrd => Str; compoundOrd : DForm => Str} =
      \unit,teen,ten,unitOrd -> {
        s = table {
          Unit => table {NCard => unit; NOrd => unitOrd} ;
          Teen => table {NCard => teen; NOrd => glue teen "երորդ"} ;
          Ten => table {NCard => ten; NOrd => glue ten "երորդ"}
          } ;
        compoundOrd = table {
          Unit => unitOrd ;
          Teen => glue teen "երորդ" ;
          Ten => glue ten "երորդ"
          }
        } ;

    scaleNum : Str -> Str -> Number -> LinNum = \prefix,scale,n ->
      mkNum (prefix ++ scale) (prefix ++ glue scale "երորդ") n ;

    plusNum : Str -> LinNum -> Number -> LinNum = \prefix,last,n -> {
      s = table {
        NCard => prefix ++ last.s ! NCard ;
        NOrd => prefix ++ last.compoundOrd
        } ;
      compoundOrd = prefix ++ last.compoundOrd ;
      attr = prefix ++ last.s ! NCard ;
      n = n
      } ;

    decimalScale : Decimal -> Str -> LinNum = \decimal,scale ->
      mkNum (decimal.s ! NCard ++ scale)
            (decimal.s ! NCard ++ glue scale "երորդ") Pl ;

    mkDig : Str -> {s : CardOrd => Str; n : Number} = \d -> {
      s = table {NCard => d; NOrd => glue d "-րդ"} ;
      n = Pl
      } ;

    mkDig1 : {s : CardOrd => Str; n : Number} = {
      s = table {NCard => "1"; NOrd => "1-ին"} ;
      n = Sg
      } ;
}
