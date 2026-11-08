concrete NumeralJpn of Numeral = CatJpn [Numeral,Digits,Decimal] ** open ResJpn, ParadigmsJpn, Prelude in {

flags coding = utf8 ;

  lincat
    Digit = {s : Str} ;
    Sub10 = {s : Str ; n : Number ; null : Str} ;
    Sub100 = {s : Str ; n : Number ; numType : NumeralType ; digit1 : Str ; digit2 : Str} ;
    Sub1000 = {s_init : Str ; s_mid : Str ; n : Number ; numType : NumeralType ;
               man : Str ; sen : Str ; null : Str} ;
    Sub1000000 = {s : Str ; n : Number ; tenPlus : Bool} ;
    Sub1000000000 = {s : Str ; n : Number ; tenPlus : Bool} ;
    Sub1000000000000 = {s : Str ; n : Number ; tenPlus : Bool} ;

  lin
    num dig = dig ;

    n2 = {s = "二"} ;  -- "ni"
    n3 = {s = "三"} ;  -- "san"
    n4 = {s = "四"} ;  -- "yon"
    n5 = {s = "五"} ;  -- "go"
    n6 = {s = "六"} ;  -- "roku"
    n7 = {s = "七"} ;  -- "shichi"
    n8 = {s = "八"} ;  -- "hachi"
    n9 = {s = "九"} ;  -- "kyuu"

    pot01 = {s = "一" ; n = Sg ; null = ""} ;

    pot0 d = d ** {n = Pl; null = ""} ;

    pot110 = {
      s = "十" ;  -- "juu"
      n = Pl ;
      numType = EndZero ;
      digit1 = "一" ;
      digit2 = "0"
      } ;

    pot111 = {
      s = "十一" ;
      n = Pl ;
      numType = EndNotZero ;
      digit1 = "一" ;
      digit2 = "一"
      } ;

    pot1to19 d = {
      s = "十" ++ d.s ;
      n = Pl ;
      numType = EndNotZero ;
      digit1 = "一" ;
      digit2 = d.s
      } ;

    pot0as1 d = {
      s = d.s ;
      n = Pl ;
      numType = SingleDigit ;
      digit1 = [] ;
      digit2 = d.s
      } ;

    pot1 d = {
      s = d.s ++ "十" ;
      n = Pl ;
      numType = EndZero ;
      digit1 = d.s ;
      digit2 = "0"
      } ;

    pot1plus d n = {
      s = d.s ++ "十" ++ n.s ;
      n = Pl ;
      numType = EndNotZero ;
      digit1 = d.s ;
      digit2 = n.s
      } ;

    pot1as2 d = {
      s_init = d.s ;
      s_mid = d.s ;
      n = d.n ;
      numType = d.numType ;
      man = d.digit1 ;
      sen = d.digit2 ;
      null = ""
      } ;

    pot2 d = {
      s_init = case d.n of {
        Sg => d.null ++ "百" ;  -- "hyaku"
        Pl => d.s ++ "百"
        } ;
      s_mid = d.s ++ "百" ;
      n = Pl ;
      numType = EndZero ;
      man = case d.n of {
        Sg => d.null ++ "十" ;
        Pl => d.s ++ "十"
        } ;
      sen = [] ;
      null = ""
      } ;

    pot21 = {
      s_init = "百" ; s_mid = "百" ; n = Pl ; numType = EndZero ;
      man = "十" ; sen = [] ; null = ""
      } ;

    pot2plus d n = {
      s_init = case d.n of {
        Sg => d.null ++ "百" ++ n.s ;
        Pl => d.s ++ "百" ++ n.s
        } ;
      s_mid = d.s ++ "百" ++ n.s ;
      n = Pl ;
      numType = case n.numType of {
        EndZero => EndZero ;
        _ => EndNotZero
        } ;
      man = case d.n of {
        Sg => d.null ++ "十" ++ n.digit1 ;
        Pl => d.s ++ "十" ++ n.digit1
        } ;
      sen = n.digit2 ;
      null = ""
      } ;

    pot2as3 d = {
      s = d.s_init ;
      n = d.n ;
      tenPlus = case d.numType of {
                  SingleDigit => False ;
                  _           => True
                }
      } ;

    pot3 d = {
      s = case d.numType of {
        EndZero => d.man ++ "万" ;  -- "man"
        EndNotZero => d.man ++ "万" ++ d.sen ++ "千" ;  -- "sen"
        SingleDigit => case d.n of {
          Sg => d.null ++ "千" ;
          Pl => d.s_init ++ "千"
          }
        } ;
      n = Pl;
      tenPlus = True
      } ;

    pot3plus d n = {
      s = case d.numType of {
        EndZero => d.man ++ "万" ++ n.s_mid ;
        EndNotZero => d.man ++ "万" ++ d.sen ++ "千" ++ n.s_mid ;
        SingleDigit => case d.n of {
          Sg => d.null ++ "千" ++ n.s_mid ;
          Pl => d.s_init ++ "千" ++ n.s_mid
          }
        } ;
      n = Pl;
      tenPlus = True
      } ;

    pot31 = {s = "千" ; n = Pl ; tenPlus = True} ;

    pot3as4 n = n ;
    pot3decimal n = {s = n.s ++ "千" ; n = Pl ; tenPlus = True} ;

    pot41 = {s = "百万" ; n = Pl ; tenPlus = True} ;
    pot4 n = {s = n.s_init ++ "百万" ; n = Pl ; tenPlus = True} ;
    pot4plus n m = {
      s = n.s_init ++ "百万" ++ m.s ; n = Pl ; tenPlus = True
      } ;
    pot4as5 n = n ;
    pot4decimal n = {s = n.s ++ "百万" ; n = Pl ; tenPlus = True} ;

    pot51 = {s = "十億" ; n = Pl ; tenPlus = True} ;
    pot5 n = {s = n.s_init ++ "十億" ; n = Pl ; tenPlus = True} ;
    pot5plus n m = {
      s = n.s_init ++ "十億" ++ m.s ; n = Pl ; tenPlus = True
      } ;
    pot5decimal n = {s = n.s ++ "十億" ; n = Pl ; tenPlus = True} ;

  lincat
    Dig = {s : Str ; n : Number ; is0 : Bool} ;

  lin
    IDig d = {
      s = d.s ;
      n = d.n ;
      tenPlus = False ;
      tail = T1
      } ;

    IIDig d n = {
      s = d.s ++ commaIf n.tail ++ n.s ;
      n = Pl ;
      tenPlus = case <d.is0, n.tenPlus> of {
        <_, True> => True ;
        <True, False> => False ;
        _ => True
        } ;
      tail = inc n.tail
      } ;

    D_0 = {s = "0" ; n = Sg ; is0 = True} ;
    D_1 = {s = "1" ; n = Sg ; is0 = False} ;
    D_2 = {s = "2" ; n = Pl ; is0 = False} ;
    D_3 = {s = "3" ; n = Pl ; is0 = False} ;
    D_4 = {s = "4" ; n = Pl ; is0 = False} ;
    D_5 = {s = "5" ; n = Pl ; is0 = False} ;
    D_6 = {s = "6" ; n = Pl ; is0 = False} ;
    D_7 = {s = "7" ; n = Pl ; is0 = False} ;
    D_8 = {s = "8" ; n = Pl ; is0 = False} ;
    D_9 = {s = "9" ; n = Pl ; is0 = False} ;

    PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {
      s = "-" ++ BIND ++ d.s ;
      n = Pl ;
      tenPlus = d.tenPlus ;
      hasDot=False
      } ;
    IFrac d i = {
      s = d.s ++
          if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
          i.s;
      n = Pl ;
      tenPlus = d.tenPlus ;
      hasDot=True
    } ;

  oper
    commaIf : DTail -> Str = \t -> case t of {
      T3 => "," ;
      _ => []
      } ;
}
