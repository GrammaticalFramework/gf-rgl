--# -path=.:../abstract:../common:../../prelude

concrete NumeralRus of Numeral = CatRus [Numeral,Digits,Decimal] ** open ResRus, InflectionRus, Prelude in {

flags  coding=utf8 ;

-- Toiska, 13/8/2000, AR with Arto Mustajoki.
-- Nikita Frolov, 2011

lincat Digit = {s : DForm => DetTable ; size : NumSize ; o : DForm => PronForms} ;
lincat Sub10 = {s : Place => DForm => DetTable ; size : NumSize ; o : Place => DForm => PronForms ; just1 : Bool} ;
lincat Sub100 = {s : Place => DetTable ; size : NumSize ; o : Place => PronForms; just1 : Bool} ;
lincat Sub1000 = {s : Place => DetTable ; size : NumSize ; o : Place => PronForms; just1 : Bool} ;
lincat Sub1000000, Sub1000000000, Sub1000000000000 = {s : DetTable ; size : NumSize ; o : PronForms; just1 : Bool} ;
-- just1 to correctly generate exactly 1000

-- : Sub1000000 -> Numeral ; -- 123456 [coercion to top category]
lin num x = {
  s = \\ g,a,c => x.s ! g ! a ! c ;
  o = x.o ;
  size = x.size
  };

-- : Digit
lin n3 = {
  s = table {
    unit => tri ;
    teen => nadsat "три" ;
    ten  => n2030 "три" ;
    hund => sta tri
    } ;
  o = table {
    unit => pronoun6AstA "третий" ;
    teen => pronounAdj1A "тринадцатый" ;
    ten  => pronounAdj1A "тридцатый" ;
    hund => pronounAdj1A "трёхсотый"
    } ;
  size = Num2_4
  } ;
lin n4 = {
  s = table {
    unit => chetyre ;
    teen => nadsat "четыр" ;
    ten  => \\g,a,c => case <c, g> of {
		    <(Nom|VocRus|Acc), _> => "сорок";
		    <(Gen|Ptv|Dat|Ins|Pre|Loc), _> => "сорока"
		  } ;
    hund => sta chetyre
    } ;
  o = table {
    unit => pronounAdj1A "четвёртый" ;
    teen => pronounAdj1A "четырнадцатый" ;
    ten  => pronounAdj1B "сороковой" ;
    hund => pronounAdj1A "четырёхсотый"
    } ;
  size = Num2_4
  } ;
lin n5 = {
  s = table {
    unit => n59 "пят" ;
    teen => nadsat "пят" ;
    ten  => n5070 "пят" ;
    hund => sot (n59 "пят")
    } ;
  o = table {
    unit => pronounAdj1A "пятый" ;
    teen => pronounAdj1A "пятнадцатый" ;
    ten  => pronounAdj1A "пятидесятый" ;
    hund => pronounAdj1A "пятисотый"
    } ;
  size = Num5
  } ;
lin n6 = {
  s = table {
    unit => n59 "шест" ;
    teen => nadsat "шест" ;
    ten  => n5070 "шест" ;
    hund => sot (n59 "шест")
    } ;
  o = table {
    unit => pronounAdj1B "шестой" ;
    teen => pronounAdj1A "шестнадцатый" ;
    ten  => pronounAdj1A "шестидесятый" ;
    hund => pronounAdj1A "шестисотый"
    } ;
  size = Num5
  } ;
lin n7 = {
  s = table {
    unit => n59 "сем" ;
    teen => nadsat "сем" ;
    ten  => n5070 "сем" ;
    hund => sot (n59 "сем")
    } ;
  o = table {
    unit => pronounAdj1B "седьмой" ;
    teen => pronounAdj1A "семнадцатый" ;
    ten  => pronounAdj1A "семидесятый" ;
    hund => pronounAdj1A "семисотый"
    } ;
  size = Num5
  } ;
lin n8 = {
  s = table {
    unit => vosem ;
    teen => nadsat "восем" ;
    ten  => \\g,a,c => case <c, g> of {
      <(Nom|VocRus|Acc), _> => "восемьдесят";
      <(Gen|Ptv|Dat|Pre|Loc), _> => "восьмидесяти" ;
      <Ins, _> => "восемьюдесятью"
		};
    hund => sot vosem
    } ;
  o = table {
    unit => pronounAdj1B "восьмой" ;
    teen => pronounAdj1A "восемнадцатый" ;
    ten  => pronounAdj1A "восьмидесятый" ;
    hund => pronounAdj1A "восьмисотый"
    } ;
  size = Num5
  } ;
lin n9 = {
  s = table {
    unit => n59 "девят" ;
    teen => nadsat "девят" ;
    ten => \\g,a,c =>
      case <c, g> of {
        <(Nom|VocRus|Acc), _> => "девяносто" ;
        <(Gen|Ptv|Dat|Ins|Pre|Loc), _> => "девяноста"
      };
    hund => sot (n59 "девят")
    } ;
  o = table {
    unit => pronounAdj1A "девятый" ;
    teen => pronounAdj1A "девятнадцатый" ;
    ten  => pronounAdj1A "девяностый" ;
    hund => pronounAdj1A "девятисотый"
    } ;
  size = Num5
  } ;

oper n59 : Str -> DetTable
  = \n -> \\g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _> => n + "ь" ;
		  <(Gen|Ptv|Dat|Pre|Loc), _> => n + "и" ;
		  <Ins, _> => n + "ью"
		} ;

oper n2030 : Str -> DetTable
  = \n -> \\g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _> => n + "дцать" ;
		  <(Gen|Ptv|Dat|Pre|Loc), _> => n + "дцати" ;
		  <Ins, _> => n + "дцатью"
		} ;

oper n5070 : Str -> DetTable
  = \n -> \\g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _> => n + "ьдесят" ;
		  <(Gen|Ptv|Dat|Pre|Loc), _> => n + "идесяти" ;
		  <Ins, _> => n + "ьюдесятью"
		} ;

oper tri : DetTable =
  \\ g, a, c =>
    case <c, g> of {
		  <(Nom|VocRus|Acc), _> => "три";
		  <(Gen|Ptv|Pre|Loc), _> => "трёх";
		  <Dat, _> => "трём";
		  <Ins, _> => "тремя"
		} ;

oper chetyre : DetTable =
  \\ g, a, c =>
    case <c, g> of {
		  <(Nom|VocRus|Acc), _> => "четыре" ;
		  <(Gen|Ptv|Pre|Loc), _> => "четырех" ;
		  <Dat, _> => "четырем" ;
		  <Ins, _> => "четырьмя"
		} ;

oper vosem : DetTable =
  \\ g, a, c =>
    case <c, g> of {
		  <(Nom|VocRus|Acc), _> => "восемь" ;
		  <(Gen|Ptv|Dat|Pre|Loc), _> => "восьми ";
		  <Ins, _> => "восемью"
		} ;

-- a little tribute to Burgess
oper nadsat : Str -> DetTable
  = \n -> \\g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _> => n + "надцать" ;
		  <(Gen|Ptv|Dat|Pre|Loc), _> => n + "надцати" ;
		  <Ins, _> => n + "надцатью"
		} ;

oper sta : DetTable -> DetTable
  = \n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _> => n ! Fem ! Animate ! c + "ста" ;
		  <Gen|Ptv, _> => n ! Fem ! Animate ! c + "сот" ;
		  <Dat, _> => n ! Fem ! Animate ! c + "стам" ;
		  <Ins, _> => n ! Fem ! Animate ! c + "стами" ;
		  <Pre|Loc, _> => n ! Fem ! Animate ! c + "стах"
		} ;

oper sot : DetTable -> DetTable
  = \n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _> => n ! Fem ! Animate ! c + "сот" ;
		  <Gen|Ptv, _> => n ! Fem ! Animate ! c + "сот" ;
		  <Dat, _> => n ! Fem ! Animate ! c + "стам" ;
		  <Ins, _> => n ! Fem ! Animate ! c + "стами" ;
		  <Pre|Loc, _> => n ! Fem ! Animate ! c + "ста"
		} ;

lin n2 = {
  s = table {
    unit => \\ g, a, c => case <c, g, a> of {
		  <(Nom|VocRus|Acc), Fem, _> => "две" ;
		  <(Nom|VocRus|Acc), (Masc|Neut), Inanimate> => "два" ;
		  <Nom|VocRus, (Masc|Neut), Animate> => "два" ;
		  <Acc      , _, Animate> => "двух" ;
		  <(Gen|Ptv|Pre|Loc), _, _> => "двух" ;
		  <Dat, _, _> => "двум" ;
		  <Ins, _, _> => "двумя"
		  };
    teen => nadsat "две" ;
    ten  => n2030 "два" ;
    hund => \\ g, a, c => case <c, g> of {
		  <(Nom|VocRus|Acc), _> => "двести" ;
		  <Gen|Ptv, _> => "двухсот" ;
		  <Dat, _> => "двумстам" ;
		  <Ins, _> => "двумястами" ;
		  <Pre|Loc, _> => "двухстах"
		  }
		} ;
  o = table {
    unit => pronounAdj1B "второй" ;
    teen => pronounAdj1A "двенадцатый" ;
    ten  => pronounAdj1A "двадцатый" ;
    hund => pronounAdj1A "двухсотый"
    } ;
  size = Num2_4
  } ;

-- : Sub10 ;                               -- 1
lin pot01 = {
  s = table {
    attr => table {
      hund => \\ g, a, c => case <g, a, c> of {
        <_, _, (Nom|VocRus|Acc)> => "сто";
        <_, _, (Gen|Ptv|Dat|Ins|Pre|Loc)> => "ста"
        } ;
		  _ => \\ g, a, c => []
		  } ;
    indep => table {
      hund => \\ g, a, c => case <g, a, c> of {
        <_, _, (Nom|VocRus|Acc)> => "сто";
        <_, _, (Gen|Ptv|Dat|Pre|Loc)> => "ста";
        <_, _, Ins> => "сотней"
			  };
      _ => \\ g, a, c => case <g, a, c> of {
        <Masc, Animate, Acc> => "одного";
        <Masc, Inanimate, Acc> => "один";
        <Masc, _, Nom|VocRus> => "один";
        <Masc, _, Gen|Ptv> => "одного";
        <Masc, _, Dat> => "одному";
        <Masc, _, Ins> => "одним";
        <Masc, _, Pre|Loc> => "одном";
        <Fem, _, Nom|VocRus> => "одна";
        <Fem, _, (Gen|Ptv|Dat|Ins|Pre|Loc)> => "одной";
        <Fem, _, Acc> => "одну";
        <Neut, _, (Nom|VocRus|Acc)> => "одно";
        <Neut, _, Gen|Ptv> => "одного";
        <Neut, _, Dat> => "одному";
        <Neut, _, Ins> => "одним";
        <Neut, _, Pre|Loc> => "одном"
        }
		  }
		} ;
  o = table {
    attr => table {
      hund => immutableAdjectiveCases "сто" ;   -- as in "сто второй"?
		  _ => immutableAdjectiveCases ""
		  } ;
    indep => table {
      hund =>  pronounAdj1A "сотый" ;
      _ => pronounAdj1A "первый"
		  }
		} ;
  just1 = True ;
  size = Num1
  } ;

-- : Digit -> Sub10 ;                       -- d * 1
lin pot0 d = {
  s = table {_ => d.s} ;
  o = table {_ => d.o} ;
  just1 = False ;
  size = d.size
  } ;

-- : Sub100 ;                       -- 10
lin pot110 = {
  s = \\p => n59 "десят" ;
  o = \\p => pronounAdj1A "десятый" ;
  just1 = False ;
  size = Num5
  } ;

-- : Sub100 ;                             -- 11
lin pot111 = {
  s = \\p => nadsat "один" ;
  o = \\p => pronounAdj1A "одиннадцатый" ;
  just1 = False ;
  size = Num5
  } ;

-- : Digit -> Sub100 ;                  -- 10 + d
lin pot1to19 d = {
  s = table {_ => d.s ! teen} ;
  o = table {_ => d.o ! teen} ;
  just1 = False ;
  size = Num5
  } ;

-- : Sub10 -> Sub100 ;                   -- coercion of 1..9
lin pot0as1 n = {
  s = table {p => n.s ! p ! unit} ;
  o = table {p => n.o ! p ! unit} ;   --?
  just1 = n.just1 ;
  size = n.size
  } ;

-- : Digit -> Sub100 ;                      -- d * 10
lin pot1 d = {
  s = table {_ => d.s ! ten} ;
  o = table {_ => d.o ! ten} ;
  just1 = False ;
  size = Num5
  } ; ---

-- : Digit -> Sub10 -> Sub100 ;         -- d * 10 + n
lin pot1plus d e = {
  s = table {_ => \\g, a, c => d.s ! ten ! g ! a ! c ++ e.s ! indep ! unit ! g ! a ! c} ;
  o = \\p => prependPF (d.s ! ten ! Masc ! Inanimate ! Nom) (e.o ! p ! unit) ;
  just1 = False ;
  size = e.size
  } ;

-- : Sub100 -> Sub1000 ;                 -- coercion of 1..99
lin pot1as2 n = {s = n.s ; size = n.size ; just1 = n.just1 ; o = n.o} ;

-- : Sub10 -> Sub1000 ;                     -- m * 100
lin pot2 d = {
  s = table {p => d.s ! p ! hund} ;
  o = table {p => d.o ! p ! hund} ;
  just1 = False ;
  size = Num5
  } ;

-- : Sub10 -> Sub100 -> Sub1000 ;       -- m * 100 + n
lin pot2plus d e = {
  s = \\p, g, a, c => d.s ! p ! hund ! g ! a ! c ++ e.s ! indep ! g ! a ! c ;
  o = \\p => prependPF (d.s ! p ! hund ! Masc ! Inanimate ! Nom) (e.o ! p) ;
  just1 = False ;
  size = e.size
  } ;

-- : Sub1000 -> Sub1000000 ;             -- coercion of 1..999
lin pot2as3 n = {
  s = n.s ! indep ;
  o = n.o ! indep ;   -- ???
  just1 = n.just1 ;   -- ???
  size = n.size
  } ;

-- : Sub1000 -> Sub1000000 ;                -- m * 1000
lin pot3 n = {  -- TODO: fix cases like: 111000, 100000
  s = \\g, a, c => n.s ! attr ! Fem ! a ! c ++ mille.s ! animNumSizeNum Animate c n.size ! numSizeCase c n.size ;
  o = prependPF (case n.just1 of {
       False => n.s ! attr ! Neut ! Inanimate ! Gen ++ BIND ;
       True => ""
       }
    )
    (pronounAdj1AstA "тысячный")   ; --TODO: not as simple. Gen or Nom?
  just1 = False ;
  size = Num5
  } ;

-- : Sub1000 -> Sub1000 -> Sub1000000 ; -- m * 1000 + n
lin pot3plus n m = {
  s = \\g, a, c => n.s ! attr ! Fem ! a ! c
    ++ mille.s ! animNumSizeNum Animate c n.size ! numSizeCase c n.size
    ++ m.s ! indep ! g ! a ! c ;
  o = prependPF (n.s ! attr ! Neut ! Inanimate ! Nom ++ mille.s ! animNumSizeNum Animate Nom n.size ! numSizeCase Nom n.size)
    (m.o ! indep) ; -- TODO: chk
  just1 = False ;
  size = Num5
  } ;
lin pot3as4 n = n ;

lin pot4as5 n = n ;

-- numerals as sequences of digits

lincat
  Dig = TDigit ;

  lin
    IDig d = d ** {tail = T1} ;

    IIDig d i = {
    s = d.s ++ spaceIf i.tail ++ i.s ;
    n = Pl ;
    size = i.size ;
    tail = inc i.tail
  } ;

  D_0 = mk2Dig "0" Num5 ;
  D_1 = mk4Dig "1" "1" Sg Num1 ; ----
  D_2 = mk2Dig "2" Num2_4 ;
  D_3 = mk2Dig "3" Num2_4 ;
  D_4 = mk2Dig "4" Num2_4 ;
  D_5 = mk2Dig "5" Num5 ;
  D_6 = mk2Dig "6" Num5 ;
  D_7 = mk2Dig "7" Num5 ;
  D_8 = mk2Dig "8" Num5 ;
  D_9 = mk2Dig "9" Num5 ;

  PosDecimal d = d ** {hasDot=False} ;
  NegDecimal d = {
    s = "-" ++ BIND ++ d.s ;
    n = Pl ;
    size = d.size ;
    hasDot=False
  } ;
  IFrac d i = {
    s=d.s ++
      if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
      i.s;
    n = Pl ;
    size = d.size ;
    hasDot=True
  } ;

  oper
    spaceIf : DTail -> Str = \t -> case t of {
      T3 => SOFT_SPACE ;
      _  => BIND
      } ;

    inc : DTail -> DTail = \t -> case t of {
      T1 => T2 ;
      T2 => T3 ;
      T3 => T1
      } ;

  oper

  mk3Dig : Str -> Str -> NumSize -> TDigit = \c,o,size -> mk4Dig c o Pl size ;
  mk2Dig : Str -> NumSize -> TDigit = \c,size -> mk3Dig c (c + "o") size ;
  mk4Dig : Str -> Str -> Number -> NumSize -> TDigit = \c,o,n,size -> {
    s = c ; ---- gender
    n = n ;
    size = size
    } ;

  TDigit = {
    n : Number ;
    s : Str ;
    size : NumSize
  } ;
}