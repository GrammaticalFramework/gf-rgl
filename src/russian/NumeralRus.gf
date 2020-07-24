--# -path=.:../abstract:../common:../../prelude

concrete NumeralRus of Numeral = CatRus [Numeral,Digits] ** open ResRus, Prelude in {

flags  coding=utf8 ;

-- Toiska, 13/8/2000, AR with Arto Mustajoki.
-- Nikita Frolov, 2011

lincat Digit = {s : DForm => Gender => Animacy => Case => Str ; size : NumSize} ;
lincat Sub10 = {s : Place => DForm => Gender => Animacy =>  Case => Str ; size : NumSize} ;
lincat Sub100 = {s : Place => Gender => Animacy => Case => Str ; size : NumSize} ;
lincat Sub1000 = {s : Place => Gender => Animacy => Case => Str ; size : NumSize} ;
lincat Sub1000000 = {s : Gender => Animacy => Case => Str ; size : NumSize} ;

lin num x = {s = \\ g,a,c => x.s ! g ! a ! c; n = Pl ; size = x.size};

lin n3  =
  {s = table {unit => tri ;
              teen => nadsat "три" ;
              ten  => n2030 "три" ;
              hund => sta tri} ;
  size = Num2_4} ;
lin n4  =
  {s = table {unit => chetyre ;
              teen => nadsat "четыр" ;
              ten  => \\ g, a, c =>
       		case <c, g> of {
		  <(Nom|VocRus|Acc), _                > => "сорок";
		  <(Gen|Ptv|Dat|Ins|Pre|Loc), _  > => "сорока" } ;
              hund => sta chetyre } ;
  size = Num2_4} ;
lin n5  =
  {s = table {unit => n59 "пят" ;
              teen => nadsat "пят" ;
              ten  => n5070 "пят" ;
              hund => sot (n59 "пят")} ;
  size = Num5} ;
lin n6  =
  {s = table {unit => n59 "шест" ;
              teen => nadsat "шест" ;
              ten  => n5070 "шест" ;
              hund => sot (n59 "шест")} ;
  size = Num5} ;
lin n7  =
  {s = table {unit => n59 "сем" ;
              teen => nadsat "сем" ;
              ten  => n5070 "сем" ;
              hund => sot (n59 "сем") } ;
  size = Num5} ;
lin n8  =
  {s = table {unit => vosem ;
              teen => nadsat "восем" ;
              ten  => \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _           > => "восемьдесят";
		  <(Gen|Ptv|Dat|Pre|Loc), _  > => "восьмидесяти" ;
		  <Ins, _                > => "восемьюдесятью"
		};
              hund => sot vosem
     } ;
  size = Num5} ;
lin n9  =
  {s = table {unit => n59 "девят" ;
              teen => nadsat "девят" ;
              ten  => \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _  >               => "девяносто";
		  <(Gen|Ptv|Dat|Ins|Pre|Loc), _  > => "девяноста"
		};
              hund => sot (n59 "девят") } ;
  size = Num5} ;

oper n59 : Str -> (Gender => Animacy => Case => Str)  = \ n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _           > => n + "ь";
		  <(Gen|Ptv|Dat|Pre|Loc), _  > => n + "и";
		  <Ins,      _           > => n + "ью"
		};

oper n2030 : Str -> (Gender => Animacy => Case => Str)  = \ n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _           > => n + "дцать";
		  <(Gen|Ptv|Dat|Pre|Loc), _  > => n + "дцати" ;
		  <Ins, _                > => n + "дцатью"
		};

oper n5070 : Str -> (Gender => Animacy => Case => Str)  = \ n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _           > => n + "ьдесят";
		  <(Gen|Ptv|Dat|Pre|Loc), _  > => n + "идесяти" ;
		  <Ins, _                > => n + "ьюдесятью"
		};

oper tri : Gender => Animacy => Case => Str = \\ g, a, c =>
       		case <c, g> of {
		  <(Nom|VocRus|Acc), _       > => "три";
		  <(Gen|Ptv|Pre|Loc), _  > => "трех";
		  <Dat, _             > => "трем";
		  <Ins,      _       > => "тремя"
		};

oper chetyre : Gender => Animacy => Case => Str = \\ g, a, c =>
       		case <c, g> of {
		  <(Nom|VocRus|Acc), _       > => "четыре";
		  <(Gen|Ptv|Pre|Loc), _  > => "четырех";
		  <Dat, _             > => "четырем";
		  <Ins,      _       > => "четырьмя"
		};

oper vosem : Gender => Animacy => Case => Str = \\ g, a, c =>
       		case <c, g> of {
		  <(Nom|VocRus|Acc), _  >          => "восемь";
		  <(Gen|Ptv|Dat|Pre|Loc), _  > => "восьми";
		  <Ins,      _  >          => "восемью"
		};

-- a little tribute to Burgess
oper nadsat : Str -> (Gender => Animacy => Case => Str)  = \ n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _  >          => n + "надцать";
		  <(Gen|Ptv|Dat|Pre|Loc), _  > => n + "надцати";
		  <Ins,      _  >          => n + "надцатью"
		};

oper sta : (Gender => Animacy => Case => Str) -> (Gender => Animacy => Case => Str)  = \ n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _  >     => n ! Fem ! Animate ! c + "ста";
		  <Gen|Ptv, _        >     => n ! Fem ! Animate ! c + "сот";
		  <Dat, _        >     => n ! Fem ! Animate ! c + "стам";
		  <Ins, _       >     => n ! Fem ! Animate ! c + "юстами";
		  <Pre|Loc, _   >     => n ! Fem ! Animate ! c + "стах"
		};

oper sot : (Gender => Animacy => Case => Str) -> (Gender => Animacy => Case => Str)  = \ n -> \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _  >     => n ! Fem ! Animate ! c + "сот";
		  <Gen|Ptv, _        >     => n ! Fem ! Animate ! c + "сот";
		  <Dat, _        >     => n ! Fem ! Animate ! c + "стам";
		  <Ins, _       >     => n ! Fem ! Animate ! c + "юстами";
		  <Pre|Loc, _   >     => n ! Fem ! Animate ! c + "ста"
		};

lin pot01  =
  {s = table {attr => table {hund => \\ g, a, c =>
			       case <g, a, c> of {
				 <_, _, (Nom|VocRus|Acc)          > => "сто";
				 <_, _, (Gen|Ptv|Dat|Pre|Loc) > => "ста";
				 <_, _, Ins               > => "сотней"
			       };
			     _ => \\ g, a, c => []} ;
              _    => table {hund => \\ g, a, c =>
			       case <g, a, c> of {
				 <_, _, (Nom|VocRus|Acc)          > => "сто";
				 <_, _, (Gen|Ptv|Dat|Pre|Loc) > => "ста";
				 -- TODO: case agreement with nouns
				 <_, _, Ins               > => "сотней"
			       };
                             _    => \\ g, a, c =>
			       case <g, a, c> of {
				 <Masc, Animate, Acc> => "одного";
				 <Masc, Inanimate, Acc> => "один";
				 <Masc, _, Nom|VocRus      > => "один";
				 <Masc, _, Gen|Ptv      > => "одного";
				 <Masc, _, Dat      > => "одному";
				 <Masc, _, Ins     > => "одним";
				 <Masc, _, Pre|Loc > => "одном";
				 <Fem, _, Nom|VocRus       > => "одна";
				 <Fem, _, (Gen|Ptv|Dat|Ins|Pre|Loc) > => "одной";
				 <Fem, _, Acc> => "одну";
				 <Neut, _, (Nom|VocRus|Acc) > => "одно";
				 <Neut, _, Gen|Ptv       > => "одного";
				 <Neut, _, Dat       > => "одному";
				 <Neut, _, Ins      > => "одним";
				 <Neut, _, Pre|Loc  > => "одном"}}} ;
   size = Num1} ;

lin n2  =
  {s = table {unit => \\ g, a, c =>
		case <c, g, a> of {
		  <(Nom|VocRus|Acc), Fem, _ > => "две";
		  <(Nom|VocRus|Acc), (Masc|Neut), Inanimate   > => "два";
		  <Nom|VocRus, (Masc|Neut), Animate     > => "два";
		  <Acc      , _, Animate     > => "двух";
		  <(Gen|Ptv|Pre|Loc), _, _      > => "двух";
		  <Dat, _, _                 > => "двум";
		  <Ins, _, _                > => "двумя"
		};
              teen => nadsat "две" ;
              ten  => n2030 "два" ;
              hund => \\ g, a, c =>
		case <c, g> of {
		  <(Nom|VocRus|Acc), _  >     => "двести";
		  <Gen|Ptv, _        >     => "двухсот";
		  <Dat, _        >     => "двумстам";
		  <Ins, _       >     => "двумястами";
		  <Pre|Loc, _   >     => "двухстах"} } ;
   size = Num2_4} ;

lin pot0 d =
  {s = table {_ => d.s} ; size = d.size} ;
lin pot110  =
  {s = \\ p => n59 "десят" ; size = Num5} ;
lin pot111  =
  {s = \\ p => nadsat "один" ; size = Num5} ; --- 11
lin pot1to19 d =
  {s = table {_ => d.s ! teen} ; size = Num5} ;
lin pot0as1 n =
  {s = table {p => n.s ! p ! unit} ; size = n.size} ;
lin pot1 d =
  {s = table {_ => d.s ! ten} ; size = Num5} ; ---
lin pot1plus d e =
  {s = table {_ => \\ g, a, c => d.s ! ten ! g ! a ! c ++ e.s ! indep ! unit ! g ! a ! c} ; size = e.size} ;
lin pot1as2 n =
  {s = n.s ; size = n.size} ;
lin pot2 d =
  {s = table {p => d.s ! p ! hund} ; size = Num5} ;
lin pot2plus d e =
  {s = \\ p, g, a, c => d.s ! p ! hund ! g ! a ! c ++ e.s ! indep ! g ! a ! c ; size = e.size} ;
lin pot2as3 n =
  {s = n.s ! indep ; size = n.size} ;
lin pot3 n =
  {s = \\ g, a, c => n.s ! attr ! Fem ! a ! c ++ mille ! n.size ; size = Num5} ;
lin pot3plus n m =
  {s = \\ g, a, c => n.s ! attr ! Fem ! a ! c ++ mille ! n.size ++ m.s ! indep ! g ! a ! c ; size = Num5} ;

--- TODO
--- raz/odin

-- numerals as sequences of digits

  lincat
    Dig = TDigit ;

  lin
    IDig d = {s = d.s ; n = d.n ; size = d.size} ;

    IIDig d i = {
      s = d.s ++ BIND ++ i.s ;
      n = Pl ;
      size = i.size
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

