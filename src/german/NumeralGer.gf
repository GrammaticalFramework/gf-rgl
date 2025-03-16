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

  n2 = mkDigit  "zwei"  "zwölf"    "zwanzig"  "zweite" ;
  n3 = mkDigit  "drei"  "dreizehn" "dreissig" "dritte" ;
  n4 = regDigit "vier" ;
  n5 = regDigit "fünf" ;
  n6 = mkDigit  "sechs"  "sechzehn" "sechzig" "sechste" ;
  n7 = mkDigit  "sieben" "siebzehn" "siebzig" "siebte" ;
  n8 = mkDigit  "acht"   "achtzehn" "achtzig" "achte" ;
  n9 = regDigit "neun" ;

  pot01 = {
    s = \\f => table {
          NCard (AMod gn c) => "ein" + pronEnding ! gn ! c ;
          NCard APred => "ein" ;
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

  -- HL 15.3.2025 added:
  --    pot21, pot31, pot41, pot4, pot4plus, pot51, pot5, pot5plus, pot5decimal
  --    and opers oneCardOrd, cardOrdNum, toFem
  -- changed: multiple to get pot21      = hundert    (Eng: a hundred,   Swe: hundra)
   --                  versus pot2 pot01 = einhundert (Eng: one hundred, Swe: etthundra)
  --          likewise for [ein]tausend, [eine] Million, Milliarde
  -- Q: add BIND or "und" in pot4plus, pot5plus ?

  pot21 = {s = \\g => cardOrd "hundert" "hundertste" ! g ; n = Pl} ;
  pot2 d = {s = \\g =>
    multiple (d.s ! DUnit) d.n ++ cardOrd "hundert" "hundertste" ! g ; n = Pl} ;
  pot2plus d e = {s = \\g => 
    multiple (d.s ! DUnit) d.n ++ "hundert" ++ BIND ++ e.s ! g ; n = Pl} ;
  pot2as3 n = n ;

  pot31 = {s = \\g => cardOrd "tausend" "tausendste" ! g ; n = Pl} ;
  pot3 n = {s = \\g => 
    multiple n.s n.n ++ cardOrd "tausend" "tausendste" ! g ; n = Pl} ;
  pot3plus n m = {s = \\g =>
    multiple n.s n.n ++ "tausend" ++ m.s ! g ; n = Pl} ;
  pot3as4 n = n ;
  pot3decimal d = {s = \\g =>
    d.s ! invNum ++ cardOrd "Tausend" "tausendste" ! g ; n = Pl} ;

  pot41 = {s = \\g => oneCardOrd "Million" "millionste" ! g ; n = Pl} ;
  pot4 n = let tab = cardOrdNum n.n "Million" "millionste" in {
    s = table{
      NCard af => n.s ! (NCard (toFem af)) ++ tab ! (NCard af) ;
      NOrd af  => n.s ! (NCard APred) ++ BIND ++ tab ! (NOrd af)
      } ;
    n = Pl} ;
  pot4plus n m = let tab = cardOrdNum n.n "million" "millionste" in {
    s = table{
      NCard af => n.s ! (NCard (toFem af)) ++ BIND ++ tab ! (NCard af) ++ m.s ! NCard af ;
      NOrd af  => n.s ! (NCard (toFem APred)) ++ BIND ++ tab ! (NCard (toFem af)) ++ m.s ! NOrd af
      } ;
    n = Pl} ;
  pot4as5 n = n ;
  pot4decimal d = {s = \\g =>
    d.s ! invNum ++ cardOrdNum d.n "Million" "-millionste" ! g ; n = Pl} ;

  pot51 = {s = \\g => oneCardOrd "Milliarde" "milliardste" ! g ; n = Pl} ;
  pot5 n = let tab = cardOrdNum n.n "Milliarde" "milliardste" in {
    s = table{
      NCard af => n.s ! (NCard (toFem af)) ++ tab ! (NCard af) ;
      NOrd af  => n.s ! (NCard APred) ++ BIND ++ tab ! (NOrd af)
      } ;
    n = Pl} ;
  pot5plus n m = let tab = cardOrdNum n.n "milliarde" "milliardste" in {
    s = table{
      NCard af => n.s ! (NCard (toFem af)) ++ BIND ++ tab ! (NCard af) ++ "und" ++ m.s ! NCard af ;
      NOrd af  => n.s ! (NCard (toFem APred)) ++ BIND ++ tab ! (NCard (toFem af)) ++ m.s ! NOrd af
      } ;
    n = Pl} ;
  pot5decimal d = {s = \\g =>
    d.s ! invNum ++ cardOrdNum d.n "Milliarde" "-milliardste" ! g ; n = Pl} ;

oper
  multiple : (CardOrd => Str) -> Number -> Str = \d,n -> 
  --    case n of {Sg => [] ; _ => d ! invNum ++ BIND} ;
    d ! invNum ++ BIND ;  -- HL

  oneCardOrd : Str -> Str -> CardOrd => Str =
    \million,millionste -> table {
      NCard (AMod (GSg _) c) => "ein" + pronEnding ! (GSg Fem) ! c ++ million ;
      NCard (AMod GPl _)     => pluralN million ;
      NCard APred => "ein" + pronEnding ! (GSg Fem) ! Nom ++ million ;
      NOrd a => (regA (init millionste)).s ! Posit ! a
      } ;

  cardOrdNum : Number -> Str -> Str -> CardOrd => Str = \n,drei,dritte ->
    case n of {Sg => cardOrd drei dritte ; _ => cardOrd (pluralN drei) dritte} ;

  toFem : AForm -> AForm = \af -> case af of {
    AMod _ c => AMod (GSg Fem) c ;
    APred    => AMod (GSg Fem) Nom} ;

--------------------

  lincat 
    Dig = TDigit ;

  lin
    IDig d = {s = d.s ;
              n = d.n ;
              isDig = True ;
              tail1to19 = notB d.isZero} ;

    -- HL 11/2023 added case endings of ordinals to digits
    -- NCard Masc Nom (= invNum):       0,1,     19,    20,21,...
    -- NOrd (AMod (GSg Masc) Nom): 0ter,1ter,...,19ter, 20ster,21ster,...,99ster, 100ster
    --                                101ter,...,119ter,120ster,...             , 200ster
    IIDig d i =
      let isPld : Bool = case d.n of {Sg => False ; _ => True} ;
          i = lin Digits i ; -- suppress warning missing lock_Digits
          b : Bool = case i.isDig of {True => isPld ; _ => notB i.tail1to19} ;
          i' : Digits = case b of {True => IDig (mkDig (i.s ! invNum ++ BIND ++ "s")) ;
                                   _  => i}
      in {s = table {NCard af => d.s ! invNum ++ BIND ++ i.s ! NCard af ;
                     NOrd af => d.s ! invNum ++ BIND ++ i'.s ! NOrd af} ;
          n = Pl ;
          isDig = False ;
          tail1to19 = case i.isDig of {True => notB isPld ; False => i.tail1to19} ;
      } ;

    D_0 = mkDig "0" ** {isZero = True} ;
    D_1 = mk2Dig "1" ; -- with cardinal inflection and number Sg
    D_2 = mkDig "2" ;
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
      s = \\o => d.s ! invNum ++
        if_then_Str d.hasDot BIND (BIND ++ "." ++ BIND) ++
        i.s ! o;
      n = Pl;
      hasDot=True
    } ;

  oper
    mkDig : Str -> TDigit = \c -> mk3Dig c (c + "t") Pl ; -- like Duden 464 (4.Auflage)

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard _ => c ;                        -- 0,...,9
                 NOrd af => (regA o).s ! Posit ! af} ; -- (ein) 0ter .. 9ter | (der) 0te ... 9te
      n = n ;                                          -- NOrd APred: "0t"
      isZero = False
      } ;

    mk2Dig : Str -> TDigit = \crd ->
      {s = table {NCard af => crd ;
                  NOrd af => (regA (crd + "t")).s ! Posit ! af} ;
       n = Sg ;
       isZero = False
      } ;

    TDigit = {
      n : Number ;
      s : CardOrd => Str ;
      isZero : Bool
    } ;

}
