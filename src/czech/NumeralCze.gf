concrete NumeralCze of Numeral = CatCze [Numeral,Digits,Decimal] **
  open ResCze, Prelude in {

-- Keep inflection until the numeral receives its case. Compound numerals
-- use agreement with their final component (dvacet jeden rok, dvacet dvě děti).
-- This is one standard variant. The more usual genitive-plural alternative
-- (dvacet jedna roků, dvacet dva žáků) is not generated here.
-- See https://prirucka.ujc.cas.cz/?id=792.
oper
  -- Only units vary in count agreement. Keeping four full Determiners here
  -- would create a product of independent agreement states during compilation.
  LinDigit : Type = {unit : Determiner ; teen,ten,hundred : Gender => Case => Str} ;
  digit : Determiner -> Str -> Str -> Str -> LinDigit = \u,teen,ten,hundred -> {
    unit = u ; teen = (regNumeral teen (teen + "i")).s ;
    ten = (regNumeral ten (ten + "i")).s ;
    hundred = \\_,c => case c of {Nom|Acc|ResCze.Voc => hundred ; _ => (scale QuantifiedScale u hundredN).s ! Neutr ! c}
    } ;
  counted : (Gender => Case => Str) -> Determiner = \s -> {
    s = s ; size = Num5 ; head = CountedHead
    } ;
  hundreds : LinDigit -> Determiner = \d -> {
    s = d.hundred ; size = NumScale ; head = ScaleHead Neutr d.unit.size QuantifiedScale
    } ;
  plus : Determiner -> Determiner -> Determiner = \a,b -> {
    s = \\g,c => a.s ! g ! c ++ b.s ! g ! c ; size = b.size ; head = b.head
    } ;
  scale : ScaleAgreement -> Determiner -> Noun -> Determiner = \agr,d,n -> {
    s = \\_,c => d.s ! n.g ! c ++ numSizeForm n.s d.size c ; size = NumScale ;
    head = case d.head of {CountedHead => ScaleHead n.g d.size agr ; h => h}
    } ;
  bareScale : ScaleAgreement -> Noun -> Determiner = \agr,n -> {
    s = \\_,c => n.s ! Sg ! c ; size = NumScale ; head = ScaleHead n.g Num1 agr
    } ;
  decimalScale : ScaleAgreement -> {s : Str ; size : NumSize ; hasDot : Bool} -> Noun -> Determiner = \agr,d,n -> {
    s = \\_,c => d.s ++ case d.hasDot of {
      True => n.s ! Sg ! Gen ; False => numSizeForm n.s d.size c
      } ; size = NumScale ; head = ScaleHead n.g d.size agr
    } ;
  hundredN : Noun = nounFormsNoun ((declMESTO "sto") ** {pgen = "set" ; pdat = "stům" ; ploc = "stech"}) ;
  thousandN : Noun = nounFormsNoun ((declSTROJ "tisíc") ** {pgen = "tisíc"}) ;
  millionN : Noun = nounFormsNoun (declHRAD "milion") ;
  billionN : Noun = nounFormsNoun (declZENA "miliarda") ;

lincat
  Digit,Sub10 = LinDigit ;
  Sub100,Sub1000,Sub1000000,Sub1000000000,Sub1000000000000 = Determiner ;
lin
  num x = x ;
  n2 = digit twoNumeral "dvanáct" "dvacet" "dvě stě" ;
  n3 = digit threeNumeral "třináct" "třicet" "tři sta" ;
  n4 = digit fourNumeral "čtrnáct" "čtyřicet" "čtyři sta" ;
  n5 = digit (regNumeral "pět" "pěti") "patnáct" "padesát" "pět set" ;
  n6 = digit (regNumeral "šest" "šesti") "šestnáct" "šedesát" "šest set" ;
  n7 = digit (regNumeral "sedm" "sedmi") "sedmnáct" "sedmdesát" "sedm set" ;
  n8 = digit (regNumeral "osm" "osmi") "osmnáct" "osmdesát" "osm set" ;
  n9 = digit (regNumeral "devět" "devíti") "devatenáct" "devadesát" "devět set" ;
  pot01 = {unit = oneNumeral ; teen = (regNumeral "jedenáct" "jedenácti").s ;
    ten = (regNumeral "deset" "deseti").s ; hundred = (bareScale QuantifiedScale hundredN).s} ;
  pot0 d = d ;
  pot0as1 d = d.unit ;
  pot110 = regNumeral "deset" "deseti" ;
  pot111 = regNumeral "jedenáct" "jedenácti" ;
  pot1to19 d = counted d.teen ;
  pot1 d = counted d.ten ;
  pot1plus d e = plus (counted d.ten) e.unit ;
  pot1as2 n = n ;
  pot21 = bareScale QuantifiedScale hundredN ;
  pot2 d = hundreds d ;
  pot2plus d e = plus (hundreds d) e ;
  pot2as3 n = n ;
  -- Generation default: hundreds/thousands take quantified agreement.
  -- Tisíc also admits nominal agreement; the noun's declension is independent.
  pot31 = bareScale QuantifiedScale thousandN ;
  pot3 n = scale QuantifiedScale n thousandN ;
  pot3plus n m = plus (scale QuantifiedScale n thousandN) m ;
  pot3as4 n = n ;
  pot3decimal d = decimalScale QuantifiedScale d thousandN ;
  -- Millions/billions instead agree with their nominal head (dva miliony jsou).
  pot41 = bareScale NominalScale millionN ;
  pot4 n = scale NominalScale n millionN ;
  pot4plus n m = plus (scale NominalScale n millionN) m ;
  pot4as5 n = n ;
  pot4decimal d = decimalScale NominalScale d millionN ;
  pot51 = bareScale NominalScale billionN ;
  pot5 n = scale NominalScale n billionN ;
  pot5plus n m = plus (scale NominalScale n billionN) m ;
  pot5decimal d = decimalScale NominalScale d billionN ;

-- -- Numerals as sequences of digits have a separate, simpler grammar
  lincat Dig = {s:Str ; size : NumSize} ;

  lin
    IDig d = d ;

    IIDig d dd = {s = d.s ++ Predef.BIND ++ dd.s ; size = Num5} ; ---- leading zeros ??

    D_0 = { s = "0" ; size = Num5} ;
    D_1 = { s = "1" ; size = Num1} ;
    D_2 = { s = "2" ; size = Num2_4} ;
    D_3 = { s = "3" ; size = Num2_4} ;
    D_4 = { s = "4" ; size = Num2_4} ;
    D_5 = { s = "5" ; size = Num5} ;
    D_6 = { s = "6" ; size = Num5} ;
    D_7 = { s = "7" ; size = Num5} ;
    D_8 = { s = "8" ; size = Num5} ;
    D_9 = { s = "9" ; size = Num5} ;

    PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {
      s = "-" ++ Predef.BIND ++ d.s ;
      size = d.size ;
      hasDot=False
      } ;
    IFrac d i = {
      s = d.s ++
          if_then_Str d.hasDot BIND (BIND++","++BIND) ++
          i.s ;
      size = Num5 ;
      hasDot=True
      } ;

}
