concrete NumeralCze of Numeral =

  CatCze **
  
  open
    ResCze,
    Prelude
  in {

-- from gf-contrib/numerals/czech.gf, added inflections
-- AR 2020-03-20


oper LinNumeral = {s : NumeralForms ; size : NumSize} ;
oper LinDigit = {unit : NumeralForms ; teen, ten, hundred : Str ; size : NumSize} ;

lincat Digit = LinDigit ;
lincat Sub10 = LinDigit ;

lincat Sub100 = LinNumeral ;
lincat Sub1000 = LinNumeral ;
lincat Sub1000000 = LinNumeral ;

oper mkNum : NumeralForms -> Str -> Str -> Str -> NumSize -> LinDigit = 
  \dva, dvanast, dvadsat, dveste, sz -> {
    unit = dva ;
    teen = dvanast + "náct" ;
    ten  = dvadsat ;
    hundred = dveste ;
    size = sz
   } ;

oper mk2Num : NumeralForms -> Str -> Str -> Str -> LinDigit =
  \unit, teenbase, tenbase, hundred ->  
    mkNum unit teenbase (tenbase + "cet") hundred Num2_4 ; 

oper mk5Num : Str -> Str -> Str -> Str -> LinDigit =
  \unit,uniti, teenbase, tenbase ->  
  mkNum (regNumeralForms unit uniti) teenbase (tenbase + "desát") (unit ++ "set") Num5 ;

oper bigNumeral : Str -> LinNumeral = \s -> {s = invarNumeralForms s ; size = Num5} ;

lin num x = x ;

lin n2 = mk2Num twoNumeralForms "dva" "dva" ("dvě" ++ "stě") ;
lin n3 = mk2Num threeNumeralForms "tři" "tři" ("tři" ++ "sta") ;
lin n4 = mk2Num fourNumeralForms "čtr" "čtyři" ("čtyři" ++ "sta") ;
lin n5 = mk5Num "pět" "pěti" "pat" "pa" ;
lin n6 = mk5Num "šest" "šesti" "šest" "še" ;
lin n7 = mk5Num "sedm" "sedmi" "sedm" "sedm";
lin n8 = mk5Num "osm" "osmi" "osm" "osm";
lin n9 = mk5Num "devět" "devíti" "devate" "deva" ;

lin pot01 = {
  unit = oneNumeralForms ; hundred = "sto" ; ten = "deset" ; teen = "jedenáct" ;
  size = Num1
  } ; 
lin pot0 d = d ; 

lin pot110 = bigNumeral "deset" ;
lin pot111 = bigNumeral "jedenáct" ;
lin pot1to19 d = bigNumeral d.teen ;

lin pot0as1 n = {s = n.unit ; size = n.size} ;
lin pot1 d = bigNumeral d.ten ;
lin pot1plus d e = {
  s = invarNumeralForms (d.ten ++ e.unit.msnom) ; ---- TODO inflection?
  size = tfSize e.size
  } ;
  ---- variants { d.s ! ten ++ e.s ! unit ; glue (glue (e.s ! unit) "a") (d.s ! ten)} ; size = tfSize e.size} ;

lin pot1as2 n = n ;
lin pot2 d = bigNumeral d.hundred ;
lin pot2plus d e = {
  s = invarNumeralForms (d.hundred ++ e.s.msnom) ;  ---- TODO inflection?
  size = tfSize e.size
  } ;

lin pot2as3 n = n ;
lin pot3 n = bigNumeral (mkTh n.s.msnom n.size) ;

lin pot3plus n m = {
  s = invarNumeralForms (mkTh n.s.msnom n.size ++ m.s.msnom) ;  ---- TODO inflection?
  size = tfSize m.size
  } ;

oper tfSize : NumSize -> NumSize = \sz -> 
  table {Num1 => Num5 ; other => other} ! sz ; 

oper mkTh : Str -> NumSize -> Str = \attr,size -> 
  case size of {
    Num1 => "tisíc" ; 
    Num2_4 => attr ++ "tisíce" ; 
    Num5 => attr ++ "tisíc"
    } ;



-- -- Numerals as sequences of digits have a separate, simpler grammar
  lincat Dig = {s:Str ; n : NumSize} ;

  lin
    IDig d = d;
    
    IIDig d dd = {s = d.s ++ Predef.BIND ++ dd.s ; n = Num5} ; ---- leading zeros ??

    D_0 = { s = "0" ; n = Num1} ; ---- ??
    D_1 = { s = "1" ; n = Num1} ; 
    D_2 = { s = "2" ; n = Num2_4} ;
    D_3 = { s = "3" ; n = Num2_4} ;
    D_4 = { s = "4" ; n = Num2_4} ;
    D_5 = { s = "5" ; n = Num5} ;
    D_6 = { s = "6" ; n = Num5} ;
    D_7 = { s = "7" ; n = Num5} ;
    D_8 = { s = "8" ; n = Num5} ;
    D_9 = { s = "9" ; n = Num5} ;

}
