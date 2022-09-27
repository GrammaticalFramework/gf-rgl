concrete NumeralHrv of Numeral =

----  CatHrv [Numeral, Digits] **
  
  open
    ResHrv,
    Prelude
  in {

-- AR 2022-09-27
---- TODO ordinal forms

lincat Numeral = LinNumeral ; ---- TODO move to Cat
lincat Digits = {s : Str ; size : NumSize} ;

param NumSize = NS_1 | NS_2_4 | NS_5_20 | NS_20_ ;

oper LinNumeral = {s : AdjForms ; size : NumSize} ;
oper LinDigit = {unit : AdjForms ; teen, ten, hundred : Str ; size : NumSize} ;

lincat Digit = LinDigit ;
lincat Sub10 = LinDigit ;

lincat Sub100 = LinNumeral ;
lincat Sub1000 = LinNumeral ;
lincat Sub1000000 = LinNumeral ;

oper mkDigit : AdjForms -> Str -> Str -> Str -> NumSize -> LinDigit = 
  \dva, dvanaest, dvadeset, dvjesto, size -> {
    unit = dva ;
    teen = dvanaest ;
    ten  = dvadeset ;
    hundred = dvjesto ;
    size = size
   } ;

oper mkBigDigit : (unit, teen, ten, hundred : Str) -> LinDigit =
  \unit,ten,teen,hundred -> mkDigit (invarAdjForms unit) ten teen hundred NS_5_20 ;
  
oper bigNumeral : Str -> LinNumeral = \s -> {
  s = invarAdjForms s ;
  size = NS_20_
  } ;

lin num x = x ;

lin n2 =
  let
    dva = invarAdjForms "dva" ** {  --- BCMS: cases rarely used
      fsnom, fsacc = "dvije" ;
      msgen = "dvaju" ;
      fsgen = "dviju" ;
      msdat = "dvama" ;
      fsdat = "dvjema"
      }
  in mkDigit dva "dvanaest" "dvadeset" "dvjesto" NS_2_4 ;
  
lin n3 =
  let
    tri = invarAdjForms "tri" ** {  --- BCMS: cases rarely used
      msgen, fsgen = "triju" ;
      msdat, fsdat, msloc, mksins = "trima"
      }
  in
  mkDigit tri "trinaest" "trideset" "tristo" NS_2_4 ;
  
lin n4 =
  let
    cetiri = invarAdjForms "četiri" ** {  --- BCMS: cases rarely used
      msgen, fsgen = "četiriju" ;
      msdat, fsdat, msloc, mksins = "četirima"
      }
  in
  mkDigit cetiri "četrnaest" "četrdeset" "četiristo" NS_2_4 ;
  
lin n5 = mkBigDigit "pet" "petnaest" "pedeset" "petsto" ;
lin n6 = mkBigDigit "šest" "šesnaest" "šezdeset" "šeststo" ;
lin n7 = mkBigDigit "sedam" "sedamnaest" "sedamdeset" "sedamsto" ;
lin n8 = mkBigDigit "osam" "osamnaest" "osamdeset" "osamsto" ;
lin n9 = mkBigDigit "devet" "devetnaest" "devedeset" "devetsto" ;

lin pot01 = mkDigit (velikA "jedan") "jedanaest" "deset" "sto" NS_1 ; 
lin pot0 d = d ; 


lin pot110 = bigNumeral "deset" ;
lin pot111 = bigNumeral "jedanaest" ;
lin pot1to19 d = bigNumeral d.teen ;

lin pot0as1 n = {s = n.unit ; size = n.size} ;
lin pot1 d = bigNumeral d.ten ;
lin pot1plus d e = {
  s = invarAdjForms (d.ten ++ e.unit.msnom) ; ---- TODO inflection of e
  size = e.size
  } ;

lin pot1as2 n = n ;
lin pot2 d = bigNumeral d.hundred ;
lin pot2plus d e = {
  s = invarAdjForms (d.hundred ++ e.s.msnom) ;  ---- TODO inflection of e
  size = e.size
  } ;

lin pot2as3 n = n ;

{-
lin pot3 n = bigNumeral (mkTh (determinerStr n) n.size) ;

lin pot3plus n m = {
  s = (invarNumeral (mkTh (determinerStr n) n.size ++ determinerStr m)).s ;  ---- TODO inflection?
  size = tfSize m.size
  } ;

oper tfSize : NumSize -> NumSize = \sz -> 
  table {Num1 => Num5 ; other => other} ! sz ; 

oper mkTh : Str -> NumSize -> Str = \attr,size -> 
  case size of {
    Num1 => "tisíc" ; 
    Num2_4 => attr ++ "tisíc" ; 
    Num5 => attr ++ "tisí"
    } ;

oper determinerStr : Determiner -> Str = \d -> d.s ! Masc Anim ! Nom ;
-}

-- -- Numerals as sequences of digits have a separate, simpler grammar
  lincat Dig = {s : Str ; size : NumSize} ;

  lin
    IDig d = d ;
    
    IIDig d dd = {s = d.s ++ Predef.BIND ++ dd.s ; size = dd.size} ;

    D_0 = { s = "0" ; size = NS_1} ; ---- ??
    D_1 = { s = "1" ; size = NS_1} ; 
    D_2 = { s = "2" ; size = NS_2_4} ;
    D_3 = { s = "3" ; size = NS_2_4} ;
    D_4 = { s = "4" ; size = NS_2_4} ;
    D_5 = { s = "5" ; size = NS_5_20} ;
    D_6 = { s = "6" ; size = NS_5_20} ;
    D_7 = { s = "7" ; size = NS_5_20} ;
    D_8 = { s = "8" ; size = NS_5_20} ;
    D_9 = { s = "9" ; size = NS_5_20} ;

}
