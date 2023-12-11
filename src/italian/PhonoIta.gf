resource PhonoIta = open Prelude in {
  flags coding=utf8 ;

--3 Elision
--
-- The phonological rule of *elision* can be defined as follows in GF.
-- In Italian it includes both vowels and the *impure 's'*.

oper 
  vocale : Strs = strs {
    "a" ; "e" ; "h" ; "i" ; "o" ; "u" ; "è" ; "y" ; "A" ; "E" ; "I" ; "O" ; "U" ; "H"
    } ;

  sImpuro : Strs = strs {
    "sb" ; "sc" ; "sd" ; "sf" ; "sg" ; "sh" ; "sl" ; "sm" ; "sn" ; "sp" ; "sq" ; "sr" ; "st" ; "sv" ;
    "Sb" ; "Sc" ; "Sd" ; "Sf" ; "Sg" ; "Sh" ; "Sl" ; "Sm" ; "Sn" ; "Sp" ; "Sq" ; "Sr" ; "St" ; "Sv"
    } ;

  xyz : Strs = strs {
    "x" ; "X" ;
    "z" ; "Z" ;
    "y" ; "Y"
  } ;

  gn : Strs = strs {
    "gn" ; "Gn"
  } ;

  pn : Strs = strs {
    "pn" ; "Pn"
  } ;

  ps : Strs = strs {
    "ps" ; "Pn"
  } ;

  elision : (_,_,_ : Str) -> Str = \il, l', lo -> 
    let ll = case last l' of {"'" => l' ++ Predef.BIND ; _ => l'} in
    pre {il ; ll / vocale ; lo / sImpuro ; lo / xyz ; lo / gn ; lo / pn ; lo / ps } ;
---    pre {vocale => l' ;  sImpuro => lo ; _ => il} ;  --- doesn't work properly 15/4/2014

}
