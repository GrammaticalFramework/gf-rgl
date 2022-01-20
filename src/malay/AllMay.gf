--# -path=.:../abstract:../common:../prelude

concrete AllMay of AllMayAbs =
  LangMay,
  ExtendMay
  ** {
    lin sing_V2 = mkV2 "nyanyi" ;
  } ;
