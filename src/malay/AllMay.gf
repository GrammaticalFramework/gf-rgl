--# -path=.:../abstract:../common:../prelude

concrete AllMay of AllMayAbs =
  LangMay,
  ExtendMay
  ** open ParadigmsMay in {
    lin sing_V2 = mkV2 "nyanyi" ;
  } ;
