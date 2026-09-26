--# -path=.:../abstract:../common:../prelude

concrete AllEus of AllEusAbs = 
  LangEus,
  ExtendEus
  ** open ExtraEus in {} ;
