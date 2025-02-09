--# -path=.:../abstract:../common:../prelude:../hindustani:../api

concrete AllHin of AllHinAbs =
  LangHin,
  ExtendHin
  ** open ExtraHin in {} ;
