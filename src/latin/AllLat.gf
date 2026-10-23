--# -path=.:../abstract:../common:../prelude:../api

concrete AllLat of AllLatAbs = 
  LangLat,
  ExtendLat
  ** open ExtraLat in {} ;
