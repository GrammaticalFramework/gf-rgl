--# -path=.:../abstract:../common:../prelude

concrete AllLat of AllLatAbs = 
  LangLat,
  ExtendLat
  ** open ExtraLat in {} ;
