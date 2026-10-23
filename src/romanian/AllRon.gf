--# -no-pmcfg 
--# -path=.:../abstract:../common:../prelude:../api

concrete AllRon of AllRonAbs =
  LangRon, ExtendRon
  ** {} ;
