--# -path=.:../romance:../abstract:../common:../api:../prelude

concrete AllIta of AllItaAbs = 
  LangIta,
--  IrregIta,
  ExtraIta 
  ** open ExtendIta
  in {} ;
