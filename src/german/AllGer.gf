--# -path=.:../abstract:../common:../api:../prelude

concrete AllGer of AllGerAbs =
  LangGer,
  IrregGer,
  ExtendGer
  ** open ExtraGer in {} ---- to force compilation
  ;
