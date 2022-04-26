--# -path=.:../abstract:../common:../api:../prelude

concrete AllGer of AllGerAbs = 
  LangGer,
  IrregGer,
----  ExtendGer, ---- to replace ExtraGer
  ExtraGer
  **
  open ExtendGer in ---- to force compilation
    {} ;
