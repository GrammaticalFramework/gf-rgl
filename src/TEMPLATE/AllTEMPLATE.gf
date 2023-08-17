--# -path=.:../abstract:../common:../prelude

concrete AllTEMPLATE of AllTEMPLATEAbs =
  LangTEMPLATE,
  ExtendTEMPLATE
  ** open ParadigmsTEMPLATE in {
    lin sing_V2 = mkV2 "nyanyi" ;
  } ;
