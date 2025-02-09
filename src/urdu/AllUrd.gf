--# -path=.:../abstract:../common:../prelude:../hindustani:../api

concrete AllUrd of AllUrdAbs =
  LangUrd,
  ExtendUrd
  ** open ExtraUrd in {} ;
