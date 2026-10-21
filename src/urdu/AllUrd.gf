--# -path=.:../abstract:../common:../prelude:../hindustani:../api

concrete AllUrd of AllUrdAbs =
  LangUrd,
  ExtendUrd --# notpresent
  ** open ExtraUrd in {} ;
