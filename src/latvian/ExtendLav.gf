--# -path=.:../common:../abstract

concrete ExtendLav of Extend =
  CatLav ** ExtendFunctor -
  [
    iFem_Pron, weFem_Pron, youFem_Pron, youPolFem_Pron, youPlFem_Pron,
    theyFem_Pron,
    ComplDirectVQ, ComplDirectVS
   ]
  with
    (Grammar = GrammarLav) **
  open
    ResLav,
    ParadigmsPronounsLav in {

lin iFem_Pron = mkPronoun_I Fem ;
    weFem_Pron = mkPronoun_We Fem ;

    youFem_Pron = mkPronoun_You_Sg Fem ;
    youPolFem_Pron = mkPronoun_You_Pol Fem ;
    youPlFem_Pron = mkPronoun_You_Pl Fem ;

    theyFem_Pron = mkPronoun_They Fem ;

}
