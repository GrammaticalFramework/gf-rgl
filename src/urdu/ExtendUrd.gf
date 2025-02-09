--# -path=.:../common:../abstract

concrete ExtendUrd of Extend =
  CatUrd ** ExtendFunctor -
  [
    ComplDirectVQ, ComplDirectVS
   ]
  with
    (Grammar = GrammarUrd) ** {

}
