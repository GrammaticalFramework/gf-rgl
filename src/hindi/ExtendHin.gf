--# -path=.:../common:../abstract

concrete ExtendHin of Extend =
  CatHin ** ExtendFunctor -
  [
    ComplDirectVQ, ComplDirectVS
   ]
  with
    (Grammar = GrammarHin) ** {

}
