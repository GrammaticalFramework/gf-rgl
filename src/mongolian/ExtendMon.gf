--# -path=.:../common:../abstract

concrete ExtendMon of Extend =
  CatMon ** ExtendFunctor
  with
    (Grammar = GrammarMon) ** {

}
