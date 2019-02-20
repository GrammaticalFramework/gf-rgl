--# -path=.:../common:../abstract:../prelude

concrete ExtendPes of Extend =
  CatPes ** ExtendFunctor - [
    GenNP
    ]
  with (Grammar=GrammarPes)
  ** open Prelude, ResPes in {

lin
  -- NP -> Quant ; -- this man's
  GenNP np = np ** {
    mod = Ezafe ; -- the possessed will get Ezafe
    s = \\num => np.s ! Bare -- possesser is unmarked; https://sites.la.utexas.edu/persian_online_resources/language-specific-grammar/ezfe/
  } ;

}
