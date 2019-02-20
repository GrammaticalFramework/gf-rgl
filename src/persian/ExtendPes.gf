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
    s = \\num => np.s ! Bare ; -- the possessed will get Ezafe, possesser is just unmarked; https://sites.la.utexas.edu/persian_online_resources/language-specific-grammar/ezfe/
    mod = Ezafe -- not necessarily from Pron, but it should come after the noun, and if mod=True, then DetCN places determiner after cn.
  } ;

}
