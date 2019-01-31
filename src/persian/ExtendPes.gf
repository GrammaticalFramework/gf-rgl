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
    s = \\num => np.s ! NPC bEzafa ; -- the possessed will get aEzafa, possesser is just unmarked; https://sites.la.utexas.edu/persian_online_resources/language-specific-grammar/ezfe/
    fromPron = True -- not necessarily from Pron, but it should come after the noun, and if fromPron=True, then DetCN places determiner after cn.
  } ;

}