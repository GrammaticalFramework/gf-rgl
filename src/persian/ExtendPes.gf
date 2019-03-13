--# -path=.:../common:../abstract:../prelude

concrete ExtendPes of Extend =
  CatPes ** ExtendFunctor - [
    GenNP, ApposNP, ICompAP
    ]
  with (Grammar=GrammarPes)
  ** open Prelude, ResPes in {

lin
  -- NP -> Quant ; -- this man's
  GenNP np = np ** {
    mod = Ezafe ; -- the possessed will get Ezafe
    s = \\num,cmpd => np.s ! Bare -- possesser is unmarked; https://sites.la.utexas.edu/persian_online_resources/language-specific-grammar/ezfe/
  } ;

  -- : NP -> NP -> NP
  ApposNP np1 np2 = np1 ** {
    s = \\m => np1.s ! m ++ np2.s ! m
  } ;

  ICompAP ap = {s = "چقدر" ++ ap.s ! Bare} ;
}
