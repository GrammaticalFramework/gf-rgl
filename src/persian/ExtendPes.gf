--# -path=.:../common:../abstract:../prelude

concrete ExtendPes of Extend =
  CatPes ** ExtendFunctor - [
    GenNP, ApposNP, ICompAP, AdvIsNP, InOrderToVP, ByVP
    ,GerundNP,GerundCN,GerundAdv,EmbedPresPart,EmbedSSlash
    ]
  with (Grammar=GrammarPes)
  ** open Prelude, ResPes in {

lin
  -- NP -> Quant ; -- this man's
  GenNP np = makeQuant [] [] ** np ** {
    mod = Ezafe ; -- the possessed will get Ezafe
    s = \\num,cmpd => np2str np -- possesser is unmarked; https://sites.la.utexas.edu/persian_online_resources/language-specific-grammar/ezfe/
  } ;

  -- : NP -> NP -> NP
  ApposNP np1 np2 = np1 ** {
    s = \\m => np1.s ! m ++ np2.s ! m
  } ;

  ICompAP ap = {s = "چقدر" ++ ap.s ! Bare} ;
  -- : VP -> CN ;          -- publishing of the document (can get a determiner)
  GerundCN vp = useN (indeclN (showVPH Inf defaultAgr vp)) ;

  -- : VP -> NP ;          -- publishing the document (by nature definite)
  GerundNP vp = indeclNP (showVPH Inf defaultAgr vp) ;

  -- : VP -> Adv ;         -- publishing the document (prepositionless adverb)
  GerundAdv vp = lin Adv {s = showVPH Inf defaultAgr vp} ;

  -- : VP -> SC ;
  EmbedPresPart vp = lin SC {s = showVPH Inf defaultAgr vp} ;

  -- : SSlash -> SC
  EmbedSSlash ss = {s = conjThat ++ ss.s ! Indic ++ ss.c2.s} ;

  -- : Adv -> NP -> Cl -- here is the car / here are the cars
  AdvIsNP adv np = mkClause (indeclNP adv.s ** {a = np.a}) (UseComp (CompNP np)) ;

  -- : VP -> Adv ;         -- by publishing the document
  ByVP vp = lin Adv {s = with_Prep.s ++ showVPH' VO False VVPres Inf defaultAgr vp } ;

  -- : VP -> Adv ;         -- (in order) to publish the document
  InOrderToVP vp = lin Adv {s = for_Prep.s ++ showVPH PerfStem defaultAgr vp} ;




}
