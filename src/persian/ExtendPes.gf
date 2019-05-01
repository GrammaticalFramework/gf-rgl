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
  -- Not optimal: complement with آن should go after subject, but SSlash is already fixed.
  -- You can get the more idiomatic word order by using other RGL functions, so it's
  -- less critical to tweak this function and SSlash (pretty marginal category). /IL
  EmbedSSlash ss = {s = "آنچه" ++ appComp ss.c2 (\\_ => "آن") ++ ss.s ! Indic} ;

  -- : Adv -> NP -> Cl -- here is the car / here are the cars
  AdvIsNP adv np = mkClause (indeclNP adv.s ** {a = np.a}) (UseComp (CompNP np)) ;

  -- : VP -> Adv ;         -- by publishing the document
  ByVP vp = lin Adv {s = with_Prep.s ++ showVPH' VO False VVPres Inf defaultAgr vp } ;

  -- : VP -> Adv ;         -- (in order) to publish the document
  InOrderToVP vp = lin Adv {s = for_Prep.s
    ++ case vp.passive of {
         Replace => showVPH PerfStem defaultAgr <vp ** {s = \\vf => []} : VP> ; -- only show prefix
         _ => showVPH PerfStem defaultAgr vp}
   } ;
}
