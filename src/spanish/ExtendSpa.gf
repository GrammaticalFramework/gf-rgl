--# -path=alltenses:../common:../abstract:../romance
concrete ExtendSpa of Extend = CatSpa ** ExtendRomanceFunctor -
  [
 CompVP,
 CompoundAP,
 CompoundN,
 ExistsNP,
 GenRP,
 GenRP,
 IAdvAdv,
 ICompAP,
 InOrderToVP,
 WithoutVP,
 iFem_Pron,
 theyFem_Pron,
 weFem_Pron,
 youFem_Pron,
 youPlFem_Pron,
 youPolFem_Pron,
 youPolPlFem_Pron,
 youPolPl_Pron
    ]                   -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarSpa), (Syntax = SyntaxSpa), (ResRomance = ResSpa) **
  open
  GrammarSpa,
  ResSpa,
  MorphoSpa,
  Coordination,
  Prelude,
  ParadigmsSpa in {
    -- put your own definitions here

  lin
    iFem_Pron     = agr2pron ! {g=Fem ; n=Sg ; p=P1} ;
    youFem_Pron   = agr2pron ! {g=Fem ; n=Sg ; p=P2} ;
    weFem_Pron    = agr2pron ! {g=Fem ; n=Pl ; p=P1} ;
    youPlFem_Pron = agr2pron ! {g=Fem ; n=Pl ; p=P2} ;
    theyFem_Pron  = agr2pron ! {g=Fem ; n=Pl ; p=P3} ;

    youPolFem_Pron = mkPronoun
      "usted" "la" "le" "usted"
      "su" "su" "sus" "sus"
      Fem Sg P3 ;
    youPolPl_Pron = mkPronoun
      "ustedes" "los" "les" "ustedes"
      "su" "su" "sus" "sus"
      Masc Pl P3 ;
    youPolPlFem_Pron = mkPronoun
      "ustedes" "las" "les" "ustedes"
      "su" "su" "sus" "sus"
      Fem Pl P3 ;

    ICompAP ap = {
      s =\\a => "qué tan" ++ ap.s ! (genNum2Aform a.g a.n) ;
      cop = serCopula
      } ;

    IAdvAdv adv = {
      s = "qué tan" ++ adv.s
      } ;

    WhatSSC s = {s = \\_ => "lo que" ++ s.s ! Indic} ; --- mood

    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "existir"))) ;

    CompoundN noun noun2 = { -- order is different because that's needed for correct translation from english
      s = \\n => noun2.s ! n
        ++ variants {"de" ; genForms "del" "de la" ! noun.g}
        ++ noun.s ! Sg ;
      g = noun2.g
      } ;

    CompoundAP noun adj = {
      s = \\af => case (aform2aagr af) of {
        {n = n} => adj.s ! Posit ! (genNum2Aform noun.g n) ++ "de" ++ noun.s ! n
        } ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    WithoutVP vp = {
      s = "sin" ++ infStr vp
      } ;

    InOrderToVP vp = {
      s = "para" ++ infStr vp
      } ;

    --TODO: actually use ant
    CompVP ant p vp = let
      neg = negation ! p.p
      in {
        s = \\agr => ant.s ++ p.s ++ "de" ++ neg.p1 ++ infVP vp agr ;
        cop = serCopula
      } ;

} ;
