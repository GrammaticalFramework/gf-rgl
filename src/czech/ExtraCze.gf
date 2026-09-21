concrete ExtraCze of ExtraCzeAbs = CatCze ** open ResCze, Prelude, (V = VerbCze) in {
lin
  -- The adjective agrees with the subject; ordinary slash completion
  -- supplies the object's case, prepositional form and clitic placement.
  SlashV2AP v ap = (V.SlashV2a v) ** {compl = ap.pred} ;

  -- Dative experiencer, with agreement controlled by the predicative NP.
  -- je mi pět let / jsou mi dva roky / mému synovi je pět let
  DativeCopulaCl experiencer predicate = {
    subj = case experiencer.hasClit of {True => [] ; False => experiencer.s ! Dat} ;
    clit = case experiencer.hasClit of {True => experiencer.clit ! Dat ; False => []} ;
    compl = predicate.s ! Nom ; verb = copulaVerbForms ; a = predicate.a ;
    isDrop = experiencer.hasClit ; clitPresent = experiencer.hasClit
    } ;

  DativeCopulaQCl experiencer predicate = {
    q = predicate.s ! Nom ;
    subj = case experiencer.hasClit of {True => [] ; False => experiencer.s ! Dat} ;
    clit = case experiencer.hasClit of {True => experiencer.clit ! Dat ; False => []} ;
    compl = [] ; verb = copulaVerbForms ; a = predicate.a ;
    yesNo = False
    } ;
}
