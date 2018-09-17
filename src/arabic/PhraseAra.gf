concrete PhraseAra of Phrase = CatAra ** open 
  ParamX, 
  Prelude, 
  ResAra in {
  flags coding=utf8;

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ! Masc ++ voc.s} ;--FIXME

    UttS s = {s = \\g => s.s} ; ---- OK? AR

    UttIAdv s = {s = \\g => s.s} ; ---- OK? AR

    UttQS qs = {s = \\g => qs.s ! QDir} ;
    UttImpSg pol imp = {s = \\g => imp.s ! pol.p ! g ! ResAra.Sg ++ pol.s} ;
    UttImpPl,UttImpPol = \pol,imp -> {s = \\g => imp.s ! pol.p ! g ! ResAra.Pl ++ pol.s} ;
--
    UttIP ip = {s = \\_ => ip.s} ; ---- AR

     -- AP = { s : Species => Gender => Number => State => Case => Str } ;
    UttAP ap = {s = \\g => ap.s ! NoHum ! g ! ResAra.Sg ! Def ! Nom } ;  ---- OK? IL
    -- Card =  {s : Gender => State => Case => Str } ;
    UttCard c = {s =  \\g => c.s ! g ! Def ! Nom } ;  ---- OK? IL

--    UttIAdv iadv = iadv ;
    UttNP np = {s = \\_ => np.s ! Nom} ;
--    UttVP vp = {s = infVP False vp (agrP3 Sg)} ;
    UttAdv adv = {s = \\_ => adv.s} ;
--
    NoPConj = {s = []} ;
--    PConjConj conj = conj ;
--
    NoVoc = {s = []} ;
--    VocNP np = {s = "،" ++ np.s ! Nom} ;
--
}
