concrete PhraseAra of Phrase = CatAra ** open
  ParamX,
  Prelude,
  ResAra in {
  flags coding=utf8;

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ! Masc ++ voc.s} ;--FIXME

    UttQS qs = {s = \\g => qs.s ! QDir} ;
    UttImpSg pol imp = {s = \\g => imp.s ! pol.p ! g ! ResAra.Sg ++ pol.s} ;
    UttImpPl,UttImpPol = \pol,imp -> {s = \\g => imp.s ! pol.p ! g ! ResAra.Pl ++ pol.s} ;

    UttIP ip = {s = \\_g => ip.s ! False ! Def ! Nom} ; --IL
    UttAP ap = {s = ResAra.uttAP ap} ; --IL
    UttCard c = {s = ResAra.uttNum c} ; --IL

    UttCN cn = {s = ResAra.uttCN cn } ; --IL
    UttNP np = {s = \\_ => np.s ! Nom} ;
    UttVP vp = {s = \\g => (compVP vp).s ! {g=g ; n=Sg} ! Nom} ; --IL
    UttS,
    UttAdv,
    UttIAdv = \s -> {s = \\_ => s.s} ; ---- OK? AR
--
    NoPConj = {s = []} ;
--    PConjConj conj = conj ;
--
    NoVoc = {s = []} ;
--    VocNP np = {s = "،" ++ np.s ! Nom} ;
--
}
