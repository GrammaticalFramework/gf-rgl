concrete AdverbTel of Adverb = CatTel ** open ResTel, Prelude in {
  lin
    PositAdvAdj a = {s = a.s ! Masc ! Sg ! Dir} ;
    PositAdAAdj a = {s = a.s ! Masc ! Sg ! Dir} ;
--    ComparAdvAdj cadv a np = {
--      s = cadv.s ++ a.s ! AAdv ++ "than" ++ np.s ! Nom
--      } ;
--    ComparAdvAdjS cadv a s = {
--      s = cadv.s ++ a.s ! AAdv ++ "than" ++ s.s
--      } ;
--
    PrepNP prep np = {s = np.s ! NPC Obl ++ prep.s} ;
--
    AdAdv ada adv = {s = ada.s ++ adv.s} ;
--
    SubjS subj sent = {s = sent.s ++ subj.s} ;
--
--    AdnCAdv cadv = {s = cadv.s ++ "than"} ;
--
}
