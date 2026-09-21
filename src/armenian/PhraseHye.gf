concrete PhraseHye of Phrase = CatHye ** open ResHye in {
lin
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  UttS s = s ;
  UttInterj i = i ;
  UttNP np = {s = np.s ! Nom} ;
  UttAdv adv = adv ;
  UttCN cn = {s = cn.s ! Indef ! Nom ! Sg} ;
  UttCard card = card ;
  UttIAdv adv = adv ;
  UttIP ip = ip ;
  UttQS qs = qs ;
  UttImpSg pol imp = {s = case pol.p of {Pos => imp.s; Neg => "մի" ++ imp.s}} ;
  UttImpPl pol imp = {s = case pol.p of {Pos => imp.s; Neg => "մի" ++ imp.s}} ;
  UttImpPol pol imp = {s = case pol.p of {Pos => imp.s; Neg => "մի" ++ imp.s}} ;
  VocNP np = {s = np.s ! Nom} ;
  PConjConj conj = conj ;

  NoPConj = {s = []} ;

  NoVoc = {s = []} ;
}
