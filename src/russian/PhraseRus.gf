concrete PhraseRus of Phrase = CatRus ** open Prelude, ResRus in {

lin
  -- : S -> Utt ;                -- John walks
  UttS s = {s = s.s ! Ind} ;
  -- : QS -> Utt ;                -- is it good
  UttQS qs = {s = qs.s ! QDir} ;

  -- : Pol -> Imp -> Utt ;         -- (don't) love yourself
  UttImpSg pol imp = {s = imp.s ! pol.p ! GSg Masc} ;  -- NB: Neut used for another kind of imperative
  -- : Pol -> Imp -> Utt ;         -- (don't) love yourselves
  UttImpPl pol imp = {s = imp.s ! pol.p ! GPl} ;
  -- : Pol -> Imp -> Utt ;         -- (don't) sleep (polite)
  UttImpPol pol imp = {s = imp.s ! pol.p ! GPl} ;

  -- : IP -> Utt ;               -- who
  UttIP ip = {s = ip.nom} ; --- Acc also?

  -- : IAdv -> Utt ;               -- why
  UttIAdv iadv = iadv ;

  -- : NP -> Utt ;               -- this man
  UttNP np = {s = np.s ! Nom} ;

  -- : Adv -> Utt ;               -- here
  UttAdv adv = adv ;

  -- : VP -> Utt ;               -- to sleep
  UttVP vp
    = let a=Ag (GSg Neut) P3 in {
      s=vp.adv ! a ++ (verbInf vp.verb) ++ vp.dep ++ vp.compl ! Pos ! a
      } ;

  -- : CN -> Utt ;               -- house
  UttCN cn = {s = cn.s ! Sg ! Nom} ;
  -- : Card -> Utt ;               -- five
  UttCard card = {s=card.s ! Neut ! Inanimate ! Nom};
  -- : AP -> Utt ;               -- fine
  UttAP ap = {s = ap.s ! GSg Masc ! Animate ! Nom } ;
  -- : Interj -> Utt ;             -- alas
  UttInterj i = i ;

  -- : PConj -> Utt -> Voc -> Phr ; -- but come here, my friend
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  -- : PConj ;                     -- [plain phrase without conjunction in front]
  NoPConj = {s = []} ;
  -- : Conj -> PConj ;             -- and
  PConjConj conj = {s = conj.s2} ;

  -- : Voc ;                         -- [plain phrase without vocative]
  NoVoc = {s = []} ;
  -- : NP -> Voc ;                   -- my friend
  VocNP np = {s = "," ++ np.s ! VocRus } ;

}
