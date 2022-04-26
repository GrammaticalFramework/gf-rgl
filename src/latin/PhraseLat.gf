concrete PhraseLat of Phrase = CatLat ** open Prelude, ResLat in {
  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;
    --
-- UttS : S -> Utt
    UttS s = { s = defaultSentence s ! SOV };

--  UttQS : QS -> Utt
    UttQS qs = {s = qs.s ! QDir } ;
--  UttImpSg : Pol -> Imp -> Utt
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! VImp1 Sg } ;
--  UttImpPl : Pol -> Imp -> Utt 
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! VImp1 Pl } ;
--  UttImpPol : Pol -> Imp -> Utt
    UttImpPol pol imp = UttImpSg pol imp ;
--  UttIP : IP -> Utt
    UttIP ip = {s = ip.s ! Nom} ; --- Acc also
--  UttIAdv : IAdv -> Utt
    UttIAdv iadv = iadv ;
--  UttNP : NP -> Utt
    UttNP np = {s = np.adv ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! Nom} ;
--  UttVP : VP -> Utt
    UttVP vp = ss (vp.inf ! VInfActPres) ;

--  UttAdv : Adv -> Utt
    UttAdv adv = ss (adv.s ! Posit) ;
--  UttAP : AP -> Utt
    UttAP ap = ss (ap.s ! Ag Masc Sg Nom );
--  UttCard : Card -> Utt
    UttCard card = ss (card.s ! Masc ! Nom);
--  UttCN : CN -> Utt
    UttCN cn = ss (cn.s ! Sg ! Nom) ;
--  UttInterj : Interj -> Utt
    UttInterj interj = interj ;
    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ; ---
--
    NoVoc = {s = []} ;
    VocNP np = {s = bindComma ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! ResLat.Voc} ; ---- what is the compiler error here? AR 1/2/2014 -- answer: clash with the type name Voc 3/2
--
}
