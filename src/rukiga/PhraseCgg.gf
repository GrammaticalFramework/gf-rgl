--# -path=.:../prelude:../abstract:../common

concrete PhraseCgg of Phrase = CatCgg ** open Prelude, ResCgg in {

lin

  UttS sent = sent ;
  UttQS     qs = qs ; --: QS -> Utt ;-- does John walk
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  NoPConj = {s = ""} ;
  NoVoc = {s = ""} ;

  -- Utterances are formed from sentences, questions, and imperatives.
  UttNP     np = {s= np.s!Acc}; --: NP -> Utt ;
  UttAdv   adv = {s = adv.s}; --: Adv -> Utt ;        -- in the house
  UttImpSg  pol imp = {s = 
    case pol.p of {
        Pos  => imp.s!ImpPos;
        Neg => (mkSubjClitic (AgMUBAP2 Sg)) ++ imp.s!ImpNeg        
      }
    };--: Pol -> Imp -> Utt ; -- (do not) walk ----s
  --UttImpPl  : Pol -> Imp -> Utt ;         -- (don't) love yourselves
  UttImpPl pol imp ={s = 
    case pol.p of {
        Pos  => imp.s!ImpPos;
        Neg => (mkSubjClitic (AgMUBAP2 Pl)) ++ imp.s!ImpNeg        
      }
    };
{-
--1 Phrase: Phrases and Utterances

abstract Phrase = Cat ** {

-- When a phrase is built from an utterance it can be prefixed
-- with a phrasal conjunction (such as "but", "therefore")
-- and suffixing with a vocative (typically a noun phrase).

  fun
    PhrUtt   : PConj -> Utt -> Voc -> Phr ; -- but come here, my friend

-- Utterances are formed from sentences, questions, and imperatives.

    UttS      : S   -> Utt ;                -- John walks
    UttQS     : QS  -> Utt ;                -- is it good
    UttImpSg  : Pol -> Imp -> Utt ;         -- (don't) love yourself
    UttImpPl  : Pol -> Imp -> Utt ;         -- (don't) love yourselves
    UttImpPol : Pol -> Imp -> Utt ;         -- (don't) sleep (polite)

-- There are also 'one-word utterances'. A typical use of them is
-- as answers to questions.
-- *Note*. This list is incomplete. More categories could be covered.
-- Moreover, in many languages e.g. noun phrases in different cases
-- can be used.

    UttIP     : IP   -> Utt ;               -- who
    UttIAdv   : IAdv -> Utt ;               -- why
    UttNP     : NP   -> Utt ;               -- this man
    UttAdv    : Adv  -> Utt ;               -- here
    UttVP     : VP   -> Utt ;               -- to sleep
    UttCN     : CN   -> Utt ;               -- house
    UttCard   : Card -> Utt ;               -- five
    UttAP     : AP   -> Utt ;               -- fine
    UttInterj : Interj -> Utt ;             -- alas

-- The phrasal conjunction is optional. A sentence conjunction
-- can also be used to prefix an utterance.

    NoPConj   : PConj ;                     -- [plain phrase without conjunction in front]
    PConjConj : Conj -> PConj ;             -- and

-- The vocative is optional. Any noun phrase can be made into vocative,
-- which may be overgenerating (e.g. "I").

    NoVoc   : Voc ;                         -- [plain phrase without vocative]
    VocNP   : NP -> Voc ;                   -- my friend

-}

}
