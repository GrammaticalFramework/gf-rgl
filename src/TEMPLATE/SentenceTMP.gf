
concrete SentenceTMP of Sentence = CatTMP ** open
  TenseX, ResTMP, (AM=AdverbTMP), Prelude in {

flags optimize=all_subs ;

lin

--2 Clauses

  -- : NP -> VP -> Cl
  PredVP np vp = {
    subj = np.s ; -- ! Nom, if there are cases
    pred =
     -- table {something with tense+polarity =>
            vp.s ! TODOVF np.n np.p
            -- TODO: all of the VP's tense and polarity should be open here!
            -- PredVP only decides the subject.
     -- }
  } ;

{-
  -- : SC -> VP -> Cl ;         -- that she goes is good
  PredSCVP sc vp = ;

--2 Clauses missing object noun phrases
  -- : NP -> VPSlash -> ClSlash ;
  SlashVP =

  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  AdvSlash cls adv =

  -- : Cl -> Prep -> ClSlash ;         -- (with whom) he walks
  SlashPrep cl prep = cl ** {c2 = prep} ;

-- Imperatives
  -- : VP -> Imp ;
  ImpVP vp =

--2 Embedded sentences

  -- : S  -> SC ;
  EmbedS s =

  -- : QS -> SC ;
  EmbedQS qs =

  -- : VP -> SC ;
  EmbedVP vp =
-}
--2 Sentences

  -- : Temp -> Pol -> Cl -> S ;
  UseCl t p cl = {
    s = cl.subj ++ t.s ++ p.s ++ cl.pred --  ! t.t ! p.p  -- eventually
    } ;
{-
  -- : Temp -> Pol -> QCl -> QS ;
  UseQCl t p cl =

  -- : Temp -> Pol -> RCl -> RS ;
  UseRCl t p cl =

  -- AdvS : Adv -> S  -> S ;            -- then I will go home
  AdvS adv s =

  -- ExtAdvS : Adv -> S  -> S ;         -- next week, I will go home
  ExtAdvS adv s =

  -- : S -> Subj -> S -> S ;
  SSubjS s1 subj s2 =

  --  : S -> RS -> S ;              -- she sleeps, which is good
  RelS sent rs =
-}
}
