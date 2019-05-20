--# -path=.:../prelude:../abstract:../common

concrete SentenceCgg of Sentence = CatCgg **
  open Prelude, ResCgg in {  

lin 
  {-creating a sentence-}
  --UseCl temp pol cl = {
  --    s = temp.s ++ pol.s ++ cl.s !pol.p ! temp.t ! temp.a
  --  } ;
--2 Sentences
  --UseCl    : Temp -> Pol -> Cl  -> S ;   -- she had not slept
  UseCl  temp pol cl = let 
                subj = cl.s;
                clitic = mkSubjClitic cl.subjAgr;
                simul = cl.pres; --morphs ! VFPres; --this is not delivering the string
                ant = cl.morphs ! VFPastPart; --this is not delivering the string
                root = cl.root;
                presRestOfVerb = cl.morphs ! VFPres ! RestOfVerb;
                pastRestOfVerb = cl.perf; --morphs ! VFPastPart ! RestOfVerb;

                compl = cl.compl
                in 
  case <temp.a, pol.p> of {
      <Simul, Pos> => {s = subj ++ clitic ++ --Predef.BIND ++ 
              root ++ Predef.BIND ++ presRestOfVerb ++ compl};
      {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
      <Simul, Neg> => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ --Predef.BIND ++ 
                root ++ presRestOfVerb ++ compl};
      <Anter, Pos> => {s = subj ++ clitic ++ --Predef.BIND ++ 
                ant!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl};        
      <Anter, Neg> =>{s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ --Predef.BIND ++ 
                  ant!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
    };  --: Temp -> Pol -> QCl  -> QS ; -- has John walked

  UseQCl   = UseCl; -- : Temp -> Pol -> Cl   -> S ;  -- John has not walked
  QuestCl qcl = qcl;  --: Cl -> QCl ; -- does John (not) walk

  PredVP np vp = case vp.isCompApStem of{
              False    => {
                        s = np.s ! Nom;   --: NP -> VP -> Cl ;            -- John walks / John does not walk
                        subjAgr = np.agr;
                        pres = vp.pres;
                        perf = vp.perf;
                        root = vp.s;
                        morphs = vp.morphs;
                        {-
                        inf  = mkVerbInrf vp.root;
                      pres  = mkVerbPres vp.root; 
                      past  = mkVerbPast vp.root; 
                      presPart  = mkVerbPresPart vp.root; 
                      pastPart  = mkVerbPastPart vp.root;                              -- subject
                      -}
                      --root = vp.root ;
                        compl = vp.comp
                        };
              True    =>  {
                        s = np.s ! Nom;   --: NP -> VP -> Cl ;            -- John walks / John does not walk
                        subjAgr = np.agr;
                        pres = vp.pres;
                        perf = vp.perf;
                        root = vp.s;
                        morphs = vp.morphs;
                        {-
                        inf  = mkVerbInrf vp.root;
                      pres  = mkVerbPres vp.root; 
                      past  = mkVerbPast vp.root; 
                      presPart  = mkVerbPresPart vp.root; 
                      pastPart  = mkVerbPastPart vp.root;                              -- subject
                      -}
                      --root = vp.root ;
                        compl = mkSubjClitic np.agr ++ Predef.BIND ++ vp.comp --mkSubjClitic np.agr ++ Predef.BIND ++ vp.comp
                      }
        };--: NP -> VP -> Cl ; -- John walks / John does not walk
    

  
    {-
    Note: It seems mkSubjClitic comes with a Predef.BIND already
    prepared for the next token to bind.
    Reason: When I add a BIND command, I get two bind tokens in the linearizations
    -}

  ImpVP  vp = {
        s =table{
          ImpPos  => vp.s ++ Predef.BIND ++ vp.morphs!VFInf!RestOfVerb ++ vp.comp;
          ImpNeg =>  case vp.isCompApStem of {   -- How do I make the number dynamic use case?
                  True =>vp.morphs!VFPres!SecNegM ++ Predef.BIND ++ vp.s ++ Predef.BIND ++ 
                        vp.morphs!VFInf!RestOfVerb ++ (mkAdjPronNoIVClitic (AgMUBAP2 Sg)) ++ vp.ap;
                  False  => vp.morphs!VFPres!SecNegM ++ Predef.BIND ++ vp.s ++ Predef.BIND ++ 
                        vp.morphs!VFInf!RestOfVerb ++ vp.comp
              }
        } 
  };  --: VP -> Imp ;                 -- walk / do not walk

  -- A sentence can be modified by a relative clause referring to its contents.

    --RelS     : S -> RS -> S ;              -- she sleeps, which is good

--2 Clauses missing object noun phrases

-- This category is a variant of the 'slash category' $S/NP$ of
-- GPSG and categorial grammars, which in turn replaces
-- movement transformations in the formation of questions
-- and relative clauses. Except $SlashV2$, the construction 
-- rules can be seen as special cases of function composition, in
-- the style of CCG.
-- *Note* the set is not complete and lacks e.g. verbs with more than 2 places.

    --SlashVP   : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    SlashVP np vpslash ={
        s = np.s ! Nom;
        subjAgr     = np.agr;
        root        = vpslash.s;
        pres        = vpslash.pres;
        perf        = vpslash.perf;
        morphs      = vpslash.morphs;
        ap          = vpslash.ap;
        isRegular   = vpslash.isRegular;
        adv         = vpslash.adv;
        containsAdv = vpslash.containsAdv;
        adV         = vpslash.adV;
        containsAdV = vpslash.containsAdV
      };
    --SlashVP np vpslash =

    --AdvSlash  : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
    --SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
    --SlashVS   : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves





{-
--1 Sentence: Sentences, Clauses, and Imperatives

abstract Sentence = Cat ** {

--2 Clauses

-- The $NP VP$ predication rule form a clause whose linearization
-- gives a table of all tense variants, positive and negative.
-- Clauses are converted to $S$ (with fixed tense) with the
-- $UseCl$ function below.

  data
    PredVP    : NP -> VP -> Cl ;         -- John walks

-- Using an embedded sentence as a subject is treated separately.
-- This can be overgenerating. E.g. "whether you go" as subject
-- is only meaningful for some verb phrases.

    PredSCVP  : SC -> VP -> Cl ;         -- that she goes is good

--2 Clauses missing object noun phrases

-- This category is a variant of the 'slash category' $S/NP$ of
-- GPSG and categorial grammars, which in turn replaces
-- movement transformations in the formation of questions
-- and relative clauses. Except $SlashV2$, the construction 
-- rules can be seen as special cases of function composition, in
-- the style of CCG.
-- *Note* the set is not complete and lacks e.g. verbs with more than 2 places.

    SlashVP   : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    AdvSlash  : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
    SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
    SlashVS   : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves

--2 Imperatives

-- An imperative is straightforwardly formed from a verb phrase.
-- It has variation over positive and negative, singular and plural.
-- To fix these parameters, see [Phrase Phrase.html].

    ImpVP     : VP -> Imp ;              -- love yourselves

--2 Embedded sentences

-- Sentences, questions, and infinitival phrases can be used as
-- subjects and (adverbial) complements.

    EmbedS    : S  -> SC ;               -- that she goes
    EmbedQS   : QS -> SC ;               -- who goes
    EmbedVP   : VP -> SC ;               -- to go

--2 Sentences

-- These are the 2 x 4 x 4 = 16 forms generated by different
-- combinations of tense, polarity, and
-- anteriority, which are defined in [``Common`` Common.html].

  fun
    UseCl    : Temp -> Pol -> Cl  -> S ;   -- she had not slept
    UseQCl   : Temp -> Pol -> QCl -> QS ;  -- who had not slept
    UseRCl   : Temp -> Pol -> RCl -> RS ;  -- that had not slept
    UseSlash : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen

-- An adverb can be added to the beginning of a sentence, either with comma ("externally")
-- or without:

    AdvS     : Adv -> S  -> S ;            -- then I will go home
    ExtAdvS  : Adv -> S  -> S ;            -- next week, I will go home

-- This covers subjunctive clauses, but they can also be added to the end.

    SSubjS   : S -> Subj -> S -> S ;       -- I go home if she comes

-- A sentence can be modified by a relative clause referring to its contents.

    RelS     : S -> RS -> S ;              -- she sleeps, which is good

---- A sentence can also be post-modified by a subjunct sentence.

----    ModSubjS : S -> Subj -> S -> S ;       -- she sleeps, because she is old 
---- cf. Adverb.SubjS

--.

-- Examples for English $S$/$Cl$:

  Pres  Simul  Pos  ODir  : he sleeps
  Pres  Simul  Neg  ODir  : he doesn't sleep
  Pres  Anter  Pos  ODir  : he has slept
  Pres  Anter  Neg  ODir  : he hasn't slept
  Past  Simul  Pos  ODir  : he slept
  Past  Simul  Neg  ODir  : he didn't sleep
  Past  Anter  Pos  ODir  : he had slept
  Past  Anter  Neg  ODir  : he hadn't slept
  Fut   Simul  Pos  ODir  : he will sleep
  Fut   Simul  Neg  ODir  : he won't sleep
  Fut   Anter  Pos  ODir  : he will have slept
  Fut   Anter  Neg  ODir  : he won't have slept
  Cond  Simul  Pos  ODir  : he would sleep
  Cond  Simul  Neg  ODir  : he wouldn't sleep
  Cond  Anter  Pos  ODir  : he would have slept
  Cond  Anter  Neg  ODir  : he wouldn't have slept

-}

}
