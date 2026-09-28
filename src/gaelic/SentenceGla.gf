
concrete SentenceGla of Sentence = CatGla ** open
  TenseGla, ResGla, (AM=AdverbGla), Prelude in {

-- Keep tense, anteriority, and polarity as live parameters.  Substitution
-- optimization can otherwise specialize them away in this concrete syntax.
flags optimize=noexpand ;

lin

--2 Clauses

  -- : NP -> VP -> Cl
  PredVP np vp = {
    subj = linNP np ; -- article and CN are discontinuous in NP! linNP just picks nominative unmutated.
    n = agrNumber np.a ;
    pred = vp
  } ;

  -- : SC -> VP -> Cl ;         -- that she goes is good
  PredSCVP sc vp = {subj = sc.s ; n = Sg ; pred = vp} ;

--2 Clauses missing object noun phrases
  -- : NP -> VPSlash -> ClSlash ;
  SlashVP np vp = {subj = linNP np ; n = agrNumber np.a ; pred = vp ; c2 = vp.c2} ;

  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  AdvSlash cls adv = cls ** {pred = appendVP cls.pred adv.s} ;

  -- : Cl -> Prep -> ClSlash ;         -- (with whom) he walks
  SlashPrep cl prep = cl ** {c2 = prep} ;

-- Imperatives
  -- : VP -> Imp ;
  -- The generated morphology still contains gaps in a number of imperative
  -- cells.  The second-person singular is the dictionary stem in Gaelic, so
  -- using the stem here is both correct and total.
  ImpVP vp = {s = vp.s} ;

--2 Embedded sentences

  -- : S  -> SC ;
  EmbedS s = s ;

  -- : QS -> SC ;
  EmbedQS qs = qs ;

  -- : VP -> SC ;
  EmbedVP vp = {s = "a" ++ vp.noun} ;
--2 Sentences

  -- : Temp -> Pol -> Cl -> S ;
  UseCl t p cl = {
    s = case <t.t,t.a,p.p> of {
      <GPres,GSimul,GPos> => case cl.pred.copular of {
        True => "tha" ++ cl.subj ++ cl.pred.complement ;
        False => "tha" ++ cl.subj ++ AG ++ cl.pred.noun
        } ;
      <GPres,GAnter,GPos> => "tha" ++ cl.subj ++ cl.pred.participle ;
      -- The productive do-periphrasis keeps the finite verb first and the
      -- subject before the verbal noun and all of its complements.
      <GPast,GSimul,GPos> => case cl.pred.copular of {
        True => "bha" ++ cl.subj ++ cl.pred.complement ;
        False => "rinn" ++ cl.subj ++ cl.pred.noun
        } ;
      <GPast,GAnter,GPos> => "bha" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GPos> => case cl.pred.copular of {
        True => "bidh" ++ cl.subj ++ cl.pred.complement ;
        False => "bidh" ++ cl.subj ++ AG ++ cl.pred.noun
        } ;
      <GCond,_,GPos> => case cl.pred.copular of {
        True => "bhiodh" ++ cl.subj ++ cl.pred.complement ;
        False => "bhiodh" ++ cl.subj ++ AG ++ cl.pred.noun
        } ;
      <GPres,_,GNeg> => case cl.pred.copular of {
        True => "chan eil" ++ cl.subj ++ cl.pred.complement ;
        False => "chan eil" ++ cl.subj ++ AG ++ cl.pred.noun
        } ;
      <GPast,_,GNeg> => case cl.pred.copular of {
        True => "cha robh" ++ cl.subj ++ cl.pred.complement ;
        False => "cha robh" ++ cl.subj ++ AG ++ cl.pred.noun
        } ;
      <GFut,_,GNeg> => case cl.pred.copular of {
        True => "cha bhi" ++ cl.subj ++ cl.pred.complement ;
        False => "cha bhi" ++ cl.subj ++ AG ++ cl.pred.noun
        } ;
      <GCond,_,GNeg> => case cl.pred.copular of {
        True => "cha bhiodh" ++ cl.subj ++ cl.pred.complement ;
        False => "cha bhiodh" ++ cl.subj ++ AG ++ cl.pred.noun
        }
      }
    } ;
  -- : Temp -> Pol -> QCl -> QS ;
  UseQCl t p cl = {s = cl.s ! t.t ! t.a ! p.p} ;

  -- : Temp -> Pol -> RCl -> RS ;
  UseRCl t p cl = {s = cl.s ! t.t ! t.a ! p.p} ;

  -- AdvS : Adv -> S  -> S ;            -- then I will go home
  AdvS adv s = {s = adv.s ++ s.s} ;

  -- ExtAdvS : Adv -> S  -> S ;         -- next week, I will go home
  ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;

  -- : S -> Subj -> S -> S ;
  SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;

  --  : S -> RS -> S ;              -- she sleeps, which is good
  RelS sent rs = {s = sent.s ++ "," ++ rs.s} ;

  UseSlash t p cls = {s = (UseCl t p cls).s} ;
  SlashVS np vs ss = {subj = linNP np ; n = agrNumber np.a ; pred = appendVP vs ss.s ; c2 = emptyPrep} ;
  AdvImp adv imp = {s = adv.s ++ imp.s} ;
}
