
concrete SentenceMay of Sentence = CatMay ** open
  TenseX, ResMay, (AM=AdverbMay), Prelude in {

flags optimize=all_subs ;

lin

--2 Clauses

  -- : NP -> VP -> Cl
  PredVP = predVP ;

  -- : SC -> VP -> Cl ;         -- that she goes is good (Saeed p. 94)
  --PredSCVP sc vp = ;

--2 Clauses missing object noun phrases
  -- : NP -> VPSlash -> ClSlash ;
  SlashVP = predVPSlash ;

  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  -- AdvSlash cls adv = cls ** insertAdv adv cls ;

  -- : Cl -> Prep -> ClSlash ;         -- (with whom) he walks
  SlashPrep cl prep = cl ** {c2 = prep} ;

 {-
  -- : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves
  SlashVS np vs ss = {} ;


  -- : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
  UseSlash t p cls = {} ;

--2 Imperatives
  -- : VP -> Imp ;
  ImpVP vp = {s = \\num,pol => linVP (VImp num pol) Statement vp} ;

--2 Embedded sentences

  -- : S  -> SC ;
  EmbedS s = {s = s.s ! True} ; -- choose subordinate

  -- : QS -> SC ;
  -- EmbedQS qs = { } ;

  -- : VP -> SC ;
  EmbedVP vp = {s = infVP vp} ;
-}
--2 Sentences

  -- : Temp -> Pol -> Cl -> S ;
  UseCl t p cl = {
    s = cl.subj ++ t.s ++ p.s ++ cl.pred ! Active ! p.p;
    } ;

  -- : Temp -> Pol -> QCl -> QS ;
  UseQCl t p cl = {
    s = t.s ++ p.s ++ cl.subj ++ cl.pred ! Active ! p.p ;
    } ;

  -- : Temp -> Pol -> RCl -> RS ;
  UseRCl t p cl = {
    s = \\per => cl.subj ++ t.s ++ p.s ++ cl.pred ! per ! p.p ;
    } ;

  -- AdvS : Adv -> S  -> S ;            -- then I will go home
  AdvS = advS "" ;

  -- ExtAdvS : Adv -> S  -> S ;         -- next week, I will go home
  ExtAdvS = advS (SOFT_BIND ++ ",");

  -- : S -> Subj -> S -> S ;
  -- SSubjS s1 subj s2 = AdvS (AM.SubjS subj s2) s1 ;

  --  : S -> RS -> S ;              -- she sleeps, which is good
  -- RelS sent rs = advS {s = rs.s ! Sg3 Masc ++ SOFT_BIND ++ ","} sent ;

oper

  advS : (comma : Str) -> Adverb -> S -> S = \comma,a,sent -> sent ** {
    s = a.s ++ comma ++ sent.s
    } ;

}
