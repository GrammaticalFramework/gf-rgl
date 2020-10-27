concrete SentenceKor of Sentence = CatKor ** open
  TenseX, ResKor, (AK=AdverbKor), Prelude in {

flags optimize=all_subs ;

lin

--2 Clauses

  -- : NP -> VP -> Cl
  PredVP = predVP ;

  -- : SC -> VP -> Cl ;         -- that she goes is good (Saeed p. 94)
  PredSCVP sc vp = predVP' sc.s vp ;

--2 Clauses missing object noun phrases
  -- : NP -> VPSlash -> ClSlash ;
  -- SlashVP = predVP ;
{-
  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  AdvSlash cls adv = cls ** insertAdv adv cls ;

--    SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks

  -- : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves
--  SlashVS np vs ss = {} ;


  --  : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
  UseSlash t p cls = {
    } ;

--2 Imperatives
  -- : VP -> Imp ;
  ImpVP vp = {s = \\num,pol => linVP (VImp num pol) Statement vp} ;

--2 Embedded sentences
-}
  -- : S  -> SC ;
  EmbedS s = {s = s.s ! Subord ++ "것이"} ; -- TODO check subject case

  -- : QS -> SC ;
  -- EmbedQS qs = { } ;

  -- : VP -> SC ;
  EmbedVP vp = {s = linVP (VAttr Pos) vp ++ "것이"} ;

--2 Sentences

  -- : Temp -> Pol -> Cl -> S ;
  UseCl t p cl = {
    s = \\c => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! c ;
    p = case p.p of { -- Phono of VStem
          Pos => cl.p ;
          Neg => cl.pNeg } ;
    } ;

  -- : Temp -> Pol -> QCl -> QS ;
  UseQCl t p cl = {
    s = \\st => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! Statement st} ;

  -- : Temp -> Pol -> RCl -> RS ;
  UseRCl t p rcl = {
    s = \\c => t.s ++ p.s ++ rcl.s ! t.t ! t.a ! p.p ! c ;
    p = case p.p of { -- Phono of VStem
          Pos => rcl.p ;
          Neg => rcl.pNeg } ;
    } ;

  -- AdvS : Adv -> S  -> S ;            -- then I will go home
  AdvS = advS "" ;

  -- ExtAdvS : Adv -> S  -> S ;         -- next week, I will go home
  ExtAdvS = advS (SOFT_BIND ++ ",");

  -- : S -> Subj -> S -> S ;
  -- SSubjS s1 subj s2 = AdvS (AK.SubjS subj s2) s1 ;

  --  : S -> RS -> S ;              -- she sleeps, which is good
  -- RelS sent rs = advS {s = rs.s ! Sg3 Masc ++ SOFT_BIND ++ ","} sent ;

oper

  advS : (comma : Str) -> Adverb -> S -> S = \comma,a,sent -> sent ** {
    s = \\c => a.s ++ comma ++ sent.s ! c
    } ;

}
