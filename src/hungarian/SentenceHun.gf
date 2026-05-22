concrete SentenceHun of Sentence = CatHun ** open
  TenseX, ResHun, (AK=AdverbHun), Prelude in {

flags optimize=all_subs ;

lin

--2 Clauses

  -- : NP -> VP -> Cl
  PredVP = predVP ;

  -- : SC -> VP -> Cl ;         -- that she goes is good (Saeed p. 94)
  PredSCVP sc vp = predVP (indeclNP sc.s) vp ;

--2 Clauses missing object noun phrases
  -- : NP -> VPSlash -> ClSlash ;
  SlashVP np vps = predVP np (vps ** {s = vps.s ! Indef ; obj = []}) ;

  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  AdvSlash cls adv = cls ** {
    s = \\t,a,p => cls.s ! t ! a ! p ++ adv.s
    } ;

  -- : Cl -> Prep -> ClSlash ;         -- (with whom) he walks
  SlashPrep cl prep = cl ** {
    c2 = prep.c ;
    s = \\t,a,p => cl.s ! t ! a ! p ++ prep.s
    } ;

{-

  -- : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves
--  SlashVS np vs ss = {} ;


  --  : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
  UseSlash t p cls = {
    } ;

--2 Imperatives
-}
  -- : VP -> Imp ;
  ImpVP vp = {
    s = \\num,pol =>
      if_then_Pol pol [] "ne"
      ++ vp.s ! VPres P2 num
      ++ vp.obj
      ++ vp.adv
    } ;

  -- : Adv -> Imp -> Imp ;
  AdvImp adv imp = {
    s = \\num,pol => adv.s ++ imp.s ! num ! pol
    } ;
--2 Sentences

  -- : S  -> SC ;
  EmbedS s = {s = s.s} ;

  -- : QS -> SC ;
  EmbedQS qs = {s = qs.s} ;

  -- : Temp -> Pol -> Cl -> S ;
  UseCl t p cl = {
    s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p
    } ;

  -- : Temp -> Pol -> QCl -> QS ;
  UseQCl t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p} ;

  -- : Temp -> Pol -> RCl -> RS ;
  UseRCl t p cl = {s = \\g,n,c => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! g ! n ! c} ;

  -- AdvS : Adv -> S  -> S ;            -- then I will go home
  AdvS = advS "" ;

  -- ExtAdvS : Adv -> S  -> S ;         -- next week, I will go home
  ExtAdvS = advS (SOFT_BIND ++ ",");

  -- : S -> Subj -> S -> S ;
  SSubjS s1 subj s2 = {
    s = s1.s ++ bindComma ++ subj.s ++ s2.s
    } ;

  --  : S -> RS -> S ;              -- she sleeps, which is good
  -- RelS sent rs = advS {s = rs.s ! Sg3 Masc ++ SOFT_BIND ++ ","} sent ;

oper

  advS : (comma : Str) -> SS -> S -> S = \comma,a,sent -> sent ** {
    s = a.s ++ comma ++ sent.s
    } ;

}
