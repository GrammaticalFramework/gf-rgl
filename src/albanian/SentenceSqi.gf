concrete SentenceSqi of Sentence = CatSqi ** open Prelude, ParamX, ResSqi in {

oper
  mkClause : Str -> Agr -> VP -> ParamX.Tense -> Anteriority -> Polarity -> Str =
    \subj,a,vp,t,ant,pol -> case <ant,t> of {
      <Simul,ParamX.Past> => subj ++ negation pol ++
               haveAux ! ParamX.Pres ! agrNumber a ! a.p ++ vp.participle ! a ! Nom ;
      <Simul,_> => subj ++ negation pol ++ futureParticle t ++
               vp.indicative ! sqiTense t ! agrNumber a ! a.p ! agrGender a ! Nom ;
      <Anter,_> => subj ++ negation pol ++ haveAux ! t ! agrNumber a ! a.p ++
               vp.participle ! a ! Nom
    } ;

lin
  PredVP np vp = {s = \\t,a,p => mkClause (np.s ! Nom) np.a vp t a p} ;
  PredSCVP sc vp = {s = \\t,a,p => mkClause sc.s {gn=GSg Masc; p=P3} vp t a p} ;

  SlashVP np sl = {
    s = \\t,a,p => np.s ! Nom ++ negation p ++ futureParticle t ++
          sl.indicative ! sqiTense t ! agrNumber np.a ! np.a.p ;
    c2 = sl.c2
  } ;
  AdvSlash cl adv = cl ** {s = \\t,a,p => cl.s ! t ! a ! p ++ adv.s} ;
  SlashPrep cl prep = {s = cl.s; c2 = prep} ;
  SlashVS np vs ss = {
    s = \\t,a,p => np.s ! Nom ++ negation p ++ vs.indicative ! sqiTense t ! agrNumber np.a ! np.a.p ++ "që" ++ ss.s ;
    c2 = {s=[]; c=Acc}
  } ;

  ImpVP vp = {s = \\pol,n => case pol of {Pos => []; Neg => "mos"} ++ vp.imperative ! n ! Nom} ;
  AdvImp adv imp = {s = \\p,n => adv.s ++ imp.s ! p ! n} ;

  EmbedS s = {s = "që" ++ s.s} ;
  EmbedQS q = {s = q.s} ;
  EmbedVP vp = {s = "të" ++ vp.subjunctive ! Sg ! P3 ! Masc ! Nom} ;

  UseCl t p cl = {s = cl.s ! t.t ! t.a ! p.p} ;
  UseQCl t p cl = {s = cl.s ! t.t ! t.a ! p.p} ;
  UseRCl t p cl = {s = \\a => cl.s ! a ! t.t ! t.a ! p.p} ;
  UseSlash t p cl = {s = cl.s ! t.t ! t.a ! p.p; c2=cl.c2} ;

  AdvS adv s = {s = adv.s ++ s.s} ;
  ExtAdvS adv s = {s = adv.s ++ SOFT_BIND ++ "," ++ s.s} ;
  SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;
  RelS s rs = {s = s.s ++ SOFT_BIND ++ "," ++ rs.s ! {gn=GSg Masc;p=P3}} ;
}
