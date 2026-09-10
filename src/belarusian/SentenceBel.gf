concrete SentenceBel of Sentence = CatBel ** open ResBel, (R = ParamX), Prelude in {

lin
  PredVP np vp = {
    s = \\t,p => np.s ! Nom ++ vp.s ! t ! p ! np.a
  } ;
  PredSCVP sc vp = {
    s = \\t,p => sc.s ++ vp.s ! t ! p ! defaultAgr
  } ;

  SlashVP np vp = {
    s = \\t,p => np.s ! Nom ++ vp.s ! t ! p ! np.a ++ vp.post ;
    c = vp.c
  } ;
  AdvSlash cl adv = {
    s = \\t,p => cl.s ! t ! p ++ adv.s ;
    c = cl.c
  } ;
  SlashPrep cl prep = {
    s = \\t,p => cl.s ! t ! p ++ prep.s ;
    c = prep
  } ;
  SlashVS np vs ss = {
    s = \\t,p => np.s ! Nom ++ finiteVerb vs t p np.a ++ ss.s ;
    c = ss.c
  } ;

  ImpVP vp = {s = \\p,n => vp.imp ! p ! n} ;
  AdvImp adv imp = {s = \\p,n => adv.s ++ imp.s ! p ! n} ;

  EmbedS s = {s = "што" ++ s.s} ;
  EmbedQS qs = {s = qs.s} ;
  EmbedVP vp = {s = vp.inf} ;

  UseCl temp pol cl = {s = temp.s ++ pol.s ++ cl.s ! temp.t ! pol.p} ;
  UseQCl temp pol cl = {s = temp.s ++ pol.s ++ cl.s ! temp.t ! pol.p} ;
  UseRCl temp pol cl = {s = cl.s ! temp.t ! pol.p} ;
  UseSlash temp pol cl = {s = cl.s ! temp.t ! pol.p; c = cl.c} ;

  AdvS adv s = {s = adv.s ++ s.s} ;
  ExtAdvS adv s = {s = adv.s ++ s.s} ;
  SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;
  RelS s rs = {s = s.s ++ rs.s} ;

}
