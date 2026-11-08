concrete SentenceKaz of Sentence = CatKaz ** open ResKaz, (P = ParamX) in {
  lin
    PredVP np vp = mkClause np vp ;
    PredSCVP sc vp = mkClause {s=\\_ => sc.s;a=defaultAgr} vp ;
    UseCl temp pol cl = {
      s=case <temp.t,temp.a> of {
          <P.Pres,P.Simul> => cl.pres ! pol.p;
          <P.Pres,P.Anter> => cl.anter ! pol.p;
          <P.Past,P.Simul> => cl.past ! pol.p;
          <P.Past,P.Anter> => cl.anter ! pol.p;
          <P.Fut,_> => cl.fut ! pol.p;
          <P.Cond,_> => cl.cond ! pol.p
        }
    } ;
    UseQCl temp pol cl = {
      s=case <temp.t,temp.a> of {
          <P.Pres,P.Simul> => cl.pres ! pol.p;
          <P.Pres,P.Anter> => cl.anter ! pol.p;
          <P.Past,P.Simul> => cl.past ! pol.p;
          <P.Past,P.Anter> => cl.anter ! pol.p;
          <P.Fut,_> => cl.fut ! pol.p;
          <P.Cond,_> => cl.cond ! pol.p
        }
    } ;
    UseRCl temp pol rcl = rcl ;
    UseSlash temp pol cl = {
      s=cl.pres ! pol.p
    } ;
    AdvS adv s = {
      s=adv.s ++ s.s
    } ;
    ExtAdvS adv s = {s=adv.s ++ "," ++ s.s} ;
    SSubjS a sub b = {
      s=a.s ++ b.s ++ sub.s
    } ;
    RelS s rs = {
      s=s.s ++ "," ++ rs.s
    } ;
    ImpVP vp = {s=vp.imperative} ;
    AdvImp adv imp = {
      s=\\pol,form,n => adv.s ++ imp.s ! pol ! form ! n
    } ;
    EmbedS s = s ;
    EmbedQS s = s ;
    SlashVP np vp = mkClause np vp ;
    AdvSlash cl adv = {
      pres=\\p => adv.s ++ cl.pres ! p;
      past=\\p => adv.s ++ cl.past ! p;
      fut=\\p => adv.s ++ cl.fut ! p;
      cond=\\p => adv.s ++ cl.cond ! p;
      anter=\\p => adv.s ++ cl.anter ! p
    } ;
    SlashPrep cl prep = cl ;
    SlashVS np vs cl = mkClause np (prefixVerb cl.s vs) ;
}
