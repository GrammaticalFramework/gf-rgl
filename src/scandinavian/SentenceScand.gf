incomplete concrete SentenceScand of Sentence = 
  CatScand ** open CommonScand, ResScand, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin
    PredVP np vp = mkClause (np.s ! nominative) np.a vp ;

    PredSCVP sc vp = mkClause sc.s (agrP3 Neutr Sg) vp ;

    ImpVP vp = {
      s = \\pol,n => 
        let 
          agr   = {g = Utr ; n = n ; p = P2} ;
          verb  = vp.s ! Act ! VPImperat ;
	  neg   = verb.a1 ! pol ! agr ;
	  pron  = vp.n1 ! agr
        in
        verb.fin ++ neg.p1 ++ verb.inf ++ pron ++ neg.p2 ++ vp.n2 ! agr ++ vp.a2 ++ vp.ext
    } ;

    AdvImp adv imp = {
      s = \\p,n => adv.s ++ imp.s ! p ! n
    } ;

    SlashVP np vp = 
      mkClause 
        (np.s ! nominative) np.a 
        vp **
      {n3 = vp.n3 ; c2 = vp.c2} ;

    AdvSlash slash adv = {
      s  = \\t,a,b,o => slash.s ! t ! a ! b ! o ++ adv.s ;
      n3 = slash.n3 ;
      c2 = slash.c2
    } ;

    SlashPrep cl prep = cl ** {n3 = \\_ => [] ; c2 = {s = prep.s ; hasPrep = True}} ;

    SlashVS np vs slash = 
      mkClause
        (np.s ! nominative) np.a 
        (insertExt (conjThat ++ slash.s ! Sub) (predV vs)) **
      {n3 = slash.n3 ; c2 = slash.c2} ;

    EmbedS  s  = {s = conjThat ++ s.s ! Sub} ;
    EmbedQS qs = {s = qs.s ! QIndir} ;
    EmbedVP vp = {s = infMark ++ infVP vp (agrP3 Utr Sg)} ; --- agr

    UseCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! o
    } ;
    UseQCl t p cl = {
      s = \\q => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! q
    } ;
    UseRCl t p cl = {
      s = \\r,rc => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! r ! rc ;
      c = cl.c
    } ;
    UseSlash t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! o ;
      n3 = cl.n3 ;
      c2 = cl.c2
    } ;

    AdvS a s = {s = \\o => a.s ++ s.s ! Inv} ;
    ExtAdvS a s = {s = \\o => a.s ++ comma ++ s.s ! Inv} ;

    RelS s r = {s = \\o => s.s ! o ++ comma ++ r.s ! agrP3 Neutr Sg ! RPrep True } ; --- vilket

    SSubjS a s b = {s = \\o => a.s ! o ++ comma ++ s.s ++ b.s ! Sub} ;

}
