concrete SentenceSlv of Sentence = CatSlv ** open Prelude, ResSlv in {

lin
    PredVP np vp = mkClause (np.s ! Nom) np.a np.isPron vp ;

    ImpVP vp = {
      s = \\p,g,n => vp.s ! p ! VImper2 n ++ vp.s2 ! {g=g; n=n; p=P2} ;
    } ;

    SlashVP np vp = mkClause (np.s ! Nom) np.a np.isPron vp ** {c2 = vp.c2} ;

    UseCl  t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p
    } ;
    UseQCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p
    } ;

    ExtAdvS a s = {s = a.s ++ bindComma ++ s.s} ;
    AdvS a s = {s = a.s ++ s.s} ;
    SSubjS a subj b = {s = a.s ++ subj.s ++ b.s} ;
    EmbedS s = s ;
    EmbedQS s = s ;
    PredSCVP sc vp = mkClause sc.s {g=Neut;n=Sg;p=P3} False vp ;
    AdvImp adv imp = {s = \\p,g,n => adv.s ++ imp.s ! p ! g ! n} ;
    UseRCl t p rcl = {s=\\a => t.s ++ p.s ++ rcl.s!a!t.t!t.a!p.p} ;
    AdvSlash cls adv = cls ** {s=\\t,a,p=>cls.s!t!a!p++adv.s} ;
    SlashPrep cl prep = cl ** {c2=prep} ;

}
