concrete SentenceSwa of Sentence = CatSwa ** open Prelude, ResSwa in {
  flags optimize=all_subs ; coding=utf8 ;
  lin
    PredVP np vp = mkClause (np.s ! npNom) (agrFeatures np.a) vp ;
    PredSCVP sc vp = mkClause sc.s (agre G4 Sg P3) vp ;
    SlashVP np vp = mkClause (np.s ! npNom) (agrFeatures np.a) vp ** {c2 = vp.c2} ;
    AdvSlash cl adv = cl ** {s = \\t,a,b => cl.s ! t ! a ! b ++ adv.s} ;
    SlashPrep cl prep = cl ** {c2 = prep.s ! Sg ! G1} ;
    SlashVS np vs ss = mkClause (np.s ! npNom) (agrFeatures np.a)
      (insertObj (\\_ => "kwamba" ++ ss.s ++ ss.c2) (predV vs)) ** {c2 = ss.c2} ;
    ImpVP vp = {s = \\pos,n => vp.imp ! pos ! n ++ vp.obj ! agre G1 n P2} ;
    AdvImp adv imp = {s = \\pos,n => adv.s ++ imp.s ! pos ! n} ;
    EmbedS s = {s = "kwamba" ++ s.s} ;
    EmbedQS q = {s = q.s ! QIndir} ;
    EmbedVP vp = {s = infVP Pos (agre G1 Sg P3) vp} ;
    UseCl t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p} ;
    UseQCl t p q = {s = \\f => t.s ++ p.s ++ q.s ! t.t ! t.a ! p.p ! f} ;
    UseRCl t p r = {s = \\a => t.s ++ p.s ++ r.s ! t.t ! t.a ! p.p ! a ; c = r.c} ;
    UseSlash t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ; c2 = cl.c2} ;
    AdvS adv s = {s = adv.s ++ s.s} ;
    ExtAdvS adv s = {s = adv.s ++ frontComma ++ s.s} ;
    SSubjS a sub b = {s = a.s ++ sub.s ++ b.s} ;
    RelS s r = {s = s.s ++ frontComma ++ r.s ! Ag G4 Sg P3} ;
}
