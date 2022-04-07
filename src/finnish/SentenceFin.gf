concrete SentenceFin of Sentence = CatFin ** open Prelude, ResFin, StemFin in {

  flags optimize=all_subs ;
    coding=utf8 ;

  lin

    PredVP np vp = mkClausePol (orB np.isNeg vp.vptyp.isNeg) (subjForm np vp.s.sc) np.a vp ;

    PredSCVP sc vp = mkClause (\_ -> sc.s) (agrP3 Sg) vp ;

    ImpVP vp0 = let vp = vp2old_vp vp0 in {
      s = \\pol,agr =>
        let
          verb  = vp.s ! VIImper ! Simul ! pol ! agr ;
          compl = vp.s2 ! False ! pol ! agr ++ vp.adv ! pol ++ vp.ext  --- False = like inf (osta auto)
        in
        verb.fin ++ verb.inf ++ compl ;
    } ;

-- The object case is formed at the use site of $c2$, in $Relative$ and $Question$.

    SlashVP np vp = {
      s = \\t,a,p => (mkClause (subjForm np vp.s.sc) np.a vp).s ! t ! a ! p ! SDecl ;
      c2 = vp.c2
      } ;

    AdvSlash slash adv = {
      s  = \\t,a,b => slash.s ! t ! a ! b ++ adv.s ;
      c2 = slash.c2
      } ;

    SlashPrep cl prep = {
      s = \\t,a,p => cl.s ! t ! a ! p ! SDecl ;
      c2 = prep
      } ;

    SlashVS np vs slash = {
      s = \\t,a,p =>
        (mkClause (subjForm np vs.sc) np.a
          (insertExtrapos ("että" ++ slash.s)
             (predSV vs))
        ).s ! t ! a ! p ! SDecl ;
      c2 = slash.c2
      } ;


    EmbedS  s  = {s = etta_Conj ++ s.s} ;
    EmbedQS qs = {s = qs.s} ;
    EmbedVP vp = {s = infVP SCNom Pos (agrP3 Sg) vp Inf1} ; --- case,pol,agr,infform

    UseCl  t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! SDecl} ;
    UseQCl t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p} ;
    UseRCl t p cl = {
      s = \\r => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! r ;
      c = cl.c
      } ;
    UseSlash t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ;
      c2 = cl.c2
    } ;

    AdvS a s = {s = a.s ++ s.s} ;
    ExtAdvS a s = {s = a.s ++ SOFT_BIND ++ "," ++ s.s} ;

    RelS s r = {s = s.s ++ SOFT_BIND ++ "," ++ r.s ! agrP3 Sg} ; ---- mikä

    SSubjS a subj b = {s = a.s ++ SOFT_BIND ++ "," ++ subj.s ++ b.s} ;

}
