concrete SentenceTel of Sentence = CatTel ** open Prelude, ResTel in {

  flags optimize=all_subs ;

  lin

    PredVP np vp = mkClause np vp ;

    PredSCVP sc vp = mkClause {s = \\_ => sc.s ; a = agrP3 Neutr Sg} vp ;

    ImpVP vp = {s = let f = vp.s ! Pos ! VPImp in
      vp.obj.s ++ vp.comp ! defaultAgr ++ f.neg ++ f.inf ++ f.fin} ;

    AdvImp adv imp = {s = adv.s ++ imp.s} ;

    AdvS a s = {s = a.s ++ s.s} ;

    UseCl temp pol cl = {
      s = temp.s ++ pol.s ++ cl.s ! tenseVPH temp.t temp.a ! pol.p
      } ;

    UseQCl temp pol cl = {
      s = temp.s ++ pol.s ++ cl.s ! tenseVPH temp.t temp.a ! pol.p
      } ;

    UseRCl temp pol cl = {
      s = cl.s ! tenseVPH temp.t temp.a ! pol.p
      } ;

    ExtAdvS adv sent = {s = adv.s ++ sent.s} ;
    SSubjS first subj second = {s = first.s ++ subj.s ++ second.s} ;
    EmbedS sent = {s = sent.s} ;
    EmbedQS sent = {s = sent.s} ;

    SlashVP np vps = {
      s = (mkClause np vps).s ;
      c2 = vps.c2
      } ;

    AdvSlash slash adv = slash ** {
      s = \\t,p => slash.s ! t ! p ++ adv.s
      } ;
}
