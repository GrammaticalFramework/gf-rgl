concrete SentenceMkd of Sentence = CatMkd ** open Prelude,ResMkd in {
  lin AdvImp a i = {s = \\p,gn => a.s ++ i.s ! p ! gn} ;
  lin AdvS a s = {s = a.s ++ s.s} ;
  lin AdvSlash c a = {s = c.s ++ a.s} ;
  lin EmbedQS qs = {s = qs.s} ;
  lin EmbedS s = {s = s.s} ;
  lin EmbedVP vp = {s = "да" ++ vp.present ! Perfective ! Sg ! P3 ++ vp.compl ! {g=GSg Masc; p=P3}} ;
  lin ExtAdvS a s = {s = a.s ++ s.s} ;
  lin ImpVP vp = {
         s = \\p,gn=>case p of {
                       Pos => vp.imperative ! Perfective ! genNum2num gn ;
                       Neg => "не" ++ vp.imperative ! Imperfective ! genNum2num gn
                     } ++
                     vp.compl ! {g=gn; p=P2}
      } ;
  lin PredSCVP sc vp = {s = \\t,a,p,o => sc.s
                                           ++ vp.present ! Imperfective ! Sg ! P1} ;
  lin PredVP np vp = {s = mkClause (np.s ! RSubj) np.a vp} ;
  lin RelS s rs = {s = s.s ++ rs.s ! GSg Masc} ;
  lin SSubjS s s2 s3 = {s = s.s ++ s2.s ++ s3.s} ;
  lin SlashPrep cl p = {s = cl.s ! Main ! VPresent ! Simul ! Pos
                              ++ p.s} ;
  lin SlashVP np v = {s = np.s ! RSubj
                            ++ v.present ! Imperfective ! Sg ! np.a.p} ;
  lin SlashVS np vs s = {s = np.s ! RSubj
                               ++ vs.present ! Imperfective ! Sg ! np.a.p ++ s.s} ;
  lin UseCl t p cl = {s = t.s ++ p.s ++ cl.s ! Main ! t.t ! t.a ! p.p} ;
  lin UseQCl t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p} ;
  lin UseRCl t p cl = {s = \\gn => t.s ++ p.s ++ cl.s ! gn ! t.t ! t.a ! p.p} ;
  lin UseSlash t p c = {s = t.s ++ p.s ++ c.s} ;
}
