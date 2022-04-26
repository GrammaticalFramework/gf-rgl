concrete SentenceChi of Sentence = CatChi **
  open Prelude, ResChi in {

  flags optimize=all_subs ;

  lin

    PredVP np vp = mkClause np.s vp ;

    PredSCVP sc vp = mkClause sc.s vp ;

    ImpVP vp = {
      s = table {
        Pos =>          infVP vp ;
        Neg => neg_s ++ infVP vp
        }
      } ;

    SlashVP np vp =
      mkClauseCompl np.s vp []
      ** {c2 = vp.c2} ;

    SlashVS np vs sslash = <mkClause np.s vs sslash.s : Clause> ** {c2 = sslash.c2} ;


    -- yet another reason for discontinuity of clauses
    AdvSlash slash adv = slash ** {vp = insertAdv adv slash.vp} ;
---- parser loops with unknown tokens if this version is used AR 20/4/2014
----      mkClauseCompl slash.np <insertAdv adv slash.vp : VP> []
----         ** {c2 = slash.c2} ;

    SlashPrep cl prep = cl ** {c2 = prep} ;

    EmbedS  s  = ss (conjThat ++ linS s) ;
    EmbedQS qs = ss (qs.s ! False) ;
    EmbedVP vp = ss (infVP vp) ;

    UseCl  t p cl = {
      preJiu = cl.np ;
      postJiu = t.s ++ p.s ++ cl.postJiu ! p.p ! t.t} ;

    UseQCl t p cl = {s = \\isDir => t.s ++ p.s ++ cl.s ! isDir ! p.p ! t.t} ;
    UseRCl t p cl = {s = t.s ++ p.s ++ cl.s ! p.p ! t.t} ;
    UseSlash t p cl = {s = t.s ++ p.s ++ cl.s ! p.p ! t.t ; c2 = cl.c2} ;

    AdvS a s = s ** {
      preJiu = a.s ++ s.preJiu -- tomorrow she
    } ;

    ExtAdvS a s = s ** {
      preJiu = a.s ++ chcomma ++ s.preJiu -- tomorrow, she
    } ;

    RelS s r = s ** {
      postJiu = s.postJiu ++ r.s ;
    } ;

    -- a="she walks", b="I die"
    -- result: preJiu="if she walks, I", postJiu="die"
    SSubjS a subj b = {
      preJiu = linS a ++ subj.prePart     -- if she walks,
            ++ b.preJiu ;                 -- I
      postJiu = b.postJiu ++ subj.sufPart -- die
    } ;

}
