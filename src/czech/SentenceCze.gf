concrete SentenceCze of Sentence = CatCze ** open Prelude, ResCze in {
lin
    PredVP np vp = {
      -- A dropped subject still contributes an empty constituent, so PGF
      -- retains the pronoun and constrains its person through agreement.
      subj = case np.isDrop of {True => np.clit ! Nom ; False => np.s ! Nom} ;
      verb = vp.verb ; clit = vp.clit ! np.a ; compl = vp.compl ! np.a ;
      a = np.a ; isDrop = np.isDrop ; clitPresent = vp.clitPresent
      } ;

    UseCl temp pol cl = let v = pol.s ++ verbAgr cl.verb cl.a pol.p in
      case cl.isDrop of {
        True => sentence cl.clitPresent (temp.s ++ cl.subj ++ v) cl.clit cl.compl ;
        False => sentence cl.clitPresent (temp.s ++ cl.subj) cl.clit (v ++ cl.compl)
        } ;

    UseQCl temp pol cl = let v = pol.s ++ verbAgr cl.verb cl.a pol.p in {
      s = temp.s ++ case cl.yesNo of {
        True => v ++ cl.clit ++ cl.subj ++ cl.compl ;
        False => cl.q ++ cl.clit ++ v ++ cl.subj ++ cl.compl
        } ;
      ind = temp.s ++ case cl.yesNo of {
        True => "jestli" ++ cl.clit ++ cl.subj ++ v ++ cl.compl ;
        False => cl.q ++ cl.clit ++ v ++ cl.subj ++ cl.compl
        }
      } ;

    UseRCl temp pol rcl = {
      s = \\a => temp.s ++ rcl.subj ! a ++ rcl.clit ! a ++
        pol.s ++ verbAgr rcl.verb a pol.p ++ rcl.compl ! a
      } ;
    ImpVP vp = {s = \\pos,a =>
      imperativeAgr vp.verb a pos ++ vp.clit ! a ++ vp.compl ! a
      } ;
    EmbedS s = {s = (frontSentence "že" s).s} ;
    EmbedQS qs = {s = qs.ind} ;
    EmbedVP vp = let agr = Ag Neutr Sg P3 in
      {s = vp.verb.inf ++ vp.clit ! agr ++ vp.compl ! agr} ;
    AdvS a s = frontSentence a.s s ;
    ExtAdvS a s = prefixSentence (a.s ++ SOFT_BIND ++ ",") s ;
    SSubjS a subj b = appendSentence a (SOFT_BIND ++ "," ++ (frontSentence subj.s b).s) ;
}
