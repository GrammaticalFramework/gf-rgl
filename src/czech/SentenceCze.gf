concrete SentenceCze of Sentence = CatCze ** 
  open Prelude, ResCze in {

lin
    PredVP np vp = {
      subj  = case np.hasClit of {
        True  => np.clit ! Nom ;  -- pro-drop
        False => np.s ! Nom
	} ;
      verb  = vp.verb ;
      clit  = vp.clit ! np.a ;
      compl = vp.compl ! np.a ;
      a = np.a ;
      } ;

    UseCl temp pol cl = {
      s = temp.s ++ cl.subj ++ cl.clit ++ pol.s ++ verbAgr cl.verb cl.a pol.p ++ cl.compl ;
      } ;

    --- TODO is inversion the standard? ; add indirect questions
    UseQCl temp pol cl = {
      s = temp.s ++ cl.clit ++ pol.s ++ verbAgr cl.verb cl.a pol.p ++ cl.subj ++ cl.compl ; 
      } ;

    UseRCl temp pol rcl = {
      s = \\a => temp.s ++
                 rcl.subj ! a ++ rcl.clit ! a ++
		 pol.s ++ verbAgr rcl.verb a pol.p ++
		 rcl.compl ! a ;
      } ;

    ImpVP vp = {s = \\pos,a =>
      imperativeAgr vp.verb a pos ++ vp.clit ! a ++ vp.compl ! a
      } ;
    EmbedS s = {s = "že" ++ s.s} ;

    EmbedQS qs = {s = qs.s} ;

    EmbedVP vp = let agr = Ag Neutr Sg P3 in
      {s = vp.clit ! agr ++ vp.verb.inf ++ vp.compl ! agr} ;

    AdvS a s = {s = a.s ++ s.s} ;

    ExtAdvS a s = {s = a.s ++ SOFT_BIND ++ "," ++ s.s} ;

    SSubjS a subj b = {s = a.s ++ SOFT_BIND ++ "," ++ subj.s ++ b.s} ;

}
