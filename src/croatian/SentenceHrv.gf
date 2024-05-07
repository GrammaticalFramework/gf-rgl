concrete SentenceHrv of Sentence = CatHrv ** 
  open Prelude, ResHrv in {

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
      s = temp.s ++ cl.subj ++ cl.clit ++ pol.s ++ verbAgr cl.verb cl.a CTPres ++ cl.compl ;
      } ; ---- TODO tense, negation

    --- TODO is inversion the standard? ; add indirect questions
    UseQCl temp pol cl = {
      s = temp.s ++ cl.clit ++ pol.s ++ verbAgr cl.verb cl.a CTPres ++ cl.subj ++ cl.compl ; 
      } ; ---- TODO tenses

    UseRCl temp pol rcl = {
      s = \\a => temp.s ++
                 rcl.subj ! a ++ rcl.clit ! a ++
		 pol.s ++ verbAgr rcl.verb a CTPres ++
		 rcl.compl ! a ;
      } ;  ---- TODO tenses


    AdvS adv s = {s = adv.s ++ s.s} ;
    ExtAdvS adv s = {s = adv.s ++ Predef.BIND ++ "," ++ s.s} ;
    
}
