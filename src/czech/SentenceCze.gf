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

    UseQCl temp pol cl = {
      s = temp.s ++ cl.clit ++ pol.s ++ verbAgr cl.verb cl.a pol.p ++ cl.subj ++ cl.compl ; ---- inversion optional
      } ;

    UseRCl temp pol rcl = {
      s = \\a => temp.s ++
                 rcl.subj ! a ++ rcl.clit ! a ++
		 pol.s ++ verbAgr rcl.verb a pol.p ++
		 rcl.compl ! a ;
      } ;
    
}
