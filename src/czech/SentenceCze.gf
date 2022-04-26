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
    
}
