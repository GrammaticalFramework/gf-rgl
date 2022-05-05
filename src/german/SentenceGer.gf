concrete SentenceGer of Sentence = CatGer ** open ResGer, Prelude in {

  flags optimize=all_subs ;

  lin

    PredVP np vp =
      let subj = mkSubj np vp.c1
      in mkClause subj.p1 subj.p2 vp ;

	{- applies verb's subject case to subject ;
	   forces 3rd person sg agreement for any non-nom subjects -->
			"uns graut" "*uns grauen"
	   allows pre/post-positions in subjects -->
	 		"nach mir wurde gedürstet" "*mir wurde gedürstet" 
			can't think of case of postpositions in subject -}

    PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;

    ImpVP vp =  let vps = useVP vp in {
      s = \\pol,n => 
        let 
          ps = case n of {
            ImpF _ True => <P3,"Sie",True> ; -- setzen Sie sich
            _ => <P2,[],False>
            } ;
          agr  = Ag Fem (numImp n) ps.p1 ; --- g does not matter
          verb = vps.s ! False ! agr ! VPImperat ps.p3 ;
          inf  = vp.inf.inpl.p2 ++ verb.inf ;  -- HL .s/.inpl.p2
          obj  = (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 ++ (vp.nn ! agr).p4
        in
--        verb.fin ++ ps.p2 ++ (vp.nn ! agr).p1 ++ vp.a1 ! pol ++ obj ++ vp.a2 ++ inf ++ vp.ext
        verb.fin ++ ps.p2 ++ (vp.nn ! agr).p1 ++ vp.a1 ++ negation ! pol ++ obj ++ vp.a2 ++ inf ++ vp.ext
    } ; 

    SlashVP np vp =
      let subj = mkSubj np vp.c1 ;                       -- HL 3/2022: need a mkClSlash to prevent
      in mkClause subj.p1 subj.p2 vp ** { c2 = vp.c2 } ; -- reflexives in vp instantiated to np.a

    AdvSlash slash adv = {
      s  = \\m,t,a,b,o => slash.s ! m ! t ! a ! b ! o ++ adv.s ;
      c2 = slash.c2
    } ;

    SlashPrep cl prep = cl ** {c2 = prep} ;

    SlashVS np vs slash =
		let subj = mkSubj np PrepNom 
		in mkClause subj.p1 subj.p2 
			(insertExtrapos (conjThat ++ slash.s ! Sub) (predV vs)) **
        			{c2 = slash.c2} ;

    EmbedS  s  = {s = conjThat ++ s.s ! Sub} ;  -- no leading comma, if sentence-initial
    EmbedQS qs = {s = qs.s ! QIndir} ;
    EmbedVP vp = {s = useInfVP False vp} ;

    UseCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! t.m ! t.t ! t.a ! p.p ! o
      } ;
    UseQCl t p cl = {
      s = \\q => t.s ++ p.s ++ cl.s ! t.m ! t.t ! t.a ! p.p ! q
      } ;
    UseRCl t p cl = {
      s = \\r => t.s ++ p.s ++ cl.s ! t.m ! t.t ! t.a ! p.p ! r ;
      c = cl.c
      } ;
    UseSlash t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! t.m ! t.t ! t.a ! p.p ! o ;
      c2 = cl.c2
      } ;

    AdvS a s = {s = table {Sub => a.s ++ s.s ! Sub ; o => a.s ++ s.s ! Inv}} ;
    ExtAdvS a s = 
      {s = table {Sub => a.s ++ "," ++ s.s ! Sub ; o => a.s ++ "," ++ s.s ! Inv}} ;

    SSubjS a s b = {s = \\o => a.s ! o ++ "," ++ s.s ++ b.s ! Sub} ;

	RelS s r = {s = \\o => s.s ! o ++ "," ++ r.s ! RSentence} ; --- "was"


}
