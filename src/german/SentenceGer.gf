concrete SentenceGer of Sentence = CatGer ** open ResGer, Prelude in {

  flags optimize=all_subs ;

  lin

    PredVP np vp =
      let subj = mkSubject np vp.c1
      in mkClause subj.s subj.a vp ;

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
            ImpF _ True => <P3,"Sie",True> ; -- setzen Sie sich Ihren Hut auf
            _ => <P2,[],False>               -- but: nimm [ihren | deinen | *Ihren] Hut
            } ;                              -- vp should be reflexive, ComplRSlash
          vagr = VAg (numImp n) ps.p1 ;
          verb = vps.s ! False ! vagr ! VPImperat ps.p3 ;
          agr = case <numImp n, ps.p1, ps.p3> of {
                  <_, P3,True>  => AgPlPol ;  -- sich | Ihr-
                  <Sg,P2,False> => AgSgP2 ;   -- dich | dein-
                  <Pl,P2,False> => AgPl P2 ;  -- euch | euer-
                  _ => AgSgP1 -- default, does not occur
                  } ;
          neg  = negation ! pol ;
          inf  = vp.inf.inpl.p2 ++ verb.inf ;  -- HL .s/.inpl.p2
          obj  = (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 ++ (vp.nn ! agr).p4 ++ vp.adj
        in
        verb.fin ++ ps.p2 ++ (vp.nn ! agr).p1 ++ vp.a1 ++ neg ++ obj ++ vp.a2 ++ inf ++ vp.ext
    } ;

    AdvImp adv imp = {
      s = \\pol,impform => adv.s ++ imp.s ! pol ! impform
    } ;

-- to save compile time: HL 7/22, comment SlashVP out:
-- + SlashV2VNP 199065600 (46080,240)
-- + SlashVP 414720 (28224,204)

    SlashVP np vp =
      let subj = mkSubject np vp.c1 ;                  -- HL 3/2022: need a mkClSlash to prevent
      in mkClause subj.s subj.a vp ** { c2 = vp.c2 } ; -- reflexives in vp instantiated to np.a
                                                       -- cf. tests/german/TestLangGer.gf
    AdvSlash slash adv = {
      s  = \\m,t,a,b,o => slash.s ! m ! t ! a ! b ! o ++ adv.s ;
      c2 = slash.c2
    } ;

    SlashPrep cl prep = cl ** {c2 = prep} ;

    SlashVS np vs slash =
      let subj = mkSubject np PrepNom ;
          vp = insertExtrapos (conjThat ++ slash.s ! Sub) (predV vs)
      in mkClause subj.s subj.a vp ** {c2 = slash.c2} ;

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
