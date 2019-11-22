concrete SentenceLat of Sentence = CatLat ** open Prelude, ResLat in {

  flags optimize=all_subs ;

  lin

    PredVP np vp = -- NP -> VP -> Cl
      mkClause np vp ;
--
--    PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;
--
--    ImpVP vp = {
--      s = \\pol,n => 
--        let 
--          agr   = AgP2 (numImp n) ;
--          verb  = infVP True vp agr ;
--          dont  = case pol of {
--            CNeg True => "don't" ;
--            CNeg False => "do" ++ "not" ;
--            _ => []
--            }
--        in
--        dont ++ verb
--    } ;
   
--  SlashVP  : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    SlashVP np vp = 
      mkClause np ( vp ** {c2 = vp.c2} ) ;
--
--    AdvSlash slash adv = {
--      s  = \\t,a,b,o => slash.s ! t ! a ! b ! o ++ adv.s ;
--      c2 = slash.c2
--    } ;

--  SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
    SlashPrep cl prep = cl ** {c2 = prep.s} ;
--
--    SlashVS np vs slash = 
--      mkClause (combineNounPhrase np ! PronNonDrop ! Nom) np.a 
--        (insertObj (\\_ => conjThat ++ slash.s) (predV vs))  **
--        {c2 = slash.c2} ;
--
--    EmbedS  s  = {s = conjThat ++ s.s} ;
--    EmbedQS qs = {s = qs.s ! QIndir} ;
--    EmbedVP vp = {s = infVP False vp (agrP3 Sg)} ; --- agr
--
    UseCl  t p cl = -- Temp -> Pol-> Cl -> S
      (combineClause cl (lin Tense t) t.a (lin Pol p) VQFalse) ;

    -- 	UseQCl : Temp -> Pol -> QCl -> QS -- maybe use mkQuestion
    UseQCl t p cl =
      {
	s = let qs = combineClause cl t t.a p VQTrue in
	  \\q => case q of {
	  QDir => cl.q ++ defaultSentence qs ! SVO ; -- t.s ++ p.s ++ cl.q ++ cl.s ! PreV ++ cl.v ! t.t ! t.a ! VQTrue ! PreV ! CPostV ++ cl.o ! PreV ;
	  QIndir => cl.q ++ defaultSentence qs ! SOV -- t.s ++ p.s ++ cl.q ++ cl.s ! PreV ++ cl.o ! PreV ++ cl.v ! t.t ! t.a ! VQTrue ! PreV ! CPostV
	  }
      } ;
    -- UseRCl : Temp -> Pol -> RCl -> RS ;
    UseRCl t p cl = {
      s = \\g,n => defaultSentence (combineClause (cl.s ! g ! n) (lin Tense t) t.a (lin Pol p) VQFalse) ! SOV ;
--      s = \\r => t.s ++ p.s ++ cl.s ! t.t ! t.a ! ctr p.p ! r ;
--      c = cl.c
    } ;
--    UseSlash t p cl = {
--      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! ctr p.p  ! ODir ;
--      c2 = cl.c2
--    } ;
--
    -- AdvS : Adv -> S -> S
    AdvS adv s = -- { s = s.s ; o = s.o ; v = s.v ; neg = s.neg ; t = s.t ; p = s.p ; sadv = adv.s ! Posit ++ s.sadv } ;
      s ** { sadv = adv.s ! Posit ++ s.sadv } ;

-- This covers subjunctive clauses, but they can also be added to the end.
    --  SSubjS : S -> Subj -> S -> S ;       -- I go home if she comes
    -- TO FIX
--    SSubjS s1 subj s2 = { s =  \\_ => subj.s ++ s2.s ! PreS ++ s1.s ! PreS ; sadv = lin Adv (mkAdverb []) } ;
    
--    RelS s r = {s = s.s ! APreV ++ "," ++ r.s } ;
--
--  oper
--    ctr = contrNeg True ;  -- contracted negations
--
}

