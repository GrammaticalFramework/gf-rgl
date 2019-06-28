concrete ExtraLat of ExtraLatAbs =
  CatLat, ConjunctionLat ** 
  open ResLat, ParadigmsLat, Coordination, Prelude in {
  lincat CS = Str ;
  lin
    useS s = combineSentence s ! SPreO ! PreO ! CPreV ! SOV ;
    -- PastPartAP      : VPSlash -> AP ;         -- lost (opportunity) ; (opportunity) lost in space
--    PastPartAP vp = { s = vp.part ! VPassPerf } ;
    
    -- UsePronNonDrop p = -- Pron -> NP
    --   p.pers **
    --   {
    -- 	p = p.p ;
    -- 	s = \\_ => p.pers.s ! PronNonDrop ! PronNonRefl ;
    -- 	adv = "" ;
    -- 	preap, postap = { s = \\_ => "" } ;
    -- 	det = { s = \\_,_ => "" ; sp = \\_,_ => "" ; n = p.pers.n } ;
    --   } ;
    
    AdjCNPre ap cn =  -- AP -> CN -> CN
      addAdjToCN (lin AP ap) (lin CN cn) Post ;

    -- -- ConjNP   : Conj -> ListNP -> NP ;     -- she or we
    -- ConjNPque conj nps = 
    --   {
    -- 	s = case conj.c of {
    -- 	  Et => case nps.isBase of {
    --         False => \\cse => coord conj.c {init = (nps.s ! Et).init ! cse ; last = (nps.s ! Et).last ! cse } ;-- (conjunctDistrTable Case conj (nps.s ! Et)).s ;
    --         True => \\cse => (nps.s ! Et).init ! cse ++ (nps.s ! Et).last ! cse ++ BIND ++ "que" 
    -- 	    } ;
    -- 	  c => \\cse => coord conj.c {init = (nps.s ! c).init ! cse ; last = (nps.s ! c).last ! cse } -- (conjunctDistrTable Case conj (nps.l ! Et)).s
    -- 	  } ;
    -- 	n = case conj.c of { Et => Pl ; _ => nps.n } ;
    --   	g = nps.g ;
    --   	p = nps.p ;
    --   	adv = nps.adv ;
    --   	preap = nps.preap ;
    -- 	postap = nps.postap ;
    -- 	det = { s = \\_,_ => "" ; sp = \\_,_ => "" ; n = nps.n };
    --   } ;

    everybodyFem_NP = regNP  "quisque" "quemque" "cuiusque" "cuique" "quoque" "quisque" Fem Sg ;-- regNP "quisquae" Sg ; -- L...
    somebodyFem_NP = regNP "aliquis" "aliquem" "alicuius" "clicui" "aliquo" "aliquis" Fem Sg ; -- Bayer-Lindauer 60.1
    nobodyFem_NP = regNP "nemo" "neminem" "neminis" "nemini" "nemine" "nemo" Fem Sg ; -- Bayer Lindauer 60.4
    
    Nom_Prep = mkPrep "" Nom ;
    Gen_Prep = mkPrep "" Gen ;
    Acc_Prep = mkPrep "" Acc ;
    Dat_Prep = mkPrep "" Dat ;
    Abl_Prep = mkPrep "" Abl ;
    inAbl_Prep = mkPrep "in" Abl ;
    onAbl_Prep = mkPrep "in" Abl ; -- L...
} 
