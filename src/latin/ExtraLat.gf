concrete ExtraLat of ExtraLatAbs =
  CatLat, ConjunctionLat ** 
  open ResLat, ParadigmsLat, RelativeLat, NounLat, Prelude in {
  lincat CS = SAdvPos => AdvPos => DetPos => VPos => ComplPos => Order => Str ;
	 TestRS = { s : Gender => Number => SAdvPos => AdvPos => DetPos => VPos => ComplPos => Order => Str } ;
  lin
    useS s = combineSentence s ;
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

    -- UttS_SVO : S -> Utt
    UttS_SVO s = { s = defaultSentence s ! SVO };
    -- UttS_VInS : S -> Utt
    UttS_VInS s = { s = combineSentence s ! SAPreS ! APreV ! DPostN ! VInS ! CPostV ! SVO } ;

    TestRCl t p cl = {
      s = \\g,n => combineSentence (combineClause (cl.s ! g ! n) (lin Tense t) t.a (lin Pol p) VQFalse) ;
      } ;
    
    -- UseRCl_OSV : Temp -> Pol -> RCl -> RS ;
    UseRCl_OSV t p cl = {
      s = \\g,n => combineSentence (combineClause (cl.s ! g ! n) (lin Tense t) t.a (lin Pol p) VQFalse) ! SAPreO ! APreV ! DPreN ! VReg ! CPostV ! OSV ;
    } ;
    -- UseRCl_OVS : Temp -> Pol -> RCl -> RS ;
    UseRCl_OVS t p cl = {
      s = \\g,n => combineSentence (combineClause (cl.s ! g ! n) (lin Tense t) t.a (lin Pol p) VQFalse) ! SAPreO ! APreV ! DPreN ! VReg ! CPostV ! OVS  ; -- SAPreO APreV DPreN VReg CPostV OVS
      } ;
    -- UseRCl_SOV : Temp -> Pol -> RCl -> RS ;
    UseRCl_SOV t p cl = {
      s = \\g,n => combineSentence (combineClause (cl.s ! g ! n) (lin Tense t) t.a (lin Pol p) VQFalse) ! SAPreS ! APreV ! DPreN ! VReg ! CPostV ! SOV  ;
    } ;
    -- UseRCl_SVO : Temp -> Pol -> RCl -> RS ;
    UseRCl_SVO t p cl = {
      s = \\g,n => combineSentence (combineClause (cl.s ! g ! n) (lin Tense t) t.a (lin Pol p) VQFalse) ! SAPreS ! APreV ! DPreN ! VReg ! CPostV ! SVO  ;
    } ;
    --  PrepNP_DPostN : Prep -> NP -> Adv ;        -- in the house
    PrepNP_DPostN prep np =
      mkAdv (prep.s ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPostN ! prep.c ) ;

    -- ApposCN_DPostN : CN -> NP -> CN
    ApposCN_DPostN cn np =
      cn **
      {
	s = \\n,c => cn.s ! n ! c ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPostN ! c ;
      } ; -- massable = cn.massable } ;

    --  CompNP_DPostN   : NP  -> Comp ;            -- (be) the man
    CompNP np = {s = \\_ =>
		   (combineNounPhrase np) ! PronNonDrop ! DPostN ! Nom
      } ;

    --  DetNP_Fem   : Det -> NP ;  -- these five
    DetNP_Fem det = {
      s = \\_ => det.s ! Fem ;
      g = Fem ;
      n = det.n ;
      p = P3 ;
      adv = "" ;
      preap, postap = { s = \\_ => "" } ;
      det = { s,sp = \\_ => "" ; n = det.n } ;
      } ;

    --     AdjAsNP_Fem : AP -> NP ;   -- green (is good)
    AdjAsNP_Fem ap = {
      s = \\_,c => ap.s ! (Ag Fem Sg c) ;
      adv = "" ;
      det = { s, sp = \\_ => "" } ;
      g = Fem ;
      n = Sg ;
      p = P3 ;
      postap = { s = \\_ => "" } ;
      preap = { s = \\_ => "" } ;
      } ;

    -- PredVP_VP_Ellipsis : NP -> VP -> Cl
    PredVP_VP_Ellipsis np  = 
      mkClause np emptyVP ;

    --  SlashVP_VP_Ellipsis  : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    SlashVP_VP_Ellipsis np = 
      mkClause np emptyVP ;

    -- FunRP_RP_Ellipsis : Prep -> NP -> RP ;
    FunRP_RP_Ellipsis p np = FunRP p np (lin RP { s = \\_ => "" }) ;

    RelNP_NP_Ellipsis rs = RelNP emptyNP rs ;
    
    comma_Conj = mkConj "" "," "" Pl Comma ;
} 
