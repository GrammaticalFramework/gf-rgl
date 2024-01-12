abstract ExtraGerAbs = Extra [
  VPI,ListVPI,BaseVPI,ConsVPI,MkVPI,ComplVPIVV,ConjVPI,ClSlash,RCl,
  VPS,ListVPS,BaseVPS,ConsVPS,ConjVPS,MkVPS,PredVPS,EmptyRelSlash,
  VPSlash, PassVPSlash, PassAgentVPSlash, CompIQuant, PastPartAP, PastPartAgentAP,
  Temp,Tense,Pol,S,NP,VV,VP,Conj,IAdv,IQuant,IComp,ICompAP,IAdvAdv,Adv,AP,
  Cl, Num, CN, Utt, Predet,
  Foc,FocObj,FocAdv,FocAP,UseFoc,
    RNP,ReflRNP,ReflPron,ReflPoss,PredetRNP
    ,RNPList,ConjRNP,Base_rr_RNP,Base_nr_RNP,Base_rn_RNP,Cons_rr_RNP,Cons_nr_RNP
    ,DetNPMAsc,DetNPFem
  ] ** {
  flags coding=utf8;
  
  cat
	FClause ; -- formal clause 
  fun
    PPzuAdv   : CN -> Adv ;  -- zum Lied, zur Flasche
    TImpfSubj : Tense ;      -- ich möchte...   --# notpresent

    moegen_VV : VV ;         -- ich mag/möchte singen

    DetNPMasc, DetNPFem : Det -> NP ;

	EsVV : VV -> VP -> VP ; -- ich genieße es zu schlafen
 	EsV2A : V2A -> AP -> S -> VP ; -- ich finde es schön, dass ...

  	VPass : V -> FClause ;   -- (es) wird getanzt
  	AdvFor : Adv -> FClause -> FClause ; -- es wird heute gelacht - addition of adverbs
  	FtoCl : FClause -> Cl ;  -- embedding FClause within the RGL, to allow generation of S, Utt, etc.

    Pass3V3 : V3 -> VPSlash ; -- wir bekommen den Beweis erklärt

    -- further constructions using RNP, declared in abstract/Extra.gf or Extend.gf:

    AdvRNP : NP -> Prep -> RNP -> RNP ;   -- a dispute with his wife
    AdvRVP : VP -> Prep -> RNP -> VP ;    -- lectured about her travels
    AdvRAP : AP -> Prep -> RNP -> AP ;    -- adamant in his refusal

    ReflA2RNP : A2 -> RNP -> AP ;         -- indifferent to their surroundings
                                               -- NOTE: generalizes ReflA2

    PossPronRNP : Pron -> Num -> CN -> RNP -> NP ; -- his abandonment of his wife and children

}
