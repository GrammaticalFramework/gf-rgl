--# -path=..:alltenses

concrete InfinitiveFin of Infinitive =
    GrammarFin,
    LexiconFin
  ** open
    ResFin,
    StemFin,
    Prelude
  in {

lin
  PresPartPassSubjVP vp = vp ** {
    s = vpVerbOlla ** {sc = SCGen} ;
    s2 = \\b,p,a => vp.s.s ! PresPartPass (AN (NCase Sg Nom)) ++ vp.s2 ! b ! p ! a ;
    } ;
    
  PresPartPassObjVP vpslash = vpslash ** {
    s = vpVerbOlla ** {sc = npform2subjcase vpslash.c2.c} ;
    s2 = \\b,p,a => vpslash.c2.s.p2 ++ vpslash.s.s ! PresPartPass (AN (NCase Sg Nom)) ++ vpslash.s2 ! b ! p ! a ;
    } ;

  PastPartPassAdv np vp = {
    s = np.s ! NPCase Gen ++
        vp.s.s ! PastPartPass (AN (NCase Sg Part)) ++
	vp.s2 ! True ! Pos ! np.a ++
	vp.adv ! Pos ++
	vp.ext
	} ;
	
  AgentPartAP np vp = {
    s = \\_, nf =>
          np.s ! NPCase Gen ++
    	  vp.s2 ! True ! Pos ! np.a ++
	  vp.adv ! Pos ++
	  vp.c2.s.p2 ++
          vp.s.s ! AgentPart (AN nf) ++
	  vp.ext ;
	hasPrefix = False ;
	p = []
	} ;

  Inf2InessAdv np vp = {
    s = np.s ! NPCase Gen ++
        infVP SCNom Pos np.a vp Inf2Iness
    } ;
    
  Inf2InessPassAdv np vps = {
    s = np.s ! NPCase Part ++
        infVP SCNom Pos np.a <vps : VP> Inf2InessPass
    } ;


  
--  {s = vp.s.s ! Inf Inf1Long} ;
--  {s = vp.s.s ! Inf Inf2Instr} ;

--  {s = vp.s.s ! Inf Inf2InessPass} ;
--  {s = vp.s.s ! Inf Inf2Adess} ;
--  {s = vp.s.s ! Inf InfPresPart} ;



  
}