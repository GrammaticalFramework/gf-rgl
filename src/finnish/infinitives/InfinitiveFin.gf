--# -path=..:alltenses

concrete InfinitiveFin of Infinitive =
    GrammarFin - [
      VPSlashPrep,
      PassV2],
    LexiconFin
  ** open
    ResFin,
    StemFin,
    Prelude,
    ParadigmsFin
  in {

lincat
  RAdv = {s : Agr => Str} ;
  
lin
  UseV2 v2 = predSV v2 ;
  RAdvVP vp radv =  insertObj (\\_,_ => radv.s) vp ; ---- can be wrong word order

  X_NP = MassNP (UseN (mkN "X" "X:n")) ;
  Y_NP = MassNP (UseN (mkN "Y" "Z:n")) ;
  Z_NP = MassNP (UseN (mkN "Z" "Z:n")) ;

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

  PresPartActAP vp = {
    s = \\_, nf => preCompVP vp (PresPartAct (AN nf)) ;
    hasPrefix = False ;
    p = []
    } ;
    
  PastPartActAP vp = {
    s = \\_, nf => preCompVP vp (PastPartAct (AN nf)) ;
    hasPrefix = False ;
    p = []
    } ;
    
  PresPartPassAP vp = {
    s = \\_, nf => preCompVP <vp : VP> (PresPartPass (AN nf)) ;
    hasPrefix = False ;
    p = []
    } ;
    
  PastPartPassAP vp = {
    s = \\_, nf => preCompVP <vp : VP> (PastPartPass (AN nf)) ;
    hasPrefix = False ;
    p = []
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

  Inf1LongRAdv vp = {
    s = \\a => 
          infVP SCNom Pos infAdvAgr vp Inf1Long ++ BIND ++
          case vp.s.h of {Back => possSuffix a ; Front => possSuffixFront a}
    } ;
    
  Inf2InessAdv np vp = {
    s = np.s ! NPCase Gen ++
        infVP SCNom Pos np.a vp Inf2Iness
    } ;

  Inf2InessRAdv vp = {
    s = \\a => 
          infVP SCNom Pos infAdvAgr vp Inf2Iness ++ BIND ++
          case vp.s.h of {Back => possSuffix a ; Front => possSuffixFront a}
    } ;
    

  Inf2InessPassAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf2InessPass
    } ;

  Inf2InessPassInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf2InessPass
    } ;

  Inf2InstrAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf2Instr
    } ;
    
  Inf2InstrInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf2Instr
    } ;
    
  Inf3InessAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf3Iness
    } ;
    
  Inf3InessInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf3Iness
    } ;

  Inf3ElatAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf3Elat
    } ;
    
  Inf3ElatInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf3Elat
    } ;

  Inf3IllatAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf3Illat
    } ;
    
  Inf3IllatInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf3Illat
    } ;


  Inf3AdessAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf3Adess
    } ;
    
  Inf3AdessInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf3Adess
    } ;


  Inf3AbessAdv vp = {
    s = infVP SCNom Pos infAdvAgr vp Inf3Abess
    } ;
    
  Inf3AbessInvAdv np vps = {
    s = infAppCompl vps.c2 np ++
        infVP SCNom Pos np.a <vps : VP> Inf3Abess
    } ;

  ComplPresPartActVS vs np vp  =
    insertExtrapos (subjPartVP np vp (NPCase Gen) (PresPartAct (AN (NCase Sg Gen)))) (predSV vs) ;
  ComplPastPartActVS vs np vp  =
    insertExtrapos (subjPartVP np vp (NPCase Gen) (PastPartAct (AN (NCase Sg Gen)))) (predSV vs) ;

  ComplPresPartActReflVS vs vp  =
    insertObj (\\_,_,agr => subjPartAgrVP vp (PresPartAct (AN (NPossGen Sg))) agr) (predSV vs) ;
  ComplPastPartActReflVS vs vp  =
    insertObj (\\_,_,agr => subjPartAgrVP vp (PastPartAct (AN (NPossGen Sg))) agr) (predSV vs) ;

  ComplPresPartPassVS vs np vps  =
    insertExtrapos (subjPartVP np <vps : VP> (NPCase Part) (PresPartPass (AN (NCase Sg Gen)))) (predSV vs) ;
  ComplPastPartPassVS vs np vps  =
    insertExtrapos (subjPartVP np <vps : VP> (NPCase Part) (PastPartPass (AN (NCase Sg Gen)))) (predSV vs) ;
  


oper
  infAppCompl : Compl -> ResFin.NP -> Str = \co, np ->
    appCompl False Neg co np ;  -- not fin, Acc becomes Part

  infAdvAgr : Agr = agrP3 Sg ; --- ?

  -- hänen syövän, häntä syödyn, häntä syötävän
  subjPartVP : ResFin.NP -> StemFin.VP -> NPForm -> VForm -> Str = \np, vp, npform, vform ->
    np.s ! npform ++
    vp.s.s ! vform ++ 
    vp.s2 ! True ! Pos ! np.a ++
    vp.adv ! Pos ++
    vp.ext ;

  -- tiedän syöväni, tiedän syöneeni
  subjPartAgrVP : StemFin.VP -> VForm -> Agr -> Str = \vp, vform, agr ->
    vp.s.s ! vform ++ BIND ++
    case vp.s.h of {Back => possSuffix agr ; Front => possSuffixFront agr} ++
    vp.s2 ! True ! Pos ! agr ++
    vp.adv ! Pos ++
    vp.ext ;


  -- ruohoa syövä, Ranskassa valmistettu
  preCompVP : StemFin.VP -> VForm -> Str = \vp, vform ->
    vp.s2 ! True ! Pos ! infAdvAgr ++
    vp.adv ! Pos ++
    vp.s.s ! vform ++ 
    vp.ext ;

  
}