--# -path=.:../common:../abstract

concrete ExtendSco of Extend = ExtendEng-[PassVPSlash,PassAgentVPSlash,
  ProgrVPSlash,EmbedPresPart,PastPartAgentAP,ByVP,InOrderToVP,
  PurposeVP,PredIAdvVP,NominalizeVPSlashNP,WithoutVP,UttVPShort,
  ReflPron,ReflPoss,MkVPS,MkVPI] ** open Prelude, ResSco in {

oper
  scoPassVPSlash : VPSlash -> Str -> ResSco.VP =
   \vps,ag ->
    let
      be = predAux auxBe ;
      ppt = vps.ptp
    in be ** {
        p = [] ;
        ad = \\_ => [] ;
        s2 = \\a => vps.ad ! a ++ ppt ++ vps.p  ++ vps.s2 ! a ++ ag ++ vps.c2 ; ---- place of agent
        isSimple = False ;
        ext = vps.ext
    } ;

lin PassVPSlash vps = scoPassVPSlash (lin VPSlash vps) [] ;
    PassAgentVPSlash vps np = scoPassVPSlash (lin VPSlash vps) ("bi" ++ np.s ! NPAcc) ;
    ProgrVPSlash vp = insertObjc (\\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a)
      (predAux auxBe ** {c2 = vp.c2; gapInMiddle = vp.gapInMiddle; missingAdv = vp.missingAdv});

    EmbedPresPart vp = {s = infVP VVPresPart vp False Simul CPos (agrP3 Sg)} ;

    PastPartAgentAP vp np = {
      s = \\a => vp.ad ! a ++ vp.ptp ++ vp.p ++ vp.c2 ++ vp.s2 ! a ++ "bi" ++ np.s ! NPAcc ++ vp.ext ;
      isPre = False
      } ;

    ByVP vp = {
      s = "bi" ++ vp.ad ! AgP3Sg Neutr ++ vp.prp ++ vp.p ++ vp.s2 ! AgP3Sg Neutr ++ vp.ext
      } ;

    InOrderToVP vp = {
      s = variants {"in order"; []} ++ infVP VVInf vp False Simul CPos (AgP3Sg Neutr)
      } ;

    PurposeVP vp = {s = infVP VVInf vp False Simul CPos (agrP3 Sg)} ;

    PredIAdvVP iadv vp = {
      s = \\t,a,p,q => iadv.s ++ infVP VVInf vp False Simul CPos (agrP3 Sg)
      } ;

    NominalizeVPSlashNP vpslash np =
      let vp : ResSco.VP = insertObjPre (\\_ => vpslash.c2 ++ np.s ! NPAcc) vpslash ;
          a = AgP3Sg Neutr
      in lin NP {s = \\_ => vp.ad ! a ++ vp.prp ++ vp.s2 ! a ; a = a} ;

    WithoutVP vp = {
      s = "withoot" ++ vp.ad ! AgP3Sg Neutr ++ vp.prp ++ vp.p ++ vp.s2 ! AgP3Sg Neutr ++ vp.ext
      } ;

    UttVPShort vp = {s = infVP VVAux vp False Simul CPos (agrP3 Sg)} ;

    ReflPron = {s = reflPron} ;
    ReflPoss num cn = {
      s = \\a => possPron ! a ++ num.s ! True ! Nom ++ cn.s ! num.n ! Nom
      } ;

    MkVPS t p vp = scoMkVPS (lin Temp t) (lin Pol p) (lin VP vp) ;

    MkVPI vp = scoMkVPI (lin VP vp) ;

oper
  scoMkVPS : CatEng.Temp -> CatEng.Pol -> ResSco.VP -> VPS = \t,p,vp -> lin VPS {
    s = \\o,a =>
      let verb = mkVerbForms a vp ! t.t ! t.a ! p.p ! o ! a ;
          compl = vp.s2 ! a ++ vp.ext
      in {fin = verb.aux ++ t.s ++ p.s ;
          inf = verb.adv ++ vp.ad ! a ++ verb.fin ++ verb.inf ++ vp.p ++ compl}
    } ;

  scoMkVPI : ResSco.VP -> VPI = \vp -> lin VPI {
    s = table {
      VVAux      => \\a => vp.ad ! a ++ vp.inf ++ vp.p ++ vp.s2 ! a ++ vp.ext ;
      VVInf      => \\a => "tae" ++ vp.ad ! a ++ vp.inf ++ vp.p ++ vp.s2 ! a ++ vp.ext ;
      VVPresPart => \\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a ++ vp.ext
      }
    } ;

}
