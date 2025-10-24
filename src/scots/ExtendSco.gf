--# -path=.:../common:../abstract

concrete ExtendSco of Extend = ExtendEng-[passVPSlash,PassVPSlash,PassAgentVPSlash,ProgrVPSlash] ** open Prelude, ResSco in {

oper
  passVPSlash : VPSlash -> Str -> ResSco.VP =
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

lin PassVPSlash vps = passVPSlash (lin VPSlash vps) [] ;
    PassAgentVPSlash vps np = passVPSlash (lin VPSlash vps) ("by" ++ np.s ! NPAcc) ;
    ProgrVPSlash vp = insertObjc (\\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a)
      (predAux auxBe ** {c2 = vp.c2; gapInMiddle = vp.gapInMiddle; missingAdv = vp.missingAdv});

}
