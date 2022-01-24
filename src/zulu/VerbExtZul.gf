concrete VerbExtZul of VerbExt = CatZul ** open ResZul, Prelude, ParamX in {

  lin

    CopAP ap = {
      s = case ap.t of {
        AdjType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a ; -- u- / uzoba-
            adjpref =  adjPrefLookup!a ++BIND ; -- m-
            cop_base = ap.s!(aformN a) -- khulu
          in
            pcp ++ adjpref ++ cop_base ;
          RelCl => \\a,p,t => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop p a) ; -- o- / onge-
            pcp = ap_cop_pref vform a ; -- [] / zoba-
            adjpref =  relAdjPrefLookup!a ++ BIND ; -- m-
            cop_base = ap.s!(aformN a) -- khulu
          in
            rcp ++ pcp ++ adjpref ++ cop_base
        } ;
        RelType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a ; -- u-
            cop_base = ap.s!AF1 -- qotho
          in
            pcp ++ cop_base ;
          RelCl => \\a,p,t => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop p a) ; -- o- / onge-
            pcp = ap_cop_pref vform a ; -- [] / zoba-
            cop_base = ap.s!AF1 -- qotho
          in
            rcp ++ pcp ++ cop_base
        } ;
        EnumType => table { -- TODO!
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a ;
            adjpref =  adjPrefLookup!a ++BIND ;
          in
            (ap_cop_pref vform a) ++ adjpref ++ ap.s!AF1 ;
          RelCl => \\a,p,t => (enumConc p a) ++BIND++ ap.s!AF1
        }
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      vptype = CopDescr
    } ;

    CopNP np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          pcp = (id_pre_cop_pref vform a) ; -- u- / uzoba
          cp = (id_cop_pref np.agr) ; -- ng-
          cop_base = np.s!Full -- umfundi
        in
          pcp ++ cp ++ cop_base ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop p a) ; -- o-
          pcp = (id_pre_cop_pref vform a) ; -- [] / zoba
          cp = (id_cop_pref np.agr) ; -- ng-
          cop_base = np.s!Full -- umfundi
        in
          rcp ++ pcp ++ cp ++ cop_base
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      vptype = CopIdent
    } ;

    CopNPAssoc np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          pcp = (assoc_pre_cop_pref vform a) ; -- u- / uzoba
          cp = (assoc_cop_pref p np.agr) ; -- ne-
          cop_base = np.s!Reduced -- moto
        in
          pcp ++ cp ++ cop_base ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop p a) ; -- o-
          pcp = (assoc_pre_cop_pref vform a) ; -- [] / zoba
          cp = (assoc_cop_pref p np.agr) ; -- ne
          cop_base = np.s!Reduced -- moto
        in
          rcp ++ pcp ++ cp ++ cop_base
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      vptype = CopAssoc
    } ;

    ComplV2Nonspec v2 np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t
        in case np.proDrop of {
          True => (tensePref vform) ++ (objConc np.agr v2.r v2.syl) ++ v2.s!(rform (VFIndic MainCl p t) True) ++ np.s!Full ;
          False => case p of {
            Neg => (tensePref vform) ++ v2.s!(rform (VFIndic MainCl p t) True) ++ np.s!Reduced ;
            Pos => (tensePref vform) ++ v2.s!(rform (VFIndic MainCl p t) True) ++ np.s!Full
          }
        } ;
        RelCl => \\a,p,t => let
          vform = (VFIndic RelCl p t)
        in case np.proDrop of {
          True => (relConc p a) ++ (objConc np.agr v2.r v2.syl) ++ v2.s!(rform vform True) ++ np.s!Full ;
          False => case p of {
            Neg => (relConc p a) ++ v2.s!(rform vform True) ++ np.s!Reduced ;
            Pos => (relConc p a) ++ v2.s!(rform vform True) ++ np.s!Full -- ***present tense hack
          }
        }
      } ;
      iadv, advs, comp = [] ;
      ap_comp = \\_ => [] ;
      hasComp = case np.proDrop of {
        True => False ;
        False => True
      } ;
      r = v2.r ;
      vptype = VNPCompl
    } ;

}
