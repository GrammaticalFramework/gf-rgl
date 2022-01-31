concrete VerbExtZul of VerbExt = CatZul ** open ResZul, Prelude, ParamX in {

  lin

    CopAP ap = {
      s = case ap.t of {
        AdjType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a AdjType ; -- u- / uzoba
            adjpref =  adjPref a vform ; -- m-
            cop_base = ap.s!(aformN a) -- khulu
          in
            pcp ++ adjpref ++ cop_base ;
          RelCl => \\a,p,t => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop p a RC) ; -- o- / onge-
            pcp = ap_cop_pref vform a AdjType ; -- [] / zoba
            adjpref =  adjPref a vform ; -- m-
            cop_base = ap.s!(aformN a) -- khulu
          in
            rcp ++ pcp ++ adjpref ++ cop_base
        } ;
        RelType => table {
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a RelType ; -- u-
            cop_base = ap.s!AF1 -- qotho
          in
            pcp ++ cop_base ;
          RelCl => \\a,p,t => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop p a RC) ; -- o- / onge-
            pcp = ap_cop_pref vform a RelType ; -- [] / zoba
            cop_base = ap.s!AF1 -- qotho
          in
            rcp ++ pcp ++ cop_base
        } ;
        EnumType => table { -- TODO!
          MainCl => \\a,p,t => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a EnumType;
            adjpref =  adjPref a vform ;
          in
            (ap_cop_pref vform a EnumType) ++ adjpref ++ ap.s!AF1 ;
          RelCl => \\a,p,t => (enumConc p a) ++BIND++ ap.s!AF1
        }
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      syl = SylMult ;
      vptype = CopDescr
    } ;

    CopNP np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          pcp = (id_pre_cop_pref vform a) ; -- u- / uzoba / akazukuba
          cp = (id_cop_pref np.agr) ; -- ng-
          cop_base = np.s!Full -- umfundi
        in
          pcp ++ cp ++ cop_base ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop p a RC) ; -- o-
          pcp = (id_pre_cop_pref vform a) ; -- [] / zoba / zukuba
          cp = (id_cop_pref np.agr) ; -- ng-
          cop_base = np.s!Full -- umfundi
        in
          rcp ++ pcp ++ cp ++ cop_base
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      syl = SylMult ;
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
          rcp = (relConcCop p a RC) ; -- o-
          pcp = (assoc_pre_cop_pref vform a) ; -- [] / zoba
          cp = (assoc_cop_pref p np.agr) ; -- ne
          cop_base = np.s!Reduced -- moto
        in
          rcp ++ pcp ++ cp ++ cop_base
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      syl = SylMult ;
      vptype = CopAssoc
    } ;

    ComplV2Nonspec v2 np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          tp = tensePref vform v2.r v2.syl ;
          oc = objConc np.agr v2.r v2.syl ;
          r = v2.s!(rform (VFIndic MainCl p t) True) ;
          obj = case p of {
            Pos => np.s!Full ;
            Neg => np.s!Reduced
          } ;
        in case np.proDrop of {
          True => tp ++ oc ++ r ++ obj ;
          False => tp ++ r ++ obj
        } ;
        RelCl => \\a,p,t => let
          vform = (VFIndic RelCl p t) ;
          rc = relConc vform a v2.r ;
          tp = tensePref vform v2.r v2.syl ;
          oc = objConc np.agr v2.r v2.syl ;
          r = v2.s!(rform vform True) ;
          obj = case p of {
            Pos => np.s!Full ;
            Neg => np.s!Reduced
          } ;
        in case np.proDrop of {
          True => rc ++ tp ++ oc ++ r ++ obj ;
          False => rc ++ tp ++ r ++ obj
        }
      } ;
      iadv, advs, comp = [] ;
      ap_comp = \\_ => [] ;
      hasComp = case np.proDrop of {
        True => False ;
        False => True
      } ;
      r = v2.r ;
      syl = v2.syl ;
      vptype = VNPCompl
    } ;

}
