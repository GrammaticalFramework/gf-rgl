concrete VerbExtZul of VerbExt = CatZul,CatExtZul ** open ResZul, Prelude, ParamX in {

  lin

    CopAP ap = {
      s = case ap.t of {
        AdjType => table {
          MainCl => \\a,p,t,l => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a AdjType ; -- u- / uzoba / ube- / waye- / wayenge-
            adjpref =  adjPref a vform ; -- m-
            cop_base = ap.s!(aformN a) -- khulu
          in
            pcp ++ adjpref ++ cop_base ;
          RelCl => \\a,p,t,l => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop vform a RC) ; -- o-
            pcp = ap_cop_pref vform a AdjType ; -- [] / -nge- / zoba / -be- / -benge- -waye- / -wayenge-
            adjpref =  adjPref a vform ; -- m-
            cop_base = ap.s!(aformN a) -- khulu
          in
            rcp ++ pcp ++ adjpref ++ cop_base
        } ;
        RelType => table {
          MainCl => \\a,p,t,l => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a RelType ; -- u-
            cop_base = ap.s!AF1 -- qotho
          in
            pcp ++ cop_base ;
          RelCl => \\a,p,t,l => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop vform a RC) ; -- o-
            pcp = ap_cop_pref vform a RelType ; -- [] / -nge- / zoba / -benge-
            cop_base = ap.s!AF1 -- qotho
          in
            rcp ++ pcp ++ cop_base
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
        MainCl => \\a,p,t,l => let
          vform = VFIndic MainCl p t ;
          pcp = (id_pre_cop_pref vform a) ; -- u- / uzoba / akazukuba
          cp = (id_cop_pref np.agr) ; -- ng-
          cop_base = np.s!NFull -- umfundi
        in
          pcp ++ cp ++ cop_base ;
        RelCl => \\a,p,t,l => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop vform a RC) ; -- o-
          pcp = (id_pre_cop_pref vform a) ; -- [] / zoba / zukuba
          cp = (id_cop_pref np.agr) ; -- ng-
          cop_base = np.s!NFull -- umfundi
        in
          rcp ++ pcp ++ cp ++ cop_base
      } ;
      comp, iadv, advs = [] ;
      hasComp = np.heavy ;
      r = RC ; -- should not be used
      syl = SylMult ;
      vptype = CopIdent
    } ;

    CopNPAssoc np = {
      s = table {
        MainCl => \\a,p,t,l => let
          vform = VFIndic MainCl p t ;
          pcp = (assoc_pre_cop_pref vform a) ; -- u- / uzoba
          cp = (assoc_cop_pref p np.agr) ; -- ne-
          cop_base = np.s!NReduced -- moto
        in
          pcp ++ cp ++ cop_base ;
        RelCl => \\a,p,t,l => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop vform a RC) ; -- o-
          pcp = (assoc_pre_cop_pref vform a) ; -- [] / zoba
          cp = (assoc_cop_pref p np.agr) ; -- ne
          cop_base = np.s!NReduced -- moto
        in
          rcp ++ pcp ++ cp ++ cop_base
      } ;
      comp, iadv, advs = [] ;
      hasComp = np.heavy ;
      r = RC ; -- should not be used
      syl = SylMult ;
      vptype = CopAssoc
    } ;

    ComplV2Nonspec v2 np = {
      s = table {
        MainCl => \\a,p,t,l => let
          vform = VFIndic MainCl p t ;
          tp = tensePref vform v2.r v2.syl ;
          oc = objConc np.agr v2.r v2.syl ;
          longform = case np.heavy of {
            True => False ;
            False => True
          } ;
          r = v2.s!(rform (VFIndic MainCl p t) longform) ;
          obj = case p of {
            Pos => np.s!NFull ;
            Neg => np.s!NReduced
          } ;
        in case np.proDrop of {
          True => tp ++ oc ++ r ++ obj ;
          False => tp ++ r ++ obj
        } ;
        RelCl => \\a,p,t,l => let
          vform = (VFIndic RelCl p t) ;
          rc = relConc vform a v2.r ;
          tp = tensePref vform v2.r v2.syl ;
          oc = objConc np.agr v2.r v2.syl ;
          longform = case np.heavy of {
            True => False ;
            False => True
          } ;
          r = v2.s!(rform vform longform) ;
          obj = case p of {
            Pos => np.s!NFull ;
            Neg => np.s!NReduced
          } ;
        in case np.proDrop of {
          True => rc ++ tp ++ oc ++ r ++ obj ;
          False => rc ++ tp ++ r ++ obj
        }
      } ;
      iadv, advs, comp = [] ;
      ap_comp = \\_ => [] ;
      hasComp = np.heavy ;
      r = v2.r ;
      syl = v2.syl ;
      vptype = VNPCompl
    } ;

    CopLoc loc = {
      s = \\c,a,p,t,l => loc.s!c!a!p!t ;
      comp,advs,iadv = [] ;
      hasComp = True ;
      r = RC ;
      syl = SylMult ;
      vptype = CopLoc
    } ;

}
