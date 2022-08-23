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
            rcp = (adjConcCop vform a RC) ; -- o-
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
      imp_s = let
        imp_vform = VFIndic MainCl Pos PresTense
      in table {
        Sg => let
          agr = (Second Sg)
        in table {
          Pos => "yiba" ++ ap.s!AF1 ;
          Neg => "ungabi" ++ ap.s!AF1
        } ;
        Pl => let
          agr = (Second Pl)
        in table {
          Pos => "yibani" ++ ap.s!AF1 ;
          Neg => "ningabi" ++ ap.s!AF1
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
      imp_s = table {
        Sg => table {
          Pos => "yiba" ++ (id_cop_pref np.agr) ++ np.s!NFull ;
          Neg => "ungabi" ++ (id_cop_pref np.agr) ++ np.s!NFull
        } ;
        Pl => table {
          Pos => "yibani" ++ (id_cop_pref np.agr) ++ np.s!NFull ;
          Neg => "ningabi" ++ (id_cop_pref np.agr) ++ np.s!NFull
        }
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
      imp_s = let
        cop = (assoc_cop_pref Pos np.agr) ++ np.s!NReduced
      in table {
        Sg => table {
          Pos => "yiba" ++ cop ;
          Neg => "ungabi" ++ cop
        } ;
        Pl => table {
          Pos => "yibani" ++ cop ;
          Neg => "ningabi" ++ cop
        }
      } ;
      comp, iadv, advs = [] ;
      hasComp = np.heavy ;
      r = RC ; -- should not be used
      syl = SylMult ;
      vptype = CopAssoc
    } ;

    ComplV2Nonspec v2 np = let
      oc = objConc np.agr v2.r v2.syl ;
      longform = case np.heavy of {
        True => False ;
        False => True
      }
    in {
      s = table {
        MainCl => \\a,p,t,l => let
          vform = VFIndic MainCl p t ;
          tp = tensePref vform v2.r v2.syl ;
          -- oc = objConc np.agr v2.r v2.syl ;
          -- longform = case np.heavy of {
          --   True => False ;
          --   False => True
          -- } ;
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
          -- oc = objConc np.agr v2.r v2.syl ;
          -- longform = case np.heavy of {
          --   True => False ;
          --   False => True
          -- } ;
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
      imp_s = let
        obj_full = np.s!NFull ;
        obj_red = np.s!NReduced
      in table {
        Sg => table {
          Pos => case np.proDrop of {
            True => oc ++ v2.s!R_e ++ obj_full ;
            False => v2.s!R_a ++ obj_full
          } ;
          Neg => case np.proDrop of {
            True => "unga" ++BIND++ oc ++ v2.s!R_i ++ obj_red ;
            False => "unga" ++BIND++ v2.s!R_i ++ obj_red
          }
        } ;
        Pl => table {
          Pos => case np.proDrop of {
            True => oc ++ v2.s!R_e ++BIND++"ni" ++ obj_full ;
            False => v2.s!R_a ++BIND++"ni" ++ obj_full
          } ;
          Neg => case np.proDrop of {
            True => "ninga" ++BIND++ oc ++ v2.s!R_i ++ obj_red ;
            False => "ninga" ++BIND++ v2.s!R_i ++ obj_red
          }
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
      imp_s = loc.imp_s ;
      comp,advs,iadv = [] ;
      hasComp = True ;
      r = RC ;
      syl = SylMult ;
      vptype = CopLoc
    } ;

    -- CopPoss np = {
    --   s = table {
    --     MainCl => \\a,p,t,l => let
    --       vform = VFIndic MainCl p t ;
    --       pcp = (id_pre_cop_pref vform a) ; -- u- / uzoba / akazukuba
    --       cp = (id_cop_pref np.agr) ; -- ng-
    --       cop_base = poss_concord_agr!(Third C17 Sg)!(nominit!np.agr) ++BIND++ np.s!NPoss -- utshani
    --     in
    --       pcp ++ cp ++ cop_base ;
    --     RelCl => \\a,p,t,l => let
    --       vform = VFIndic RelCl p t ;
    --       rcp = (relConcCop vform a RC) ; -- o-
    --       pcp = (id_pre_cop_pref vform a) ; -- [] / zoba / zukuba
    --       cp = (id_cop_pref np.agr) ; -- ng-
    --       cop_base = poss_concord_agr!(Third C17 Sg)!(nominit!np.agr) ++BIND++ np.s!NPoss -- utshani
    --     in
    --       rcp ++ pcp ++ cp ++ cop_base
    --   } ;
    --   comp, iadv, advs = [] ;
    --   hasComp = np.heavy ;
    --   r = RC ; -- should not be used
    --   syl = SylMult ;
    --   vptype = CopIdent
    -- } ;

    CopQuant qs = {
      s = table {
          MainCl => \\a,p,t,l => let
            vform = VFIndic MainCl p t ;
            pcp = ap_cop_pref vform a RelType ; -- u-
            cop_base = qs.s!a -- qotho
          in
            pcp ++ cop_base ;
          RelCl => \\a,p,t,l => let
            vform = VFIndic RelCl p t ;
            rcp = (relConcCop vform a RC) ; -- o-
            pcp = ap_cop_pref vform a RelType ; -- [] / -nge- / zoba / -benge-
            cop_base = qs.s!a -- qotho
          in
            rcp ++ pcp ++ cop_base
      } ;
      imp_s = let
        imp_vform = VFIndic MainCl Pos PresTense
      in table {
        Sg => let
          agr = (Second Sg)
        in table {
          Pos => "yiba" ++ qs.s!agr ;
          Neg => "ungabi" ++ qs.s!agr
        } ;
        Pl => let
          agr = (Second Pl)
        in table {
          Pos => "yibani" ++ qs.s!agr ;
          Neg => "ningabi" ++ qs.s!agr
        }
      } ;
      comp, iadv, advs = [] ;
      hasComp = True ;
      r = RC ; -- should not be used
      syl = SylMult ;
      vptype = CopDescr
    } ;

}
