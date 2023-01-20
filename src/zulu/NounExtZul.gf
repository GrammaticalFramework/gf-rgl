concrete NounExtZul of NounExt = CatZul,CatExtZul ** open ResZul, Prelude, ParamX in {

  lin

    -- Quant is used for demonstratives, and QuantStem for all/only

    PronPostdetNP pron postdet = {
      empty = pron.empty ;
      s = \\nform => pron.s!nform ++ postdet.s!pron.agr ;
      agr = pron.agr ;
      i = RC ;
      proDrop = pron.proDrop ;
      isPron = True ;
      heavy = True
    } ;

    QuantPostdet q = {
      s = \\a => q.s!a
    } ;

    DemPostdet q = {
      s = \\a => q.s!True!a
    } ;

    QuantDemPostdet q d = {
      s = \\a => q.s!a ++ d.s!True!a
    } ;

    DemQuantPostdet d q = {
      s = \\a => d.s!True!a ++ q.s!a
    } ;

    DetNum n = n ; -- ** { spec = Spec } ;

    -- NonspecDet n = n ** { spec = Nonspec } ;

    PostdetCN cn postdet det = let
      agr = Third cn.c det.n
    in {
      empty = cn.empty ;
      -- s = \\nform => cn.s!det.n!nform ++ cn.mod!det.n ++ postdet.s!agr ;
      s = \\nform => cn.s!det.n!nform ++ postdet.s!agr ++ det.s ;
      agr = agr ;
      i = nominit!agr ;
      proDrop = False ;
      isPron = False ;
      heavy = True
    } ;

    RelN rs n = {
      empty = n.empty ;
      s = \\num,nform => rs.s!(Third n.c num) ++ n.s!num!nform ;
      -- mod = \\_ => [] ;
      c = n.c ;
      emph = False
    } ;

    -- TODO : check mod
    ApposCN cn n = {
      s = \\num,nform => cn.s!num!nform ++ n.s!num!NFull ;
      -- mod = \\num => n.s!num!Full ++ cn.mod!num ;
      c = cn.c ; -- takes agr of cn
      empty = cn.empty ;
      emph = cn.emph
    } ;

    -- TODO : check mod
    ApposN cn n = {
      s = \\num,nform => cn.s!num!nform ++ n.s!num!NFull ;
      -- mod = \\num => n.s!num!Full ++ cn.mod!num ;
      c = n.c ; -- takes agr of n
      empty = cn.empty ;
      emph = cn.emph
    } ;

    PredetN predet n = {
      s = case predet.hasDem of {
        True => \\num => table {
          NFull | NReduced | NPoss => predet.s!(Third n.c num) ++ n.s!num!NReduced ;
          NLoc => "ku" ++BIND++ predet.s!(Third n.c num) ++ n.s!num!NReduced
        } ;
        False => \\num,nform => predet.s!(Third n.c num) ++ n.s!num!nform
      } ;
      -- mod = \\_ => [] ;
      c = n.c ;
      empty = n.empty ;
      emph = False
    };

    QuantPredet q = {
      s = \\a => q.s!a ;
      hasDem = False
    } ;

    DemPredet q = {
      s = \\a => q.s!False!a ;
      hasDem = True
    } ;

    QuantDemPredet q d = {
      s = \\a => q.s!a ++ d.s!False!a ;
      hasDem = True
    } ;

    EmphCN cn = {
      s = \\num => table {
        NFull => pron_stem!(Third cn.c num) ++BIND++ "na" ++ cn.s!num!NFull ;
        NReduced => pron_stem!(Third cn.c num) ++ cn.s!num!NFull ;
        NPoss => poss_pron_stem!(Third cn.c num) ++ cn.s!num!NFull ;
        NLoc => "ku" ++BIND++ pron_stem!(Third cn.c num) ++ cn.s!num!NFull
      } ;
      -- mod = cn.mod ;
      c = cn.c ;
      empty = cn.empty ;
      emph = True
    } ;

    ContrastCN cn = {
      s = \\num,nform => cn.s!num!nform ++ pron_stem!(Third cn.c num) ++BIND++ "na" ;
      -- mod = \\num => pron_stem!(Third cn.c num) ++BIND++ "na" ++ cn.mod!num ;
      c = cn.c ;
      empty = cn.empty ;
      emph = cn.emph ;
      emph = cn.emph
    } ;

    UsePNPl pn = let
      agr = Third pn.c Pl
    in {
      empty = pn.empty ;
      s = pn.s!Pl ;
      agr = agr ;
      i = nominit!agr ;
      proDrop = False ;
      isPron = False ;
      heavy = True
    } ;

    -- Deverb15 v =
    -- let
    --   agr = Third C15 Sg ;
    -- in
    -- {
    --   s = \\_ => table {
    --     NFull => case v.r of {
    --       RC => "uku"++BIND++(v.s!R_a) ;
    --       (RA|RE) => "ukw"++BIND++(v.s!R_a) ;
    --       _ => "uk"++BIND++(v.s!R_a)
    --     } ;
    --     NReduced => case v.r of {
    --       RC => "ku"++BIND++(v.s!R_a) ;
    --       (RA|RE) => "kw"++BIND++(v.s!R_a) ;
    --       _ => "k"++BIND++(v.s!R_a)
    --     } ;
    --     NPoss => case v.r of {
    --       RC => "ku"++BIND++(v.s!R_a) ;
    --       (RA|RE) => "kw"++BIND++(v.s!R_a) ;
    --       _ => "k"++BIND++(v.s!R_a)
    --     } ;
    --     NLoc => case v.r of {
    --       RC => "eku"++BIND++(v.s!R_e)++BIND++"ni" ;
    --       (RA|RE) => "ekw"++BIND++(v.s!R_e)++BIND++"ni" ;
    --       _ => "ek"++BIND++(v.s!R_e)++BIND++"ni"
    --     }
    --   } ;
    --   c = C15 ;
    --   empty = []
    -- } ;

    -- not built to work for copulative VPs for now (specifically for agreement)
    Deverb15 pol vp =
      let
        agr = Third C15 Sg
      in
      {
        s = \\nform => vp.inf_s!nform!pol.p ++ pol.s ;
        -- s = table {
        --   NFull => case <pol.p,vp.r> of {
        --     <Neg,_ > => "uku"++BIND++"nga"++BIND++v.s!R_i ;
        --     <_,RC> => "uku"++BIND++v.s!R_a ;
        --     <_,(RA|RE)> => "ukw"++BIND++v.s!R_a ;
        --     <_,_> => "uk"++BIND++v.s!R_a
        --   } ;
        --   NReduced => case <pol.p,vp.r> of {
        --     <Neg,_ > => "ku"++BIND++"nga"++BIND++v.s!R_i ;
        --     <_,RC> => "ku"++BIND++v.s!R_a ;
        --     <_,(RA|RE)> => "kw"++BIND++v.s!R_a ;
        --     <_,_> => "k"++BIND++v.s!R_a
        --   } ;
        --   NPoss => case <pol.p,vp.r> of {
        --     <Neg,_ > => "ku"++BIND++"nga"++BIND++v.s!R_i ;
        --     <_,RC> => "ku"++BIND++v.s!R_a ;
        --     <_,(RA|RE)> => "kw"++BIND++v.s!R_a ;
        --     <_,_> => "k"++BIND++v.s!R_a
        --   } ;
        --   NLoc => case <pol.p,vp.r> of {
        --     <Neg,_ > => "ku"++BIND++poss_pron_stem!(Third C15 Sg) ++ "uku"++BIND++"nga"++BIND++v.s!R_i ;
        --     <_,_> => "ku"++BIND++poss_pron_stem!(Third C15 Sg) ++ "uku"++BIND++v.s!R_a
        --   }
        -- } ;
        agr = agr ;
        i = RU ;
        proDrop = False ;
        isPron = False ;
        heavy = True ;
        empty = []
    } ;

    LocNP np = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          pcp = ap_cop_pref vform a RelType ; -- u- / uzoba / akazukuba
          cop_base = loc_n_cop_base np vform
        in
          case vform of {
            VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
            VFIndic _ _ _ => pcp ++ cop_base
          } ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop vform a RC) ; -- o- / onge-
          pcp = ap_cop_pref vform a RelType ; -- u- / uzoba / akazukuba
          cop_base = loc_n_cop_base np vform
        in
          case vform of {
            VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
            VFIndic _ _ _ => rcp ++ pcp ++ cop_base
          }
      } ;
    } ;

    -- LocNLoc locn = {
    --   s = table {
    --     MainCl => \\a,p,t => let
    --       vform = VFIndic MainCl p t ;
    --       pcp = ap_cop_pref vform a RelType ; -- u- / uzoba / akazukuba
    --       cop_base = locn.s
    --     in
    --       case vform of {
    --         VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
    --         VFIndic _ _ _ => pcp ++ cop_base
    --       } ;
    --     RelCl => \\a,p,t => let
    --       vform = VFIndic RelCl p t ;
    --       rcp = (relConcCop vform a RC) ; -- o- / onge-
    --       pcp = ap_cop_pref vform a RelType ; -- u- / uzoba / akazukuba
    --       cop_base = locn.s
    --     in
    --       case vform of {
    --         VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
    --         VFIndic _ _ _ => rcp ++ pcp ++ cop_base
    --       }
    --   } ;
    -- } ;

    -- LocNPossNP locn np = {
    --   s = locn.s ++ poss_concord_agr!(Third C17 Sg)!(np.i) ++BIND++ np.s!NPoss;
    -- } ;

    LocAdvLoc locadv = {
      s = table {
        MainCl => \\a,p,t => let
          vform = VFIndic MainCl p t ;
          pcp = ap_cop_pref vform a RelType ; -- u- / uzoba / akazukuba
          s = case locadv.reqLocS of {
            True => "s"++BIND ;
            False => []
          } ;
          cop_base = locadv.s
        in
          case vform of {
            VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
            VFIndic _ _ _ => pcp ++ s ++ cop_base
          } ;
        RelCl => \\a,p,t => let
          vform = VFIndic RelCl p t ;
          rcp = (relConcCop vform a RC) ; -- o- / onge-
          pcp = ap_cop_pref vform a RelType ; -- u- / uzoba / akazukuba
          s = case locadv.reqLocS of {
            True => "s"++BIND ;
            False => []
          } ;
          cop_base = locadv.s
        in
          case vform of {
            VFIndic _ Neg PresTense => (kho_cop vform a) ++ cop_base;
            VFIndic _ _ _ => rcp ++ pcp ++ s ++ cop_base
          }
      } ;
      imp_s = table {
        Sg => table {
          Pos => "yiba" ++ "s"++BIND++ locadv.s ;
          Neg => "ungabi" ++ "s"++BIND++ locadv.s
        } ;
        Pl => table {
          Pos => "yibani" ++ "s"++BIND++ locadv.s ;
          Neg => "ningabi" ++ "s"++BIND++ locadv.s
        }
      } ;
      -- inf_s = table {
      --   Pos => "ukuba" ++ "s"++BIND++ locadv.s ;
      --   Neg => "ukungabi" ++ "s"++BIND++ locadv.s
      -- } ;
      inf_s = table {
        NFull => table {
          Pos => "ukuba" ++ "s"++BIND++ locadv.s ;
          Neg => "ukungabi" ++ "s"++BIND++ locadv.s
        } ;
        NReduced | NPoss => table {
          Pos => "kuba" ++ "s"++BIND++ locadv.s ;
          Neg => "kungabi" ++ "s"++BIND++ locadv.s
        } ;
        NLoc => table {
          Pos => "ku"++BIND++poss_pron_stem!(Third C15 Sg) ++"ukuba" ++ "s"++BIND++ locadv.s ;
          Neg => "ku"++BIND++poss_pron_stem!(Third C15 Sg) ++"ukungabi" ++ "s"++BIND++ locadv.s
        }
      }
    } ;



}
