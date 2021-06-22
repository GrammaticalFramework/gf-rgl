concrete NounZul of Noun = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      empty,predet_pre,predet_post = cn.empty ;
      s = \\nform => det.s ++ cn.s ! det.n ! nform ;
      mod = cn.mod ! det.n ;
      agr = Third cn.c det.n ;
      proDrop = False ;
      isPron = False ;
      reqLocS = True ; -- TODO: change if a Det is ever added that has a non-empty string
      qdef = det.qdef ;
    } ;

    UsePN pn = {
      empty,predet_pre,predet_post = pn.empty ;
      s = \\nform => pn.s ! Sg ! nform ;
      mod = pn.empty ;
      agr = Third pn.c Sg ;
      proDrop = False ;
      isPron = False ;
      reqLocS = True ; -- TODO: change if a Det is ever added that has a non-empty string
      qdef = Article Def ;
    } ;

    UsePron pron = {
      empty,predet_pre,predet_post = pron.empty ;
      s = case pron.proDrop of {
        False => pron.s ;
        True => table {
          Full => pron.empty ;
          Reduced => pron.s!Reduced ;
          Poss => pron.s!Poss ;
          Loc => pron.s!Loc
        }
      } ;
      mod = pron.empty ;
      agr = pron.agr ;
      proDrop = pron.proDrop ;
      isPron = True ;
      reqLocS = True ;
      qdef = Article Def
    } ;

    RelNP np rs = {
      empty = np.empty ;
      s = \\nform => np.s!nform ;
      mod = np.mod ++ rs.s!np.agr ;
      predet_pre = np.predet_pre ;
      predet_post = np.predet_post ;
      agr = np.agr ;
      proDrop = np.proDrop ;
      isPron = np.isPron ;
      reqLocS = np.reqLocS ;
      qdef = np.qdef
    } ;

    PredetNP pred np = {
      empty = np.empty ;
      s = np.s ;
      mod = np.mod ;
      predet_post = case pred.isPost of {
        True => quantConc!np.agr ++BIND++ pred.s ;
        False => []
      } ;
      predet_pre = case pred.isPost of {
        True => [] ;
        False => quantConc!np.agr ++BIND++ pred.s
      } ;
      agr = np.agr ;
      proDrop = np.proDrop ;
      isPron = np.isPron ;
      reqLocS = case pred.isPost of {
        True => np.reqLocS ;
        False => False
      } ;
      qdef = np.qdef
      } ;

--     PPartNP np v2 = {
--       s = \\c => np.s ! c ++ v2.s ! VPPart ;
--       a = np.a
--       } ;

--     RelNP np rs = {
--       s = \\c => np.s ! c ++ frontComma ++ rs.s ! np.a ++ finalComma ;
--       a = np.a
--       } ;

    -- AdvNP np adv = {
    --   empty = np.empty ;
    --   s = \\nform => np.s!nform ++ adv.s ;
    --   -- loc = np.loc ;
    --   predet_pre = np.predet_pre ;
    --   predet_post = np.predet_post ;
    --   -- desc = np.desc ++ adv.s ;
    --   agr = np.agr ;
    --   proDrop = np.proDrop ;
    --   isPron = np.isPron ;
    --   reqLocS = np.reqLocS ;
    --   qdef = np.qdef
    -- } ;
    -- { s : Str ; reqLocS : Bool } ;

--     ExtAdvNP np adv = {
--       s = \\c => np.s ! c ++ embedInCommas adv.s ;
--       a = np.a
--       } ;
--

    DetQuant quant num = {
      s  = quant.s ++ num.s ;
      n  = num.n ;
      qdef = quant.qdef
    } ;

--     DetQuantOrd quant num ord = {
--       s  =            quant.s  ! num.hasCard ! num.n ++ num.s ! quant.isDef ! Nom ++ ord.s ! Nom;
--       sp = \\g,_,c => quant.s  ! num.hasCard ! num.n ++ num.s ! quant.isDef ! Nom ++ ord.s ! npcase2case c ;
--       n  = num.n ;
--       hasNum = True
--       } ;
--
--     DetNP det = {
--       -- s = case det.hasNum of {True => \\_ => det.s ; _ => \\c => det.sp ! c} ;
--       s = det.sp ! Neutr ! False ;
--       a = agrP3 det.n
--       } ;

    -- PossPron pron = {
    --   s = pron_stem!pron.agr ;
    --   qdef = PronounDef
    -- } ;

    --NOTE: this comes, commented out, from the EMRG
    -- PossPron cn pron =
    -- let
    --   stem = case pron.agr of {
    --     First Sg => "mi" ;
    --     First Pl => "thu" ;
    --     Second Sg => "kho" ;
    --     Second Pl => "nu" ;
    --     Third _ Sg => "khe" ;
    --     Third _ Pl => "bo"
    --   } ;
    --   proninit = case pron.agr of {
    --     First Sg => RC ;
    --     First Pl => RI ;
    --     Second Sg => RC ;
    --     Second Pl => RI ;
    --     Third _ Sg => RC ;
    --     Third _ Pl => RC
    --   } ;
    -- in {
    --   s = \\n,cpf => cn.s!n!cpf ++ poss_concord!cn.c!n!proninit ++BIND++ stem ;
    --   loc = \\n => cn.loc!n ++ poss_concord!cn.c!n!proninit ++BIND++ stem ;
    --   desc = \\n => cn.desc!n ;
    --   c = cn.c
    -- } ;

    NumSg = { s = [] ; n = Sg } ;
    NumPl = { s = [] ; n = Pl } ;

--     NumCard n = n ** {hasCard = True} ;
--
--     NumDigits n = {s,sp = \\_ => n.s ! NCard ; n = n.n} ;
--     OrdDigits n = {s    = n.s ! NOrd} ;
--
--     NumNumeral numeral = {s,sp = \\d => numeral.s ! d ! NCard; n = numeral.n} ;
--     OrdNumeral numeral = {s    = numeral.s ! True ! NOrd} ;
--
--     AdNum adn num = {s  = \\_,c => adn.s ++ num.s !False!c ;
--                      sp = \\_,c => adn.s ++ num.sp!False!c ;
--                      n  = num.n} ;
--
--     OrdSuperl a = {s = \\c => a.s ! AAdj Superl c } ;
--
--     OrdNumeralSuperl n a = {s = \\c => n.s ! True ! NOrd ! Nom ++ a.s ! AAdj Superl c } ;

    DefArt = { s = [] ; qdef = Article Def } ;

    IndefArt = { s = [] ; qdef = Article Indef } ;
--       sp = \\g,hasCard,n => case <n,hasCard> of {
--         <Sg,False> => table {NCase Gen => "one's"; _ => "one" };
--         <Pl,False> => table {NCase Gen => "ones'"; _ => "ones" } ;
--         _          => \\c => []
--         } ;
--       isDef = False
--       } ;

    MassNP cn = {
      empty,predet_pre,predet_post = cn.empty ;
      s = cn.s ! Sg ;
      mod = cn.mod ! Sg ;
      agr = Third cn.c Sg ;
      proDrop = False ;
      isPron = False ;
      reqLocS = True ;
      qdef = Article Indef
    } ;

    UseN n = n ** { mod = \\_ => [] } ;
--     UseN2 n = n ;
-- ---b    UseN3 n = n ;
--
--     Use2N3 f = {
--       s = \\n,c => f.s ! n ! Nom ;
--       g = f.g ;
--       c2 = f.c2
--       } ;
--
--     Use3N3 f = {
--       s = \\n,c => f.s ! n ! Nom ;
--       g = f.g ;
--       c2 = f.c3
--       } ;
--
--     ComplN2 f x = {s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! NPAcc ; g = f.g} ;
--     ComplN3 f x = {
--       s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! NPAcc ;
--       g = f.g ;
--       c2 = f.c3
--       } ;

    AdjCN ap cn = {
      empty = cn.empty ++ ap.empty ;
      s = cn.s ;
      mod = \\num =>
      let
        agr = Third cn.c num ;
        adjf = case ap.t of {
          AdjType => aformN agr ;
          _ => AF1
        } ;
        conc = case ap.t of {
          AdjType => adjConcLookup!agr ;
          RelType => relConc!agr!RelC ;
          EnumType => enumConcLookup!agr
        }
      in
        cn.mod!num ++ conc ++BIND++ ap.s!adjf ;
      c = cn.c
    } ;

    RelCN cn rs = {
      empty = cn.empty ;
      s = \\num,nform => cn.s!num!nform ;
      mod = \\num => cn.mod!num ++ rs.s!(Third cn.c num) ;
      c = cn.c
    } ;

    -- AdvCN cn adv = {
    --   empty = cn.empty ;
    --   s = \\num,nform => cn.s!num!nform ;
    --   mod = \\num => cn.mod!num ++ case adv.reqLocS of {
    --     True => shortRelConc!(Third cn.c num) ++BIND++ "s" ++BIND++ adv.s ;
    --     False => shortRelConc!(Third cn.c num) ++BIND++ adv.s
    --   } ;
    --   c = cn.c
    -- } ;

--     SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g} ;
--
    ApposCN cn np = {
      empty = cn.empty ;
      s = \\nform,num => cn.s!nform!num ;
      mod = \\num => cn.mod!num ++ np.s!Full ;
      c = cn.c
    } ;

    -- flashing of the lights / ukukhanya kwezibani
    PossNP cn np = {
      empty,predet_pre,predet_post = cn.empty ;
      s = \\n,nform => cn.s!n!nform ;
      mod = \\num => cn.mod!num ++ poss_concord!cn.c!num!(initNP np.isPron np.agr) ++BIND++ np.s!Poss ;
      c = cn.c
    } ;

--     PartNP cn np = {s = \\n,c => cn.s ! n ! c ++ "of" ++ np.s ! NPAcc ; g = cn.g} ;
--
--     CountNP det np = {
--       s = \\c => det.sp ! Neutr ! False ! c ++ "of" ++ np.s ! NPAcc ;
--       a = agrP3 det.n
--       } ;
--
--     AdjDAP dap ap = {
--       s = dap.s ++ ap.s ! agrgP3 dap.n Masc ;       --- post-ap's ? "this larger than life (movie)"
--       sp = \\g,_,c => case c of {
--                         NCase Gen => dap.sp ! g ! True ! NCase Nom ++ ap.s ! agrgP3 dap.n g ++ BIND ++ "'s" ;
--                         c         => dap.sp ! g ! True ! c ++ ap.s ! agrgP3 dap.n g
--                       } ;
--       n = dap.n ;
--       hasNum = dap.hasNum
--       } ;
--
--     DetDAP d = d ;


}
