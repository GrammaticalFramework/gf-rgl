concrete NounZul of Noun = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      empty,predet_pre,predet_post = cn.empty ;
      s = \\nform => det.s ++ cn.s ! det.n ! nform ;
      -- loc =  det.s ++ cn.loc ! det.n ;
      desc = cn.desc ! det.n ;
      agr = Third cn.c det.n ;
      proDrop = False ;
      isPron = False ;
      reqLocS = True ; -- TODO: change if a Det is ever added that has a non-empty string
      qdef = det.qdef ;
    } ;

    UsePN pn = {
      empty,predet_pre,predet_post = pn.empty ;
      s = \\nform => pn.s ! Sg ! nform ;
      -- loc =  cn.loc ! Sg ;
      desc = pn.empty ;
      agr = Third pn.c Sg ;
      proDrop = False ;
      isPron = False ;
      reqLocS = True ; -- TODO: change if a Det is ever added that has a non-empty string
      qdef = Article Def ;
    } ;

    UsePron pron = {
      empty,predet_pre,predet_post = pron.empty ;
      s = pron.s ;
      -- loc = "ki" ++BIND++ (pron.s!Full) ;
      desc = [] ;
      agr = pron.agr ;
      proDrop = pron.proDrop ;
      isPron = True ;
      reqLocS = True ;
      qdef = Article Def
    } ;

    PredetNP pred np = {
      empty = np.empty ;
      s = np.s ;
      -- s = \\nform => case pred.isPost of {
      --   True => np.s ! nform ++ quantConc!np.agr ++BIND++ pred.s ;
      --   False => quantConc!np.agr ++BIND++ pred.s ++ np.s ! nform
      -- } ;
      -- loc = case pred.isPost of {
      --   True => "ki" ++BIND++ np.loc ++ quantConc!np.agr ++BIND++ pred.s ;
      --   False => quantConc!np.agr ++BIND++ pred.s ++ "ki" ++BIND++ np.loc
      -- } ;
      predet_post = case pred.isPost of {
        True => quantConc!np.agr ++BIND++ pred.s ;
        False => []
      } ;
      predet_pre = case pred.isPost of {
        True => [] ;
        False => quantConc!np.agr ++BIND++ pred.s
      } ;
      desc = np.desc ;
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

    AdvNP np adv = {
      empty = np.empty ;
      s = np.s ;
      -- loc = np.loc ;
      predet_pre = np.predet_pre ;
      predet_post = np.predet_post ;
      desc = np.desc ++ adv.s ;
      agr = np.agr ;
      proDrop = np.proDrop ;
      isPron = np.isPron ;
      reqLocS = np.reqLocS ;
      qdef = np.qdef
    } ;
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
      -- loc = cn.loc ! Sg ;
      desc = cn.desc ! Sg ;
      agr = Third cn.c Sg ;
      proDrop = False ;
      isPron = False ;
      reqLocS = True ;
      qdef = Article Indef
    } ;

    UseN n = n ** { desc = \\_ => [] } ;
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
      -- loc = cn.loc ;
      desc = \\num =>
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
          conc ++BIND++ ap.s!adjf
            ++ cn.desc ! num ;
      c = cn.c
    } ;

    RelCN cn rs = {
      empty = cn.empty ;
      s = cn.s ;
      -- loc = cn.loc ;
      desc = \\n => cn.desc!n ++ rs.s!(Third cn.c n) ;
      -- desc = cn.desc ;
      c = cn.c
    } ;

    AdvCN cn adv = {
      empty = cn.empty ;
      s = cn.s ;
      -- loc = cn.loc ;
      desc = case adv.reqLocS of {
        True => \\n => cn.desc!n ++ poss_concord!cn.c!n!RC ++BIND++ "s" ++BIND++ adv.s ;
        False => \\n => cn.desc!n ++ adv.s
      } ;
      c = cn.c
    } ;

--     SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g} ;
--
--     ApposCN cn np = {s = \\n,c => cn.s ! n ! Nom ++ np.s ! NCase c ; g = cn.g} ;

    -- flashing of the lights / ukukhanya kwezibani
    PossNP cn np = {
      empty,predet_pre,predet_post = cn.empty ;
      s = \\n,cpf => cn.s!n!cpf ;
      -- loc = \\n => cn.loc!n ;
      desc = \\n => cn.desc!n ++ poss_concord!cn.c!n!(initNP np.isPron np.agr) ++BIND++ np.s!Poss ++ np.desc ;
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
