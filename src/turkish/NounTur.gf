--# -path=.:../abstract:../common:../../prelude

concrete NounTur of Noun = CatTur ** open ResTur, SuffixTur, HarmonyTur, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s =
        case det.useGen of {
          NoGen => \\c => det.s ++ cn.s ! det.n ! c ;
          YesGen a => \\c => det.s ++ cn.gen ! det.n ! a ;
          UseIndef => \\c => det.s ++ cn.s ! det.n ! c
        } ;
      a = agrP3 det.n
      } ;

    UsePron p = p ;

    -- TODO: look further into how correct this is.
    UsePN pn = { s = \\c => pn.s ! Sg ! c; a = {n = Sg; p = P1}} ;

    PossPron p = {s = []; useGen = YesGen p.a} ;

    DetQuant quant num = {
      s  = quant.s ++ num.s ! Sg ! Nom ;
      n  = num.n;
      useGen = quant.useGen
    } ;

    DetQuantOrd quant num o = {
      s  = quant.s ++ num.s ! Sg ! Nom ++ o.s ! num.n ! Nom ;
      n  = num.n;
      useGen = quant.useGen
    } ;

    NumSg = {s = \\num,c => []; n = Sg} ;
    NumPl = {s = \\num,c => []; n = Pl} ;

    NumCard n = n ** {n = Sg} ;

    NumNumeral numeral = {s = numeral.s ! NCard} ;

    OrdDigits  dig = {s = \\c => dig.s ! NOrd ! c} ;
    OrdNumeral num = {s = \\c => num.s ! NOrd ! c} ;
    OrdSuperl  a = {s = \\n,c => "en" ++ a.s ! n ! c} ;

    DefArt = {s = []; useGen = NoGen} ;
    IndefArt = {s = []; useGen = UseIndef} ;

    UseN n = n ;

    UseN2 n = n;

    MassNP cn = { s = cn.s ! Sg; a = { n = Sg; p = P1 } } ;

    ComplN2 f x =
      let
        h : Harmony = {vow = f.harmony.vow; con = f.harmony.con}
      in
        case f.c.c of {
          Nom => {
            s = \\n, c => x.s ! Gen ++ f.s ! n ! Acc;
            gen = \\_, _ => "TODO"
          };
          Acc => {s = \\_,_ => "TODO"; gen = \\_, _ => "TODO"};
          Gen => {
            s =
              \\n, c =>
                x.s ! Gen ++ f.gen ! n ! {n = Sg; p = P3}
                ++ BIND ++ (caseSuffixes ! c).st ! h.con ! h.vow;
            gen = \\_, _ => "TODO"
          };
          Dat => {
            s = \\n, c =>
              x.s ! Gen ++ f.gen ! n ! {n = Sg; p = P3}
                ++ datSuffixN.st ! h.con ! h.vow;
            gen = \\_, _ => "TODO"
          };
          Loc => {s = \\_,_ => "TODO"; gen = \\_, _ => "TODO"};
          Ablat => {s = \\_,_ => "TODO"; gen = \\_, _ => "TODO"};
          Abess _ => {s = \\_,_ => "TODO"; gen = \\_, _ => "TODO"}
        };


    AdjCN ap cn = {
      s = \\n,c => ap.s ! Sg ! Nom ++ cn.s ! n ! c;
      gen = \\n, a => ap.s ! Sg ! Nom ++ cn.gen ! n ! a
      } ;

    -- lin CN = {s : Number => Case => Str; gen : Number => Agr => Str} ;
    AdvCN cn adv = {
      s = \\n, c => adv.s ++ cn.s ! n ! c;
      gen = \\n, a => adv.s ++ cn.gen ! n ! a
    } ;

    AdvNP np adv = {
      s = \\c => adv.s ++ np.s ! c;
      a = np.a
    } ;

    AdNum adn num = {
      s = \\n,c => num.s ! n ! adn.c ++ adn.s
    } ;

    AdjDAP det ap = {
      s = \\n,c => det.s ! n ! c ++ ap.s ! n ! c
    } ;

    -- TODO: further test how correct this is.
    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ np.s ! c ;
      gen = cn.gen
    } ;

    ComplN3 f x = {
      s       = \\n, c => x.s ! Ablat ++ f.s ! n ! c ;
      gen     = f.gen ;
      c       = f.c2 ;
      harmony = f.harmony ;
    } ;

    -- TODO: not entirely sure `CountNP` is sensible in Turkish but it should
    -- look something like this.
    CountNP det np = {
      s = \\c => det.s ++ np.s ! Ablat ;
      a = np.a
    } ;

    DetDAP d = { s = \\n,c => d.s } ;

    -- TODO: further check the correctness of this.
    DetNP det = {
      s = \\c => det.s ;
      a = {n = det.n ; p = P1}
    } ;

    ExtAdvNP np adv = {
      s = \\c => np.s ! c ++ "," ++ adv.s ;
      a = np.a
    } ;

    NumDigits n = {
      s = n.s ! NCard ; n = n.n
    } ;

    OrdNumeralSuperl n a = {
      s = \\num,cs => (n.s ! NOrd ! Sg ! cs) ++ a.s ! Sg ! cs
    } ;

    PPartNP np v2 = {
      s = \\c => np.s ! c ++ v2.s ! (VPast np.a);
      a = np.a
    } ;

    -- TODO: some tweaking will be needed when getting the
    -- getting genitive constructions to work correctly.
    --
    -- Further check the correctness.
    PartNP cn np = {
      s   = \\n,c => np.s ! Gen ++ cn.s ! n ! Gen ;
      gen = cn.gen
    } ;

    PossNP cn np = {
      s   = \\n,c => np.s ! Gen ++ cn.s ! n ! c ;
      gen = cn.gen
    } ;

    -- TODO: currently I am not able to generate trees for this but should be
    -- quite close what is needed.
    PredetNP pred np = {
      s = \\c => pred.s ++ np.s ! c ;
      a = np.a
    } ;

    SentCN cn sc = {
      s = \\n,c => "(TODO: SentCN)" ;
      gen = cn.gen
    } ;

    -- TODO: currently not able to generate trees.
    RelCN cn rs = {
      s   = \\n,c => "(TODO: RelCN)" ;
      gen = cn.gen
    } ;

    RelNP np rs = {
      s   = \\c => "(TODO: RelNP)" ;
      gen = np.gen ;
      a   = np.a ;
      c   = np.c
    } ;

}
