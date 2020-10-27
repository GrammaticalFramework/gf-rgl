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
      h   = cn.h ;
      a = agrP3 det.n
      } ;

    UsePron p = p ;

    UsePN pn = { 
      s = \\c => pn.s ! Sg ! c;
      h = pn.h;
      a = {n = Sg; p = P3}
    } ;

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

    Use2N3 n = variants {} ;

    MassNP cn = {
      s = cn.s ! Sg;
      h = cn.h;
      a = { n = Sg; p = P1 }
    } ;

    ComplN2 f x =
        case f.c.c of {
          Nom => {
            s = \\n, c => x.s ! Gen ++ f.s ! n ! Acc;
            gen = \\_, _ => "TODO";
            h = f.h
          };
          Acc => {
            s = \\_,_ => "TODO";
            gen = \\_, _ => "TODO";
            h = f.h};
          Gen => {
            s =
              \\n, c =>
                x.s ! Gen ++ f.gen ! n ! {n = Sg; p = P3}
                ++ BIND ++ (caseSuffixes ! c).st ! f.h.con ! f.h.vow;
            gen = \\_, _ => "TODO";
            h = f.h
          };
          Dat => {
            s = \\n, c =>
              x.s ! Gen ++ f.gen ! n ! {n = Sg; p = P3}
                ++ datSuffixN.st ! f.h.con ! f.h.vow;
            gen = \\_, _ => "TODO";
            h = f.h
          };
          Loc => {
            s = \\_,_ => "TODO";
            gen = \\_, _ => "TODO";
            h = f.h
          };
          Ablat => {
            s = \\_,_ => "TODO";
            gen = \\_, _ => "TODO";
            h = f.h
          };
          Abess _ => {
            s = \\_,_ => "TODO";
            gen = \\_, _ => "TODO";
            h = f.h
          }
        };


    AdjCN ap cn = {
      s = \\n,c => ap.s ! Sg ! Nom ++ cn.s ! n ! c;
      gen = \\n, a => ap.s ! Sg ! Nom ++ cn.gen ! n ! a;
      h   = cn.h
      } ;

    -- lin CN = {s : Number => Case => Str; gen : Number => Agr => Str} ;
    AdvCN cn adv = {
      s = \\n, c => adv.s ++ cn.s ! n ! c;
      gen = \\n, a => adv.s ++ cn.gen ! n ! a;
      h   = cn.h
    } ;

    AdvNP np adv = {
      s = \\c => adv.s ++ np.s ! c;
      h = np.h ;
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
      s   = \\n,c => cn.s ! n ! c ++ np.s ! c ;
      gen = cn.gen ;
      h   = cn.h
    } ;

    ComplN3 f x = {
      s   = \\n, c => x.s ! Ablat ++ f.s ! n ! c ;
      gen = f.gen ;
      c   = f.c2 ;
      h   = f.h ;
    } ;

    -- TODO: not entirely sure `CountNP` is sensible in Turkish but it should
    -- look something like this.
    CountNP det np = {
      s = \\c => det.s ++ np.s ! Ablat ;
      h = np.h ;
      a = np.a
    } ;

    DetDAP d = { s = \\n,c => d.s } ;

    -- TODO: further check the correctness of this.
    DetNP det = {
      s = \\c => det.s ;
      h = {vow=I_Har; con=SCon Soft} ;  -- to be fixed
      a = {n = det.n ; p = P1}
    } ;

    ExtAdvNP np adv = {
      s = \\c => np.s ! c ++ "," ++ adv.s ;
      a = np.a ;
      h = np.h
    } ;

    NumDigits n = {
      s = n.s ! NCard ; n = n.n
    } ;

    OrdNumeralSuperl n a = {
      s = \\num,cs => (n.s ! NOrd ! Sg ! cs) ++ a.s ! Sg ! cs
    } ;

    PPartNP np v2 = {
      s = \\c => np.s ! c ++ v2.s ! (VPast np.a);
      h = np.h ;
      a = np.a
    } ;

    -- TODO: some tweaking will be needed when getting the
    -- getting genitive constructions to work correctly.
    --
    -- Further check the correctness.
    PartNP cn np = {
      s   = \\n,c => np.s ! Gen ++ cn.s ! n ! Gen ;
      gen = cn.gen ;
      h   = cn.h
    } ;

    PossNP cn np = {
      s   = \\n,c => np.s ! Gen ++ cn.s ! n ! c ;
      gen = cn.gen ;
      h   = cn.h
    } ;

    -- TODO: currently I am not able to generate trees for this but should be
    -- quite close what is needed.
    PredetNP pred np = {
      s = \\c => pred.s ++ np.s ! c ;
      h = np.h ;
      a = np.a
    } ;

    SentCN cn sc = {
      s   = \\n,c => "(TODO: SentCN)" ;
      gen = cn.gen ;
      h   = cn.h
    } ;

    -- TODO: currently not able to generate trees.
    RelCN cn rs = {
      s   = \\n,c => "(TODO: RelCN)" ;
      gen = cn.gen ;
      h   = cn.h
    } ;

    RelNP np rs = {
      s   = \\c => "(TODO: RelNP)" ;
      gen = np.gen ;
      h   = np.h ;
      a   = np.a ;
      c   = np.c
    } ;

}
