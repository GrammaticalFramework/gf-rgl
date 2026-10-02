concrete NounCze of Noun =
  CatCze
**

  open ResCze, Prelude in {

lin
    DetCN det cn =
      let s : Case => Str = \\c => det.s ! nounGender cn (numSizeNumber det.size) ! c ++ numSizeForm cn.s det.size c ;
          agr = numeralAgr (nounGender cn (numSizeNumber det.size)) det P3
      in npForms s s ** {
        clit = s ;
        a = agr ;
        m = numeralModAgr (nounGender cn (numSizeNumber det.size)) det ;
        hasClit = False ; isDrop = False ; isPron = False ;
      } ;

    MassNP cn =
      let s = cn.s ! Sg in npForms s s ** {
      clit = s ;
      a = Ag cn.g Sg P3 ; m = Mod cn.g Sg ; isPron = False ;
      hasClit = False ; isDrop = False ;
      } ;

    DetQuant = quantifyNumeral ;

    OrdSuperl a = adjFormsAdjective a.superl ;
    -- With a scale head, the quantifier modifies the scale, but the following
    -- ordinal modifies the counted noun: s tímto tisícem nejlepších korun.
    DetQuantOrd quant num ord =
      let det = quantifyNumeral quant num in det ** {
      s = \\g,c => det.s ! g ! c ++
        ord.s ! g ! numSizeNumber num.size ! countCase num.size c
      } ;

    DefArt = {s = \\_,_,_ => []} ;
    IndefArt = {s = \\_,_,_ => []} ;
    NumPl = invarDeterminer [] Num2_4 ;
    NumSg = invarDeterminer [] Num1 ;

    UsePron pron =
      let s : Case => Str = table {
        Nom | ResCze.Voc => pron.nom ;
        Gen => pron.gen ;
        Dat => pron.dat ;
        Acc => pron.acc ;
        Loc => pron.loc ;
        Ins => pron.ins
        } ;
      prep : Case => Str = table {
        Nom | ResCze.Voc => pron.nom ;
        Gen => pron.pgen ;
        Dat => pron.pdat ;
        Acc => pron.pacc ;
        Loc => pron.loc ;
        Ins => pron.pins
        }
      in npForms s prep ** {
      clit = table {
        Nom => pron.cnom ;
        ResCze.Voc => pron.nom ;
        Gen => pron.cgen ;
        Dat => pron.cdat ;
        Acc => pron.cacc ;
        Loc => pron.loc ;
        Ins => pron.ins
        } ;
      a = pron.a ; m = modifierAgr pron.a ;
      hasClit = True ; isDrop = pron.isDrop ; isPron = True ;
      } ;

    PossPron pron = justDemPronFormsAdjective pron.poss ;

    UsePN pn = npForms pn.s pn.s ** {
      clit = pn.s ;
      a = Ag pn.g Sg P3 ; m = Mod pn.g Sg ; isPron = False ;
      hasClit = False ; isDrop = False ;
      } ;

    AdjCN ap cn = {
      s = \\n,c => preOrPost (notB ap.isPost) (ap.s ! nounGender cn n ! n ! c) (cn.s ! n ! c) ;
      g = cn.g ; gPl = cn.gPl
      } ;

    RelCN cn rs = {
      s = \\n,c => cn.s ! n ! c ++ rs.s ! Ag (nounGender cn n) n P3 ;
      g = cn.g ; gPl = cn.gPl
      } ;

    AdvCN cn adv = {
      s = \\n,c => cn.s ! n ! c ++ adv.s ;
      g = cn.g ; gPl = cn.gPl
      } ;

    AdvNP np adv =
      let forms = appendNPForms np adv.s in np ** forms ** {
      clit = forms.s ;
      hasClit = False ; isDrop = False ;
      } ;

    UseN n = nounFormsNoun n ;

    UseN2 n = nounFormsNoun n ;

    ComplN2 n np =
      let cn = nounFormsNoun n ;
          obj = fullComplement n.c2 np.s np.prep
      in cn ** {s = \\num,c => cn.s ! num ! c ++ obj} ;

    PossNP cn np = cn ** {s = \\n,c => cn.s ! n ! c ++ np.s ! Gen} ;
    PartNP cn np = cn ** {s = \\n,c => cn.s ! n ! c ++ np.s ! Gen} ;

    DetNP det =
      let forms : Case => Str = \\c => det.s ! Masc Inanim ! c ;
          agr = numeralAgr (Masc Inanim) det P3 in
      npForms forms forms ** {
        clit = forms ; a = agr ;
        m = numeralModAgr (Masc Inanim) det ;
        hasClit = False ; isDrop = False ; isPron = False
      } ;

    AdNum ad num = num ** {s = \\g,c => ad.s ++ num.s ! g ! c} ;

    DetDAP det = det ;
    AdjDAP dap ap = dap ** {
      s = \\g,c => dap.s ! g ! c ++ ap.s ! g ! numSizeNumber dap.size ! c
      } ;

    CountNP det np =
      let forms : Case => Str = \\c => det.s ! Neutr ! c ++ np.s ! Gen ;
          agr = numeralAgr Neutr det P3 in
      npForms forms forms ** {
        clit = forms ; a = agr ; m = Mod Neutr Pl ;
        hasClit = False ; isDrop = False ; isPron = False
      } ;

    OrdNumeral num = {
      s = \\g,n,c => num.s ! g ! c ;
      pred = \\_ => num.s ! Neutr ! Nom ;
      isPost = False
      } ;

    OrdDigits ds = {
      s = \\_,_,_ => ds.s ++ SOFT_BIND ++ "." ;
      pred = \\_ => ds.s ++ SOFT_BIND ++ "." ; isPost = False
      } ;

    ExtAdvNP np adv = AdvNP np adv ;
    QuantityNP decimal unit =
      let forms : Case => Str = \\_ => decimal.s ++ unit.s in
      npForms forms forms ** {
        clit = forms ; a = AgQuant Neutr ; m = Mod Neutr Pl ;
        hasClit = False ; isDrop = False ; isPron = False
      } ;

    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ np.s ! c ; ---- TODO check apposition order
      g = cn.g ; gPl = cn.gPl
      } ;

    NumCard c = c ;
    NumDigits ds = invarDeterminer ds.s ds.size ;
    NumDecimal ds = invarDeterminer ds.s ds.size ;
    NumNumeral nu = nu ;

    SentCN cn sc = cn ** {s = \\n,c => cn.s ! n ! c ++ sc.s} ;

    PredetNP pred np =
      let forms = predetNPForms (andB pred.postPron np.isPron)
        (\\c => predetForm pred np.m c) np
      in np ** forms ** {
      -- A predeterminer modifies a full NP: jen já, jen jeho. Its scope
      -- cannot be preserved by an omitted subject or an object clitic.
      clit = forms.s ;
      hasClit = False ; isDrop = False
      } ;

}
