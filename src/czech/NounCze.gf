concrete NounCze of Noun =
  CatCze
**

  open ResCze, Prelude in {

lin
    DetCN det cn = {
        s,prep,clit = \\c => det.s ! nounGender cn (numSizeNumber det.size) ! c ++ numSizeForm cn.s det.size c ;
        a = numeralAgr (nounGender cn (numSizeNumber det.size)) det P3 ;
        hasClit = False ; isDrop = False ;
      } ;

    MassNP cn = {
      s,prep,clit = \\c => cn.s ! Sg ! c ;
      a = Ag cn.g Sg P3 ;
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

    UsePron pron = {
      s = table {
        Nom | ResCze.Voc => pron.nom ;
        Gen => pron.gen ;
        Dat => pron.dat ;
        Acc => pron.acc ;
        Loc => pron.loc ;
        Ins => pron.ins
        } ;
      clit = table {
        Nom => pron.cnom ;
        ResCze.Voc => pron.nom ;
        Gen => pron.cgen ;
        Dat => pron.cdat ;
        Acc => pron.cacc ;
        Loc => pron.loc ;
        Ins => pron.ins
        } ;
      prep = table {
        Nom | ResCze.Voc => pron.nom ;
        Gen => pron.pgen ;
        Dat => pron.pdat ;
        Acc => pron.pacc ;
        Loc => pron.loc ;
        Ins => pron.pins
        } ;
      a = pron.a ;
      hasClit = True ; isDrop = pron.isDrop ;
      } ;

    PossPron pron = justDemPronFormsAdjective pron.poss ;

    UsePN pn = {
      s,clit,prep = \\c => pn.s ! c ;
      a = Ag pn.g Sg P3 ;
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

    AdvNP np adv = {
      s,clit = \\c => np.s ! c ++ adv.s ;
      prep = \\c => np.prep ! c ++ adv.s ;
      a = np.a ;
      hasClit = False ; isDrop = False ;
      } ;

    UseN n = nounFormsNoun n ;

    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ np.s ! c ; ---- TODO check apposition order
      g = cn.g ; gPl = cn.gPl
      } ;

    NumCard c = c ;
    NumDigits ds = invarDeterminer ds.s ds.size ;
    NumDecimal ds = invarDeterminer ds.s ds.size ;
    NumNumeral nu = nu ;

    SentCN cn sc = cn ** {s = \\n,c => cn.s ! n ! c ++ sc.s} ;

    PredetNP pred np = np ** {
      -- A predeterminer modifies a full NP: jen já, jen jeho. Its scope
      -- cannot be preserved by an omitted subject or an object clitic.
      s,clit = \\c => pred.s ++ np.s ! c ;
      prep = \\c => pred.s ++ np.prep ! c ;
      hasClit = False ; isDrop = False
      } ;

}
