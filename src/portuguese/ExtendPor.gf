--# -path=alltenses:../common:../abstract:../romance
concrete ExtendPor of Extend = CatPor ** ExtendRomanceFunctor -
  [
     EmptyRelSlash,
     BaseVPI, ConsVPI, ConjVPI, MkVPI, ComplVPIVV,
     RNP, ReflRNP, ReflPron, ReflPoss, PredetRNP,
     AdvRNP, AdvRVP, AdvRAP, ReflA2RNP, PossPronRNP, ReflPossPron,
     ProgrVPSlash,
     CompoundAP,
     CompoundN,
     ExistsNP,
     GenRP,
     GenRP,
     ICompAP,
     InOrderToVP,
     EmbedSSlash,
     WithoutVP,
     iFem_Pron,
     theyFem_Pron,
     weFem_Pron,
     youFem_Pron,
     youPlFem_Pron,
     youPolFem_Pron,
     youPolPlFem_Pron,
     youPolPl_Pron
    ]                   -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarPor), (Syntax = SyntaxPor), (ResRomance = ResPor) **
  open
  MorphoPor,
  Coordination,
  Prelude,
  ParadigmsPor,
  (L = LexiconPor),
  (S = StructuralPor),
  (P = ParamX) in {

  lin
    EmptyRelSlash = RelSlash IdRP ;

  lincat
    RNP = {s : Agr => Case => Str ; isPron : Bool} ;
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;

  lin
    MkVPI vp = {s = \\a => infVP vp RPos a} ;
    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    ConjVPI = conjunctDistrTable Agr ;
    ComplVPIVV vv vpi =
      insertComplement (\\a => prepCase vv.c2.c ++ vpi.s ! a) (predV vv) ;

    -- A progressive wrapper would consume the object gap.  In relative and
    -- interrogative clauses the Portuguese simple form preserves that gap.
    ProgrVPSlash vps = vps ;

    ReflRNP v rnp = case <rnp.isPron,v.c2.isDir> of {
      <True,True> => insertRefl v ;
      _ => insertComplement
             (\\a => let agr = verbAgr a in
                    v.c2.s ++ rnp.s ! agr ! v.c2.c) v
      } ;

    ReflPron = {
      s = \\agr,_ => reflTonicPron agr ;
      isPron = True
      } ;

    ReflPoss num cn = {
      s = \\agr,c => possCase cn.g num.n c ++
                       reflPossDet agr num.n cn.g ++ cn.s ! num.n ;
      isPron = False
      } ;

    PredetRNP pred rnp = {
      s = \\agr,c => pred.s ! complAgr agr ! c ++ rnp.s ! agr ! pred.c ;
      isPron = False
      } ;

    AdvRNP np prep rnp = {
      s = \\agr,c => (np.s ! c).ton ++ prep.s ++ rnp.s ! agr ! prep.c ;
      isPron = False
      } ;

    AdvRVP vp prep rnp =
      insertComplement (\\a => prep.s ++ rnp.s ! verbAgr a ! prep.c) vp ;

    AdvRAP ap prep rnp = ap ** {
      s = \\af => ap.s ! af ++ prep.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! prep.c ;
      isPre = False
      } ;

    ReflA2RNP a rnp = a ** {
      s = \\af => a.s ! af ++ a.c2.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! a.c2.c ;
      isPre = False
      } ;

    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => possCase cn.g num.n c ++ pron.poss ! num.n ! cn.g ++
                  cn.s ! num.n ++ rnp.s ! pron.a ! ResPor.genitive ;
      a = agrP3 cn.g num.n ;
      hasClit = False ;
      isNeg = False
      } ;

    ReflPossPron = PossPron he_Pron ;

  oper
    reflTonicPron : Agr -> Str = \agr -> case <agr.n,agr.p> of {
      <Sg,P1> => "mim" ;
      <Sg,P2> => "ti" ;
      <Pl,P1> => "nós" ;
      <Pl,P2> => "vós" ;
      <_,P3> => "si"
      } ;

    reflPossDet : Agr -> Number -> Gender -> Str = \agr,n,g ->
      case <agr.n,agr.p> of {
        <Sg,P1> => genNumForms "meu" "minha" "meus" "minhas" ! g ! n ;
        <Sg,P2> => genNumForms "teu" "tua" "teus" "tuas" ! g ! n ;
        <Sg,P3> => genNumForms "seu" "sua" "seus" "suas" ! g ! n ;
        <Pl,P1> => genNumForms "nosso" "nossa" "nossos" "nossas" ! g ! n ;
        <Pl,P2> => genNumForms "vosso" "vossa" "vossos" "vossas" ! g ! n ;
        <Pl,P3> => genNumForms "seu" "sua" "seus" "suas" ! g ! n
        } ;

  lin
    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV exist_V)) ;

    GenRP nu cn = {
      s = \\_b,_aagr,_c => cujo ! g ! n ++ num ++ cn.s ! n ;
      a = aagr g n ;
      hasAgr = True
      } where {
        cujo = genNumForms "cujo" "cuja" "cujos" "cujas" ;
        g = cn.g ;
        n = nu.n ;
        num = if_then_Str nu.isNum (nu.s ! g) []
      } ;

  lin
    ICompAP ap = {
      s =\\a => "o quão" ++ ap.s ! (genNum2Aform a.g a.n) ;
      cop = serCopula
      } ;

    IAdvAdv adv = {
      s = "o quão" ++ adv.s
      } ;

  oper
    exist_V : V ;
    exist_V = mkV "existir" ;

  lin
    EmbedSSlash s = {s = \\_ => "o que" ++ s.s ! {g = Masc ; n = Sg} ! Indic} ;

  lin
    CompoundN noun noun2 = { -- order is different because that's needed for correct translation from english
      s = \\n => noun2.s ! n ++
                 case noun2.relType of {
                   NRelPrep p => artDef True noun.g Sg (CPrep p) ;  -- tasa de suicidio
                   NRelNoPrep => []                        -- connessione internet = internet connection
                 } ++
                 noun.s ! Sg ;
      g = noun2.g ;
      relType = noun2.relType
      } ;

    CompoundAP noun adj = {
      s = \\af => case (aform2aagr af) of {
        {n = n} => adj.s ! genNum2Aform noun.g n ++ "de" ++ noun.s ! n
        } ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    WithoutVP vp = {
      s = "sem" ++ infStr vp
      } ;

    InOrderToVP vp = {
      s = "a fim de" ++ infStr vp
      } ;

  lincat ListComp = {s1,s2 : Agr => Str ; cop : CopulaType} ;

  lin
    -- should one allow different copulas?
    BaseComp x y = twoTable Agr x y ** {cop = x.cop } ;
    ConsComp xs x = consrTable Agr comma xs x ** xs ;
    ConjComp conj cs = conjunctDistrTable Agr conj cs ** {cop = cs.cop} ;

  lincat ListImp = {s1,s2 : RPolarity => P.ImpForm => Gender => Str} ;

  lin
    BaseImp = twoTable3 RPolarity P.ImpForm Gender ;
    ConsImp = consrTable3 RPolarity P.ImpForm Gender comma ;
    ConjImp conj is = conjunctDistrTable3 RPolarity P.ImpForm Gender conj is ;

  lin
    iFem_Pron = pronAgr S.i_Pron Fem Sg P1 ;
    weFem_Pron = pronAgr S.we_Pron Fem Pl P1 ;
    youFem_Pron = pronAgr S.youSg_Pron Fem Sg P3 ;
    youPlFem_Pron = pronAgr S.youPl_Pron Fem Pl P3 ;
    youPolPl_Pron = mkPronoun "vós" "vos" "vos" "vós"
      "vosso" "vossa" "vossos" "vossas"
      Masc Pl P2 ;
    youPolFem_Pron = pronAgr S.youPol_Pron Fem Sg P2 ;
    youPolPlFem_Pron = pronAgr youPolPl_Pron Fem Pl P2 ;
    theyFem_Pron = mkPronFrom S.they_Pron "elas" "as" "lhes" "elas" Fem Pl P3 ;

} ;
