--# -path=alltenses:../common:../abstract:../romance
concrete ExtendCat of Extend = CatCat ** ExtendRomanceFunctor -
  [EmptyRelSlash,
   BaseVPI, ConsVPI, ConjVPI, MkVPI, ComplVPIVV,
   BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
   ProgrVPSlash, ReflPoss, AdvRNP, AdvRVP, AdvRAP, PossPronRNP,
   CompoundAP, InOrderToVP]
  -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarCat), (Syntax = SyntaxCat), (ResRomance = ResCat) **
  open
  GrammarCat,
  ResCat,
  MorphoCat,
  Coordination,
  Prelude,
  ParadigmsCat,
  (P = ParamX) in {
    -- put your own definitions here

  lin EmptyRelSlash = RelSlash IdRP ;

  lincat
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;

  lin
    MkVPI vp = {s = \\a => infVP vp RPos a} ;
    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    ConjVPI = conjunctDistrTable Agr ;
    ComplVPIVV vv vpi =
      insertComplement (\\a => prepCase vv.c2.c ++ vpi.s ! a) (predV vv) ;

  lincat [Comp] = {s1,s2 : Agr => Str ; cop : CopulaType} ;

  lin
    BaseComp x y = twoTable Agr x y ** {cop = x.cop} ;
    ConsComp xs x = consrTable Agr comma xs x ** xs ;
    ConjComp conj cs = conjunctDistrTable Agr conj cs ** {cop = cs.cop} ;

  lincat [Imp] = {s1,s2 : RPolarity => P.ImpForm => Gender => Str} ;

  lin
    BaseImp = twoTable3 RPolarity P.ImpForm Gender ;
    ConsImp = consrTable3 RPolarity P.ImpForm Gender comma ;
    ConjImp = conjunctDistrTable3 RPolarity P.ImpForm Gender ;

    ProgrVPSlash vps = GrammarCat.ProgrVP (lin VP vps) ** {c2 = vps.c2} ;

    ReflPoss num cn = {
      s = \\agr,c => possCase cn.g num.n c ++ he_Pron.poss ! num.n ! cn.g ++ cn.s ! num.n
      } ;
    AdvRNP np prep rnp = {
      s = \\agr,c => (np.s ! c).ton ++ prep.s ++ rnp.s ! agr ! prep.c
      } ;
    AdvRVP vp prep rnp =
      insertComplement (\\a => prep.s ++ rnp.s ! verbAgr a ! prep.c) vp ;
    AdvRAP ap prep rnp = ap ** {
      s = \\af => ap.s ! af ++ prep.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! prep.c ;
      isPre = False
      } ;
    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => possCase cn.g num.n c ++ pron.poss ! num.n ! cn.g ++
                  cn.s ! num.n ++ rnp.s ! pron.a ! ResCat.genitive ;
      a = agrP3 cn.g num.n ;
      hasClit = False ;
      isNeg = False
      } ;

    CompoundAP noun adj = {
      s = \\af => adj.s ! af ++ "de" ++ noun.s ! (aform2number af) ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    InOrderToVP vp = {s = "per tal de" ++ infStr vp} ;



} ;
