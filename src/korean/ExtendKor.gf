--# -path=.:../common:../abstract

concrete ExtendKor of Extend = CatKor
  -- ** ExtendFunctor - [ApposNP]
  -- with (Grammar=GrammarKor)
  ** open Prelude, ResKor, NounKor, Coordination, TenseX,
          (G=GrammarKor) in {

  lincat
    VPS, [VPS], VPI, [VPI], RNP, [Comp], [Imp] = SS ;

  lin
    UttAdV adv = {s = adv.s} ;

    -- : NP -> NP -> NP
    ApposNP np1 np2 = np1 ** {s = \\nf => np1.s ! nf ++ np2.s ! nf} ;

    EmptyRelSlash cl = cl ;

    ReflPron = {s="자신"} ;
    ReflPoss n cn = {s="자신의" ++ cn.rs ++ cn.s ! Bare} ;
    AdvRNP np prep rnp = {s=np.s ! Bare ++ rnp.s ++ prep.s ! Vowel} ;
    AdvRVP vp prep rnp = insertAdv vp {s=rnp.s ++ prep.s ! Vowel} ;
    AdvRAP ap prep rnp = ap ** {compar=rnp.s ++ prep.s ! Vowel ++ ap.compar} ;
    PossPronRNP pron n cn rnp = G.DetCN (G.DetQuant (G.PossPron pron) n)
      (cn ** {rs=glue rnp.s "의" ++ cn.rs}) ;

    GenModNP n np cn = G.DetCN (G.DetQuant G.DefArt n)
      (cn ** {rs=glue (np.s ! Bare) "의" ++ cn.rs}) ;
    CompoundN first head = head ** {
      s = \\nf => first.s ! Bare ++ head.s ! nf
      } ;
    CompoundAP n a = a ** {s=\\vf=>n.s ! Bare ++ a.s ! vf; compar=[]} ;
    PresPartAP vp = do_A ** {s=\\vf=>linVP vf vp; compar=[]} ;
    PastPartAP slash = do_A ** {
      s=\\vf=>slash.adv ++ slash.s ! vf; compar=[]
      } ;
    PastPartAgentAP slash agent = do_A ** {
      s=\\vf=>glue (agent.s ! Bare) "에게" ++ slash.adv ++
             slash.s ! vf; compar=[]
      } ;
    PassVPSlash slash = useV slash ** {adv=slash.adv} ;
    PassAgentVPSlash slash agent = insertAdv (PassVPSlash slash)
      {s=glue (agent.s ! Bare) "에게"} ;
    ProgrVPSlash slash = slash ** {
      s=\\vf=>glue (slash.s ! VStem Pos) "고" ++ have_V.s ! vf
      } ;
    GerundCN vp = useN (stringN (linVP (VAttr Pos) vp ++ "것")) ;
    GerundNP vp = stringNP (linVP (VAttr Pos) vp ++ "것") ;
    GerundAdv vp = {s=glue (linVP (VStem Pos) vp) "면서"} ;
    ByVP vp = {s=linVP (VAttr Pos) vp ++ "것으로써"} ;
    PositAdVAdj a = {s=glue (a.s ! VStem Pos) "게"} ;
    ComplBareVS vs s = useV vs ** {adv=s.s ! Subord ++ "고"} ;
    ComplSlashPartLast = G.ComplSlash ;
    UttVPShort = G.UttVP ;

    UseDAP d = d ** {s=d.sp} ;
    UseDAPMasc = UseDAP ;
    UseDAPFem = UseDAP ;

    MkVPS t pol vp = {s=t.s ++ pol.s ++
      vp.nObj ! DeclObj ++ vp.adv ++
      tenseVP t.t t.a (Statement Formal) pol.p vp} ;
    BaseVPS x y = {s=x.s ++ y.s} ;
    ConsVPS x xs = {s=x.s ++ xs.s} ;
    ConjVPS co xs = {s=co.s1 ++ xs.s} ;
    PredVPS np vps = {s=\\_=>np.s ! Subject ++ vps.s; p=Vowel} ;
    MkVPI vp = {s=linVP (VAttr Pos) vp ++ "것"} ;
    BaseVPI x y = {s=x.s ++ y.s} ;
    ConsVPI x xs = {s=x.s ++ xs.s} ;
    ConjVPI co xs = {s=co.s1 ++ xs.s} ;
    ComplVPIVV vv vpi = useV vv ** {adv=glue vpi.s "을"} ;
    BaseImp x y = {s=x.s ! Pos ++ y.s ! Pos} ;
    ConsImp x xs = {s=x.s ! Pos ++ xs.s} ;
    ConjImp co xs = {s=\\_=>co.s1 ++ xs.s} ;
    BaseComp x y = {s=x.s ! VAttr Pos ++ y.s ! VAttr Pos} ;
    ConsComp x xs = {s=x.s ! VAttr Pos ++ xs.s} ;
    ConjComp co xs = emptyComp ** {s=\\_=>co.s1 ++ xs.s} ;

    TPastSimple = TPast ;

  oper
    stringNP : Str -> NounPhrase = \s -> {s=\\_=>s; p=Consonant} ;
    stringN : Str -> Noun = \s -> stringNP s ** {c=baseCounter} ;
} ;
