--# -path=.:../romance:../abstract:../common:../prelude

instance DiffPor of DiffRomance - [iAdvQuestionInv,chooseTA,otherInv,partAgr,stare_V,vpAgrSubj,vpAgrClits] = open CommonRomance, PhonoPor, BeschPor, Prelude in {

  flags optimize=noexpand ;
        coding=utf8 ;

  param
    Prepos = P_de | P_a | P_em | P_por ;

    VType = VTer | VHaver | VRefl ;

  oper
    partAgr : VType -> Bool ;
    -- exception
    partAgr _ = False ;

    vpAgrSubj : Verb -> VPAgrType ;
    -- exception
    vpAgrSubj v = <verbDefaultPart v, False> ;

  oper
    conjunctCase : Case -> Case = \c -> case c of {
      Nom => Nom ;
      _ => Acc
      } ;

  oper
    clitInf b cli inf = inf ++ bindIf b ++ cli ;

  oper
    pronArg = \n,p,acc,dat ->
      let
        paccp = case acc of {
          CRefl   => <reflPron n p Acc, p,True> ;
          CPron ag an ap => <argPron ag an ap Acc, ap,True> ;
          _ => <[],P2,False>
          } ;
        pdatp = case dat of {
          CPron ag an ap => <argPron ag an ap dative, ap,True> ;
          _ => <[],P2,False>
          } ;
        peither = case acc of {
          CRefl | CPron _ _ _ => True ;
          _ => case dat of {
            CPron _ _ _ => True ;
            _ => False
            }
          } ;
        defaultPronArg = <pdatp.p1 ++ paccp.p1, [], peither>
      in
  ---- 8/6/2008 efficiency problem in pgf generation: replace the case
  ---- expr with a constant produces an error in V3 predication with
  ---- two pronouns
      defaultPronArg ;

  oper
    mkImperative isPol p vp =
      \\pol,g,n => case pol of {
        RPos   => neg.p1 ++ imper ++ bindHyphenIf refl.isRefl ++ refl.pron
          ++ bindHyphenIf hasClit ++ clit ++ compl ;
        RNeg _ => neg.p1 ++ refl.pron ++ clit ++ subj ++ compl
      } where {
        pe : Person  = case <isPol,p> of {<_,P1> => P1 ; <True,_> => P2 ; <_,_> => P3} ;
        refl = case vp.s.vtyp of {
          VRefl => {pron = reflPron n pe Acc ; isRefl = True} ;
          _     => {pron = [] ; isRefl = False}
          } ;
        clit    = vp.clit1 ++ vp.clit2 ;
        hasClit = vp.clit3.hasClit ;
        imper   = vp.s.s ! vImper n pe ;
        subj    = vp.s.s ! VFin (VPres Conjunct) n pe ;
        neg     = vp.neg ! pol ;
        agr     = {g = g ; n = n ; p = pe} ;
        compl   = neg.p2 ++ vp.comp ! agr ++ vp.ext ! pol
      } ;

    bindHyphenIf : Bool -> Str ;
    bindHyphenIf b = case b of {
      True => BIND ++ "-" ++ BIND ;
      _    => []
      } ;

  param
    Copula = SerCop | EstarCop | FicarCop ;

  oper
    CopulaType = Copula ;
    selectCopula coptyp = case coptyp of {
      SerCop => essere_V ;
      EstarCop => stare_V ;
      FicarCop => ficar_V
      } ;

    serCopula = SerCop ;
    estarCopula = EstarCop ;
    ficarCopula = FicarCop ;

  oper
    iAdvQuestionInv : Direct = DDir ;
    otherInv : Direct = DDir ;

  oper
    -- the other Cases are defined in ResRomance
    dative   : Case = CPrep P_a ;
    genitive : Case = CPrep P_de ;
    locative : Case = CPrep P_em ;
    ablative : Case = CPrep P_por ;

  oper
    vRefl _ = VRefl ;
    isVRefl : VType -> Bool = \ty -> case ty of {
      VRefl => True ;
      _ => False
      } ;

  oper
    prepCase = \c -> case c of {
      Nom => [] ;
      Acc => [] ;
      CPrep P_de => "de" ;
      CPrep P_a  => "a" ;
      CPrep P_em => "em" ;
      CPrep P_por => "por"
      } ;

  oper
    partitive = \_,c -> prepCase c ;

  oper
    a : Gender => Number => Str ;
    a = genNumForms "ao" "à" "aos" "às" ;

    de : Gender => Number => Str ;
    de = genNumForms "do" "da" "dos" "das" ;

    artDef : Bool -> Gender -> Number -> Case -> Str ;
    -- not sure if isNP is relevant
    artDef _isNP g n c = case c of {
      Nom | Acc => genNumForms "o" "a" "os" "as" ;
      CPrep P_de => de ;
      CPrep P_a => a ;
      CPrep P_em => genNumForms "no" "na" "nos" "nas" ;
      CPrep P_por => genNumForms "pelo" "pela" "pelos" "pelas"
      } ! g ! n ;

    artIndef = \isNP,g,n,c -> case isNP of {
      True => case n of {
        Sg  => prepCase c ++ genForms "um" "uma" ! g ;
        _   => prepCase c ++ genForms "uns" "umas" ! g
        } ;
      _ => case n of {
        Sg  => prepCase c ++ genForms "um" "uma" ! g ;
        _   => prepCase c
        }
      } ;

  oper
    possCase = \_,_,c -> prepCase c ;

  oper
    auxVerb : VType -> (VF => Str) ;
    auxVerb vt = case vt of {
      VTer   => ter_V.s ;
      VHaver => haver_V.s ;
      _      => ter_V.s
      } ;

    negation : RPolarity => (Str * Str) = table {
      RPos => <[],[]> ;
      RNeg _ => <"não",[]>
      } ;

    copula : Verb = verboV (ser_Besch "ser") ;

  oper
    conjThan = "que" ;
    conjThat = "que" ;

  oper
    subjIf = "se" ;

  oper
    relPron : Bool => AAgr => Case => Str = \\_b,a,c =>
      case c of {
        Nom | Acc => "que" ;
        CPrep P_a => "cujo" ;
        _ => prepCase c ++ "cujo"
      } ;

    pronSuch : AAgr => Str = aagrForms "tal" "tal" "tais" "tais" ;

  oper
    partQIndir = [] ;

  oper
    reflPron : Number -> Person -> Case -> Str = \n,p,c ->
      let pro = argPron Fem n p c
      in
      case p of {
        P3 => case c of {
          Acc | CPrep P_a => "se" ;
          _ => "si"
          } ;
        _ => pro
      } ;

  oper
    auxPassive : Verb = copula ;

  oper
    vpAgrClits : Verb -> AAgr -> VPAgrType ;
    -- exception
    vpAgrClits v a = <verbDefaultPart v, False> ;

  oper
    subjPron = \_ -> [] ;

  oper
    polNegDirSubj = RPos ;

  oper
    infForm _ _ _ _  = True ;

  oper
    vpAgrClit : Agr -> VPAgr = \a ->
      vpAgrNone ;

  oper
    chooseTA : RTense -> Anteriority
      -> (VF => Str) -> (VF => Str)
      -> Number -> Person -> Mood -> Str -> Str * Str ;
    chooseTA t a verb vaux n p m part = case <t,a> of {
    <RPast,Simul>  => <verb ! VFin (VImperf m) n p, []> ; --# notpresent
    <RPast,Anter>  => <vaux ! VFin (VImperf m) n p, part> ; --# notpresent
    <RFut,Simul>   => <verb ! VFin VFut n p, []> ; --# notpresent
    <RFut,Anter>   => <vaux ! VFin VFut n p, part> ; --# notpresent
    <RCond,Simul>  => <verb ! VFin VCondit n p, []> ; --# notpresent
    <RCond,Anter>  => <vaux ! VFin VCondit n p, part> ; --# notpresent
    <RPasse,Simul> => <verb ! VFin VPasse n p, []> ; --# notpresent
    <RPasse,Anter>  => <vaux ! VFin (VPres m) n p, part> ; --# notpresent
    <RPres,Anter>  => <verb ! VFin VPasse n p, []> ; --# notpresent
    <RPres,Simul>  => <verb ! VFin (VPres m) n p, []>
    } ;

-- oper's opers
  oper
    argPron : Gender -> Number -> Person -> Case -> Str =
      let
        cases : (x,y : Str) -> Case -> Str = \me,moi,c -> case c of {
          Acc | CPrep P_a => me ;
          _ => moi
          } ;
        cases3 : (x,y,z : Str) -> Case -> Str = \les,leur,eux,c -> case c of {
          Acc => les ;
          CPrep P_a => leur ;
          _ => eux
          } ;
      in
      \g,n,p -> case <<g,n,p> : Gender * Number * Person> of {
        <_,Sg,P1> => cases "me" "mim" ;
        <_,Sg,P2> => cases "te" "ti" ;
        <_,Pl,P1> => cases "nos" "nós" ;
        <_,Pl,P2> => cases "vos" "vós" ;
        <Fem,Sg,P3> => cases3 "a" "sua" "ela" ;
        <_,  Sg,P3> => cases3 "o" "seu" "ele" ;
        <Fem,Pl,P3> => cases3 "as" "suas" "elas" ;
        <_,  Pl,P3> => cases3 "os" "seus" "eles"
      } ;

    essere_V : Verb = verboV (ser_Besch "ser") ;
    stare_V : Verb = verboV (estar_Besch "estar") ;

    ter_V   : Verb = verboV (ter_Besch "ter") ;
    haver_V : Verb = verboV (haver_Besch "haver") ;
    ficar_V : Verb = verboV (ficar_Besch "ficar") ;

    verboV : Verbum -> Verb ;
    -- make a verb of type haver
    verboV v = verbBesch v ** {vtyp = VTer ; p = [] } ;

} ;
