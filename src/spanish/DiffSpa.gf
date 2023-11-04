--# -path=.:../romance:../abstract:../common:prelude

instance DiffSpa of DiffRomance - [iAdvQuestionInv,otherInv,partAgr,stare_V,vpAgrSubj,vpAgrClits,contractInf] = open CommonRomance, PhonoSpa, BeschSpa, Prelude in {

  flags optimize=noexpand ;
    coding=utf8 ;

---- exceptions ----------------
  oper
    partAgr : VType -> Bool = \vtyp -> False ;
    vpAgrSubj : Verb -> VPAgrType = \v -> <verbDefaultPart v, False> ;
    vpAgrClits : Verb -> AAgr -> VPAgrType = \v,a -> <verbDefaultPart v, False> ;
    contractInf : Bool -> Bool -> Bool = orB ; --- the effect of this is that the clitic is bound to the infinitive: matarte
--------------------------------

  param
    Prepos = P_de | P_a ;
    VType = VHabere | VRefl ;

  oper
    dative   : Case = CPrep P_a ;
    genitive : Case = CPrep P_de ;

    prepCase = \c -> case c of {
      Nom => [] ;
      Acc => [] ;
      CPrep P_de => "de" ;
      CPrep P_a  => "a"
      } ;


    artDef : Bool -> Gender -> Number -> Case -> Str = \isNP,g,n,c ->
      case isNP of {
       True => case <g,n,c> of {
        <Masc,Sg, _>          => prepCase c ++ "el" ;
	<Fem, Sg, _> => prepCase c ++ "la" ; ----- ??
        <Masc,Pl, _> => prepCase c ++ "los" ;
        <Fem ,Pl, _> => prepCase c ++ "las"
        } ;
      _ => case <g,n,c> of {
        <Masc,Sg, CPrep P_de> => "del" ;
        <Masc,Sg, CPrep P_a>  => "al" ;
        <Masc,Sg, _>          => prepCase c ++ "el" ;
        <Fem ,Sg, CPrep P_de> => chooseDeLa ;
        <Fem ,Sg, CPrep P_a>  => chooseALa ;
	<Fem, Sg, _> => prepCase c ++ chooseLa ;
        <Masc,Pl, _> => prepCase c ++ "los" ;
        <Fem ,Pl, _> => prepCase c ++ "las"
        }
      } ;

    artIndef = \isNP,g,n,c -> case isNP of {
      True => case n of {
        Sg  => prepCase c ++ genForms "uno"  "una" ! g ;
        _   => prepCase c ++ genForms "unos" "unas" ! g
        } ;
      _ => case n of {
        Sg  => prepCase c ++ genForms "un"   "una" ! g ;
        _   => prepCase c
        }
      } ;

    possCase = \_,_,c -> prepCase c ;

    partitive = \_,c -> prepCase c ;

{-
    partitive = \g,c -> case c of {
      CPrep P_de => "de" ;
      _ => prepCase c ++ artDef g Sg (CPrep P_de)
      } ;
-}

-- AForm and comparatives
  param
    AFormComplex = AF Gender Number | AAttr Gender | AA ;
  oper
    AForm = AFormComplex ;
    aform2aagr : AForm -> AAgr = \a -> case a of {
      DiffSpa.AF g n => aagr g n ;
      _              => aagr Masc Sg -- "le plus lentement"
      } ;
    genNum2Aform = \g,n -> DiffSpa.AF g n ;
    genNumPos2Aform : Gender -> Number -> Bool -> AForm = \g,n,isPre ->
      case <n,isPre> of {
        <Sg,True> => AAttr g ;
        _         => genNum2Aform g n
      } ;
    piuComp = "más" ;

    conjunctCase : Case -> Case = \c -> case c of {
      Nom => Nom ;
      _ => Acc
      } ;

    auxVerb : VType -> (VF => Str) = \_ -> haber_V.s ;

    vpAgrClit : Agr -> VPAgr = \a ->
      vpAgrNone ;

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
----        defaultPronArg = <pdatp.p1 ++ paccp.p1, [], orB paccp.p3 pdatp.p3>
      in
      ----  case <<paccp.p2, pdatp.p2> : Person * Person> of {
      ----     <P3,P3> => <"se" ++ paccp.p1, [], True> ;
      ----     _ => defaultPronArg
      ---     } ;
      ---- 8/6/2008 efficiency problem in pgf generation: replace the case expr with
      ---- a constant produces an error in V3 predication with two pronouns
         defaultPronArg ;

    infForm _ _ _ _  = True ;


    mkImperative isPol p vp =
      \\pol,g,n => case pol of {
        RPos   => neg.p1 ++ imper ++ bindIf refl.isRefl ++ refl.pron
          ++ bindIf hasClit ++ clit ++ particle ++ compl ;
        RNeg _ => neg.p1 ++ refl.pron ++ clit ++ subj ++ particle ++ compl
      } where {
        pe   = case isPol of {True => P3 ; _ => p} ;
        refl = case vp.s.vtyp of {
          VRefl => {pron = reflPron n pe Acc ; isRefl = True} ;
          _     => {pron = [] ; isRefl = False}
          } ;
        particle = vp.s.p ;
        clit    = vp.clit1 ++ vp.clit2 ;
        hasClit = vp.clit3.hasClit ;
        imper   = vp.s.s ! vImper n pe ;
        subj    = vp.s.s ! VFin (VPres Conjunct) n pe ;
        neg     = vp.neg ! pol ;
        agr     = {g = g ; n = n ; p = pe} ;
        compl   = neg.p2 ++ vp.comp ! agr ++ vp.ext ! pol
      } ;


    CopulaType = Bool ;
    selectCopula = \isEstar -> case isEstar of {True => estar_V ; False => copula} ;
    serCopula = False ;
    estarCopula = True ;

    iAdvQuestionInv : Direct = DDir ;
    otherInv : Direct = DDir ;

    negation : RPolarity => (Str * Str) = table {
      RPos => <[],[]> ;
      RNeg _ => <"no",[]>
      } ;

    conjThan = "que" ;
    conjThat = "que" ;
    subjIf = "si" ;


    clitInf b cli inf = inf ++ bindIf b ++ cli ;

    relPron : Bool => AAgr => Case => Str = \\b,a,c =>
      case c of {
        Nom | Acc => "que" ;
        CPrep P_de => "cuyo" ;
        _ => prepCase c ++ "que"
        } ;

    pronSuch : AAgr => Str = aagrForms "tál" "tál" "tales" "tales" ;

    quelPron : AAgr => Str = aagrForms "cuál" "cuál" "cuales" "cuales" ;

    partQIndir = [] ; ---- ?

    reflPron : Number -> Person -> Case -> Str = \n,p,c ->
        let pro = argPron Fem n p c
        in
        case p of {
        P3 => case c of {
          Acc | CPrep P_a => "se" ;
          _ => "sí"
          } ;
        _ => pro
        } ;

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
        <_,Sg,P1> => cases "me" "mí" ;
        <_,Sg,P2> => cases "te" "tí" ;
        <_,Pl,P1> => cases "nos" "nosotras" ; --- nosotros
        <_,Pl,P2> => cases "vos" "vosotras" ; --- vosotros
        <Fem,Sg,P3> => cases3 "la" "le" "ella" ;
        <_,  Sg,P3> => cases3 "lo" "le" "él" ;
        <Fem,Pl,P3> => cases3 "las" "les" "ellas" ;
        <_,  Pl,P3> => cases3 "los" "les" "ellos"
        } ;

    vRefl _ = VRefl ;
    isVRefl : VType -> Bool = \ty -> case ty of {
      VRefl => True ;
      _ => False
      } ;

    copula, essere_V : Verb = verbBeschH (ser_1 "ser") ;

    estar_V, stare_V, auxPassive : Verb = verbBeschH (estar_2 "estar") ;

    haber_V : Verb = verbBeschH (haber_3 "haber") ;

    verbBeschH : Verbum -> Verb = \v -> verbBesch v ** {vtyp = VHabere ; p = []} ;

    subjPron = \_ -> [] ;

    polNegDirSubj = RPos ;

param
  HasArt = NoArt | UseArt ;

oper
  superlCanBePost = True ;

}
