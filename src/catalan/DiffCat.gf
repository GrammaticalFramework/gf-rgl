--# -path=.:../romance:../abstract:../common:prelude
instance DiffCat of DiffRomance - [partAgr,stare_V,vpAgrSubj,vpAgrClits,AFormSimple] = open CommonRomance, PhonoCat, BeschCat, Prelude in {

  flags optimize=noexpand ;
  coding=utf8 ;

---- exceptions ----------------
  oper
    partAgr : VType -> Bool = \vtyp -> False ;
    vpAgrSubj : Verb -> VPAgrType = \v -> <verbDefaultPart v, False> ;
    vpAgrClits : Verb -> AAgr -> VPAgrType = \v,a -> <verbDefaultPart v, False> ;

--------------------------------

  param
    Prepos = P_de | P_a | P_per ;
    VType = VHabere | VRefl ;

oper
    dative   : Case = CPrep P_a ;
    genitive : Case = CPrep P_de ;
    ablative : Case = CPrep P_per ;

    prepCase = \c -> case c of {
      Nom => [] ;
      Acc => [] ;
      CPrep P_de => elisDe ;
      CPrep P_per => "per" ;
      CPrep P_a  => "a"
      } ;


    artDef : Bool -> Gender -> Number -> Case -> Str = \isNP,g,n,c ->
      case <g,n,c> of {        ---- TODO: check the NP forms
        <Masc,Sg, CPrep P_de> => pre {"del" ; ("de l'" ++ Predef.BIND) / vocalForta} ;
        <Masc,Sg, CPrep P_a>  => pre {"al"  ; ("a l'"  ++ Predef.BIND)  / vocalForta} ;
        <Masc,Pl, CPrep P_de> => "dels" ;
        <Fem, Pl, CPrep P_de> => ["de les"] ;
        <Masc,Sg, CPrep P_per> => "pel" ;
        <Fem, Sg, CPrep P_per> => ["per la"] ;
        <Masc,Pl, CPrep P_per> => "pels" ;
        <Fem, Pl, CPrep P_per> => ["per les"] ;
        <Masc,Pl, CPrep P_a>  => "als" ;
        <Fem, Pl, CPrep P_a>  => ["a les"] ;
        <Masc,Sg, _>    => elisEl ;
        <Fem, Sg, _>    => prepCase c ++ elisLa ;
        <Masc,   Pl, _ >   => "els" ;
        <Fem,   Pl, _ >   => "les"
        } ;



    artIndef = \isNP,g,n,c -> case isNP of {
     True => case <n,c> of {
      <Sg,CPrep P_de>   => genForms ("d' ++ Predef.BIND ++ un") ("d' ++ Predef.BIND ++ una") ! g ;
      <Sg,_> => prepCase c ++ genForms "un" "una" ! g ;
      <Pl,CPrep P_de>   => genForms ("d' ++ Predef.BIND ++ uns") ("d' ++ Predef.BIND ++ unes") ! g ; -- AR 3/12/2014
      <Pl,_> => prepCase c ++ genForms "uns" "unes" ! g
      } ;
     _ => case <n,c> of {
      <Sg,CPrep P_de>   => genForms ("d' ++ Predef.BIND ++ un") ("d' ++ Predef.BIND ++ una") ! g ;
      <Sg,_> => prepCase c ++ genForms "un" "una" ! g ;
      <Pl,_> => prepCase c --- ++ genForms "uns" "unes" ! g --- take this as a determiner
      }
     } ;

-- AForm and comparatives
  param
    AFormComplex = AF Gender Number | AAttrMasc | AA ;
  oper
    AForm = AFormComplex ;
    aform2aagr : AForm -> AAgr = \a -> case a of {
      DiffCat.AF g n => aagr g n ;
      _              => aagr Masc Sg -- "le plus lentement"
      } ;
    genNum2Aform : Gender -> Number -> AForm = DiffCat.AF ;
    genNumPos2Aform : Gender -> Number -> Bool -> AForm = \g,n,isPre ->
      case <g,n,isPre> of {
        <Masc,Sg,True> => AAttrMasc ;
        _              => genNum2Aform g n
      } ;
    piuComp = "més" ;

    possCase = \_,_,c -> prepCase c ;

    partitive = \g,c -> case c of {
      CPrep P_de => "de" ;
      _ => prepCase c ++ artDef False g Sg (CPrep P_de)
      } ;

    conjunctCase : Case -> Case = \c -> case c of {
      Nom => Nom ;
      _ => Acc
      } ;

    auxVerb : VType -> (VF => Str) = \_ -> haver_V.s ;

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
          }
       in case <paccp.p2, pdatp.p2> of {
         ---- AR 8/6/2008 efficiency problem in pgf generation:
         ---- replace the case expr with
         ---- a constant produces an error in V3 predication with two pronouns
         ---- <P3,P3> => <"se" ++ paccp.p1, [],True> ;
         _       => <pdatp.p1 ++ paccp.p1, [],orB paccp.p3 pdatp.p3>
         } ;

    --case <p,acc,dat> of {
    --    <Sg,P2,CRefl,CPron {n = Sg ; p = P1}> => <"te" ++ "me", []> ;
    --    <_,_,CPron {n = Sg ; p = P2},CPron {n = Sg ; p = P1}> => <"te" ++ "me", []> ;

   infForm _ _ _ _  = True ;

   mkImperative isPol p vp =
     \\pol,g,n => case pol of {
       RPos   => neg.p1 ++ imper ++ bindIf refl.isRefl ++ refl.pron
                   ++ bindIf hasClit ++ clit ++ compl ;
       RNeg _ => neg.p1 ++ refl.pron ++ clit ++ compl ++ subj
     } where {
       pe   = case isPol of {True => P3 ; _ => p} ;
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

    CopulaType = Bool ;
    selectCopula = \isEstar -> case isEstar of {True => estar_V ; False => copula} ;
    serCopula = False ;
    estarCopula = True ;

    negation : RPolarity => (Str * Str) = table {
      RPos => <[],[]> ;
      RNeg _ => <"no",[]>
      } ;

    conjThan = "que" ;
    conjThat = "que" ;
    subjIf = "si" ;



    clitInf b cli inf = inf ++ bindIf b ++ cli ; --- JS copied from DiffSpa

    relPron : Bool => AAgr => Case => Str = \\b,a,c =>
      case c of {
        Nom | Acc => "que" ;
        CPrep P_a => "cuyo" ;
        _ => prepCase c ++ "cuyo"
        } ;

    pronSuch : AAgr => Str = aagrForms "tal" "tal" "tals" "tals" ;

    quelPron : AAgr => Str = aagrForms "qual" "qual" "quals" "quals" ;

    partQIndir = [] ; ---- ?

    reflPron : Number -> Person -> Case -> Str = \n,p,c ->
        let pro = argPron Fem n p c
        in
        case p of {
        P3 => case c of {
          Acc | CPrep P_a => "es" ;
          _ => "si"
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
      \g,n,p -> case <g,n,p> of {
        <_,Sg,P1> => cases "em" "mí" ;
        <_,Sg,P2> => cases "et" "tú" ;
        <_,Pl,P1> => cases "ens" "nosaltres" ; --- nosotros
        <_,Pl,P2> => cases "us" "vosaltres" ; --- vosotros
        <Fem,Sg,P3> => cases3 "la" "li" "ella" ;
        <Masc,  Sg,P3> => cases3 "el" "li" "ell" ;
        <Fem,Pl,P3> => cases3 "les" "les" "elles" ;
        <Masc,  Pl,P3> => cases3 "els" "els" "ells"
        } ;

    vRefl _ = VRefl ;
    isVRefl : VType -> Bool = \ty -> case ty of {
      VRefl => True ;
      _ => False
      } ;

    haver_V, auxPassive : Verb = verbBeschH (estar_54 "estar") ;

    essere_V, copula = verbBeschH (ser_52 "ser" True) ;
    stare_V, estar_V = verbBeschH (estar_54 "estar") ;

    verbBeschH : Verbum -> Verb = \v -> verbBesch v ** {vtyp = VHabere ; p = []} ;

    subjPron = \_ -> [] ;

    polNegDirSubj = RPos ;

param
  HasArt = NoArt | UseArt ;

oper
  superlCanBePost = False ;

}
