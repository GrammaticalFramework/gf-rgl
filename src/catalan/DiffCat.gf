--# -path=.:../romance:../abstract:../common:prelude
instance DiffCat of DiffRomance - [partAgr,vpAgrSubj,vpAgrClits] = open CommonRomance, PhonoCat, BeschCat, Prelude in {

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

   mkImperative b p vp =
      \\pol,g,n =>
        let
          pe    = case b of {True => P3 ; _ => p} ;
          agr   = {g = g ; n = n ; p = pe} ;
          refl  = case vp.s.vtyp of {
            VRefl => <reflPron n pe Acc,True> ;
            _ => <[],False>
            } ;

          clpr  =  <vp.clit1 ++ vp.clit2, [],vp.clit3.hasClit> ;  ---- TODO: True if clit
----          clpr  = <[],[],False> ; ----e pronArg agr.n agr.p vp.clAcc vp.clDat ;
----e          verb  = case <aag.n, pol,pe> of {
----e            <Sg,Neg,P2> => (vp.s ! VPInfinit Simul clpr.p3).inf ! aag ;
----e            _ => (vp.s ! VPImperat).fin ! agr
----e            } ;
          verb  = vp.s.s ! vImper n pe ; ----e
          neg   = vp.neg ! pol ;
          compl = neg.p2 ++ clpr.p2 ++ vp.comp ! agr ++ vp.ext ! pol
        in
        neg.p1 ++ verb ++ bindIf refl.p2 ++ refl.p1 ++ bindIf clpr.p3 ++ clpr.p1 ++ compl
         ;
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

    auxPassive : Verb = verbBeschH (estar_54 "estar") ;

    copula = verbBeschH (ser_52 "ser") ;
    estar_V = verbBeschH (estar_54 "estar") ;

    haver_V : Verb = verbBeschH (haver_59 "haver" True) ;

    verbBeschH : Verbum -> Verb = \v -> verbBesch v ** {vtyp = VHabere ; p = []} ;

    subjPron = \_ -> [] ;

    polNegDirSubj = RPos ;

}
