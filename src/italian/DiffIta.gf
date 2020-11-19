--# -path=.:../romance:../abstract:../common:prelude

instance DiffIta of DiffRomance - [contractInf] = open CommonRomance, PhonoIta, BeschIta, Prelude in {

  flags optimize=all ;
    coding=utf8 ;
------
-- exception to interface
  oper contractInf : Bool -> Bool -> Bool = orB ; -- Ita has special contracted inf forms with clitics

------
  param 
    Prepos = P_di | P_a | P_da | P_in | P_su | P_con ;
    VType = VHabere | VEsse | VRefl ;

  oper
    dative   : Case = CPrep P_a ;
    genitive : Case = CPrep P_di ;

    prepCase = \c -> case c of {
      Nom | Acc => [] ;
      CPrep p => case p of {
        P_di => "di" ;
        P_a  => pre {"a" ; "ad" / vocale} ;
        P_da => "da" ;
        P_in => "in" ;
        P_su => "su" ;
        P_con => "con"
        }
      } ;

    artDef : Bool -> Gender -> Number -> Case -> Str = \isNP,g,n,c ->
     case isNP of {
       True => prepCase c ++ case <g,n> of {
        <Masc,Sg> => "lui" ;
        <Fem ,Sg> => "lei" ;
        <_,Pl>    => "loro" 
        } ;
       _ => case <g,n,c> of {
        <_, _, CPrep P_di>  => prepArt "de" ;
        <_, _, CPrep P_da>  => prepArt "da" ;
        <_, _, CPrep P_a>   => prepArt "a" ;
        <_, _, CPrep P_in>  => prepArt "ne" ;
        <_, _, CPrep P_su>  => prepArt "su" ;
        <_, _, CPrep P_con> => prepArt "co" ;
        <Masc,Sg, Nom| Acc> => elision "il" "l'" "lo" ;
        <Fem ,Sg, _>        => elision "la" "l'" "la" ;
        <Masc,Pl, _>        => elision "i" "gli" "gli" ;
        <Fem ,Pl, _>        => "le"
        }
      } 
       where {
         prepArt : Tok -> Tok = \de -> case <g,n> of {
           <Masc,Sg> => elision (de + "l")   (de + "ll'") (de + "llo") ;
           <Masc,Pl> => elision (de + "i")   (de + "gli") (de + "gli") ;
           <Fem, Sg> => elision (de + "lla") (de + "ll'") (de + "lla") ;
           <Fem, Pl> => de + "lle"
           }
         } ;


-- In these two, "de de/du/des" becomes "de".

    artIndef = \isNP, g,n,c -> case <n,isNP> of {
      <Sg,True> => prepCase c ++ 
                   genForms "uno" "una" ! g ;
      <Sg,_>    => prepCase c ++ 
                   genForms (pre {"un" ; "uno" / sImpuro}) (elision "una" "un'" "una") ! g ;
      <Pl,True> => prepCase c ++ 
                   genForms "alcuni" "alcune" ! g ;
      _         => prepCase c 
      } ;

    possCase = artDef False ;
    
    partitive = \_,c -> prepCase c ;
    
{-
    partitive = \g,c -> case c of {
      CPrep P_di => "di" ;
      _ => prepCase c ++ artDef False g Sg (CPrep P_di)
      } ;
-}
    conjunctCase : Case -> Case = \c -> case c of {
      Nom => Nom ;
      _ => Acc 
      } ;

    auxVerb : VType -> (VF => Str) = \vtyp -> case vtyp of {
      VHabere => avere_V.s ;
      _ => copula.s
      } ;

    vpAgrClit : Agr -> VPAgr = \a0 ->
      let a = complAgr a0 in
      VPAgrClit a.g a.n ;

    partAgr : VType -> Bool = \vtyp -> case vtyp of { -- works for all except Spa
      VHabere => False ;
      _ => True
      } ;

    pronArg = \n,p,acc,dat ->
      let 
        pacc = case acc of {  --- only accusative refl handled
          CRefl => case p of {
            P3 => "si" ;
            _  => argPron Fem n p Acc False --- no clitic after acc
            } ;
          CPron ag an ap => argPron ag an ap Acc False ;
          _ => []
          } ;
        hasAcc = case acc of {
          CNone => False ;
          _ => True
          } ;
        hasDat = case dat of {
          CNone => False ;
          _ => True
          } ;
        pdat = case dat of {
          CPron ag an ap => argPron ag an ap dative hasAcc ;
          _ => []
          } ;
       in
       <pdat ++ pacc, [], orB hasAcc hasDat> ;

    infForm n p x y = (pronArg n p x y).p3 ;

    mkImperative b p vp = 
      \\pol,g,n => 
        let 
          pe    = case b of {True => P3 ; _ => p} ;
          agr   = {g = g ; n = n ; p = pe} ;
          refl  = case vp.s.vtyp of {
            VRefl => <reflPron n pe Acc,True> ;
            _ => <[],False> 
            } ;

          clpr  = <vp.clit1 ++ vp.clit2,[],vp.clit3.hasClit> ;
          iform = orB clpr.p3 refl.p2 ;
          verb  = case <n,pol,pe> of {
            <Sg, RNeg True, P2> => vp.s.s ! VInfin iform ; ----  ! aag ;
            <Sg, RNeg False, P2> => vp.s.s ! VInfin iform ; ----  ! aag ;
            _ => vp.s.s ! vImper n pe
            } ;
          neg   = vp.neg ! pol ;
          compl = vp.comp ! agr ++ vp.ext ! pol
        in
        neg.p1 ++ verb ++ bindIf refl.p2 ++ refl.p1 ++ bindIf clpr.p3 ++ clpr.p1 ++ compl ;

    CopulaType = {} ;
    selectCopula = \isEstar -> copula ;
    serCopula = <> ;
    estarCopula = <> ;

    negation : RPolarity => (Str * Str) = table {
      RPos => <[],[]> ;
      RNeg _ => <"non",[]>
      } ;

    conjThan = "di" ; --- che
    conjThat = "che" ;

    subjIf = "se" ;

    clitInf b cli inf = inf ++ bindIf b ++ cli ;

    relPron : Bool => AAgr => Case => Str = \\b,a,c => 
      case c of {
        Nom | Acc => "che" ;
        CPrep P_di => "cui" ;
        _ => prepCase c ++ "cui" --- ilquale
        } ;

    pronSuch : AAgr => Str = aagrForms "tale" "tale" "tali" "tali" ;

    quelPron : AAgr => Str = aagrForms "quale" "quale" "quali" "quali" ;

    partQIndir = "ciò" ;

    reflPron : Number -> Person -> Case -> Str = 
      let 
        cases : (x,y : Str) -> (Case -> Str) = \me,moi,c -> case c of {
          Acc | CPrep P_a => me ;
          _ => moi
          } ;
      in 
      \n,p -> case <n,p> of { 
        <Sg,P1> => cases "mi" "me" ;
        <Sg,P2> => cases "ti" "te" ;
        <Pl,P1> => cases "ci" "noi" ; -- unlike French with just one alt!
        <Pl,P2> => cases "vi" "voi" ;
        _ => cases "si" "se"
        } ;

    argPron : Gender -> Number -> Person -> Case -> Bool -> Str = 
      let 
        cases : (x,y,z : Str) -> Case -> Bool -> Str = 
          \ci,ce,noi,c,isPre -> case c of {
          Acc | CPrep P_a => if_then_Str isPre ce ci ;
          _ => noi
          } ;
        cases4 : (x,y,z,u : Str) -> Case -> Bool -> Str =
          \lo,gli,glie,lui,c,isPre -> case c of {
          Acc => lo ; 
          CPrep P_a => if_then_Str isPre glie gli ;
          _ => lui
          } ;
      in 
      \g,n,p -> case <g,n,p> of { 
        <_,Sg,P1> => cases "mi" "me" "me" ;
        <_,Sg,P2> => cases "ti" "te" "te" ;
        <_,Pl,P1> => cases "ci" "ce" "noi" ; -- unlike French with just one alt!
        <_,Pl,P2> => cases "vi" "ve" "voi" ;
        <Masc,Sg,P3> => cases4 "lo" "gli" "glie" "lui" ;
        <Fem, Sg,P3> => cases4 "la" "le"  "glie" "lei" ;
        <_,   Pl,P3> => cases4 "li" "li" "glie"  "loro"
        } ;

    vRefl _ = VRefl ;
    isVRefl : VType -> Bool = \ty -> case ty of {
      VRefl => True ;
      _ => False
      } ;

    auxPassive : Verb = venire_V ;

    copula, essere_V = verbBesch (essere_1 "essere") ** {vtyp = VEsse ; p = []} ;
    avere_V = verbBesch (avere_2 "avere") ** {vtyp = VHabere ; p = []} ;
    venire_V = verbBesch (venire_110 "venire") ** {vtyp = VEsse ; p = []} ;

  datClit = "ci" ;
  genClit = "ne" ;

  subjPron = \_ -> [] ;

  polNegDirSubj = RPos ;

}
