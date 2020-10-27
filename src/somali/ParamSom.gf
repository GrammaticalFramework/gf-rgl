resource ParamSom = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

oper
   --TODO: make patterns actually adjusted to Somali
  v : pattern Str = #("a" | "e" | "i" | "o" | "u") ;
  vstar : pattern Str = #("a" | "e" | "i" | "o" | "u" | "y" | "w") ; -- semivowels included
  vv : pattern Str = #("aa" | "ee" | "ii" | "oo" | "uu") ;
  c : pattern Str = #("m"|"n"|"p"|"b"|"t"|"d"|"k"|"g"|"f"|"v"
                      |"s"|"h"|"l"|"j"|"r"|"z"|"c"|"q");
  cstar : pattern Str = #("m"|"n"|"p"|"b"|"t"|"d"|"k"|"g"|"f"|"v" -- semivowels included
                      |"s"|"h"|"l"|"j"|"r"|"z"|"c"|"q"|"y"|"w");
  lmnr : pattern Str = #("l" | "m" | "n" | "r") ;
  kpt : pattern Str = #("k" | "p" | "t") ;
  gbd : pattern Str = #("g" | "b" | "d") ;

  voiced : Str -> Str = \s -> case s of {
    "k" => "g" ;
    "t" => "d" ;
    "p" => "b" ;
    _   => s } ;

--------------------------------------------------------------------------------
-- Morphophonology

param
  -- Allomorphs for the definite article
  DefTA = TA | DA | SHA | DHA ;
  DefKA = KA | GA | A_ | HA ;
  DefArticle = F DefTA | M DefKA ;
  GenderDefArt = FM DefTA DefKA
             | MF DefKA DefTA
             | MM DefKA DefKA  ;

oper

  sg : GenderDefArt -> DefArticle = \gda -> gda2da gda ! Sg ;
  pl : GenderDefArt -> DefArticle = \gda -> gda2da gda ! Pl ;

  gda2da : GenderDefArt -> Number => DefArticle = \gda ->
    let da : {sg,pl:DefArticle} = case gda of {
        FM s p => {sg = F s ; pl = M p} ;
        MM s p => {sg = M s ; pl = M p} ;
        MF s p => {sg = M s ; pl = F p} } ;
    in table {Sg => da.sg ; Pl => da.pl} ;

  defAllomorph : (_,_ : Str) -> GenderDefArt = \wiilka,wiilasha ->
    case <getGender wiilka, getGender wiilasha> of {
      <Masc,Fem>  => MF (allomM wiilka) (allomF wiilasha) ;
      <Masc,Masc> => MM (allomM wiilka) (allomM wiilasha) ;
      _           => FM (allomF wiilka) (allomM wiilasha)
    } where {
        allomF : Str -> DefTA = \wiilka ->
          case wiilka of {
                _ + "ta" => TA ; _ + "sha" => SHA ;
                _ + "da" => DA ; _ + "dha" => DHA } ;
        allomM : Str -> DefKA = \wiilka ->
          case wiilka of {
                _ + "ka" => KA ; _ + "aha" => HA ;
                _ + "ga" => GA ; _ + "a"   => A_ } ;
        getGender : Str -> Gender = \word ->
          case word of {
              _ + ("ta"|"sha"|"da"|"dha") => Fem ;
              _ + "a" => Masc ;
              _ => Predef.error ("defAllomorph: expecting definite form, given" ++ word)}
        } ;

  -- Use always via quantTable!
  defStems : DefArticle => Str = table {
    M KA => "k" ;
    M GA => "g" ;
    M A_ => [] ; -- If we want magac~magiciisa, we need to split this into CA and A_.
    M HA => "ah" ; -- NB. stem vowel replaced
    F TA => "t" ;
    F DA => "d" ;
    F SHA => "sh" ; -- NB. stem l replaced
    F DHA => "dh"
    } ;

  quantTable = overload {
    quantTable : Str -> DefArticle=>Str = \iis -> let i = head iis in table {
      M HA => i + "h" + iis ;
      x => defStems ! x + iis
      } ;
    quantTable : (ayg,ayd : Str) -> DefArticle=>Str = \ayg,ayd ->
      let a = head ayg in table {
      M HA => a + "h" + ayg ;
      M x => defStems ! M x + ayg ;
      F y => defStems ! F y + ayd
      }
    } ;

  head : Str -> Str = \s -> case s of {
    x@? + _ => x ;
          _ => "" -- Predef.error "head: empty string."
    } ;

--------------------------------------------
-- Old version, may be deprecated eventually
param
  Morpheme = mO | mKa | mTa ;
oper
  allomorph : Morpheme -> Str -> Str = \x,stem ->
     case x of {
       mO => case last stem of {
                   d@("b"|"d"|"r"|"l"|"m"|"n") => d + "o" ;
                   "c"|"g"|"i"|"j"|"x"|"s"     => "yo" ;
                   _                           => "o" } ;
       mTa => case stem of {  -- Saeed p. 29
          _ + ("dh")                                    => "dha" ;
          _ + (#v|"'"|"c"|"d"|"h"|"kh"|"q"|"w"|"x"|"y") => "da" ;
          _ + "l"                                       => "sha" ;
          _   {- b,f,g,n,r,s -}                         => "ta" } ;
       mKa => case stem of { -- Saeed p. 28-29
          _ + ("r"|"g"|"w"|"y"|"i"|"u"|"aa"|"oo"|"uu") => "ga" ;
          _ + ("q"|"'"|"kh"|"x"|"c"|"h")               => "a" ;
          _ + ("e"|"o")                                => "ha" ;
          _   {- b,d,dh,f,j,l,n,r,sh-}                 => "ka" }
     } ;

--------------------------------------------------------------------------------
-- Nouns

param
  Case = Nom | Abs ;
  Gender = Masc | Fem ;
  GenNum = SgMasc | SgFem | PlInv ; -- For Quant

  Inclusion = Excl | Incl ;
  Agreement =
      Sg1
    | Sg2
    | Sg3 Gender
    | Pl1 Inclusion
    | Pl2
    | Pl3
    | Impers ; -- Verb agrees with Sg3, but needed for preposition contraction

  PrepAgr =
      Sg1_Prep
    | Sg2_Prep
    | Pl1_Prep Inclusion
    | Pl2_Prep
    | Reflexive_Prep
    | P3_Prep ;

  State = Definite | Indefinite ;

  NForm = Def Number | Indef Number | NomSg | Numerative ;

oper
  getAgr : Number -> Gender -> Agreement = \n,g ->
    case n of { Pl => Pl3 ;
                _  => Sg3 g } ;

  getNum : Agreement -> Number = \a ->
    case a of { Sg1|Sg2|Sg3 _|Impers => Sg ; _ => Pl } ;

  plAgr : Agreement -> Agreement = \agr ->
    case agr of { Sg1   => Pl1 Excl ;
                  Sg2   => Pl2 ;
                  Sg3 _ => Pl3 ;
                  agr   => agr } ;

  agr2pagr : Agreement -> PrepAgr = \a -> case a of {
    Sg1 => Sg1_Prep ;
    Sg2 => Sg2_Prep ;
    Pl1 i => Pl1_Prep i ;
    Pl2 => Pl2_Prep ;
    _   => P3_Prep
    } ;

  pagr2agr : PrepAgr -> Agreement = \a -> case a of {
    Sg1_Prep => Sg1 ;
    Sg2_Prep => Sg2 ;
    Pl1_Prep i => Pl1 i ;
    Pl2_Prep   => Pl2 ;
    _          => Sg3 Masc
  } ;

  isP3 = overload {
    isP3 : Agreement -> Bool = \agr ->
      case agr of {Sg3 _ | Pl3 => True ; _ => False} ;
    isP3 : PrepAgr -> Bool = \agr ->
      case agr of {P3_Prep => True ; _ => False} ;
  } ;


  gender : {gda : GenderDefArt} -> Gender = \n ->
    case n.gda of {FM _ _ => Fem ; _ => Masc} ;

  gennum : {gda : GenderDefArt} -> Number -> GenNum = \gda,n ->
    case n of {Pl => PlInv ; Sg =>
      case gda.gda of {FM _ _ => SgFem ; _ => SgMasc}
    } ;

  npgennum : {a : Agreement} -> GenNum = \n ->
    case n.a of {
      Sg2|Sg3 Fem  => SgFem ;
      Sg1|Sg3 Masc => SgMasc ;
      _ => PlInv } ;

--------------------------------------------------------------------------------
-- Numerals

param
  DForm = Hal | Mid | Kow ; -- three variants of number 1

  CardOrd = NOrd | NCard ;

  -- to know whether to put oo in between numeral and CN
  NumType = NoNum | Basic | Compound ;

oper
  isNum : NumType -> Bool = \nt -> case nt of {
    NoNum => False ;
    _     => True
    } ;
--------------------------------------------------------------------------------
-- Adjectives

param
  AForm = AF Number Case ; ---- TODO: past tense

  ModType = NoMod | AMod | OtherMod ;

oper
  -- to flip ModType
  notMod : ModType -> ModType = \mt -> case mt of {
    NoMod => OtherMod ;
    _ => NoMod
  } ;

--------------------------------------------------------------------------------
-- Prepositions

param
  Preposition = U | Ku | Ka | La | NoPrep ;

  PrepCombination = Ugu | Uga | Ula | Kaga | Kula | Kala
                  | Passive | Loo | Lagu | Laga | Lala -- TODO all combinations with impersonal la: Loogu, Looga, Loola, Lagaga, Lagula, Lagala
                  | Single Preposition ;

oper
  combine : Preposition -> Preposition -> PrepCombination = \p1,p2 ->
    let oneWay : Preposition => Preposition => PrepCombination = \\x,y =>
        case <x,y> of {
          <U,U|Ku> => Ugu ;
          <U,Ka>   => Uga ;
          <U,La>   => Ula ;
          <Ku|Ka,
          Ku|Ka> => Kaga ;
          <Ku,La>  => Kula ;
          <Ka,La>  => Kala ;
          <NoPrep,p> => Single p ;
          <p,NoPrep> => Single x ;
          <p,_> => Single x } -- for trying both ways
     in case oneWay ! p2 ! p1 of {
          Single _ => oneWay ! p1 ! p2 ;
          z        => z } ;

  combinePassive : Preposition -> PrepCombination = \p ->
    case p of {
      U => Loo ;
      Ku => Lagu ;
      Ka => Laga ;
      La => Lala ;
      _ => Passive
    } ;

  isPassive : {c2 : PrepCombination} -> Bool = \vp ->
    case vp.c2 of {
      Passive | Lagu | Laga | Loo | Lala => True ;
      _ => False
    } ;

--------------------------------------------------------------------------------
-- Verbs

-- Saeed p. 84-85
-- Tense: Past/Present/Future
-- Aspect: Simple/Progressive/Habitual
-- Mood: Declarative/Imperative/Conditional/Optative/Potential
-- Negation: Positive/Negative
-- Sentence subordination: Main/Subordinate
-- Not every possible combination of these categories occurs, as we shall see: for example, tense and aspect are only marked in declarative sentences; there is no negation in potential sentences, etc. We can group the possible combinations into the twelve verbal paradigms below, details of which are given in the next three sections for suffix verbs, prefix verbs and yahay 'be':
-- 1. Imperative
-- 2. Infinitive
-- 3. Past simple
-- 4. Past progressive
-- 5. Past habitual
-- 6. Present habitual
-- 7. Present progressive
-- 8. Future
-- 9. Conditional
-- 10. Optative
-- 11. Potential
-- 12. Subordinate clause forms  -- same as negative present. But they carry subject markers when made into SC.

param

  Aspect = Simple | Progressive ;

  VForm =
      VInf
    | VPres Aspect VAgr Polarity
    | VNegPast Aspect
    | VPast Aspect VAgr
    | VImp Number Polarity
    | VRel GenNum {- Saeed p. 95-96 + ch 8
                     Reduced present general in relative clauses;  as absolutive
                      1/2SG/3SG M/2PL/3PL suga (VRel MascSg)
                      3 SG F sugta (VRel FemSg)
                      1PL sugna (VRel PlInv) -}
    | VRelNeg     -- Saeed p. 211 have: lahayn, be: ahayni
    | VNegCond GenNum ;

  VAgr =
      Sg1_Sg3Masc
    | Sg2_Sg3Fem
    | Pl1_
    | Pl2_
    | Pl3_ ;

  PredType = NoPred | Copula | NoCopula ;

  STM = Waa PredType | Waxa ;

  VVForm = Infinitive | Subjunctive | Waa_In ;

oper
  if_then_Pol : Polarity -> Str -> Str -> Str = \p,t,f ->
    case p of {Pos => t ; Neg => f } ;

  forceAgr : Agreement -> (VForm=>Str) -> (VForm=>Str) = \agr,tbl -> table {
    VPres asp _a pol => tbl ! VPres asp (agr2vagr agr) pol ;
    VPast asp _a     => tbl ! VPast asp (agr2vagr agr) ;
    x                => tbl ! x
    } ;

  agr2vagr : Agreement -> VAgr = \agr -> case agr of {
    Sg1|Sg3 Masc|Impers => Sg1_Sg3Masc ;
    Sg2|Sg3 Fem => Sg2_Sg3Fem ;
    Pl1 _ => Pl1_ ; Pl2 => Pl2_ ; Pl3 => Pl3_
  } ;

  isNeg : VForm -> Bool = \vf -> case vf of {
    VNegPast _ => True ;
    VNegCond _ => True ;
    VRelNeg    => True ;
    VImp _ Neg => True ;
    VPres _ _ Neg => True ;
    _ => False
    } ;

  showSTM : STM -> Str = \stm -> case stm of {
    Waxa  => "waxa" ; Waa _ => "waa" } ;
--------------------------------------------------------------------------------
-- Clauses

param

  ClType = Statement | PolarQuestion | WhQuestion | Subord ;

}
