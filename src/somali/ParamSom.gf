resource ParamSom = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

oper
   --TODO: make patterns actually adjusted to Somali
  v : pattern Str = #("a" | "e" | "i" | "o" | "u") ;
  vstar : pattern Str = #("a" | "e" | "i" | "o" | "u" | "y" | "w") ; -- semivowels included
  vv : pattern Str = #("aa" | "ee" | "ii" | "oo" | "uu") ;
  c : pattern Str = #("m"|"n"|"p"|"b"|"t"|"d"|"k"|"g"|"f"|"v"
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
  DefArticle = M DefKA | F DefTA  ;

oper

  defAllomorph : Str -> DefArticle = \wiilka ->
    case wiilka of {
            _ + "ta" => F DA ; _ + "sha" => F SHA ;
            _ + "da" => F DA ; _ + "dha" => F DHA ;
            _ + "ka" => M KA ; _ + "aha" => M HA ;
            _ + "ga" => M GA ; _         => M A_ } ;

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
  Vowel = vA | vE | vI | vO | vU | NA ; -- For vowel assimilation
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

  AgreementPlus =
    Unassigned -- Dummy value: shows that the slot for object hasn't been filled.
  | IsPron Agreement  -- Any of Sg1 … Pl3 can be a pronoun.
  | NotPronP3 ; -- Sg3 Gender and Pl3 can be pronouns or not.

  State = Definite | Indefinite ;

  NForm = Def Number | Indef Number | NomSg | Numerative ;

oper
  getAgr : Number -> Gender -> Agreement = \n,g ->
    case n of { Pl => Pl3 ;
                _  => Sg3 g } ;

  getNum : Agreement -> Number = \a ->
    case a of { Sg1|Sg2|Sg3 _ => Sg ; _ => Pl } ;

  agr2agrplus : (isPron : Bool) -> Agreement -> AgreementPlus = \isPron,a ->
    case isPron of {True => IsPron a ; False => NotPronP3} ;

  isP3 = overload {
    isP3 : Agreement -> Bool = \agr ->
      case agr of {Sg3 _ | Pl3 | Impers => True ; _ => False} ;
    isP3 : AgreementPlus -> Bool = \agr ->
      case agr of {
        IsPron (Sg3 _ | Pl3 | Impers) => True ;
        NotPronP3 => True ;
        Unassigned => True ; -- meaningful for "does it leave an overt pronoun"
        _ => False}
  } ;

  -- gn2gennum : Gender -> Number -> GenNum = \g,n ->
  --   case <g,n> of {
  --     <Masc,Sg> => SgMasc ;
  --     <Fem,Sg>  => SgFem ;
  --     _ => PlInv } ;

  gender : {sg : DefArticle} -> Gender = \n ->
    case n.sg of {M _ => Masc ; F _ => Fem} ;
--------------------------------------------------------------------------------
-- Numerals

param
  DForm = Hal | Mid | Kow ; -- three variants of number 1

  CardOrd = NOrd | NCard ;

--------------------------------------------------------------------------------
-- Adjectives

param
  AForm = AF Number Case ; ---- TODO: past tense

--------------------------------------------------------------------------------
-- Prepositions

param
  Preposition = U | Ku | Ka | La | NoPrep ;
  PrepositionPlus = P Preposition
                  | Passive ; -- Hack: RGL only supports V2s as passive, so I can reuse V2's preposition slot for passives as well, and save >200 parameters. (Don't ask.)

  PrepCombination = Ugu | Uga | Ula | Kaga | Kula | Kala
                  | Single PrepositionPlus ;

oper
  combine : PrepositionPlus -> Preposition -> PrepCombination = \p1,p2 ->
    let oneWay : PrepositionPlus => Preposition => PrepCombination =
          \\x,y => case <x,y> of {
              <Passive,NoPrep> => Single Passive ;
              <Passive,p> => Single (P p) ; -- TODO check if this ever happens
              <P z,_> => case <z,y> of {
                      <U,U|Ku> => Ugu ;
                      <U,Ka>   => Uga ;
                      <U,La>   => Ula ;
                      <Ku|Ka,
                        Ku|Ka> => Kaga ;
                      <Ku,La>  => Kula ;
                      <Ka,La>  => Kala ;
                      <NoPrep,p> => Single (P p) ;
                      <p,NoPrep> => Single x ;
                      <p,_> => Single x }} -- for trying both ways
    in case oneWay ! P p2 ! (pp2prep p1) of {
              Single _ => oneWay ! p1 ! p2 ;
              z        => z } ;

  pp2prep : PrepositionPlus -> Preposition = \pp ->
    case pp of {P p => p ; _ => NoPrep} ;
--------------------------------------------------------------------------------
-- Verbs

-- Sayeed p. 84-85
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
    | VPres Aspect Agreement Polarity
    | VNegPast Aspect
    | VPast Aspect Agreement
    | VRel -- "som är/har/…" TODO is this used in other verbs?
    | VImp Number Polarity ;

  PredType = NoPred | Copula | NoCopula ;

oper
  if_then_Pol : Polarity -> Str -> Str -> Str = \p,t,f ->
    case p of {Pos => t ; Neg => f } ;

  forceAgr : Agreement -> (VForm=>Str) -> (VForm=>Str) = \agr,tbl -> table {
    VPres asp _a pol => tbl ! VPres asp agr pol ;
    VPast asp _a     => tbl ! VPast asp agr ;
    x                => tbl ! x
    } ;

}
