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
  Morpheme = mO | mKa | mTa ;
        -- | mKii | mTii ; -- TODO check if needed

oper
  allomorph : Morpheme -> Str -> Str = \x,stem ->
     case x of {
       mO => case last stem of {
                   d@("b"|"d"|"r"|"l"|"m"|"n") => d + "o" ;
                   "c"|"g"|"i"|"j"|"x"|"s"     => "yo" ;
                   _                           => "o" } ;

       mTa => case stem of {  -- Saeed p. 29
          _ + ("dh")                                    => "dha" ; ---- ???
          _ + (#v|"'"|"c"|"d"|"h"|"kh"|"q"|"w"|"x"|"y") => "da" ;
          _ + "l"                                       => "sha" ;
          _   {- b,f,g,n,r,s -}                         => "ta" } ;

       mKa => case stem of { -- Saeed p. 28-29
          _ + ("r"|"g"|"w"|"y"|"i"|"u"|"aa"|"oo"|"uu") => "ga" ;
          _ + ("q"|"'"|"kh"|"x"|"c"|"h")               => "a" ;
          _ + ("e"|"o")                                => "ha" ;
          _   {- b,d,dh,f,j,l,n,r,sh-}                 => "ka" }

      {-- TODO check if needed/implement elsewhere:
       mKii => case stem of {
          _+ #vv + #c => init (allomorph mKa stem) ++ "ii" ; -- Should not change stem vowel
          _ + ("'"|"x"|"c")               => "ii" ; -- Should change stem vowel
          _ => init (allomorph mKa stem) ++ "ii" } ;
       mTii => init (allomorph mTa stem) ++ "ii" -}
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

  NForm =
      Indef Number
    | Def Number Vowel -- Stems for definite and determinative suffixes
    -- Special forms only for fem. nouns ending in consonant.
    | Numerative  -- When modified by a number: either pl gen or sg abs
    | NomSg ;

oper
  getAgr : NForm -> Gender -> Agreement = \n,g ->
    case n of { Indef Pl|Def Pl _ => Pl3 ;
                _                 => Sg3 g } ;
  getNum : Agreement -> Number = \a ->
    case a of { Sg1|Sg2|Sg3 _ => Sg ; _ => Pl } ;

  agr2agrplus : (isPron : Bool) -> Agreement -> AgreementPlus = \isPron,a ->
    case isPron of {True => IsPron a ; False => NotPronP3} ;

  nf2state : {s:NForm=>Str} -> State=>Str = \ss -> table {
    Definite => ss.s ! Def Sg vA ;
    Indefinite => ss.s ! Indef Sg
    } ;

  gn2gennum : Gender -> Number -> GenNum = \g,n ->
    case <g,n> of {
      <Masc,Sg> => SgMasc ;
      <Fem,Sg>  => SgFem ;
      _ => PlInv } ;

  nf2gennum : NForm -> Gender -> GenNum = \nf,g ->
    gn2gennum g (getNum (getAgr nf g)) ;

--------------------------------------------------------------------------------
-- Numerals

param

  DForm = Unit | Ten ;

  -- If need to optimise: can remove one multiple of 2, but harder to understand
  -- CardOrdDFS = Odfs DForm | Cdfs DForm State ;
  --
  -- CardOrdState = Ost | Cst State ;

  CardOrd = NOrd | NCard ;

--------------------------------------------------------------------------------
-- Adjectives

param
  AForm = AF Number Case ; ---- TODO: past tense

--------------------------------------------------------------------------------
-- Prepositions

param
  Preposition = u | ku | ka | la | noPrep | passive ;
  PrepCombination = ugu | uga | ula | kaga | kula | kala
                  | Single Preposition ;

oper
  combine : Preposition -> Preposition -> PrepCombination = \p1,p2 ->
    let oneWay : Preposition => Preposition => PrepCombination =
          \\x,y => case <x,y> of {
                      <u,u|ku> => ugu ;
                      <u,ka>   => uga ;
                      <u,la>   => ula ;
                      <ku|ka,
                        ku|ka> => kaga ;
                      <ku,la>  => kula ;
                      <ka,la>  => kala ;
                      <noPrep,p> => Single p ;
                      <p,noPrep> => Single p ;
                      <p,_> => Single p } -- for trying both ways
    in case oneWay ! p2 ! p1 of {
              Single _ => oneWay ! p1 ! p2 ;
              x        => x } ;

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

oper
  if_then_Pol : Polarity -> Str -> Str -> Str = \p,t,f ->
    case p of {Pos => t ; Neg => f } ;

  forceAgr : Agreement -> (VForm=>Str) -> (VForm=>Str) = \agr,tbl -> table {
    VPres asp _a pol => tbl ! VPres asp agr pol ;
    VPast asp _a     => tbl ! VPast asp agr ;
    x                => tbl ! x
    } ;

}
