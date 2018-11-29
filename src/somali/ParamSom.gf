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

oper
  allomorph : Morpheme -> Str -> Str = \x,stem ->
     case x of {
       mO => case last stem of {
                   d@("b"|"d"|"r"|"l"|"m"|"n") => d + "o" ;
                   "c"|"g"|"i"|"j"|"x"|"s"     => "yo" ;
                   _                           => "o" } ;

       -- Based on the table on page 21--TODO find generalisations in patterns
       mTa => case stem of {
                    _ + ("dh")  => "a" ; ---- ??? just guessing
                    _ + ("d"|"c"|"h"|"x"|"q"|"'"|"i"|"y"|"w") => "da" ;
                    _ + "l" => "sha" ;
                    _       => "ta" } ;

       mKa => case stem of {
                    _ + ("g"|"aa"|"i"|"y"|"w") => "ga" ;
                    _ + ("c"|"h"|"x"|"q"|"'")  => "a" ;
                    _ + ("e"|"o")              => "ha" ;
                    _                          => "ka" }
     } ;


--------------------------------------------------------------------------------
-- Nouns

param
  Case = Nom | Abs ;
  Gender = Masc | Fem ;
  Vowel = vA | vE | vI | vO | vU ; -- For vowel assimilation

  Inclusion = Excl | Incl ;
  Agreement =
      Sg1
    | Sg2
    | Sg3 Gender
    | Pl1 Inclusion
    | Pl2
    | Pl3
    | Impers ; -- Verb agrees with Sg3, but needed for preposition contraction

  NForm =
      Indef Number
    | Def Number Vowel -- Stems for definite and determinative suffixes
    | Numerative       -- When modified by a number (only distinct for some feminine nouns)
    | IndefNom ;       -- Special form, only fem. nouns ending in consonant


oper
  getAgr : NForm -> Gender -> Agreement = \n,g ->
    case n of { Indef Pl|Def Pl _ => Pl3 ;
                _                 => Sg3 g } ;
  getNum : Agreement -> Number = \a ->
    case a of { Sg1|Sg2|Sg3 _ => Sg ; _ => Pl } ;

--------------------------------------------------------------------------------
-- Adjectives

param
  AForm = AF Number Case ; ---- TODO: past tense

--------------------------------------------------------------------------------
-- Numerals

-- TODO: is this necessary?
param
  CardOrd = NCard | NOrd ;

--------------------------------------------------------------------------------
-- Prepositions

param
  Preposition = u | ku | ka | la | noPrep ;
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

param
  VForm =
      VInf
    | VPres Agreement Polarity
    | VNegPast
    | VPast Agreement
    | VRel -- "som är/har/…" TODO is this used in other verbs?
    | VImp Number Polarity ; -- TODO negation

oper
  if_then_Pol : Polarity -> Str -> Str -> Str = \p,t,f ->
    case p of {Pos => t ; Neg => f } ;

-- TODO:
-- tre aspekter (enkel, progressiv, habituell),
-- fem modus (indikativ, imperativ, konjunktiv, kontiditonalis, optativ)
}
