resource ParamKor = ParamX, Hangul ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

{- Lee & Ramsey 2000, p. 24-25:
  The difference in the way these words are written has to do with the productivity of the suffix.
  While the suffixes -um and -i can be used relatively freely to derive nouns from verbs and adjectives,
  the others cannot.
  In the mind of the speaker (and the user of the orthography), the words wus-um ‘laughter’ and noph-i ‘height’
  can be thought of as regular derivations of the verb wus- and the adjective noph-, much as are the
  predicative forms wus-uni, wus-ela, noph-ase, and noph-umyen. But words like makay ‘stopper’
  (morphologically mak+ay, but written as ma+kay) and mutem ‘grave’ are not derived productively.
  The decision to write them without showing the suffix separated was based upon the assumption that
  most people think of them as single, indivisible words. Their etymologies were thought not to be obvious.
-}

 -- Patterns and replacements defined in Hangul.gf
oper

  vowFinal : Str -> Bool = \str ->
    case str of {_ + #v => True ; _ => False} ;

--------------------------------------------------------------------------------
-- Morphophonology



--------------------------------------------------------------------------------
-- Nouns

param
  NForm =
      Bare     -- no case particle
    | Topic    -- 은 or 는
    | Subject  -- 이 or 가
    | Object   -- 을 or 를
    ;

  Phono = Vowel | Consonant ; -- Whether the word ends in vowel or consonant.

oper

  allomorph : NForm -> Str -> Str = \nf,s ->
    let finalV : Bool = vowFinal s ;
    in case nf of {
         Topic   => if_then_Str finalV "는" "은" ;
         Subject => if_then_Str finalV "가" "이" ;
         Object  => if_then_Str finalV "를" "을" ;
         Bare    => []
      } ;
--------------------------------------------------------------------------------
-- Numerals

param
  DForm = Indep | Attrib ;

  CardOrd = NOrd | NCard ;

  -- TODO see if this is needed
  NumType = NoNum | IsDigit | IsNumber ;

oper
  isNum : NumType -> Bool = \nt -> case nt of {
    NoNum => False ;
    _     => True
    } ;
--------------------------------------------------------------------------------
-- Adjectives

param
  AForm =
   AAttr |
   APred VForm ;

--------------------------------------------------------------------------------
-- Prepositions

--------------------------------------------------------------------------------
-- Verbs
param
  VerbType = Active | Stative | Existential | Copula ; -- from Wikipedia https://en.wikipedia.org/wiki/Korean_verbs#Classification

  Aspect = Gnomic | Prospective | Perfect ;

  -- TODO: proper list of forms
  VForm =
      VInf
    | VFin Aspect Polarity ;


--------------------------------------------------------------------------------
-- Clauses

param

  ClType = Statement | PolarQuestion | WhQuestion | Subord ;

}
