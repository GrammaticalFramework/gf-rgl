resource ParamKor = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

{- Lee & Ramsey 2000, p. 24-25:
The difference in the way these words are written has to do with the productivity of the suffix. While the suffixes -um and -i can be used relatively freely to derive nouns from verbs and adjectives, the others cannot.  In the mind of the speaker (and the user of the orthography), the words wus-um ‘laughter’ and noph-i ‘height’ can be thought of as regular derivations of the verb wus- and the adjective noph-, much as are the predicative forms wus-uni, wus-ela, noph-ase, and noph-umyen. But words like makay ‘stopper’ (morphologically mak+ay, but written as ma+kay) and mutem ‘grave’ are not derived productively. The decision to write them without showing the suffix separated was based upon the assumption that most people think of them as single, indivisible words. Their etymologies were thought not to be obvious.
-}

oper
  v : pattern Str = #("아" | "이" | "어" |
                      "가" | "개" | "갸" | "걔" | "거" | "게" | "겨" | "계" | "고" | "과" | "괘" | "괴" | "교" | "구" | "궈" | "궤" | "귀" | "규" | "그" | "긔" | "기") ; -- TODO: figure out if this is a smart way to do it; if no better way, then complete the table.
--  maybe subpatterns for diphthongs?

  -- c : pattern Str = #("m"|"n"|"p"|"b"|"t"|"d"|"k"|"g"|"f"|"v"
  --                     |"s"|"h"|"l"|"j"|"r"|"z"|"c"|"q");
  --
  -- voiced : Str -> Str = \s -> case s of {
  --   "k" => "g" ;
  --   "t" => "d" ;
  --   "p" => "b" ;
  --   _   => s } ;

--------------------------------------------------------------------------------
-- Morphophonology



--------------------------------------------------------------------------------
-- Nouns

param
  NForm =
      Topic
    | Subject
    | Object ;


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
  AForm = AdjPres | AdjPast ; -- TODO: proper thing

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
