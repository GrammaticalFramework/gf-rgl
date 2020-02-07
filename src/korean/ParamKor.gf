resource ParamKor = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

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
