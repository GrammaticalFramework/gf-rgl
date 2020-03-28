resource ParamHun = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

oper
  v : pattern Str = #("a" | "e" | "i" | "o" | "u" | "ö" | "ü" |
                      "á" | "é" | "í" | "ó" | "ú" | "ő" | "ű") ;

  -- not used yet
  vowFinal : Str -> Bool = \str ->
    case str of {_ + #v => True ; _ => False} ;

--------------------------------------------------------------------------------
-- Morphophonology



--------------------------------------------------------------------------------
-- Nouns

param
  Case = Nom | Acc | Dat | Ins | Ess | Tra | Cau
       | Ill | Sub | All | Ine | Sup | Ade | Ela
       | Del | Abl | Ter | For | Tem
       ;

  Harm = H_a | H_e | H_o ;

--------------------------------------------------------------------------------
-- Numerals

param
  DForm = Unit  | Ten  ;
  Place = Indep | Attrib ;

  CardOrd = NOrd | NCard ; -- Not used yet

--  NumType = NoNum | IsDig | IsNum ;

-- oper
--   isNum : {numtype : NumType} -> Bool = \nt -> case nt.numtype of {
--     NoNum => False ;
--     _     => True
--     } ;
--------------------------------------------------------------------------------
-- Adjectives


--------------------------------------------------------------------------------
-- Conjunctions



--------------------------------------------------------------------------------
-- Verbs
param

  -- TODO: object agreement
  VForm =
      VInf
    | VFin Person Number ;

oper

  agr2vf : Person*Number -> VForm = \pn ->
    case <pn.p1,pn.p2> of {
      <p,n> => VFin p n
    } ;

--------------------------------------------------------------------------------
-- Clauses

-- param

  -- ClType =
  --     Statement
  --   | PolarQuestion
  --   | WhQuestion
  --   | Subord ;

}
