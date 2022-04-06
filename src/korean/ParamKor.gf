resource ParamKor = ParamX, Hangul ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

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

  NumType = NoNum | IsDig | IsNum ;

  NumOrigin = SK | NK ;

oper
  isNum : {numtype : NumType} -> Bool = \nt -> case nt.numtype of {
    NoNum => False ;
    _     => True
    } ;
--------------------------------------------------------------------------------
-- Adjectives


--------------------------------------------------------------------------------
-- Conjunctions

param

  ConjType = And | Or | Comma ;

  POS =
     VStar -- Verbs and adjectives
   | NStar ; -- Nouns and adverbs

oper
  conjTable : POS => ConjType => Phono => Str = \\p,c,ph => case <c,p,ph> of {
     -- Special "conjunction": just put comma with NPs, no conjunction.
     -- Used in a specific (proprietary) application grammar, don't remove this.
    <Comma,NStar,_> => "," ;
    <Comma,VStar,Consonant> => "이며" ;
    <Comma,VStar,Vowel> => "며" ;

    -- In the normal case, conjunctions are repeated after each element in ListX
    -- Conjunction is not an argument to BaseX and ConsX, it is added in ConjX.
    -- That's why we need a ConjType parameter to ListX categories.
    <And,VStar,_> => "고" ;
    <And,NStar,_> => "하고" ;
    <Or,_,_> => "또는" -- TODO what is or for V/AP/S?
  } ;

--------------------------------------------------------------------------------
-- Verbs
param

  Aspect =
      Generic      -- zero morpheme
    | Habitual     -- 는
    | Prospective  -- 겠
    | Perfect      -- 었/았
    ;

  Style =
      Formal  -- 하십시오체
    | Polite  -- 해요체
    | Plain   -- 해라체
    ;

  SentenceType =
      Declarative
    | Interrogative
    | Imperative
    | Propositive ;


  -- TODO: include Aspect and SentenceType.
  -- These are all Generic and Declarative.
  -- TODO: read about infinitive in (Martin 1992, p. 251)
  VForm =
      VStem Polarity -- for adding conjunctions
    | VAttr Polarity -- for subordinate clauses
    | VF Style Polarity ;

oper
  -- Default style used in the whole grammar. Change here for another style.
  linStyle = Formal ;
  linVF = VF linStyle Pos ;

  isPos : VForm -> Bool = \vf -> case vf of {
    VStem Pos => True ;
    VAttr Pos => True ;
    VF _ Pos => True ;
    _ => False
    } ;

  -- Used in an application grammar.
  -- Putting this oper here because RG internals may change.
  vf2str : VForm -> Str = \vf -> case vf of {
    VStem Pos => "s (VStem Pos)" ;
    VStem Neg => "s (VStem Neg)" ;
    VAttr Pos => "s (VAttr Pos)" ;
    VAttr Neg => "s (VAttr Neg)" ;
    VF Formal Pos => "s (VF Formal Pos)" ;
    VF Formal Neg => "s (VF Formal Neg)" ;
    VF Polite Pos => "s (VF Polite Pos)" ;
    VF Polite Neg => "s (VF Polite Neg)" ;
    VF Plain Pos  => "s (VF Plain Pos)" ;
    VF Plain Neg  => "s (VF Plain Neg)" } ;

--------------------------------------------------------------------------------
-- Clauses

param

  ClType =
      Statement Style
    -- | PolarQuestion
    -- | WhQuestion
    | WithConj
    | Subord ;

}
