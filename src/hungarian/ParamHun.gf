resource ParamHun = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Generic

oper
  if_then_Pol : Polarity -> Str -> Str -> Str = \p,t,f ->
    case p of {Pos => t ; Neg => f } ;

--------------------------------------------------------------------------------
-- Phonology

--------------------------------------------------------------------------------
-- Morphophonology



--------------------------------------------------------------------------------
-- Nouns

param

  Case =
    Nom | Acc  -- Practical to have core cases as full strings
  | Dat     -- Would be nice but is very regular, so skip it
  | Sup        -- Depends on the word which stem it uses
  -- | All     -- Can have irregularities in suffix (k)
  | Ins | Tra  -- Different for vowels and consonants
  | OblStem ;  -- The rest of the cases are regular and attach to this stem
       -- | Ill | Ine | Ela  | Ade | Abl | Sub | Sup | Del -- Locatives
       -- | Cau  -- Causal-final 'for the purpose of, for the reason that'
       -- | Ins  -- Instrumental
       -- | Tra  -- Translative
       -- | Ess | Ter | For
       -- | Tem -- Temporal, e.g. hatkor ‘six o’clock’ (from hat ‘6’)


  SubjCase = SCNom | SCDat ; -- Limited set of subject cases

  Possessor = NoPoss | Poss Number Person ;

oper
  case2stem : Case -> Case = id Case ; -- TODO add stems and cases as separate types

  caseTable : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,x15 : Str) -> Case=>Str =
   \n,a,d,il,ine,el,al,ad,ab,sub,sup,del,ca,ins,tra -> table {
      Nom => n ;
      Acc => a ;
      Dat => d ;
      Ins => ins ;
      Tra => tra ;
      OblStem => init a ;
      Sup => sup ;
      Sub => sub ;
      Del => del ;
      Ill => il ;
      Ine => ine ;
      Ela => el ;
      All => al ;
      Ade => ad ;
      Abl => ab ;
      Cau => ca } ;

  sc2case : SubjCase -> Case = \sc ->
    case sc of {
      SCNom => Nom ;
      SCDat => Dat
    } ;

--------------------------------------------------------------------------------
-- Numerals

param
  DForm = Unit  | Ten  ;
  Place = Indep | Attrib ;

  CardOrd = NOrd | NCard ; -- Not used yet

  NumType = NoNum | IsDig | IsNum ;

oper
  isNum : {numtype : NumType} -> Bool = \nt -> case nt.numtype of {
    NoNum => False ;
    _     => True
    } ;
--------------------------------------------------------------------------------
-- Adjectives


--------------------------------------------------------------------------------
-- Conjunctions



--------------------------------------------------------------------------------
-- Verbs
param

  -- For object agreement in V2
  ObjDef =
      Def
    | Indef ;

  VForm =
      VInf
    | VPres Person Number ;

oper

  agr2vf : Person*Number -> VForm = \pn ->
    case <pn.p1,pn.p2> of {
      <p,n> => VPres p n
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
