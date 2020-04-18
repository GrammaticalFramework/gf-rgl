resource ParamHun = ParamX ** open Prelude in {

--------------------------------------------------------------------------------
-- Phonology

--------------------------------------------------------------------------------
-- Morphophonology



--------------------------------------------------------------------------------
-- Nouns

param

  Case = Nom | Acc | Dat
    --   | PossStem  -- TODO: Stem where possessive suffixes attach?
       | Ill | Ine | Ela | All | Ade | Abl | Sub | Sup | Del -- Locatives
       | Cau  -- Causal-final 'for the purpose of, for the reason that'
       | Ins  -- Instrumental
       | Tra  -- Translative
       -- | Ess | Ter | For
       -- | Tem -- Temporal, e.g. hatkor ‘six o’clock’ (from hat ‘6’)
       ;


  SubjCase = SCNom | SCDat ; -- Limited set of subject cases

oper

  caseTable : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,x15 : Str) -> Case=>Str =
   \n,a,d,il,ine,el,al,ad,ab,sub,sup,del,ca,ins,tra -> table {
      Nom => n ;
      Acc => a ;
      Dat => d ;
      Ill => il ;
      Ine => ine ;
      Ela => el ;
      All => al ;
      Ade => ad ;
      Abl => ab ;
      Sub => sub ;
      Sup => sup ;
      Del => del ;
      Cau => ca ;
      Ins => ins ;
      Tra => tra } ;

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
