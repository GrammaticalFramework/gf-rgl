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
-- Quant

param
  QuantType =
    IndefArticle  -- Needed to prevent "a 2 cars"
  | IndefQuant    -- Not IndefArt, not poss, not def
  | DefQuant
  | QuantPoss PossStem -- Which possessive stem it takes
  ;

  DetType =
    DefDet  -- distinction between Article and Other no longer needed
  | IndefDet -- still need def or indef
  | DetPoss PossStem -- Sill need to know which stem it takes if Poss
  ;

  -- Singular stems. Plural is always same, no need to add here.
  PossStem = dSg_rSg1P2 | dSg_rP3 Number | dSg_rPl1 ;

oper
  -- standard trick to prevent "a one car"
  isIndefArt : {qt : QuantType} -> Bool = \quant ->
    case quant.qt of {
      IndefArticle => True ;
      _            => False
    } ;

  dt2objdef : DetType -> ObjDef = \dt -> case dt of {
    IndefDet => Indef ;
    _        => Def
    } ;

  objdef2dt : ObjDef -> DetType = \od -> case od of {
    Def => DefDet ;
    Indef => IndefDet
    } ;

  qt2dt : QuantType -> DetType = \qt -> case qt of {
    QuantPoss x => DetPoss x ;
    DefQuant => DefDet ;
    _ => IndefDet
    } ;

  agr2pstem : Person*Number -> PossStem = \pn ->
    case <pn.p1,pn.p2> of {
      <P1,Pl> => dSg_rPl1 ;
      <P3,n>  => dSg_rP3 n ;
      _       => dSg_rSg1P2
    } ;

--------------------------------------------------------------------------------
-- Nouns

param

  NumCaseStem =
    SgNom | SgAccStem | SgSup -- These may use 2-3 different stems
    -- May have irregular vowel in suffix
  | SgAll  -- May have irregular vowel in suffix
  | SgInsStem -- Instrumental and translative: -v after vowels
  | SgStem  -- Rest of the cases in Sg
  | PlStem  -- Rest of the cases in Pl
  | PossdSg_PossrP3 -- Possessed item is Sg, possessor is {Sg,Pl} P3
  | PossdSg_PossrPl1 -- Possessed item is Sg, possessor is Pl P1
  | PossdSg_PossrSg1P2 -- Possessed item is Sg, possessor is Sg P1 or {Sg,Pl} P2
  | PossdPl -- Possessed item in plural, any possessor.
  ;         -- Rest of poss forms use SgAccStem


  Case =
    Nom | Acc | Dat
--  | Ill  -- Locatives
  | Ine
  | Ela
  | All
  | Ade
  | Abl
--  | Sub
  | Sup
--  | Del
--  | Cau  -- Causal-final 'for the purpose of, for the reason that'
  | Ins  -- Instrumental
  | Tra  -- Translative
  -- | Ess | Ter | For
  -- | Tem -- Temporal, e.g. hatkor ‘six o’clock’ (from hat ‘6’)
  ;

  SubjCase = SCNom | SCDat ; -- Limited set of subject cases

  Possessor = NoPoss | Poss Person Number ;

oper

  caseTable : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,x15 : Str) -> Case=>Str =
   \n,a,d,il,ine,el,al,ad,ab,sub,sup,del,ca,ins,tra -> table {
      Nom => n ;
      Acc => a ;
      Dat => d ;
      Ins => ins ;
      Tra => tra ;
      Sup => sup ;
      Ine => ine ;
      Ela => el ;
      All => al ;
      Ade => ad ;
      Abl => ab ;
      Sub => sub ;
      Del => del ;
      Ill => il ;
      Cau => ca } ;

  sc2case : SubjCase -> Case = \sc ->
    case sc of {
      SCNom => Nom ;
      SCDat => Dat
    } ;

  case2str : Case -> Str = \c -> case c of {
    Nom => "Nom" ;
    Acc => "Acc" ;
    Dat => "Dat" ;
    Ins => "Ins" ;
    Tra => "Tra" ;
    Sup => "Sup" ;
    Ine => "Ine" ;
    Ela => "Ela" ;
    All => "All" ;
    Ade => "Ade" ;
    Abl => "Abl" ;
    Sub => "Sub" ;
    Del => "Del" ;
    Ill => "Ill" ;
    Cau => "Cau" } ;

  ncstem2str : NumCaseStem -> Str = \nc -> case nc of {
    SgNom => "SgNom" ;
    SgAccStem => "SgAccStem" ;
    SgSup => "SgSup" ;
    SgAll => "SgAll" ;
    SgInsStem => "SgInsStem" ;
    SgStem => "SgStem" ;
    PlStem => "PlStem" ;
    PossdSg_PossrP3 => "PossdSg_PossrP3" ;
    PossdSg_PossrPl1 => "PossdSg_PossrPl1" ;
    PossdSg_PossrSg1P2 => "PossdSg_PossrSg1P2" ;
    PossdPl => "PossdPl"
    } ;

  possessor2str : Possessor -> Str = \p -> case p of {
    NoPoss => "NoPoss" ;
    Poss P1 Sg => "Poss P1 Sg" ;
    Poss P2 Sg => "Poss P2 Sg" ;
    Poss P3 Sg => "Poss P3 Sg" ;
    Poss P1 Pl => "Poss P1 Pl" ;
    Poss P2 Pl => "Poss P2 Pl" ;
    Poss P3 Pl => "Poss P3 Pl"
    } ;
--------------------------------------------------------------------------------
-- Numerals

param
  DForm = Unit  | Ten  ;
  Place = Indep | Attrib ;

  CardOrd = NOrd | NCard ; -- Not used yet

  NumType = NoNum Number | IsNum ;

oper
  isNum : {n : NumType} -> Bool = \n -> case n.n of {
    IsNum => True ;
    _     => False
    } ;

  num2number : NumType -> Number = \n ->
    case n of {NoNum x => x ; IsNum => Sg} ;
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
