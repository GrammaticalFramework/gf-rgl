--# -path=.:../abstract:../common:../prelude
--# -coding=utf8

-- Ilona Nowak Wintersemester 2007/08  

-- Adam Slaski, 2009, 2010 <adam.slaski@gmail.com>

-- 1 Polish auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work. 

  resource ResLit = ParamX ** open Prelude in {

  flags  coding=utf8 ; optimize=all_subs ;

---------------------- Parameter types definition --------------------------
-- Their parameter values are atomic.
-- Some parameters, such as $Number$ or $Person$, are inherited from $ParamX$.
-- So it must not to be defined here again.
-- Read about it in ParadigmsPol.gf, where the genders are defined. 

oper harden : Str -> Str
    = \stem ->
    case stem of {
      x + "č" => x + "t" ;
      x + "dž" => x + "d" ;
      _       => stem
    } ;

oper soften : Str -> Str
    = \stem ->
    case stem of {
      x + "t" => x + "č" ;
      x + "d" => x + "dž" ;
      _       => stem
    } ;

  catLt : Str -> Str -> Str
    = \stem, ending ->
    case isPalatalizing ending of {
      True => soften stem + ending ;
      _    => stem + ending 
    } ;

  isPalatalizing : Str -> Bool
    = \ending ->
    case ending of {
      "ia" + _ => True ;
      "io" + _ => True ;
      "ią" + _ => True ;
      "iu" + _ => True ;
      "iū" + _ => True ;
      "ių" + _ => True ;
      _        => False
    } ;

--1 Nouns   

----------------------- Parameter for nouns ----------------------------------

  param 
    Gender     = Masc | Fem ; -- PT + singulia tantum
    NounAgrCat = SingPlur Gender | PlurOnly Gender | NoAgr ;
    Case       = Nom | Gen | Dat | Acc | Ins | Loc | VocL ;   
    NomType    = PersMark | Pro | Reg ;   

    DetType = NormalDet | EmptyDef | EmptyIndef ; -- artificial parameter to side-step DetNP parsing issues

-- Nouns are declined according to number and case.
-- For the sake of shorter description, these parameters are 
-- combined in the type SubstForm.

-- Il faudra peut-être limiter pour PT
  param SubstForm = SF Number Case ;

  oper CommNoun = {s : SubstForm => Str; g : NounAgrCat ; nomType : NomType};
  oper CommNoun2 = CommNoun ** { cplCase : Complement } ;
  oper CommNoun3 = CommNoun2 ** { cplCase2 : Complement } ;

  oper Adverb = {s : Str; fronted : Bool};

--2 Verbs   

----------------------- Parameter for verbs ----------------------------------

-- General information

-- Polish verb has two indicative tenses called pseudoparticiple (with meaning of the past) 
-- and finitive. Meaning ofthe second one depends on aspect: if verb is perfective then finitive 
-- form has meaning of the future, otherwise of the present. Future tense of imperfective 
-- verb is constructed with proper form of 'być' ('to be') and variantively 
-- the infinitive or the past form.  

-- So on morphological level verbs inflection looks as follow:

  param VForm =
       VInf
     | VImperSg2
     | VImperPl1
     | VImperPl2
     | VPres Number Person
     | VPast Number Person
     | VPastFreq Number Person
     | VFut Number Person
     | VHyp Number Person
     | VGerund Gender Number ;

param
  DeclClass        = D0 | D1 | D2 | D3 | D4 | D5; -- inutilisé
  --			C1a = miegoti miega miegojo - C1b = barti bara barė - C1c = kviesti kviečia kvietė - C1d - leisti leidžia leido
  --            	C2a = žiurėti žiūri žiurėjo - C3a = valgyti valgo valgė - C3b = ieškoti ieško ieškojo -      
  ConjClass        = C1a | C1b | C1c | C1d | C2a | C3a | C3b; -- Būti
  ThVowelPres      = PR_IA | PR_A | PR_O | PR_I ;   -- present thematic vowel
  ThVowelPast      = P_O | P_E ;   -- past thematic vowel
  AdvType          = AdjT | PronT | GenT | OtherT ; -- PronT subsume GenT

  oper 
   conjAdvType : AdvType -> AdvType -> AdvType ;
   conjAdvType t1 t2 = 
     case <t1,t2> of {
       <PronT, PronT> => PronT;
       <AdjT, AdjT> => AdjT;
       <GenT, GenT> => GenT;
       _ => OtherT
     };

-- Presence of voices in Polish is a matter of controversion. 
-- In this work I treat voice as syntax (not morphological) phenomenon.
-- Passive voice will be constructed from passive participle.
-- Reflexive voice will be constructed from active forms.

-- Aspect tells, if the action is already done or it is still taking place
-- at the time of speaking.

  param 
    Aspect = Dual | Imperfective | Perfective ;  
    ReflStatus = Norefl | Infix | Postfix ;
    Fronting = NePref | Unfronted ; -- Fronted = after tebe, nebe, te, be (only ne- implemented)
    CplType = AdvC | PronC | NomC ;  
    
  oper Verb : Type = {
    forms : Fronting => VForm => Str;
    refl : ReflStatus;
    asp : Aspect;
    passPastPart : AdjTable; --AForm=>Str;

    -- Might be better as one field if we decide for the mark for the PastIter
    actPastPart : AdjTable; --AForm=>Str;
    actPastFreqPart : AdjTable; --AForm=>Str;
    actPresPart : AdjTable; --AForm=>Str;
    actFutPart : AdjTable; --AForm=>Str;
  };
        
  oper VerbPhrase : Type = {
    adv : Str;
    preCompl : Polarity => GenNum => Str;
    postCompl : Polarity => GenNum => Str;
    verb : Verb;
--    withCopula : Bool;-- formed with 'to be' (she was nice, he is a man, etc.)
    exp : Bool -- expanded 
  };
  
  oper VerbPhraseSlash : Type = 
    VerbPhrase ** { cplCase : Complement };

--3 Adjectives

----------------------- Parameter for adjectives ----------------------------------

-- Description and explanation in MorphoAdjectiveLit.gf

  oper AdjForms : Type = {
    msnom, msacc, msgen, msins, msdat, msloc, -- pvoc = pnom
    mpnom, mpacc, mpgen, mpins, mpdat, mploc,

    fsnom, fsacc, fsgen, fsins, fsdat, fsloc,
    fpnom, fpacc, fpgen, fpins, fpdat, fploc, 

    nnom  -- 
    : Str ;
  } ;

  oper emptyForms : AdjForms = { msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc, fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom = "" };

  -- this is because of the bug (undocumented feature) in GF. only two levels of nested records are possible, on the third level compiler throw a strange error about more than 6664 fields demanded. tables on second level are accepted, so adj11forms is translated into table and back.
  
  param paramX = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 | X25 ;
    
  oper AdjTable : Type = paramX => Str;
  
  oper table2record : AdjTable -> AdjForms = \a -> { msnom = a!X1; msacc = a!X2; msgen = a!X3; msins = a!X4; msdat = a!X5; msloc = a!X6; mpnom = a!X7; mpacc = a!X8; mpgen = a!X9; mpins = a!X10; mpdat = a!X11; mploc = a!X12; fsnom = a!X13; fsacc = a!X14; fsgen = a!X15; fsins = a!X16; fsdat = a!X17; fsloc = a!X18; fpnom = a!X19; fpacc = a!X20; fpgen = a!X21; fpins = a!X22; fpdat = a!X23; fploc = a!X24; nnom = a!X25 };

  oper record2table : AdjForms -> AdjTable = \a -> table { X1 => a.msnom ; X2 => a.msacc; X3 => a.msgen; X4 => a.msins; X5 => a.msdat; X6 => a.msloc; X7 => a.mpnom; X8 => a.mpacc; X9 => a.mpgen; X10 => a.mpins; X11 => a.mpdat; X12 => a.mploc; X13 => a.fsnom; X14 => a.fsacc; X15 => a.fsgen; X16 => a.fsins; X17 => a.fsdat; X18 => a.fsloc; X19 => a.fpnom; X20 => a.fpacc; X21 => a.fpgen; X22 => a.fpins; X23 => a.fpdat; X24 => a.fploc; X25 => a.nnom };
  
  oper Adj : Type = {
    pos : AdjForms;
    comp : AdjForms;
    super : AdjForms;
    advpos : Str;
    advcomp : Str;
    advsuper : Str;
  };

  oper mkAtable : AdjForms -> AForm => Str = \f ->
    table {
          AF Sg Masc Nom    => f.msnom;
          AF Sg Masc Acc    => f.msacc; 
          AF Sg Masc Gen    => f.msgen;
          AF Sg Masc Ins    => f.msins;
          AF Sg Masc Dat    => f.msdat; 
          AF Sg Masc Loc    => f.msloc; 
          AF Sg Masc VocL   => f.msnom;
    
          AF Pl Masc Nom    => f.mpnom;
          AF Pl Masc Acc    => f.mpacc; 
          AF Pl Masc Gen    => f.mpgen;
          AF Pl Masc Ins    => f.mpins;
          AF Pl Masc Dat    => f.mpdat; 
          AF Pl Masc Loc    => f.mploc; 
          AF Pl Masc VocL  => f.mpnom;

--          (AF Pl Masc Nom|PlurAF Masc Nom)    => f.mpnom;
--          (AF Pl Masc Acc|PlurAF Masc Acc)    => f.mpacc; 
--          (AF Pl Masc Gen|PlurAF Masc Gen)    => f.mpgen;
--          (AF Pl Masc Ins|PlurAF Masc Ins)    => f.mpins;
--          (AF Pl Masc Dat|PlurAF Masc Dat)    => f.mpdat; 
--          (AF Pl Masc Loc|PlurAF Masc Loc)    => f.mploc; 
--          (AF Pl Masc VocL|PlurAF Masc VocL)  => f.mpnom;

          AF Sg Fem Nom   => f.fsnom ; 
          AF Sg Fem Acc   => f.fsacc; 
          AF Sg Fem Gen   => f.fsgen;
          AF Sg Fem Ins   => f.fsins;
          AF Sg Fem Dat   => f.fsdat; 
          AF Sg Fem Loc   => f.fsloc;
          AF Sg Fem VocL  => f.fsnom;   
    
--          (AF Pl Fem Nom|PlurAF Fem Nom)   => f.fpnom ; 
--          (AF Pl Fem Acc|PlurAF Fem Acc)   => f.fpacc; 
--          (AF Pl Fem Gen|PlurAF Fem Gen)   => f.fpgen;
--          (AF Pl Fem Ins|PlurAF Fem Ins)   => f.fpins;
--          (AF Pl Fem Dat|PlurAF Fem Dat)   => f.fpdat; 
--          (AF Pl Fem Loc|PlurAF Fem Loc)   => f.fploc;
--          (AF Pl Fem VocL|PlurAF Fem VocL) => f.fpnom;

          AF Pl Fem Nom   => f.fpnom ; 
          AF Pl Fem Acc   => f.fpacc; 
          AF Pl Fem Gen   => f.fpgen;
          AF Pl Fem Ins   => f.fpins;
          AF Pl Fem Dat   => f.fpdat; 
          AF Pl Fem Loc   => f.fploc;
          AF Pl Fem VocL => f.fpnom;

          NeutAFNom   => f.nnom
    };

-- We could reverse Number / Gender... 
  param AForm = AF Number Gender Case | NeutAFNom ;

--  Maybe we should remove isPost...
  oper AdjPhrase = { s : AForm => Str; adv:Str  ; isPost : Bool };

  -- No need to use PlurAF because it is used to restrict the forms.
  cast_aform_exp : NounAgrCat * Number * Case => AForm ;
  cast_aform_exp = table { 
    <SingPlur g,Sg,c> => AF Sg g c;
    <SingPlur g,Pl,c> => AF Pl g c;
    <PlurOnly g,_,c> => AF Pl g c;
    <NoAgr,_,_> => NeutAFNom
    };

  -- No need to use PlurAF because it is used to restrict the forms.
  cast_aform : GenNum * Case => AForm ;
  cast_aform = table { 
    <MascSg,c> => AF Sg Masc c;
    <MascPl,c> => AF Pl Masc c;
    <FemSg,c> => AF Sg Fem c;
    <FemPl,c> => AF Sg Masc c;
    <Neut,_> => NeutAFNom
    };

--4 Pronoun

----------------------- Parameter for pronouns -------------------------
-- The AfterPrep parameter is introduced in order to describe --FIXME
-- the variations of the third person personal pronoun forms
-- depending on whether they come after a preposition or not. 

--   param AfterPrep = Pre | Post ; --removed

-- The sp field stands for the possesive variant of the pronoun.

  oper Pron = NounPhrase ** { possForms: AForm => Str} ;

--6 Complement definition
  
  -- Limiting complement cases 
  param ComplCase = GenC | DatC | AccC | InsC | LocC ;
  
  oper 
  Complement : Type = {s : Str; cas : ComplCase} ; -- complement case + prep or "" (pb laukti manęs / mano padarytas)

  
  -- No control on 'Nom' and 'VocL'
  mkCompl : Str -> Case -> Complement;
  mkCompl prep prepCas = { 
    s = prep;
    cas = case prepCas of { Gen => GenC; Dat => DatC; Ins => InsC; Loc => LocC; _ => AccC }; 
    } ;

  extract_case = table {GenC => Gen; DatC => Dat; AccC => Acc; InsC => Ins; LocC => Loc};

--7 Various types
--  param GenNum = MascPersSg | MascAniSg | MascInaniSg | FemSg | NeutSg | MascPersPl | OthersPl;

  param GenNum = MascSg | MascPl | FemSg | FemPl | Neut;

  --- AR 7/12/2010 for VerbPol.CompCN
  oper numGenNum : GenNum -> Number = \n -> case n of {
    MascPl | FemPl => Pl ;
    _ => Sg
    } ;
  --- AR 6/2/2018 
{-

--Il faudra partir d'autre chose et arriver à autre chose... (peut-être utiliser directement genGenNum)
  oper genGenNum : AgrType -> Gender = \n -> case n of {
    (FreeAgr Fem|PlurAgr Fem) => Fem ;
    _ => Masc ;
--    NoAgr => Neut ---- NeutGr ?
    } ;
-}

  param MaybeGenNum = NoGenNum | JustGenNum GenNum;

  oper
  NounPhrase : Type = { 
    nom: Str; voc: Str; dep: ComplCase => Str; -- dep = dependent cases
    gn: GenNum; p : Person ; nomType : NomType };

  cast_gennum : NounAgrCat * Number => GenNum ;
  cast_gennum = table { 
    <SingPlur Masc,Sg> => MascSg; 
    <SingPlur Fem,Sg> => FemSg; 
    <(SingPlur Masc|PlurOnly Masc),Pl> => MascPl; 
    <(SingPlur Fem|PlurOnly Fem),Pl> => FemPl; 
    _ => Neut
  };

  extract_num = table { (MascPl|FemPl) => Pl ; _ => Sg } ;

-- dopelniacz negacji
    npcase : Polarity * ComplCase => ComplCase = 
        table { 
            <Neg, AccC> => GenC;
            <_, c>      => c
          };

-- Determiners 

  param NumComb = AgrComb | GenComb ; -- GenSing
  
  oper
  IDeterminer : Type = { s: Case => NounAgrCat => Str; nb: Number; numAgr: NumComb ; detType: DetType };
  Determiner  : Type = { s: Case => NounAgrCat => Str; nb: Number; numAgr: NumComb ; detType: DetType };

  oper 
    accom_case : NumComb * Case * NounAgrCat => Case = 
    table {
      <GenComb, _, _> => Gen ;
      x               => x.p2
    };

-- A loose translation of "its" (reflexive)
-- Est-il utilisé ou seulement le suivant ?
  oper reflPron: GenNum -> Pron = \a ->
	 { possForms = \\_ => "savo" ;
	   nom = "[SAVO]" ; -- Does not exist... 
	   voc = "[SAVO]" ; -- Does not exist...
	   dep = table {  
	     GenC => "savęs";
	     DatC => "sau";
	     AccC => "save"; 
	     InsC => "savimi";
	     LocC => "savyje"
	     };
	   p  = P3 ; -- Formal fix...
	   gn = a ;
	   nomType = Pro 
	 };

-- Le précédent est-il utilisé ?
  oper reflPronForms: ComplCase => Str = table {
	     GenC => "savęs";
	     DatC => "sau";
	     AccC => "save"; 
	     InsC => "savimi";
	     LocC => "savyje"
	     };

  finalComma : Str = pre {"," | "." => []; _ => SOFT_BIND ++ ","} ;
--  finalComma : Str = pre {"," | "." => []; "" => SOFT_BIND ++ ","; _ => []} ;

  
-- this pronoun is an approximate translation of indef article; preventing DetNP parsing problems
  a_Det = {
	  s = table {
	    Nom => table { SingPlur Masc => "kažkoks"; SingPlur Fem=>"kažkokia"; _ => "kažkoks" };
	    Acc => table { SingPlur Masc => "kažkokį"; SingPlur Fem=>"kažkokią"; _ => "kažkokį" };
	    Gen => table { SingPlur Masc => "kažkokio"; SingPlur Fem=>"kažkokios"; _ => "kažkokio" };
	    Ins => table { SingPlur Masc => "kažkokiu"; SingPlur Fem=>"kažkokia"; _=> "kažkokiu" };
	    Dat => table { SingPlur Masc => "kažkokiam"; SingPlur Fem=>"kažkokiai"; _ => "kažkokiam" };
	    Loc => table { SingPlur Masc => "kažkokiame"; SingPlur Fem=>"kažkokioje"; _=> "kažkokiame" };
	    VocL => table {SingPlur Masc => "kažkoks"; SingPlur Fem=>"kažkokia"; _ => "kažkoks" }
	  };
          detType=NormalDet ;
	  nb = Sg;
	  numAgr = AgrComb
    } ;

-- this pronoun is an approximate translation of indef article; preventing DetNP parsing problems
  a_Pl_Det = {
	  s = table {
	    Nom => table { SingPlur Masc => "kažkokie"; SingPlur Fem=>"kažkokios"; _ => "kažkokie" };
	    Acc => table { SingPlur Masc => "kažkokius"; SingPlur Fem=>"kažkokias"; _ => "kažkokius" };
	    Gen => table { SingPlur Masc => "kažkokių"; SingPlur Fem=>"kažkokių"; _ => "kažkokių" };
	    Ins => table { SingPlur Masc => "kažkokiais"; SingPlur Fem=>"kažkokiomis"; _=> "kažkokiais" };
	    Dat => table { SingPlur Masc => "kažkokiems"; SingPlur Fem=>"kažkokioms"; _ => "kažkokiems" };
	    Loc => table { SingPlur Masc => "kažkokiuose"; SingPlur Fem=>"kažkokiose"; _=> "kažkokiuose" };
	    VocL => table {SingPlur Masc => "kažkokie"; SingPlur Fem=>"kažkokios"; _ => "kažkokie" }
	  };
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = AgrComb
    } ;

-- this pronoun is an approximate translation of def article; preventing DetNP parsing problems
  the_Det = {
	  s = table {
	    Nom => table { SingPlur Masc => "šitas"; SingPlur Fem=>"šita"; _ => "šitas" };
	    Acc => table { SingPlur Masc => "šitą"; SingPlur Fem=>"šitą"; _ => "šitą" };
	    Gen => table { SingPlur Masc => "šito"; SingPlur Fem=>"šitos"; _ => "šito" };
	    Ins => table { SingPlur Masc => "šitu"; SingPlur Fem=>"šita"; _=> "šitu" };
	    Dat => table { SingPlur Masc => "šitam"; SingPlur Fem=>"šitai"; _ => "šitam" };
	    Loc => table { SingPlur Masc => "šitame"; SingPlur Fem=>"šitoje"; _=> "šitame" };
	    VocL => table {SingPlur Masc => "šitas"; SingPlur Fem=>"šita"; _ => "šitas" }
	  };
          detType=NormalDet ;
    	  nb = Sg;
	  numAgr = AgrComb
    } ;

-- this pronoun is an approximate translation of def article; preventing DetNP parsing problems
  the_Pl_Det = {
	  s = table {
	    Nom => table { SingPlur Masc => "šitie"; SingPlur Fem=>"šitos"; _ => "šitie" };
	    Acc => table { SingPlur Masc => "šituos"; SingPlur Fem=>"šitas"; _ => "šitie" };
	    Gen => table { SingPlur Masc => "šitų"; SingPlur Fem=>"šitų"; _ => "šitie" };
	    Ins => table { SingPlur Masc => "šitais"; SingPlur Fem=>"šitomis"; _=> "šitie" };
	    Dat => table { SingPlur Masc => "šitiems"; SingPlur Fem=>"šitoms"; _ => "šitie" };
	    Loc => table { SingPlur Masc => "šituose"; SingPlur Fem=>"šitose"; _=> "šitie" };
	    VocL => table {SingPlur Masc => "šitie"; SingPlur Fem=>"šitos"; _ => "šitie" }
	  };
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = AgrComb
    } ;


} ; 
