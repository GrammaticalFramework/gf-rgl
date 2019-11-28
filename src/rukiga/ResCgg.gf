--# -path=.:../prelude:../abstract:../common

-- structure of module types i.e header which defines the type of module and 
-- tells what other modules it inherits also known as extension(means that a module can inherit the contents
-- of one or more modules to which new judgements are added. You can optionally inherit parts of it using 
-- [comma separeted functions]) followed by ** and optionally an open statement(if the module type is concrete 
-- or resource i.e open <module name>[namespace identifiers]) and then followed by a mandatory body {}
-- opening a resource means you can use its contents without inheriting them (how I do not know)
-- module types include:
--    1.abstract modules
--    2.concrete modules
--    3.resource modules: these define parameter types and operations usable in several concrete syntaxes as general functions in lincat definitions
-- general structure: <module type> <module name> [of <abstrcat module name if module type is concrete>] = [<extended modules[comma, separted list of objects inherited]>] [**][open <module names[namespace identifiers]> in]{body}
resource ResCgg = ParamX ** --
  open Prelude, (Predef=Predef) in {

param 
	--Number = Sg | Pl;
	--Person = Per1 | Per2 | Per3;
  --Use Noun Class (NClass) as suggested in J&M chapter 3 pg 86.
	Gender =  MU_BA | KI_BI | N_N | KU_MA  |  BU_MA | 
					  RU_BU | GU_GA | ZERO_ZERO  |  MU_MI |  RI_MA | 
					  I_MA  | KA_BU | KA_TU | RU_N |  RU_MA |  HA | 
					  MU |  KU  |  ZERO_BU  |  ZERO_BI | ZERO_MA |  
					  ZERO_MI |  ZERO_TU |  ZERO_N  | I_ZERO  |  
					  RI_ZERO |  KU_ZERO | MU_ZERO |  RU_ZERO |  
					  KA_ZERO |ZERO_BAA | N_ZERO | KI_ZERO | Null;
	Case = Acc | Nom |Gen; -- we need to include Gen because we shall need it with Gen Pronouns
	RCase = RSubj | RObj;
  RForm = RF RCase | Such_That;
  ComplType = Nn |Ap | Adverbial |AdverbialVerb | Empty; 
  	PersonalPronounType = SubjM | Obj  | RelSubj | RelObj |
                          AdjPron2 | -- aAdjectival Prefixes with initial vowel with the semantics of "the" e.g. -- omuntu o-mu-rungi 
                          AdjPron  | -- without initial vowel i.e. -- omuntu mu-rungi           
                          --GenPron  | -- different types of pronouns
                          GenPrep1 |
                          GenPrep2 |
                          GenAdj   |
                          SStandPron ; --Self-standing pronouns
  
  ImpPol = ImpPos | ImpNeg;
  INumber = ISg | IPl | INeut;
  {-
    --there are several and i.e. 
    -- na (two nouns, 2 Noun Phrases, 2 Pronouns, 2 relative subject clauses, )
    --kandi (clauses having a commonality of subjects, object or tense)
    --the best structure is a table
  -}
  ConjArg = Nn_Nn | Nps_Nps | Pns_Pns | RelSubjCls | Other;
  AgrConj = AConj ConjArg;
  Agreement =  AgP3 Number Gender | AgMUBAP1 Number |AgMUBAP2 Number | NONE; --Default is AgP3 Sg KI_BI
  AgrExist = AgrNo | AgrYes Agreement;
  --Position = PostDeterminer | PreDeterminer ;
  Position = Post | Pre;
  Variants = V1|V2;
	--Functional forms of the regular verb
	Mood = Infinitive | Imperative | Subjunctive | Perfective;
	VerbCat = Simple | Prepositional | Causative;
	Voice = Active | Passive;
  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = VF Voice Mood VerbCat ;
  -- may not need it
  NounCat = ComNoun | PropNoun; --prepositions agree with nouns to form adverbial Phrases
  PrepForm = Form1 | Form2; -- omu and omuri, aha, ahari
  -- for Extra Tenses not implemented
  -- would be better if I had alliases
  TensesExtra = RemotePast | ImmediatePast | RemoteFuture;

  -- for Extra Aspects not implemented
  -- would be better if I had alliases
  Aspect = Performative | Perfect | Resultative | Retrospective | Habitual | Progressive | Persitive; 
{-
	Complete = Nouns with IV, 
	Incomplete = Nouns without IV: important for use with pre-determiners
	like buri i.e every

-}
NounState  = Complete | Incomplete ; 
VVMood = VVImp | VVPerf | VVBoth;



oper
  -- the is for Common Nouns only 
  Noun : Type = {s :  Number=>  NounState=> Str ; gender : Gender; nounCat:NounCat} ;

  ivs : pattern Str = #("a" | "e" | "o"); --pattern for initial vowels
  
  human_relations: pattern Str = --expand this list
  	#("Taata" | "Maama" | "Shwento" | "Shwenkuru" | "Nyinento" | "Nyinenkuru");

  	
  mkNoun : Str -> Str ->Gender ->Noun = \sg,pl, g -> {
  	s = table {
	     		Sg => table {Complete => sg; Incomplete => Predef.drop 1 sg};
	     		Pl => table {Complete => pl; Incomplete => Predef.drop 1 pl}
     		};
     	gender = g;
      nounCat = ComNoun;
	 	
	 };
	 

      

    mkVerb : Str ->Str ->Str ->Verb = \rad, end1,end2 ->{
    	s = rad;
    	pres = end1;
    	perf = end2;
      isPresBlank = False;
      isPerfBlank =  False;
    	--morphs = mkVerbMorphs;
    	isRegular = False;
	};
	--These are regular verbs with {a-ire} entry in the dictionary
	smartVerb : Str ->Verb = \rad ->{
    	s = rad;
    	pres = "a";
    	perf = "ire";
    	--morphs = mkVerbMorphs;
      isPresBlank = False;
      isPerfBlank = False;
    	isRegular = True;
	};
  
  {-  Smart paradigm
      This operation needs thorough testing with all nouns from a file
  -} 
  smartNoun : Str -> Gender -> Noun 
    = \omuntu, g ->
      case <omuntu , g> of {
        -- Handling the Tone System is also another problem.
        
        < "o" + "mu" + stem, MU_BA > => mkNoun omuntu ("aba" + stem) g ;
        --special cases like omwana, omwishiki, omwojo
        
        < "o" + "mw" +  stem, MU_BA > => mkNoun omuntu (combine_morphemes "aba" stem) g ; --same as mu_ba but the "u" + "a" of the stem to form mwa  
        < "o" + "mu" +  stem, MU_MI > => mkNoun omuntu (combine_morphemes "emi" stem) g ;
        < "o" + "ru" +  stem, RU_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        < "o" + "ru" +  stem, RU_N >  => mkNoun omuntu (combine_morphemes "en" stem) g  ; --desist from providing a singlar only but give both
        < "o" + "bu" +  stem, BU_MA >  => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        < "o" + "gu" +  stem, GU_GA >  => mkNoun omuntu (combine_morphemes "aga" stem) g  ;
        < "o" + ("ku" | "kw") +  stem, KU_MA >  => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        < "o" +  "kw" +  stem, KU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ;
        < "o" + "ku" +  stem, KU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "o" + "mu" +  stem, MU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "o" + "mu" +  stem, MU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "o" + "ru" +  stem, RU_BU >  => mkNoun omuntu (combine_morphemes "obu" stem) g  ;
        < "o" + "ru" +  stem, RU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural

        < "a" + "ha" + stem, HA_ZERO > => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "a" + "ka" + stem, KA_BU > => mkNoun omuntu (combine_morphemes "obu" stem) g  ;
        < "a" + "ka" + stem, KA_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural

        < "e" + "ki" + stem, KI_BI > => mkNoun omuntu (combine_morphemes "ebi" stem) g  ;
        < "e" + "ki" + stem, KI_ZERO > => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "e" + "i" + stem, I_MA > => mkNoun omuntu (combine_morphemes "ama" "") g  ;
        < "e" + "i" + stem, I_ZERO > => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "e" + "ri" + stem, RI_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        < "e" + "ri" + stem, RI_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "e" + "ry" + stem, I_MA | RI_MA> => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        -- --special cases shall be added with due course as errors are identified
        <"e" + "ky" + stem, KI_BI> => mkNoun omuntu (combine_morphemes "ebi" stem) g  ; 
        < _ ,N_N | ZERO_MA | ZERO_ZERO > => mkNoun omuntu  omuntu g  ;
        --< _ ,ZERO_MA > => mkNoun omuntu  ("ama" + stem) g (Predef.drop 1 omuntu);
        --< _ ,> => mkNoun omuntu  omuntu g (Predef.drop 1 omuntu);
        <_ , ZERO_BAA>  => mkNoun omuntu ("baa" + omuntu) g ;
        < _ ,_ > => mkNoun omuntu  omuntu g-- improve as we go on.
    };



{-
      This function tries to handle phonological-conditioning.

      Usage: Use it whenever you are trying to combine morphemes especially in:
      1. Pronouns
      2. Verbs and verb Phrases.
      3. Noun Phrases
      3. Adjectival Phrases e.t.c

      Given two morphemes A and B to combine,
      1. compare the last letter of the first morpheme A with the first letter of the second morpheme B
      2. Use parttern matching to obtain the right letters for the comnined word
      
      Source of rules:
      1. personal experience
      2. Morris and Kirwan Runynakore Grammar 
      3. but we shall add more as we meet them during debugging
  -}
  combine_morphemes : Str -> Str -> Str ;
  combine_morphemes = \ f, s ->
    case <(Predef.dp 1 f), (Predef.take 1 s)> of {
         <"n" , "r"> => f + "d" + (Predef.drop 1 s) ;
         <"u" , "a" | "e" | "o" | "i"> => Predef.tk 1 f + "w" + s ;
         <"i" , "a" | "e" | "o"> => Predef.tk 1 f + "y" + s ;
         <"n" , "b" | "p"> => Predef.tk 1 f + "m" + s ;
         <"n" , "m"> => Predef.tk 1 f + s ; -- However, note that for pronouns, the n changes to m
         <"n" , "h"> => Predef.tk 1 f + "mp" + Predef.drop 1 s ;
         <"i", "i">  => f + Predef.drop 1 s ;
         <_ , _ > => f + s
    } ;
  

    {-
      combine_morphemes = \ f, s ->
      case <(f), (s)> of {
           <a+ "n" , "r"+ g> =>  f ++  "d" ++   g;
           <a+ "nd" , "i"+ g> =>  a ++ Predef.BIND ++  "nz" ++ Predef.BIND ++  s;
           <a+ "u" , ("a" | "e" | "o" | "i") + g> =>  a ++ Predef.BIND ++  "w" ++ Predef.BIND ++ s ;
           <a+ "i" , ("a" | "e" | "o") +g > =>  a ++ Predef.BIND ++  "y" ++ Predef.BIND ++  s ;
           <a+ "n" , ("b" | "p") + g> =>  a ++ Predef.BIND ++  "m" ++ Predef.BIND ++ s ;
           <a+ "n" , "m" + g> =>  a ++ Predef.BIND ++  s ; -- However, note that for pronouns, the n changes to m
           <a+ "n" , "h" +g > =>  a ++ Predef.BIND ++  "mp" ++ Predef.BIND ++  g ;
           <a+ "i", "i" + g>  =>  f ++ Predef.BIND ++  g ;
           <_ , _ > => f ++   s
      } ;
    -}

    --separate
    {-
      dealing with ProperNouns 
      They do not have plurals but when a proper noun
       refers to a place then it is important to keep
       that label because is helps us disambiguate which
       preposition to use for in, on and at i.e LOCATIVES omuri, ahari, etc
    -}
    ProperNoun : Type = {s: Str ; a:Agreement ; isPlace : Bool; nounCat:NounCat};
    mkPN : Str -> Agreement -> Bool -> ProperNoun = \ pn, a, b->
    {
      s = pn ;
      a = a;
      isPlace = b;
      nounCat = PropNoun;
    } ;

    -- concatenates the string left to right
    mkClitic : Str -> Str = \c -> c ++ Predef.BIND ;

    -- concatenates the string right to left
    mkSuffix : Str -> Str = \c -> Predef.BIND ++ c ;

    -- creating clitics depending on number
    mkClitics : Str -> Str -> Number -> Str = \sg,pl,n ->
      case n of {
          Sg => mkClitic sg ; 
          Pl => mkClitic pl
        } ;

mkAgreement: Gender -> Person -> Number ->Agreement =\g,p,n ->
			case <g,p,n> of{
				 <MU_BA, P1, n> => AgMUBAP1 n;
				 <MU_BA, P2, n> => AgMUBAP2 n;
				 <g,P3,n>       => AgP3 n g;
				 <_,_,_,>       => NONE 

			};   
mkSubjPrefix : Agreement -> Str =\a ->case a of {
          AgMUBAP1 n => mkClitics "n" "tu" n;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 n => mkClitics "o" "mu" n;
          --AgMUBAP2 Pl => "mu" ;
          AgP3 n MU_BA  => mkClitics "a" "ba" n;
          --AgP3 Pl MU_BA  => "ba" ;          
          AgP3 Sg KI_BI   => mkClitic "ki" ;
          AgP3 Pl (KI_BI | ZERO_BI)   => mkClitic "bi" ;
          AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => mkClitic "ru" ; 
          AgP3 Pl RU_N => mkClitic "zi"; --| "i"; 
          AgP3 Sg N_N => mkClitic "e";
          AgP3 Pl N_N => mkClitic "zi"; --| "i";
          AgP3 Sg (MU_MI | MU_ZERO)   => mkClitic "gu" ; 
          AgP3 Pl MU_MI   => "e" ;
          AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) =>mkClitic  "ri"; 
          AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => mkClitic "ga" ;
          AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => mkClitic "ka" ; 
          AgP3 Pl (KA_BU | RU_BU)  => mkClitic "bu" ;
          AgP3 Sg ZERO_BU  => mkClitic "bu" ; 
          AgP3 Pl ZERO_BU  => mkClitic "bu" ;
          AgP3 Sg ZERO_BI  => mkClitic "bi" ; 
          AgP3 Sg ZERO_MA  => mkClitic "ga" ;
          AgP3 Pl RI_ZERO  => mkClitic "ga" ;
          AgP3 Sg KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl MU_ZERO  => mkClitic "gu" ;
          AgP3 Pl RU_ZERO  => mkClitic "ru" ;
          AgP3 Sg ZERO_TU  => mkClitic "tu" ;
          AgP3 Pl ZERO_TU  => mkClitic "tu" ;
          AgP3 Sg (ZERO_MI | ZERO_ZERO)  => mkClitic "" ;
          AgP3 Pl ZERO_MI  => mkClitic "e" ;
          AgP3 Pl KA_ZERO  => mkClitic "" ;
          _        => mkClitic "SubjNotKnown" --for checking if there is some class unaccounted for
      };
    mkSubjClitic : Agreement -> Str = \a ->
      case a of {
          AgMUBAP1 n => mkClitics "n" "tu" n;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 n => mkClitics "o" "mu" n;
          --AgMUBAP2 Pl => "mu" ;
          AgP3 n MU_BA  => mkClitics "a" "ba" n;
          --AgP3 Pl MU_BA  => "ba" ;          
          AgP3 Sg KI_BI   => mkClitic "ki" ;
          AgP3 Pl (KI_BI | ZERO_BI)   => mkClitic "bi" ;
          AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => mkClitic "ru" ; 
          AgP3 Pl RU_N => mkClitic "zi"; --| "i"; 
          AgP3 Sg N_N => mkClitic "e";
          AgP3 Pl N_N => mkClitic "zi"; --| "i";
          AgP3 Sg (MU_MI | MU_ZERO)   => mkClitic "gu" ; 
          AgP3 Pl MU_MI   => "e" ;
          AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) =>mkClitic  "ri"; 
          AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => mkClitic "ga" ;
          AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => mkClitic "ka" ; 
          AgP3 Pl (KA_BU | RU_BU)  => mkClitic "bu" ;
          AgP3 Sg ZERO_BU  => mkClitic "bu" ; 
          AgP3 Pl ZERO_BU  => mkClitic "bu" ;
          AgP3 Sg ZERO_BI  => mkClitic "bi" ; 
          AgP3 Sg ZERO_MA  => mkClitic "ga" ;
          AgP3 Pl RI_ZERO  => mkClitic "ga" ;
          AgP3 Sg KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl MU_ZERO  => mkClitic "gu" ;
          AgP3 Pl RU_ZERO  => mkClitic "ru" ;
          AgP3 Sg ZERO_TU  => mkClitic "tu" ;
          AgP3 Pl ZERO_TU  => mkClitic "tu" ;
          AgP3 Sg (ZERO_MI | ZERO_ZERO)  => mkClitic "" ;
          AgP3 Pl ZERO_MI  => mkClitic "e" ;
          AgP3 Pl KA_ZERO  => mkClitic "" ;
          _        => mkClitic "SubjNotKnown" --for checking if there is some class unaccounted for
      };


      mkSubjCliticTable : Agreement => Str = table {
          AgMUBAP1 n => mkClitics "n" "tu" n;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 n => mkClitics "o" "mu" n;
          --AgMUBAP2 Pl => "mu" ;
          AgP3 n MU_BA  => mkClitics "a" "ba" n;
          --AgP3 Pl MU_BA  => "ba" ;          
          AgP3 Sg KI_BI   => mkClitic "ki" ;
          AgP3 Pl (KI_BI | ZERO_BI)   => mkClitic "bi" ;
          AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => mkClitic "ru" ; 
          AgP3 Pl RU_N => mkClitic "zi"; --| "i"; 
          AgP3 Sg N_N => mkClitic "e";
          AgP3 Pl N_N => mkClitic "zi"; --| "i";
          AgP3 Sg (MU_MI | MU_ZERO)   => mkClitic "gu" ; 
          AgP3 Pl MU_MI   => "e" ;
          AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) =>mkClitic  "ri"; 
          AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => mkClitic "ga" ;
          AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => mkClitic "ka" ; 
          AgP3 Pl (KA_BU | RU_BU)  => mkClitic "bu" ;
          AgP3 Sg ZERO_BU  => mkClitic "bu" ; 
          AgP3 Pl ZERO_BU  => mkClitic "bu" ;
          AgP3 Sg ZERO_BI  => mkClitic "bi" ; 
          AgP3 Sg ZERO_MA  => mkClitic "ga" ;
          AgP3 Pl RI_ZERO  => mkClitic "ga" ;
          AgP3 Sg KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl MU_ZERO  => mkClitic "gu" ;
          AgP3 Pl RU_ZERO  => mkClitic "ru" ;
          AgP3 Sg ZERO_TU  => mkClitic "tu" ;
          AgP3 Pl ZERO_TU  => mkClitic "tu" ;
          AgP3 Sg (ZERO_MI | ZERO_ZERO)  => mkClitic "" ;
          AgP3 Pl ZERO_MI  => mkClitic "e" ;
          AgP3 Pl KA_ZERO  => mkClitic "" ;
          _        => mkClitic "SubjNotKnown" --for checking if there is some class unaccounted for
      };

      mkSubjCliticTableSg : Agreement => Str = table {
          AgMUBAP1 Sg => mkClitic "n" ;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 Sg => mkClitic "o" ;
          --AgMUBAP2 Pl => "mu" ;
          AgP3 Sg MU_BA  => mkClitic "a";
          --AgP3 Pl MU_BA  => "ba" ;          
          AgP3 Sg KI_BI   => mkClitic "ki" ;
          --AgP3 Pl (KI_BI | ZERO_BI)   => mkClitic "bi" ;
          AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => mkClitic "ru" ; 
          --AgP3 Pl RU_N => mkClitic "zi"; --| "i"; 
          AgP3 Sg N_N => mkClitic "e";
          --AgP3 Pl N_N => mkClitic "zi"; --| "i";
          AgP3 Sg (MU_MI | MU_ZERO)   => mkClitic "gu" ; 
          --AgP3 Pl MU_MI   => "e" ;
          AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) =>mkClitic  "ri"; 
          --AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => mkClitic "ga" ;
          AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => mkClitic "ka" ; 
          --AgP3 Pl (KA_BU | RU_BU)  => mkClitic "bu" ;
          AgP3 Sg ZERO_BU  => mkClitic "bu" ; 
          --AgP3 Pl ZERO_BU  => mkClitic "bu" ;
          AgP3 Sg ZERO_BI  => mkClitic "bi" ; 
          AgP3 Sg ZERO_MA  => mkClitic "ga" ;
          --AgP3 Pl RI_ZERO  => mkClitic "ga" ;
          AgP3 Sg KU_ZERO  => mkClitic "ku" ;
          --AgP3 Pl KU_ZERO  => mkClitic "ku" ;
          --AgP3 Pl MU_ZERO  => mkClitic "gu" ;
          --AgP3 Pl RU_ZERO  => mkClitic "ru" ;
          AgP3 Sg ZERO_TU  => mkClitic "tu" ;
          --AgP3 Pl ZERO_TU  => mkClitic "tu" ;
          AgP3 Sg (ZERO_MI | ZERO_ZERO)  => mkClitic "" ;
          --AgP3 Pl ZERO_MI  => mkClitic "e" ;
          --AgP3 Pl KA_ZERO  => mkClitic "" ;
          _        => mkClitic "Non-Signular" --for checking if there is some class unaccounted for
      };

      mkSubjCliticTablePl : Agreement => Str = table {
          --AgMUBAP1 Sg => mkClitic "n" ;
          AgMUBAP1 Pl => mkClitic "tu" ;
          --AgMUBAP2 Sg => mkClitic "o" ;
          AgMUBAP2 Pl => mkClitic "mu" ;
          --AgP3 Sg MU_BA  => mkClitic "a";
          AgP3 Pl MU_BA  => mkClitic "ba" ;          
          --AgP3 Sg KI_BI   => mkClitic "ki" ;
          AgP3 Pl (KI_BI | ZERO_BI)   => mkClitic "bi" ;
          --AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => mkClitic "ru" ; 
          AgP3 Pl RU_N => mkClitic "zi"; --| "i"; 
          --AgP3 Sg N_N => mkClitic "e";
          AgP3 Pl N_N => mkClitic "zi"; --| "i";
          --AgP3 Sg (MU_MI | MU_ZERO)   => mkClitic "gu" ; 
          AgP3 Pl MU_MI   => "e" ;
          --AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) =>mkClitic  "ri"; 
          AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => mkClitic "ga" ;
          --AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => mkClitic "ka" ; 
          AgP3 Pl (KA_BU | RU_BU)  => mkClitic "bu" ;
          --AgP3 Sg ZERO_BU  => mkClitic "bu" ; 
          AgP3 Pl ZERO_BU  => mkClitic "bu" ;
          --AgP3 Sg ZERO_BI  => mkClitic "bi" ; 
          --AgP3 Sg ZERO_MA  => mkClitic "ga" ;
          AgP3 Pl RI_ZERO  => mkClitic "ga" ;
          --AgP3 Sg KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl MU_ZERO  => mkClitic "gu" ;
          AgP3 Pl RU_ZERO  => mkClitic "ru" ;
          --AgP3 Sg ZERO_TU  => mkClitic "tu" ;
          AgP3 Pl ZERO_TU  => mkClitic "tu" ;
          --AgP3 Sg (ZERO_MI | ZERO_ZERO)  => mkClitic "" ;
          AgP3 Pl ZERO_MI  => mkClitic "e" ;
          AgP3 Pl KA_ZERO  => mkClitic "" ;
          _        => mkClitic "Plural" --for checking if there is some class unaccounted for
      };
       {-Object particle may be used as 
          1. a prefix: e.g mu-kwate = catch him,
          2. an infix: o-mu-kwate   = you catch him

      -}
      mkObjClitic : Agreement -> Str = \a ->case a of {
       
          AgMUBAP1 n => mkClitics "n" "tu" n;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 n => mkClitics "ku" "ba" n;
          --AgMUBAP2 Pl => "ba" ;
          AgP3 Sg MU_BA => mkClitic "mu" ;
          AgP3 Pl MU_BA =>  mkClitic "ba";
          AgP3 Pl (ZERO_BU | KA_BU | KA_TU | RU_BU) => mkClitic "bu" ;
          AgP3 Sg BU_MA => mkClitic "bu" ;
          AgP3 Sg KI_BI => mkClitic "ki" ; 
          AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "bi";
          AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ga" ;
          AgP3 (Sg | Pl) HA => mkClitic "ha";
          AgP3 Sg (I_ZERO | I_MA | RI_MA) => mkClitic "ri" ;
          AgP3 Sg (KA_ZERO | KA_BU | KA_TU) => mkClitic "ka" ;
          AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "ku" ;
          {- 
             #comment for the following two lines
             the follwing partciles are all used by Noun Classes of Place i.e. HA, KU and MU
             We take the particle to be "ha" for all of them although noun class KU can use
             another particle "gi" -- see Table of Concords in Appendix of Dictionary by Mpairwe and Kahangi

             Note: The particles do not change with respect to gender

             TODO: obtain clear examples of usage
          -}
          AgP3 (Sg | Pl) (HA | MU) => mkClitic "ha" ;
          AgP3 (Sg | Pl) KU => mkClitic "ha" ;  -- gi is also possible -- see comment above

          AgP3 Sg (RU_N | RU_ZERO | RU_BU | RU_MA) => mkClitic "ru" ;
          AgP3 Pl (KA_TU | ZERO_TU) => mkClitic "tu" ;
          
          AgP3 Sg (N_N | ZERO_ZERO) => mkClitic "gi" ; 
         
          AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gu" ;
          AgP3 Pl  GU_GA => "ga" ; 
          AgP3 Pl (MU_MI | ZERO_MI) => mkClitic "gi" ; 
          {-
              According to Mpaiwe & Kahangi in their table of concords, the particle for the plural
              of noun classes N_N , ZERO_ZERO , ZERO_N & RU_N can be either "i" or "zi" depending
              on object they refer to. 
              
              Problem:
              However, we cannot use the | operator in strings as GF will
              fail to compile to comletion. 
              Implication:
              Some of our output strings will have the wrong object particle attached.
              Even if the operator | worked, we would generate two versions of the linearized 
              string of which one would be right and the other wrong 
              What is the solution to this? 
          -}
          AgP3 Pl (N_N | ZERO_ZERO | ZERO_N | RU_N) => mkClitic "zi" ; --some cases require use of particle "i" 
          
          _ => mkClitic "-" -- Hopefully exhausted all forms 
        };
    Adverb = {s :Str; agr : AgrExist} ;
    mkAdv: Str -> AgrExist -> Adverb =\str, agr ->{s=str; agr=agr};
    --dealing with the adjective 
    {-
      The Adjective can be before the noun for TRUE or
      it can be after the noun (FLASE)
      Most Adjectives are stems which are meaningless 
      without adjectival prefixes. These prefixes are concords that agree
      with the noun before the them. 

      However some adjectives are self-standing.
      

      You can introduce a more meaningful name or using
      Inari's method of avoiding tables
      i.e. Adjective: Type = { pre : Str  ; post : Str; isPre: Bool; isProper : Bool}
            pre -- the adjective or adjective stem that comes before noun
            post --the adjective or adjective stem that comes after noun
            isPre --whether the adjective comes before (TRUE) or after (FALSE) the noun
            isProper -- True for a full adjective anf False for an adjectival stem
            isPrep -- does the adjective need a preposition especially those that come after the noun.
      improve that further by avoiding carrying a table of strings 
      using arne's technique
    -}
    --AdjectivalPhrase : Type {s : Str ; post : Str; isPre : Bool; isProper : Bool; isPrep: Bool} ;
    AdjectivalPhrase : Type = {s : Str ; position : Position; isProper : Bool; isPrep: Bool};
    --Adjective : Type = {s : Str ; post : Str; isPre : Bool; isProper : Bool; isPrep: Bool};
    Adjective : Type = {s : Str ; position : Position; isProper : Bool; isPrep: Bool};
    mkAdjective: Str -> Position -> Bool -> Bool -> Adjective = \ a , pos, isProper, isPrep -> 
     { s = a ; position = pos ; isPre = True; isProper = isProper; isPrep = isPrep}; 
      
    {-
        TO DO:
        --Subject prefixes / particles of clitics using bind
    -}
    -- Adjectival Prefixes with initial vowel with the semantics of "the" 
    mkAdjPronIVClitic : Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "omu" "aba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "omu" "aba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "omu" "aba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ama";
              AgP3 (Sg | Pl) (HA | MU) => mkClitic "aha" ; -- of place HA & MU
              AgP3 (Sg | Pl) KU => mkClitic "en" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aka" ;
              AgP3 Sg KI_BI   => mkClitic "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "omu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "en" ;
              AgP3 Pl ZERO_MI =>mkClitic "en" ;
              AgP3 Pl MU_MI => mkClitic "emi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "en" ;
              AgP3 Sg GU_GA => mkClitic "ogu" ;
              AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "XXX" -- error checking for any case not catered for

    };

    mkSStand :Agreement -> Str = \a -> case a of {
              AgMUBAP1 Sg =>  "nyowe";
              AgMUBAP1 Pl => "itwe";
              AgMUBAP2 Sg =>  "iwe" ; --probably an error check your grammar book
              AgMUBAP2 Pl => "imwe" ;
              AgP3 Sg MU_BA => "uwe" ;
              AgP3 Pl MU_BA => "bo" ;
              AgP3 Pl ZERO_BU =>  "bwo" ;
              AgP3 Sg BU_MA =>  "bwo" ;
              AgP3 Pl (KA_BU | RU_BU) =>  "bwo" ;
              AgP3 Pl (KI_BI | ZERO_BI) =>  "byo" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) =>  "go";
              AgP3 (Sg | Pl) HA  =>  "ho" ; -- of place HA 
              AgP3 (Sg | Pl) MU =>  "mwo" ; -- of place  MU
              AgP3 (Sg | Pl) KU =>  "yo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) => "ryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) => "ko" ;
              AgP3 Sg KI_BI   =>  "kyo" ;
              AgP3 Sg (KU_ZERO | KU_MA) =>  "kwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) =>  "gwo" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) =>  "rwo" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"two" ;
              AgP3 Sg (ZERO_ZERO | N_N) => "yo" ;
              AgP3 Pl ZERO_MI => "yo" ;
              AgP3 Pl MU_MI =>  "yo";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "zo" ;
              AgP3 Sg GU_GA => "gwo" ;
              AgP3 Pl GU_GA =>  "go" ;
              _  =>  "XXX-Failed SStand" -- error checking for any case not catered for

    };
    -- This involved only change of the personal pronouns as for selfstanding pronouns.
    -- How can it be done without code repeation?
    mkPredetPref :Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "ny" "itwe" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "we" "mwe" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "we" "bo" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "bwo" ;
              AgP3 Sg BU_MA => mkClitic "bwo" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "bwo" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "byo" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "go";
              AgP3 (Sg | Pl) HA  => mkClitic "ho" ; -- of place HA 
              AgP3 (Sg | Pl) MU => mkClitic "mwo" ; -- of place  MU
              AgP3 (Sg | Pl) KU => mkClitic "yo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "ryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ko" ;
              AgP3 Sg KI_BI   => mkClitic "kyo" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "kwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gwo" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "rwo" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "two" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "yo" ;
              AgP3 Pl ZERO_MI =>mkClitic "yo" ;
              AgP3 Pl MU_MI => mkClitic "yo";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "zo" ;
              AgP3 Sg GU_GA => mkClitic "gwo" ;
              AgP3 Pl GU_GA => mkClitic "go" ;
              _  => mkClitic "XXXPredPref" -- error checking for any case not catered for

    };
    
    -- TThis is for demonstrative pronouns which can also be use as Quantifiers
    -- How can it be done without code repeation?
    mkThis  = table{
              AgMUBAP1 Sg => mkClitic "ogu";
              AgMUBAP1 Pl => mkClitic "aba" ;
              AgMUBAP2 Sg => mkClitic "ogu"; --probably an error check your grammar book
              AgMUBAP2 Pl => mkClitic "aba" ;
              AgP3 Sg MU_BA => mkClitic "ogu";
              AgP3 Pl MU_BA => mkClitic "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "aga";
              AgP3 ( Pl) HA  => mkClitic "aha" ; -- of place HA 
              AgP3 ( Pl) MU => mkClitic "omu" ; -- of place  MU
              AgP3 ( Pl) KU => mkClitic "oku" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aka" ;
              AgP3 Sg KI_BI   => mkClitic "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "egi" ;
              AgP3 Pl ZERO_MI =>mkClitic "egi" ;
              AgP3 Pl MU_MI => mkClitic "egi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezi" ;
              AgP3 Sg GU_GA => mkClitic "ogu" ;
              AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "XXXThisThese" -- error checking for any case not catered for

    };
    {-
    -- TThis is for demonstrative pronouns which can also be use as Quantifiers
    -- How can it be done without code repeation?
    mkThis  = table{
              AgMUBAP1 Sg => mkClitic "ogu";
              --AgMUBAP1 Pl => mkClitic "aba" ;
              AgMUBAP2 Sg => mkClitic "ogu"; --probably an error check your grammar book
              --AgMUBAP2 Pl => mkClitic "aba" ;
              AgP3 Sg MU_BA => mkClitic "ogu";
              --AgP3 Pl MU_BA => mkClitic "aba" ;
              --AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obu" ;
              --AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              --AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
              --AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "aga";
              AgP3 (Sg ) HA  => mkClitic "aha" ; -- of place HA 
              AgP3 (Sg ) MU => mkClitic "omu" ; -- of place  MU
              AgP3 (Sg ) KU => mkClitic "oku" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aka" ;
              AgP3 Sg KI_BI   => mkClitic "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
              --AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "egi" ;
              --AgP3 Pl ZERO_MI =>mkClitic "egi" ;
              --AgP3 Pl MU_MI => mkClitic "egi";
              --AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezi" ;
              AgP3 Sg GU_GA => mkClitic "ogu" ;
              --AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "XXXThis" -- error checking for any case not catered for

    	};
	
    -- This is for demonstrative pronouns which can also be use as Quantifiers
    -- How can it be done without code repeation?
    mkThat  = table{
              AgMUBAP1 Sg => mkClitic ""; --"ogwo";
              AgMUBAP1 Pl => mkClitic ""; --"abo" ;
              AgMUBAP2 Sg => mkClitic ""; --"ogu"; --probably an error check your grammar book
              AgMUBAP2 Pl => mkClitic ""; --"abo" ;
              AgP3 Sg MU_BA => mkClitic ""; --"ogu";
              AgP3 Pl MU_BA => mkClitic ""; --"abo" ;
              AgP3 Pl ZERO_BU => mkClitic "obwo" ;
              AgP3 Sg BU_MA => mkClitic "obwo" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obwo" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebyo" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ago";
              AgP3 ( Pl) HA  => mkClitic "aho" ; -- of place HA 
              AgP3 ( Pl) MU => mkClitic "omwo" ; -- of place  MU
              AgP3 ( Pl) KU => mkClitic "okwo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ekyo" ;
              AgP3 Sg KI_BI   => mkClitic "ekyo" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "okwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogwo" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "orwo" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otwo" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "egyo" ;
              AgP3 Pl ZERO_MI =>mkClitic "egyo" ;
              AgP3 Pl MU_MI => mkClitic "egyo";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezo" ;
              AgP3 Sg GU_GA => mkClitic "ogwo" ;
              AgP3 Pl GU_GA => mkClitic "ago" ;
              _  => mkClitic "XXXThatThose" -- error checking for any case not catered for

    };
  -}
    -- TThis is for demonstrative pronouns which can also be use as Quantifiers
    -- How can it be done without code repeation?
    mkThat  = table {
              AgMUBAP1 Sg => mkClitic "ogwo";
              --AgMUBAP1 Pl => mkClitic "aba" ;
              AgMUBAP2 Sg => mkClitic "ogu"; --probably an error check your grammar book
              --AgMUBAP2 Pl => mkClitic "aba" ;
              AgP3 Sg MU_BA => mkClitic "ogu";
              --AgP3 Pl MU_BA => mkClitic "aba" ;
              --AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obwo" ;
              --AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              --AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
              --AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "aga";
              AgP3 (Sg ) HA  => mkClitic "aho" ; -- of place HA 
              AgP3 (Sg ) MU => mkClitic "omwo" ; -- of place  MU
              AgP3 (Sg ) KU => mkClitic "okwo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ako" ;
              AgP3 Sg KI_BI   => mkClitic "ekyo" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "okwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogwo" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "orwo" ;
              --AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "egyo" ;
              --AgP3 Pl ZERO_MI =>mkClitic "egi" ;
              --AgP3 Pl MU_MI => mkClitic "egi";
              --AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezi" ;
              AgP3 Sg GU_GA => mkClitic "ogwo" ;
              --AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "XXXThat" -- error checking for any case not catered for

    };

    -- Adjectival Prefixes without initial vowel with the semantics for adjectives used in Imperative negative form
    mkAdjPronNoIVClitic : Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "mu" "ba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "mu" "ba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "mu" "ba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "bu" ;
              AgP3 Sg BU_MA => mkClitic "bu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "bu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "bi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ma";
              AgP3 (Sg | Pl) (HA | MU) => mkClitic "ha" ; -- of place HA & MU
              AgP3 (Sg | Pl) KU => mkClitic "n" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "ri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ka" ;
              AgP3 Sg KI_BI   => mkClitic "ki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "ku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "mu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "ru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "tu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "n" ;
              AgP3 Pl ZERO_MI =>mkClitic "n" ;
              AgP3 Pl MU_MI => mkClitic "mi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "n" ;
              AgP3 Sg GU_GA => mkClitic "gu" ;
              AgP3 Pl GU_GA => mkClitic "ga" ;
              _  => mkClitic "XX" -- error checking for any case not catered for

    };
    -- Genetive Preposition: simple "of" without Initila vowel
    mkGenPrepNoIVClitic : Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "wa" "ba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "wa" "ba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "wa" "ba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "bwa" ;
              AgP3 Sg BU_MA => mkClitic "bwa" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "bwa" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "bya" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ga";
              AgP3 (Sg | Pl) HA => mkClitic "ha" ; -- of place HA 
              AgP3 (Sg | Pl) MU => mkClitic "mwa" ; -- of place MU
              AgP3 (Sg | Pl) KU => mkClitic "ya" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "rya" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ka" ;
              AgP3 Sg KI_BI   => mkClitic "kya" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "kwa" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gwa" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "rwa" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "twa" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "ya" ;
              AgP3 Pl ZERO_MI =>mkClitic "ya" ;
              AgP3 Pl MU_MI => mkClitic "emi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "za" ;
              AgP3 Sg GU_GA => mkClitic "gwa" ;
              AgP3 Pl GU_GA => mkClitic "ga" ;
              _  => mkClitic "Error mkGenPrepNoIVClitic" -- error checking for any case not catered for

    };

    -- Genetive Preposition: simple "of" with Initil vowel
    mkGenPrepWithIVClitic : Agreement => Str = table {
              AgMUBAP1 n => mkClitics "owa" "aba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "owa" "aba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "owa" "aba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "obwa" ;
              AgP3 Sg BU_MA => mkClitic "obwa" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obwa" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebya" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "aga";
              AgP3 (Sg | Pl) HA => mkClitic "aha" ; -- of place HA 
              AgP3 (Sg | Pl) MU => mkClitic "amwa" ; -- of place MU
              AgP3 (Sg | Pl) KU => mkClitic "aya" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "arya" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aka" ;
              AgP3 Sg KI_BI   => mkClitic "ekya" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "okwa" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogwa" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "orwa" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otwa" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "eya" ;
              AgP3 Pl ZERO_MI =>mkClitic "eya" ;
              AgP3 Pl MU_MI => mkClitic "emi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "eza" ;
              AgP3 Sg GU_GA => mkClitic "ogwa" ;
              AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "Error mkGenPrepWithIVClitic" -- error checking for any case not catered for

    };
   
    -- Genetive Adjectival suffix: Possessive my book= ekitabo kyagye
    mkGenAdjSuffix : Agreement -> Str =\a -> case a of {
              AgMUBAP1 n => mkClitics "ngye" "itu" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "we" "nyu" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "e" "bo" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "bwo" ;
              AgP3 Sg BU_MA => mkClitic "bwo" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "bwo" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "byo" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "go";
              AgP3 (Sg | Pl) HA => mkClitic "ho" ; -- of place HA 
              AgP3 (Sg | Pl) MU => mkClitic "mwo" ; -- of place MU
              AgP3 (Sg | Pl) KU => mkClitic "yo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "ryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ko" ;
              AgP3 Sg KI_BI   => mkClitic "kyo" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "kwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gwo" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "rwo" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "two" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "yo" ;
              AgP3 Pl ZERO_MI =>mkClitic "yo" ;
              AgP3 Pl MU_MI => mkClitic "yo";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "zo" ;
              AgP3 Sg GU_GA => mkClitic "gwo" ;
              AgP3 Pl GU_GA => mkClitic "go" ;
              _  => mkClitic "Error mkGenAdjSuffix" -- error checking for any case not catered for

    };


    mkRPs : RCase => Agreement =>Str = table{
     	RSubj => table {
     		  AgMUBAP1 Sg => mkClitic "o";
              AgMUBAP1 Pl => mkClitic "aba" ;
              AgMUBAP2 Sg => mkClitic "o"; 
              AgMUBAP2 Pl => mkClitic "aba" ;
              AgP3 Sg MU_BA => mkClitic "o";
              AgP3 Pl MU_BA => mkClitic "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              AgP3 Pl (KI_BI | ZERO_BI) =>  mkClitic "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "aga";
              AgP3 (Sg ) HA  => mkClitic "aha" ; -- of place HA 
              AgP3 (Sg ) MU => mkClitic "aha" ; -- of place  MU
              AgP3 (Sg ) KU => mkClitic "e" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aka" ;
              AgP3 Sg KI_BI   => mkClitic "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "e" ;
              AgP3 Pl ZERO_MI =>mkClitic "e" ;
              AgP3 Pl MU_MI => mkClitic "e";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezi" ;
              AgP3 Sg GU_GA => mkClitic "ogu" ;
              AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "XXXThat" -- error checking for any case not catered for
 				
 				};
	 	_ => table {
		  	AgMUBAP1 Sg => mkClitic "ou"; 
      		AgMUBAP1 Pl => mkClitic "abu" ; --note: abu or abi is used. GF does not allow free variation. However, abu is more natural
      		AgMUBAP2 Sg => mkClitic "ou"; --probably an error check your grammar book
      		AgMUBAP2 Pl => mkClitic "abu" ;
      		AgP3 Sg MU_BA => mkClitic "o";
      		AgP3 Pl MU_BA => mkClitic "abu" ;
      		AgP3 Pl ZERO_BU => mkClitic "obu" ;
      		AgP3 Sg BU_MA => mkClitic "obu" ;
      		AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
      		AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
      		AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "agu";
      		AgP3 (Sg ) HA  => mkClitic "ahu" ; -- of place HA 
      		AgP3 (Sg ) MU => mkClitic "ahu" ; -- of place  MU
      		AgP3 (Sg ) KU => mkClitic "ei" ; -- of place KU
      		AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
      		AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aku" ;
      		AgP3 Sg KI_BI   => mkClitic "eki" ;
      		AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
      		AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogu" ;
      		AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
      		AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
      		AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "ei" ;
	      	AgP3 Pl ZERO_MI =>mkClitic "ei" ;
	      	AgP3 Pl MU_MI => mkClitic "ei";
	      	AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezi" ;
	      	AgP3 Sg GU_GA => mkClitic "ogu" ;
	      	AgP3 Pl GU_GA => mkClitic "agu" ;
	     	 _  => mkClitic "XXXThat" -- error checking for any case not catered for
			
		}
 	};

 	mkIPPref : Agreement =>Str = table{	
	  AgMUBAP1 Sg => mkClitic "o";
      AgMUBAP1 Pl => mkClitic "ba" ;
      AgMUBAP2 Sg => mkClitic "o"; 
      AgMUBAP2 Pl => mkClitic "ba" ;
      AgP3 Sg MU_BA => mkClitic "o";
      AgP3 Pl MU_BA => mkClitic "ba" ;
      AgP3 Pl ZERO_BU => mkClitic "bu" ;
      AgP3 Sg BU_MA => mkClitic "bu" ;
      AgP3 Pl (KA_BU | RU_BU) => mkClitic "bu" ;
      AgP3 Pl (KI_BI | ZERO_BI) =>  mkClitic "bi" ;
      AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ga";
      AgP3 (Sg ) HA  => mkClitic "ha" ; -- of place HA 
      AgP3 (Sg ) MU => mkClitic "ha" ; -- of place  MU
      AgP3 (Sg ) KU => mkClitic "e" ; -- of place KU
      AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "ri" ;
      AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ka" ;
      AgP3 Sg KI_BI   => mkClitic "ki" ;
      AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "ku" ;
      AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gu" ;
      AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "ru" ;
      AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "tu" ;
      AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "e" ;
      AgP3 Pl ZERO_MI =>mkClitic "e" ;
      AgP3 Pl MU_MI => mkClitic "e";
      AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "zi" ;
      AgP3 Sg GU_GA => mkClitic "gu" ;
      AgP3 Pl GU_GA => mkClitic "ga" ;
      _  => mkClitic "XXXThat" -- error checking for any case not catered for
 				
 	};
	 

 		mkRObjV2 : Agreement=> Str =table {
     		  AgMUBAP1  Sg => mkClitic "ou"; 
              AgMUBAP1  Pl => mkClitic "abi" ; --note: abu or abi is used. GF does not allow free variation. However, abu is more natural
              AgMUBAP2 Sg => mkClitic "ou"; --probably an error check your grammar book
              AgMUBAP2 Pl => mkClitic "abi" ;
              AgP3 Sg MU_BA => mkClitic "ou";
              AgP3 Pl MU_BA => mkClitic "abi" ;
              AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "agi";
              AgP3 (Sg ) HA  => mkClitic "ahi" ; -- of place HA 
              AgP3 (Sg ) MU => mkClitic "ahu" ; -- of place  MU
              AgP3 (Sg ) KU => mkClitic "ei" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aki" ;
              AgP3 Sg KI_BI   => mkClitic "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "ogu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "ei" ;
              AgP3 Pl ZERO_MI =>mkClitic "ei" ;
              AgP3 Pl MU_MI => mkClitic "ei";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "ezi" ;
              AgP3 Sg GU_GA => mkClitic "ogu" ;
              AgP3 Pl GU_GA => mkClitic "agi" ;
              _  => mkClitic "XXXThat" -- error checking for any case not catered for
 				
 			}; 

    -- type for Determier necessary for catCgg.gf
    {-
        Determiners are of several types:
        1. Definite and Idefinite Articles
        2. Dmonstratives
        3. Quantifiers
        4. Cardinal numbers
        6. Ordinal numbers
    -}
    Determiner : Type = {s : Str ; s2: Agreement=>Str; ntype : NounState ; num : Number ; pos : Position; doesAgree: Bool};
    mkDet : Str -> NounState -> Number -> Position -> Determiner 
      = \ det, ns, num,pos ->
        {
          s = det;
          s2 = \\_=>[];
          ntype = ns;
          num = num;
          pos = pos;
          doesAgree = False
        };

    -- Pronouns must have agreement because they are used 
    -- depending on gender, Number and person
    -- all noun classes have pronouns in the third person
    --This is a big problem, probably we create our own abstract syntax
    {-
        TO-DO: DONE but not tested yet. I have attempted to account for all other classes by using a test.
    -}
    Pronoun : Type ={s : Case => Str; third:Agreement => Case=>Str; agr:AgrExist} ;
    {-
	    mkPron : Str -> Str ->Pronoun =\nom,acc, ->
	      
	        s = table {Nom => nom; Acc => acc};
	        agr = a;
	      };
	-}

    PolTemp = {s : Agreement => Str * Str ; end : Str} ; -- a tupple of two strings
    
    -- Structural
    -- prepositions sometimes have two kinds, near or far i.e omu or omuri
    -- ho
    Preposition = {s,other : Str; isGenPrep : Bool}; 
    mkPrep : Str -> Str ->Bool -> Preposition = \ first, other, isGenPrep -> {
      s = first ;
      other = other;
      isGenPrep = isGenPrep
    };
    NounPhrase : Type = {s :Case => Str; agr : Agreement};
    {-
      Operation to create Noun Phrases from a Determiner and Nouns.
      In Runyankore and Rukiga, depending on the particular Determiner,
      it can appear before (we call PreDeterminer) or after (PostDeterminer) the noun.
      Examples:
        A. Determiners before the Noun: This is wrong. there is a class of Predeterminers
            1. Definite aricles: Usually using the initial vowel sufficient
            2. Demonstratives: ogu muntu (This person)
            3. Every: every man = "buri muntu"
        B. Determiners fater the noun
            1. Definite aricles: Usually using the initial vowel is sufficient
            2. Demonstratives: omuntu ogu (person this)
            3. few: omuntu mu-kye
      Note: Problem stil exists because we cannot know when the  determiner string is empty
            There is a mistake here. If the determiner is empty, we end up with a 
            meaningless subject particle standing alone. we can test if det.s is a 
            string or empty.

  -}
  mkDetCN : Determiner -> Noun -> NounPhrase = \ det, cn ->
    let subjClitic = mkSubjClitic (AgP3 det.num cn.gender) 
    in
      case <det.pos, det.num> of { 
           <Post, Pl> => {s = \\_=> subjClitic ++ cn.s!det.num! det.ntype ++ subjClitic ++ det.s; agr = AgP3 det.num cn.gender; nounCat = cn.nounCat};
           <Post, Sg> => {s = \\_=>cn.s!det.num! det.ntype ++ subjClitic ++ det.s; agr = AgP3 det.num cn.gender; nounCat = cn.nounCat};
           <Pre, n> => { s =\\_ => det.s ++ cn.s !n  ! det.ntype; agr = AgP3 det.num cn.gender; nounCat = cn.nounCat} --;
          --<PostDeterminer, PFalse> => {s = \\_=> cn.s!det.ntype!det.num; agr = AgP3 det.num cn.gender }    
           };
  

    
    
      --Verb
      param
        VFormMini =  VFInf | VFPres | VFPast | VFPastAnt | VFPresAnt | VFPresProg | VFPresPart |VFPastPart;
      oper
      --Verb : Type = {s : VFormMini => Str};
      Verb : Type = {
                      s : Str; 
                      pres:Str; 
                      perf:Str; 
                      --morphs: VFormMini => VerbMorphPos=> Str; 
                      isPresBlank : Bool;
                      isPerfBlank : Bool;
                      isRegular:Bool
                    };
      
      GVerb : Type = {
                        s : Bool => Str ;
                        --morphs : VFormMini => VerbMorphPos =>Str; 
                        isAux : Bool
                      };
      {-
        The V2 sometimes uses preopsitions for formation
        of direct object. Unlike in English where the verb 
        and the preposition are disjunctive such as "send to",
        In runyakore and rukiga, the verb and preposition are
        conjunctive such as sindik-ira. 
          --The English equivalents are phrasal verbs not V2 I guess.

        Because of the fusion, I have deffered including this in 
        the compPrep. Actually, it is going to be empty in the next version
      -}
      Verb2 : Type = Verb ** {comp:Str};
      Verb3 : Type = Verb2 ** {comp2 : Str} ;
      {-
        Given a root, can you form the different verbforms?
      -}
      param
        VerbMorphPos = PreVerb | PriNegM | --ObjRel | SubjMarker | 
                       SecNegM | TAMarker | PersistiveMarker| --DObjM | IDobjM |
                       RestOfVerb;
      oper
      VMorphs : Type = VFormMini => VerbMorphPos => Str;
      VerbPhrase: Type = {
      						s:Str; 
      						pres:Str; 
      						perf:Str; 
      						--morphs: VMorphs ;
                  isPresBlank : Bool;
                  isPerfBlank : Bool;
      						isRegular:Bool;
      						comp:Str ; 
      						comp2:Str;
      						ap : Str;
      						isCompApStem : Bool; 
      						agr : AgrExist; 
      						adv:Str; 
      						containsAdv: Bool;
      						adV:Str;
      						containsAdV:Bool
      					};
      -- in VP formation, all verbs are lifted to GVerb, but morphology doesn't need to know this
     verb2gverb : Verb ->Str -> GVerb = \v, ba -> {
            s = table{
                    True =>  v.s; 
                    False => ba --the special verb to be
                      };
            --morphs = v.morphs; 
            isAux = False
          }; 
     {-
        In Runynakore & Rukiga the verb to be in english has two
        Infinitives i.e. 
          a) ri --used when it is the only main and therefore licenses a subject
          b) ba --used usually as commands, such as Imperatives or when another
                  verb is acting as the main verb. It also acts as the infitive form
     -} 
     be_GVerb : GVerb = {
        s= table{True => "ri"; False =>"b" }; 
        --morphs = \\form, morphs =>[]; 
        isAux = True};

     
       be_Copula : Verb = {
          s = "ri" ; 
          pres=[]; 
          perf=[]; 
          --morphs= mkVerbMorphs;
          isPresBlank = True;
          isPerfBlank = True;
          isRegular=False
        };
       mkBecome  :  Verb  ={
         	s = "b" ; 
          pres="a"; 
          perf="ire";
          isPresBlank = False;
          isPerfBlank = False;
          --morphs= mkVerbMorphs; 
          isRegular=False
        };


     --be1_Verb: Verb = {s="b"; pres = "e"; perf="a"; morphs = mkVerbMorphs};
     --be2_Verb: Verb = {s="ri"; pres = "e"; perf="a"; morphs = mkVerbMorphs};
      {-
      --copulative conjugations of ni and ri as used for adjectives
      
      copRiNi :Verb ={
        s= table {
        True => table{
                VPres (Agr (NC_mu_ba) Sg Per1)  => "ndi" ;
                VPres (Agr (NC_mu_ba) Pl Per1)  => "turi";
                VPres (Agr (NC_mu_ba) Sg Per2)  => "ori" ;
                VPres (Agr (NC_mu_ba) Pl Per2)  => "muri";
                VPres (Agr (NC_mu_ba) Sg Per3)  => "ari" ;
                VPres (Agr (NC_mu_ba) Pl Per3)  => "bari";
                VPres (Agr (_) _ _)=> ""
      };
      -}
      {-
          This function packages the different morphemes of the each tense of verb
          that are commonly used and have not more than two possibilities i.e.
          1. infintive marker at begining of verb
          2. ni - continuous marker at begining of the present participle / present continuous
          3. ti - Primary Negative marker
          4. ta - Secondary Negative Marker
          5. Tense and aspect markers
          6. Persistive Marker
          7. Rest of Verb. 
        These can be increased further. Note: Only those tenses clossest to the english equivalent 
        have been chosen. The full resource shall require even more.
      -}
      mkVerbMorphs:VMorphs = table{
            VFInf => table{ 
                          PreVerb => "ku";
                          PriNegM => []; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => []; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
            VFPres => table{
                          PreVerb => [];
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => []; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
            VFPresAnt => table{
                          PreVerb => [];
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => []; 
                          TAMarker => []; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"ire"
                        };
            VFPresPart => table{
                          PreVerb => [];
                          PriNegM => []; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => "riku"; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
                VFPastPart => table{
                  PreVerb => [];
                  PriNegM => []; 
                  --ObjRel => [];
                  --SubjMarker =[]; 
                  SecNegM => "ta"; 
                  TAMarker => "ku"; 
                  PersistiveMarker => [];
                  --DObjM => [];
                  --IDobjM => [];
                  RestOfVerb =>"irwe"
                };

            --mkVerbPast:Str -> Str =\root -> Predef.BIND ++"ti" ++ Predef.BIND ++ "PriNegM" ++ Predef.BIND ++ "TM1" ++ Predef.BIND ++ "Empy" ++ Predef.BIND ++ "TM2" ++ Predef.BIND ++ "Empty" ++ Predef.BIND ++ "stem" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire"; 
            VFPast => table{
                          PreVerb => [];
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => []; 
                          TAMarker => []; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"ire"
                        };
            --"ni" ++ Predef.BIND ++ "ContM" ++ Predef.BIND ++ "ti" ++ Predef.BIND ++ "PriNegM" ++ Predef.BIND ++ "TM1" ++ Predef.BIND ++ "Empy" ++ Predef.BIND ++ "TM2" ++ Predef.BIND ++ "Empty" ++ Predef.BIND ++ "stem" ++ Predef.BIND ++ root ++ Predef.BIND ++"a";
            VFPresProg => table{
                          PreVerb => "ni";
                          PriNegM => []; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => []; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
            VFPastAnt => table{                     --always requires auxiliary with 
                          PreVerb => [];
                          PriNegM => []; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => "aa"; 
                          PersistiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"ire"
                        }

      };



  --oper
    --Concatenates two strings at runtime without spaces

    glue: Str -> Str ->Str =\ x, y -> x ++ BIND ++ y;
  	--Concatenates two strings for the genetive case
  	glueGen: Agreement ->Str = \ a -> mkGenPrepNoIVClitic a ++ BIND ++ mkGenAdjSuffix a;

  --Number determining element
  Numer : Type = { s: Agreement => Str ; n : Number};

  --VPSlash : Type = VerbPhrase ** { c : Str };
  VPSlash : Type = {
  					s:Str; 
  					pres:Str; 
  					perf:Str; 
  					--morphs: VMorphs;
            isPresBlank : Bool;
            isPerfBlank : Bool; 
  					comp: Str; 
  					comp2:Str; 
  					ap:Str; 
  					isRegular:Bool; 
  					adv:Str; 
  					containsAdv:Bool;
  					adV:Str;
  					containsAdV:Bool
  					}; --comp is empty
  

  {-
	      -- Clause is a combination of a Subject, Verb and Object(s)
	      -- i.e. Subj, verb with polarity and temp features and verb complement
	      -- which is the Objects, NPs PPs APs etc.
	 -}
      Clause : Type = {   -- word order is fixed in S and QS
	      s : Str ; --subject
	      subjAgr : Agreement;
	      root : Str;
	      pres: Str;
	      perf: Str;
        isPresBlank : Bool;
        isPerfBlank : Bool;
	      --morphs : VFormMini => VerbMorphPos =>Str;
	      {-
	      inf  : Str;
	      pres  : Str; 
	      past  : Str; 
	      presPart  : Str; 
	      pastPart  : Str;                              -- subject
	      --root : Str ; -- dep. on Pol,Temp, e.g. "does","sleep"
	      -}
	      compl : Str -- after verb: complement, adverbs
	      } ;
param 
  CompSource = NounP | ADverb | AdjP | CommonNoun;
oper
  Comp : Type = {s:Str; source : CompSource };


  --Conjunctions
  Conjunction : Type = {s : AgrConj =>Str ;s2 : Str ; n : Number} ;

  -- For $Numeral$.
param
	--2 For $Numeral$

    CardOrd = NCard | NOrd;
    --DForm   = Unit Gender| Ten Gender | N20_50 Gender| N60_n70 Gender; -- | hundred | thousand | tenThousand | hundredThousand | million ;
oper
  mkNum : Str ->Str -> Gender ->Str-> Gender -> Bool-> --Str-> Gender->Str-> Gender-> Str -> Gender
  	{
  		s : Str; 
  		unit          : { s:Str ; g : Gender; stem : Str};
  		ten           : { s:Str ; g : Gender};
  		ordinal :Str;
  		--twenty_fifty  : { s:str ; g : gender; stem : Str};
  		--sixty_seventy : { s:str ; g : gender; stem : Str};
  		--eighty_ninety : { s:str ; g : gender; stem : Str};
  		isOrdDifferent: Bool -- If the ordinal number is different from the cardinal
  	} = 
    \biri, ibiri, g1, abiri,g2, isOrdDifferent  -> case isOrdDifferent of {
																True =>{
																    	s = [];
																    	unit = {s = ibiri;  g = g1; stem =biri};
																    	ten  = {s = abiri ; g = g2};
																    	ordinal = "ka" + biri;
																    	isOrdDifferent = isOrdDifferent
																		};
																False =>{
																    	s = [];
																    	unit = {s = ibiri;  g = g1; stem =biri};
																    	ten  = {s = abiri ; g = g2};
																    	ordinal = [];
																    	isOrdDifferent = isOrdDifferent
																		}
	 };

	 getGender : Agreement -> Gender =\agr ->
	  case agr of {
	  	(AgP3 n g) => g;
	  	(AgMUBAP1 n) => MU_BA;
	  	(AgMUBAP2 n) => MU_BA;
	  	 NONE => Null
	};
  getNumber : Agreement -> Number =\agr ->
    case agr of {
      (AgP3 n g) => n;
      (AgMUBAP1 n) => n;
      (AgMUBAP2 n) => n;
       NONE => Sg --default -- a hack
  };
}