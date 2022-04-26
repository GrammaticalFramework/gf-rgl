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
  NounType = Complete | Incomplete ;
  Position = PostDeterminer | PreDeterminer ;
  Case = Nom | Acc | Gen ;
  Agr =  AgP3 Number NClass | AgMUBAP1 Number |AgMUBAP2 Number ;

  NClass = MU_BA      | -- for human beings and deity e.g. omuntu/abantu
           KI_BI      | -- eki-tookye/ebi-tookye
           N_N        | -- nouns that do not vary their spelling with singular and plural (normally begin with "e")
           KU_MA      | -- okuguru / amaguru, (leave out the non plurals)okugyenda (Deverbative) / Amagyenda (Deverbative outward journey)
           BU_MA      | 
           RU_BU      | -- oruro / oburo
           GU_GA      | -- ogushaija / agashaija --dimunitive
           ZERO_ZERO  | -- nouns without initial vowel nor class syllables e.g barugu, muha, sho
           MU_MI      | --
           RI_MA      | 
           I_MA       | 
           KA_BU      |
           KA_TU      | --dimunitive version of KA_BU belong to same noun class
           RU_N       | -- orurimi / endimi
           RU_MA      |
          --those of place or location
          HA          | 
          MU          | 
          KU          |
          --aditions
          ZERO_BU     | 
          ZERO_BI     | 
          ZERO_MA     | -- amate takes the concords of plural particle "ma"
          ZERO_MI     |
          ZERO_TU     |
          ZERO_N      |
          I_ZERO      | 
          RI_ZERO     | 
          KU_ZERO     | 
          MU_ZERO     |
          RU_ZERO     |
          ZERO_BAA    |  -- human relationships
          KA_ZERO ;      --akabi (ZERO to the right means the concords of that noun are always those of the singular as used in noun-classes KA) see KA_BU, KA_TU
          

  PersonalPronounType = SubjM | Obj  | RelSubj | RelObj |
                          AdjPron2 | -- aAdjectival Prefixes with initial vowel with the semantics of "the" e.g. -- omuntu o-mu-rungi 
                          AdjPron  | -- without initial vowel i.e. -- omuntu mu-rungi           
                          --GenPron  | -- different types of pronouns
                          GenPrep1 |
                          GenPrep2 |
                          GenAdj   |
                          SStandPron ; --Self-standing pronouns
oper
  
  Noun : Type ;
  Noun = {s : NounType=>Number => Str ; nc : NClass } ;
  
  smartNoun : Str -> NClass -> Noun 
    = \omuntu, g ->
      case <omuntu , g> of {
        -- Handling the Tone System is also another problem.
        
        < "o" + "mu" + stem, MU_BA > => mkNoun omuntu ("aba" + stem) g ;
        --special cases like omwana, omwishiki, omwojo
        
        < "o" + "mw" + stem, MU_BA > => mkNoun omuntu (combine_morphemes "aba" stem) g ; --same as mu_ba but the "u" + "a" of the stem to form mwa  
        < "o" + "mu" +  stem, MU_MI > => mkNoun omuntu (combine_morphemes "emi" stem) g ;
        < "o" + "ru" +  stem, RU_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        < "o" + "ru" +  stem, RU_N >  => mkNoun omuntu (combine_morphemes "en" stem) g ; --desist from providing a singlar only but give both
        < "o" + "bu" +  stem, BU_MA >  => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        < "o" + "gu" +  stem, GU_GA >  => mkNoun omuntu (combine_morphemes "aga" stem) g ;
        < "o" + ("ku" | "kw") +  stem, KU_MA >  => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        < "o" +  "kw" +  stem, KU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g ;
        < "o" + "ku" +  stem, KU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "o" + "mu" +  stem, MU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "o" + "mu" +  stem, MU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "o" + "ru" +  stem, RU_BU >  => mkNoun omuntu (combine_morphemes "obu" stem) g ;
        < "o" + "ru" +  stem, RU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural

        < "a" + "ha" + stem, HA_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "a" + "ka" + stem, KA_BU > => mkNoun omuntu (combine_morphemes "obu" stem) g ;
        < "a" + "ka" + stem, KA_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural

        < "e" + "ki" + stem, KI_BI > => mkNoun omuntu (combine_morphemes "ebi" stem) g ;
        < "e" + "ki" + stem, KI_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "e" + "i" + stem, I_MA > => mkNoun omuntu (combine_morphemes "ama" "") g ;
        < "e" + "i" + stem, I_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "e" + "ri" + stem, RI_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        < "e" + "ri" + stem, RI_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "e" + "ry" + stem, I_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        -- --special cases shall be added with due course as errors are identified
        <"e" + "ky" + stem, KI_BI> => mkNoun omuntu (combine_morphemes "ebi" stem) g ; 
        < _ ,N_N | ZERO_MA > => mkNoun omuntu  omuntu g ;
        --< _ ,ZERO_MA > => mkNoun omuntu  ("ama" + stem) g (Predef.drop 1 omuntu);
        --< _ ,> => mkNoun omuntu  omuntu g (Predef.drop 1 omuntu);
        <_ , ZERO_BAA>  => mkNoun omuntu ("baa" + omuntu) g;
        < _ ,ZERO_ZERO > => mkNoun omuntu  omuntu g;
        < _ ,_ > => mkNoun omuntu  omuntu g -- improve as we go on.
    };

  {- Should be taken to Ajective concrete Syntanx-}
  mkNoun : Str -> Str -> NClass -> Noun ;
  mkNoun child children nc 
    = { s = table {Complete => table {  
                        Sg => child ; Pl => children } ;
                  Incomplete => table {
                        Sg => Predef.drop 1 child ; Pl => Predef.drop 1 children } }; --removal of the initial vowel
       nc = nc
      } ;
    mkN = overload {
    mkN : Str -> NClass -> Noun = smartNoun ;
    mkN : Str -> Str -> NClass -> Noun = mkNoun;
    } ;


  

  -- combine_morphemes need the function last to get 
  -- the last letter in a morphme.
  -- uses Predef.length and Predef.take
  -- Please use let so that you compute Predef.length once and use if then else
  -- if possible
  {-
  last : Str -> Str ;
  last = \ w ->
    case (Predef.length w) of {
        0 => [];
        _ => Predef.drop ((Predef.length w)-1) w
      } ;

    -}
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
  Determiner : Type = {s : Str ; ntype : NounType ; num : Number ; pos : Position } ; -- type for Determier necessary for catCgg.gf
  
  Pronoun : Type ={s : Str ; agr : Agr} ;
  
  VerbPhrase : Type = { s : Agr => Polarity => Tense => Anteriority => Str};
  
  VPSlash : Type = VerbPhrase ** { c : Str } ;
  
  Numer : Type = { s: Str ; n : Number};
  
  Clause : Type = {s : Polarity => Tense => Anteriority => Str};
  
  Adv : Type = {s : Agr => Str } ; -- check pages 116-131 of grammar book
  mkAdv : Str -> Adv = \ s -> { s= \\ agr => s };
  
  ParticleForms : Type = PersonalPronounType => Agr =>  Str;
  mkNCParticles : ParticleForms  = table {
      SubjM => table { 
              AgMUBAP1 Sg => "n" ;
              AgMUBAP1 Pl => "tu" ;
              AgMUBAP2 Sg => "wa" ;
              AgMUBAP2 Pl => "mu" ;
              AgP3 Sg MU_BA  => "a" ;
              AgP3 Pl MU_BA  => "ba" ;          
              AgP3 Sg KI_BI   => "ki" ;
              AgP3 Pl (KI_BI | ZERO_BI)   => "bi" ;
              AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => "ru" ; 
              AgP3 Pl RU_N => "zi"; --| "i"; 
              AgP3 Sg N_N => "e";
              AgP3 Pl N_N => "zi"; --| "i";
              AgP3 Sg (MU_MI | MU_ZERO)   => "gu" ; 
              AgP3 Pl MU_MI   => "e" ;
              AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) => "ri"; 
              AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => "ga" ;
              AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => "ka" ; 
              AgP3 Pl (KA_BU | RU_BU)  => "bu" ;
              AgP3 Sg ZERO_BU  => "bu" ; 
              AgP3 Pl ZERO_BU  => "bu" ;
              AgP3 Sg ZERO_BI  => "bi" ; 
              AgP3 Sg ZERO_MA  => "ga" ;
              AgP3 Pl RI_ZERO  => "ga" ;
              AgP3 Sg KU_ZERO  => "ku" ;
              AgP3 Pl KU_ZERO  => "ku" ;
              AgP3 Pl MU_ZERO  => "gu" ;
              AgP3 Pl RU_ZERO  => "ru" ;
              AgP3 Sg ZERO_TU  => "tu" ;
              AgP3 Pl ZERO_TU  => "tu" ;
              AgP3 Sg (ZERO_MI | ZERO_ZERO)  => "" ;
              AgP3 Pl ZERO_MI  => "e" ;
              AgP3 Pl KA_ZERO  => "" ;
              _        => "XX" --for checking if there is some class unaccounted for   
             };
      {-Object particle may be used as 
          1. a prefix: e.g mu-kwate = catch him,
          2. an infix: o-mu-kwate   = you catch him

      -}
      Obj => table { 
              AgMUBAP1 Sg => "n" ;
              AgMUBAP1 Pl => "tu" ;
              AgMUBAP2 Sg => "ku" ;
              AgMUBAP2 Pl => "ba" ;
              AgP3 Sg MU_BA => "mu" ;
              AgP3 Pl MU_BA => "ba";
              AgP3 Pl (ZERO_BU | KA_BU | KA_TU | RU_BU) => "bu" ;
              AgP3 Sg BU_MA => "bu" ;
              AgP3 Sg KI_BI => "ki" ; 
              AgP3 Pl (KI_BI | ZERO_BI) => "bi";
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "ga" ;
              AgP3 (Sg | Pl) HA => "ha";
              AgP3 Sg (I_ZERO | I_MA | RI_MA) => "ri" ;
              AgP3 Sg (KA_ZERO | KA_BU | KA_TU) => "ka" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "ku" ;
              {- 
                 #comment for the following two lines
                 the follwing partciles are all used by Noun Classes of Place i.e. HA, KU and MU
                 We take the particle to be "ha" for all of them although noun class KU can use
                 another particle "gi" -- see Table of Concords in Appendix of Dictionary by Mpairwe and Kahangi

                 Note: The particles do not change with respect to gender

                 TODO: obtain clear examples of usage
              -}
              AgP3 (Sg | Pl) (HA | MU) => "ha" ;
              AgP3 (Sg | Pl) KU => "ha" ;  -- gi is also possible -- see comment above

              AgP3 Sg (RU_N | RU_ZERO | RU_BU | RU_MA) => "ru" ;
              AgP3 Pl (KA_TU | ZERO_TU) => "tu" ;
              
              AgP3 Sg (N_N | ZERO_ZERO) => "gi" ; 
             
              AgP3 Sg (MU_MI | MU_ZERO) => "gu" ;
              AgP3 Pl  GU_GA => "ga" ; 
              AgP3 Pl (MU_MI | ZERO_MI) => "gi" ; 
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
              AgP3 Pl (N_N | ZERO_ZERO | ZERO_N | RU_N) => "zi" ; --some cases require use of particle "i" 
              
              _ => "-" -- Hopefully exhausted all forms 
             };
      -- who, which
      RelSubj => table {
              AgMUBAP1 Sg => "o" ;
              AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 Sg => "o" ;
              AgMUBAP2 Pl => "aba" ;
              AgP3 Sg MU_BA  => "o" ;
              AgP3 Pl MU_BA  => "aba" ;
              AgP3 Sg BU_MA => "obu" ;
              AgP3 Pl (ZERO_BU | KA_BU | RU_BU) =>"obu" ; 
              AgP3 Sg KI_BI  => "eki" ; 
              AgP3 Pl (KI_BI | ZERO_BI)  => "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "aga" ;
              AgP3 (Sg | Pl) (HA | MU) => "ha" ; -- better AgP3 _ (HA | MU) => "ha";
              AgP3 (Sg | Pl) KU => "e" ;
              AgP3 Sg (I_ZERO | I_MA | RI_ZERO | RI_MA) => "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) => "aka" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "oku" ;
              AgP3 Sg (RU_N | RU_ZERO | RU_BU | RU_MA) => "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) => "otu" ;
              AgP3 Pl RU_N    => "ezi" ; 
              AgP3 Sg N_N     => "ei" ; 
              AgP3 Pl (ZERO_ZERO | ZERO_N | RU_N | N_N) => "ezi" ;
              AgP3 Sg (MU_MI | MU_ZERO | GU_GA) => "ogu" ;
              AgP3 Sg (ZERO_ZERO | N_N ) => "e" ;
              AgP3 Pl (MU_MI | ZERO_MI) => "e" ;
              AgP3 Pl GU_GA => "aga" ;
              _        => "="  -- means something forgoten i.e. debugging purposes   
             };
      
      --Relative Object paticle such as whom/which found in row 13 of Table of concords in Mpairwe & Kahangi
      RelObj => table { 
              AgMUBAP1 Sg => "ou" ;
              AgMUBAP1 Pl => "abi" ; -- use of "abu" is also allowed (depending on what?) but omitted because of compiler issues with | operator
              AgMUBAP2 Sg => "ou" ;
              AgMUBAP2 Pl => "abi" ; -- use of "abu" is also allowed (depending on what?) but omitted because of compiler issues with | operator
              AgP3 Sg MU_BA => "ou" ;
              AgP3 Pl MU_BA => "abi" ; -- use of "abu" is also allowed (depending on what?) but omitted because of compiler issues with | operator
              AgP3 Sg BU_MA => "obu" ;
              AgP3 Pl (ZERO_BU | KA_BU |RU_BU) => "obu" ;
              AgP3 Sg KI_BI => "eki" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "ebi" ;

              {-
                The noun classes ZERO_MA,KU_MA,RI_MA,I_MA & BU_MA can use of Relative object particles
                "agi" or "agu"  (depending on noun class of clause -sure? (depending on what?)) but we 
                choose one "agi" because of compiler issues with | operator
                
                Qn: Any Solutions
              -}
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "agi" ; 
              
              -- of place HA & MU
              --both ahi and  "ahu" are valid particles  for noun classes HA and MU but "ahu" omitted 
              --because of compiler issues with | operator
              AgP3 (Sg | Pl) (HA | MU) => "ahi" ; -- better AgP3 _ (HA | MU) => "ha";

              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"eri" ;
              -- of place KU
              AgP3 (Sg | Pl) KU => "ei" ;

              --both aki and  "aku" are valid particles  for noun classes KA_ZERO & KA_BU but "aku" omitted 
              --because of compiler issues with | operator
              AgP3 Sg (KA_ZERO | KA_BU) =>"aki" ;

              AgP3 Sg (KU_ZERO | KU_MA) => "oku" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"otu" ;

              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "ezi" ; 
              AgP3 Sg (ZERO_ZERO | N_N) =>"ei" ; 
              AgP3 Sg (MU_MI | MU_ZERO | GU_GA) => "ogu" ;
              AgP3 Pl (MU_MI | ZERO_MI) => "ei" ;
              
              --both agi and  "agu" are valid particles  for noun classes GU_GA in plural but "agu" omitted 
              --because of compiler issues with | operator
              AgP3 Pl GU_GA => "agi" ;
              _        =>  "="  -- means something forgoten i.e. debugging purposes     
             };

      -- Adjectival Prefixes with initial vowel with the semantics of the 
      AdjPron2 => table {
              AgMUBAP1 Sg => "omu" ;
              AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 Sg => "omu" ;
              AgMUBAP2 Pl => "aba" ;
              AgP3 Sg MU_BA => "omu" ;
              AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => "obu" ;
              AgP3 Sg BU_MA => "obu" ;
              AgP3 Pl (KA_BU | RU_BU) =>"obu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "ama";
              AgP3 (Sg | Pl) (HA | MU) => "aha" ; -- of place HA & MU
              AgP3 (Sg | Pl) KU => "en" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>"aka" ;
              AgP3 Sg KI_BI   => "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => "omu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>"en" ;
              AgP3 Pl ZERO_MI => "en" ;
              AgP3 Pl MU_MI => "emi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "en" ;
              AgP3 Sg GU_GA => "ogu" ;
              AgP3 Pl GU_GA => "aga" ;
              _  =>  "XXX" -- error checking for any case not catered for 
               };

      -- Adjectival Prefixes without initial vowel      
      AdjPron => table { 
              AgMUBAP1 Sg => "mu" ;
              AgMUBAP1 Pl => "ba" ;
              AgMUBAP2 Sg => "mu" ;
              AgMUBAP2 Pl => "ba" ;
              AgP3 Sg MU_BA  => "mu" ;
              AgP3 Pl MU_BA  => "ba" ;
              AgP3 Pl ZERO_BU => "bu";
              AgP3 Sg BU_MA => "bu" ;

              AgP3 Pl (KA_BU | RU_BU) =>"bu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "bi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "ma" ;

              
              AgP3 (Sg | Pl) (HA | MU) => "ha" ; -- of place HA & MU
              AgP3 (Sg | Pl) KU => "n" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"ri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>"ka" ;
              AgP3 Sg KI_BI   => "ki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "ku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => "omu";
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "ru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"tu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>"n" ;
              AgP3 Pl ZERO_MI => "n" ;
              AgP3 Pl MU_MI => "emi" ;
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "n" ;
              AgP3 Sg GU_GA => "ogu" ;
              AgP3 Pl GU_GA => "aga" ;
              _        =>  "XXX"     -- for debugging purposes
               } ;
      --Genetive Preposition: exclusively of (with initial vowel) 
      GenPrep1 => table { 
              AgMUBAP1 Sg => "owa" ;
              AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 Sg => "owa" ;
              AgMUBAP2 Pl => "aba" ;
              AgP3 Sg MU_BA  => "owa" ; 
              AgP3 Pl MU_BA  => "aba" ;
              AgP3 Pl ZERO_BU => "obwa" ;
              AgP3 Sg BU_MA => "obwa" ;

              AgP3 Pl (KA_BU | RU_BU) =>"obwa" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "ebya" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "aga" ;

              
              AgP3 (Sg | Pl) HA => "aha" ; -- of place HA 
              AgP3 (Sg | Pl) MU => "omwa" ; -- of place MU
              AgP3 (Sg | Pl) KU => "eya" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"erya" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>"aka" ;
              AgP3 Sg KI_BI   => "ekya" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "okwa" ;
              AgP3 Sg (MU_MI | MU_ZERO) => "ogwa" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "orwa" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"otwa" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>"eya" ;
              AgP3 Pl ZERO_MI => "eya" ;
              AgP3 Pl MU_MI => "eya" ;
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "eza" ;
              AgP3 Sg GU_GA => "ogwa" ;
              AgP3 Pl GU_GA => "aga" ;
              _        =>  "XYY"     -- for debugging purposes
               };
      --Genetive Preposition: simply of without initial vowel
      GenPrep2 => table { 
              AgMUBAP1 Sg => "wa" ;
              AgMUBAP1 Pl => "ba" ;
              AgMUBAP2 Sg => "wa" ;
              AgMUBAP2 Pl => "ba" ;
              AgP3 Sg MU_BA  => "wa" ;
              AgP3 Pl MU_BA  => "ba" ;
              AgP3 Pl ZERO_BU => "bwa" ;
              AgP3 Sg BU_MA => "bwa" ;

              AgP3 Pl (KA_BU | RU_BU) =>"bwa" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "bya" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "ga" ;

              
              AgP3 (Sg | Pl) HA => "ha" ; -- of place HA 
              AgP3 (Sg | Pl) MU => "mwa" ; -- of place MU
              AgP3 (Sg | Pl) KU => "ya" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"rya" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>"ka" ;
              AgP3 Sg KI_BI   => "kya" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "kwa" ;
              AgP3 Sg (MU_MI | MU_ZERO) => "gwa" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "rwa";
              AgP3 Pl (ZERO_TU | KA_TU) =>"twa" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>"ya" ;
              AgP3 Pl ZERO_MI => "ya" ;
              AgP3 Pl MU_MI => "ya" ;
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "za" ;
              AgP3 Sg GU_GA => "gwa" ;
              AgP3 Pl GU_GA => "ga" ;
              _        =>  "XYY"     -- for debugging purposes
            };
      -- Genetive Adjective suffixes used to form genetive adjectives when conjugated to 
      -- the genetive prepositions particles
      -- examples: ekya-{ngye}= my own or mine, ekya-{itu}= our own or ours, 
      -- ekya-{we}-your own or yours 
      GenAdj => table { 
              AgMUBAP1 Sg => "ngye" ;
              AgMUBAP1 Pl => "itu" ;
              AgMUBAP2 Sg => "we" ;
              AgMUBAP2 Pl => "nyu" ;
              AgP3 Sg MU_BA  => "e" ;
              AgP3 Pl MU_BA  => "bo" ;
              AgP3 Pl ZERO_BU => "bwo" ;
              AgP3 Sg BU_MA => "bwo" ;

              AgP3 Pl (KA_BU | RU_BU) =>"bwo" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "byo" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "go" ;

              
              AgP3 (Sg | Pl) HA => "ho" ; -- of place HA 
              AgP3 (Sg | Pl) MU => "mwo" ; -- of place MU
              AgP3 (Sg | Pl) KU => "yo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"ryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>"ko" ;
              AgP3 Sg KI_BI => "kyo" ;
              AgP3 Sg (KU_ZERO | KU_MA) => "kwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) => "gwo" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "rwo" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"two" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>"two" ;
              AgP3 Pl ZERO_MI => "yo" ;
              AgP3 Pl MU_MI => "yo" ;
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "zo" ;
              AgP3 Sg GU_GA => "gwo" ;
              AgP3 Pl GU_GA => "go" ;
              _ =>  "XXYY"     -- for debugging purposes 
               } ;
      SStandPron => table { 
              AgMUBAP1 Sg =>"nyowe" ;
              AgMUBAP1 Pl =>"itwe" ;
              AgMUBAP2 Sg =>"iwe" ;
              AgMUBAP2 Pl =>"imwe" ;
              AgP3 Sg MU_BA  => "uwe" ;
              AgP3 Pl MU_BA  => "bo" ;
              AgP3 Pl ZERO_BU => "bwo" ;
              AgP3 Sg BU_MA => "bwo" ;

              AgP3 Pl (KA_BU | RU_BU) =>"bwo" ;
              AgP3 Pl (KI_BI | ZERO_BI) => "byo" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => "go" ;

              
              AgP3 (Sg | Pl) HA => "ho" ; -- of place HA 
              AgP3 (Sg | Pl) MU => "mwo" ; -- of place MU
              AgP3 (Sg | Pl) KU => "yo" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>"ryo" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>"ko" ;
              AgP3 Sg KI_BI   => "kyo";
              AgP3 Sg (KU_ZERO | KU_MA) => "kwo" ;
              AgP3 Sg (MU_MI | MU_ZERO) => "gwo";
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => "rwo" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>"two" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>"two" ;
              AgP3 Pl ZERO_MI => "yo" ;
              AgP3 Pl MU_MI => "yo" ;
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  => "zo" ; 
              AgP3 Sg GU_GA => "gwo" ; 
              AgP3 Pl GU_GA => "go" ;
              _ =>  "XXYY"     -- for debugging purposes 
               }
            } ;
  
  {-
      Operation to create Noun Phrases from a Determiner and Nouns.
      In Runyankore and Rukiga, depending on the particular Determiner,
      it can appear before (we call PreDeterminer) or after (PostDeterminer) the noun.
      Examples:
        A. PreDeterminers
            1. Definite aricles: Usually using the initial vowel sufficient
            2. Demonstratives: ogu muntu (This person)
            3. Every: every man = "buri muntu"
        B. PostDeterminers
            1. Definite aricles: Usually using the initial vowel sufficient
            2. Demonstratives: omuntu ogu (person this)
            3. few: omuntu mu-kye

  -}
  NounPhrase = { s : Str ; agr : Agr } ; 
  {-This function should be renamed to mkDetCN-}
  mkDeterminer: Determiner -> Noun -> NounPhrase = \ det, cn ->
    case det.pos of { 
         PreDeterminer => { s = det.s ++ cn.s ! det.ntype ! det.num; agr = AgP3 det.num cn.nc} ; 
         PostDeterminer => { s = cn.s!det.ntype!det.num ++ mkNCParticles!SubjM!AgP3 det.num cn.nc ++ det.s; agr = AgP3 det.num cn.nc} -- There is a mistake here. If the determiner is empty, we end up with a meaningless subject particle standing alone. we can test if det.s is a string or empty. 
         

                            };
  
  {-
    Operation to create scenarios in which the PRIMARY NEGATIVE MARKER
    for the verb is used. The presence of this marker negates the semantics of the verb.
    It is used in the PAST, Simultaneous and all other tenses.
    NOTE: The Primary and Secondary markers are in complementary distribution

    TO DO: Improve this!!!
  -}
  mkPol1Marker : Polarity => Tense => Anteriority => Str = table {
    Neg => table {
      Past => table {
            Anter   => "" ;
            Simul => "ti"
        } ;
      _ => table {
            _   => "ti"
      }
    } ;
    Pos => \\_ => \\_ => ""
  };


  {-
    Operation to create scenarios in which the SECONDARY NEGATIVE MARKER
    for the verb is used. The presence of this marker negates the semantics of the verb.
    It is used in the PAST, Anterior only.
    NOTE: The Primary and Secondary markers are in complementary distribution
  -}
  mkPol2Marker : Polarity => Tense => Anteriority => Str = table{
    Neg => table {
      Past => table {
            Anter   => "ta";
            Simul => ""
      };
      _ => table{
            _   => ""
      }
    };
  Pos => \\_=>\\_=> "" 
  } ;


  {-
    TO DO: stop here
  -}
  mkTenseMarker1 : Tense => Anteriority => Str = table{
    Past => table{
            Anter   =>"baire";
            Simul =>""
      };
      Pres => table{
            _   =>""
      };
      Fut => table{
            Anter => "ijakubá";-- | "zakubá";
            Simul =>"ijaku"-- | "kuzaku" | "raa"
      };
      Cond => table{
            _ => "kaa"
      }
  };

  mkTenseMarker2:Tense=>Anteriority=>Str = table{
    Past => table{
            Anter   =>"ire";
            Simul =>"ire"
      };
    Pres => table{
          Anter   =>"ire";
          Simul =>""
    };
    Fut => table{
          Anter => "ire";
          Simul =>""
    };
    Cond => table{
          Anter => "ire";
          Simul =>""
    }
  };
  
  mkAuxTenseMarker: Tense =>Anteriority=>Str =table {
    Past => table {
            Anter   =>"kaba" ;
            Simul =>"baire"
      };
    Pres => table {
          Anter   =>"baire" ;
          Simul =>[]
    };
    Fut => table {
          Anter => "ryaba" ;
          Simul =>"ryaba"
    };
    Cond => table {
          Anter => "XX" ;
          Simul =>"XX"
    }
  };


  Verb : Type ;
  Verb = { s : Agr => Polarity => Tense => Anteriority => Str } ;

  AdjectivalPhrase : Type = { s : Agr => Str } ;
  mkAdjective: Str-> Bool -> { s : Agr=> Str } = \ a , b -> case b of {
      True => { s = \\ agr => a } ; 
      False => { s = \\ agr => let agrM = mkNCParticles ! AdjPron ! agr in
          agrM + a --this is supposed to be a concatenation                              
  }
  };


  Verb2 : Type = Verb **{ c : Str } ;
  mkV2 : Str -> Verb2 = \s -> (mkVerb s) ** { c = [] } ;
  Adjective : Type = { s : Agr => Str } ;
  mkComp : AdjectivalPhrase -> VerbPhrase ; --comp means compula
  mkComp comp = {
    s = \\ agr , pol , tense, anter =>
      let aux = mkAuxTenseMarker ! tense ! anter;  
          p2 = mkPol2Marker ! pol ! tense ! anter;
          s =  mkNCParticles ! SubjM ! agr
      in aux ++ p2 + s ++ "ri" ++  comp.s!agr --why does the plus fail?
  };
   

  mkVerb : Str -> Verb ; -- write an operation for the object marker
  mkVerb run  = { s = \\ subjM , pol , tense , anter =>
    let  p1 = mkPol1Marker ! pol! tense ! anter ;
         p2 = mkPol2Marker ! pol! tense ! anter ;
         t1 = mkTenseMarker1 ! tense ! anter ; 
         t2 = mkTenseMarker2 ! tense ! anter ;
         s = mkNCParticles ! SubjM !subjM
    in 
      case < tense , anter > of {
        <Past, Anter> => p1 + s + t1 ++ s + p2 + run + t2;
        <Fut, _> => p1 + s + t1 ++ p2 + run + t2;
        _ => (p1 + s + p2 + t1 + run + t2)
          }
        };


{-

 Note: The following is copied from the file swahili/ResSwa.gf
-}

--1 Swahili auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work. To define everything that is needed to
-- implement $Test$, it moreover contains regular lexical
-- patterns needed for $Lex$.

resource ResSwa = ParamX ** open Prelude in {

--For $Noun$

-- This is the worst-case $Case$ needed for pronouns.

  param Case = Nom | Loc ;

  param Animacy = AN | IN ;

  param Gender = g1_2 | g3_4 | g5_6 | g5a_6 | g6 | g7_8 | g9_10 | g11 | g11_6 | g11_10 ; 

--2 For $Adjective$

   AForm = AF Number Gender Animacy 
	 | AA ;
 
-- The order of sentence is needed already in $VP$.

    Order = ODir | OQuest ;

 --2 For $Verb$

-- Verbs will take one of the five forms

  param 
	VForm = VInf
	       | VImper Number Person
	       | VPres Number Gender Animacy Person
               | VPast Number Gender Animacy Person
               | VFut Number Gender Animacy Person;

	
  oper

   Verb : Type = {s : VForm => Str} ;


   VerbForms : Type =  Tense => Anteriority => Polarity => Agr => Str ;

    VP : Type = {
    s  : VerbForms ;
    s2 : Agr => Str
    } ;

	
  mkV : Str -> {s : VForm => Str} = 
     \cheza -> {
     s = table { 
       VInf => case Predef.take 2 cheza of { 
    		"ku" => cheza;
    		 _ => "ku"+cheza
    	 };
       VImper n p => case <n,p> of{
		  <Sg,P2> => init cheza + "eni";
		  <_,_> => cheza}; 
       VPres n g anim p => Verbprefix n g anim p + "na" + cheza; 
       VPast n g anim p => Verbprefix n g anim p + "li" + cheza ;
       VFut n g anim p => Verbprefix n g anim p + "ta" + cheza     
       } 
     } ;

   
  predV : Verb -> VP = \verb -> {
    s = \\t,ant,b,agr => 
      let
        inf  = verb.s ! VInf ;
        imper = verb.s ! VImper agr.n agr.p;
        pres = verb.s ! VPres agr.n agr.g agr.anim agr.p ;
        past  = verb.s ! VPast agr.n agr.g agr.anim agr.p ;
        fut = verb.s ! VFut agr.n agr.g agr.anim agr.p ; 
      in
      case <t,ant,b> of {
        <_,Anter,Pos> => imper;
        <Pres,Simul,Pos> => pres  ;
      	<Past,Anter,Pos> => past ;
      	<Fut, Anter,Pos> => fut ;
      	<_,_,_> => inf
               
        };
    s2 = \\_ => []
    };

  
  Verbprefix : Number -> Gender -> Animacy -> Person -> Str = \n,g,anim,p ->
   case <anim,n,g,p> of {
    <AN,Sg,_,P1>      => "ni" ;
    <AN,Sg,_,P2>      => "u" ;
    <AN,Pl,_,P1>      => "tu" ;
    <AN,Pl,_,P2>      => "m" ;
    <AN,Sg,_,_>      => "a" ;
    <AN,Pl,_,_>      => "wa" ;
    <_,Sg,g1_2,_>   => "a" ;
    <_,Pl,g1_2,_>   => "wa" ;
    <_,Sg,g3_4,_>   => "u" ;
    <_,Pl,g3_4,_>   => "i"  ;
    <_,Sg,g5_6,_>   => "li" ;
    <_,Pl,g5_6,_>   => "ya" ;
    <_,Sg,g5a_6,_>  => "li" ;
    <_,Pl,g5a_6,_>  => "ya" ;
    <IN,_,g6,_>     => "ya" ;
    <IN,Sg,g7_8,_>   => "ki" ;
    <IN,Pl,g7_8,_>   => "vi" ;
    <IN,Sg,g9_10,_>  => "i" ;
    <IN,Pl,g9_10,_>  => "zi" ;
    <IN,_,g11,_>     => "u" ;
    <IN,Sg,g11_6,_>  => "u" ;
    <IN,Pl,g11_6,_>  => "ya" ;
    <IN,Sg,g11_10,_> => "u" ;
    <IN,Pl,g11_10,_> => "zi"  
   } ;





  

-- Auxiliary verbs have special negative forms.
param
    VVForm = 
       VVF VForm
     | VVPresNeg
     | VVPastNeg  --# notpresent
     ; 
                
--Adjectives 

 oper Adj = {s : AForm => Str} ;

--2 For $Quantifiers$
-- A 3-dimensional system of quantifiers (demonstrative pronouns) based on position of object, hearer + speaker
-- need to find linguistic term to express this

   param Spatial = SpHrObj | SpHr | HrObj ;    --w.r.t object	

-- Agreement of adjectives, verb phrases, and relative pronouns.

oper
  AGR = {n : Number ; g : Gender ; anim : Animacy ; p : Person} ;
  Agr : Type = {n : Number ; g : Gender ; anim : Animacy ; p : Person} ;
  agr : Number -> Gender -> Animacy -> Person -> Agr = \n,g,anim,p -> {n = n ; g = g ; anim = anim ; p = p} ;

-- For $Sentence$.

 Clause : Type = {
    s : Tense => Anteriority => Polarity => Str
    } ;

  mkClause : Str -> Agr -> VP -> Clause =
    \subj,agr,vp -> {
      s = \\t,a,b => 
        let 
          verb  = vp.s ! t ! a ! b ! agr 
        in
          subj ++ verb
    } ;

-}

}
