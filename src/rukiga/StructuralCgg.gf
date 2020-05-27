--# -path=.:../prelude:../abstract:../common

concrete StructuralCgg of Structural = CatCgg ** 
  open ResCgg, ParadigmsCgg, (C = ConstructX), Prelude in {


{-variants
    NOTE: Please add them to the abstract syntax, ask aarne 
    or creat you own abstract Lexicon which inherits from the 
    standard one. See how english does it. i.e. use DictCggAbs.gf for the funs.
    and DictCgg.gf for the lins.

    Actually use and extend module for Structural
-}

lin
  --Determiner : Type = {s : Str ; s2: Agreement=>Str; ntype : NounState ; num : Number ; pos : Position; doesAgree: Bool };
  a_Det = {s =[] ; s2 = \\_ => []; ntype = Complete; num = Sg; pos = Pre; doesAgree = False};     --: Det ; indefinite singular ---s
  aPl_Det = {s =[]; s2= \\_ => []; ntype = Complete; num = Pl; pos = Pre; doesAgree = False}; -- : Det ;indefinite plural   ---s
  the_Det = {s =[]; s2= \\_ => []; ntype = Complete; num = Sg; pos = Pre; doesAgree = False};  --: Det ;                   -- definite singular   ---s    thePl_Det = {s =[]; ntype = Complete; num = Pl; pos = PreDeterminer}; --: Det ;definite plural     ---s
  
  every_Det = {s ="buri"; s2 = \\_ => []; ntype=Incomplete; num=Sg; pos=Pre; doesAgree = False} ;
  few_Det = {s="kye"; s2 = \\_ => []; ntype =Complete; num=Pl; pos=Post; doesAgree = False} ;
  many_Det ={s="ingi"; s2 = \\_ => []; ntype =Complete; num=Pl; pos=Post; doesAgree = False} ;
  
  i_Pron            = {s = table{Gen => glueGen (AgMUBAP1 Sg); _=> mkSStand (AgMUBAP1 Sg)}; third = \\_,_=>[];  agr = AgrYes (AgMUBAP1 Sg)};--mkPron "nyowe" "nyowe" (AgMUBAP1 Sg);
  youSg_Pron        = {s = table{Gen => glueGen (AgMUBAP2 Sg); _=>mkSStand (AgMUBAP2 Sg)}; third = \\_,_=>[];  agr = AgrYes(AgMUBAP2 Sg)};--mkPron "iwe" "we" (AgMUBAP2 Sg); 
  he_Pron, she_Pron = {s = table{Gen => glueGen (AgP3 Sg MU_BA); _=>mkSStand (AgP3 Sg MU_BA)}; third = \\_,_=>[];  agr = AgrYes(AgP3 Sg MU_BA)};--mkPron "uwe" "uwe" (AgP3 Sg MU_BA);
  we_Pron           = {s = table{Gen => glueGen (AgMUBAP1 Pl); _=>mkSStand (AgMUBAP1 Pl)}; third = \\_,_=>[];  agr = AgrYes (AgMUBAP1 Pl)}; --mkPron "itwe" "itwe" (AgMUBAP1 Pl);
  youPl_Pron        = {s = table{Gen => glueGen (AgMUBAP2 Pl); _=>mkSStand (AgMUBAP2 Pl)}; third = \\_,_=>[]; agr =AgrYes (AgMUBAP2 Pl)};--mkPron "imwe" "imwe" (AgMUBAP2 Pl);    they_Pron         = {s = table{Gen => glueGen  AgP3 Pl MU_BA; _=>mkSStand (AgP3 Pl MU_BA)}; third = \\_,_=>[]; itP3Required=False};--mkPron "bo" "bo" (AgP3 Pl MU_BA);
  -- default implementation Using KI_BI. Use mkmkGenPrepNoIVClitic and 
  it_Pron = {
    s = \\_=>[]; 
    third = \\agr => table{Gen =>glueGen agr; _ => mkSStand agr}; 
    agr = AgrNo
    }; --mkPron "kyo" "kyo" (AgP3 Sg KI_BI); -- should form an it_Pron_NClass in extra module
  
  behind_Prep = mkPrep "enyuma ya" [] False;
  between_Prep =mkPrep "hagati ya" [] False;
  to_Prep = mkPrep "aha" [] False;
  -- several words depending on use omuri??
  in_Prep        = mkPrep "omu" "omuri" False;
  --aha-ri Kamukuzi??? works for places 
  on_Prep        = mkPrep "aha" "ahari" False;

  in8front_Prep  = mkPrep "enyuma ya" [] False; --: Prep ; -- in front of
  
--na --please this string varies with vowels use combine_morphemes or 
--combine_words when using it.
  with_Prep      = mkPrep "na" [] False; 

  from_Prep =mkPrep "kurunga" "" False;
  under_Prep = mkPrep "hansi ya" "" False;
  after_Prep = mkPrep "omu maisho" "" False; --: Prep ;
  
  ---Structural
  {-
    --there are several and i.e. 
    -- na (two nouns, 2 Noun Phrases, 2 Pronouns, 2 relative subject clauses, )
    --kandi (clauses having a commonality of subjects, object or tense)
    --the best structure is a table 
    --mkConjunction "na" "kandi" and_Conj ;
 -}
     and_Conj  = {
      s = table {
          AConj Other => "kandi";
          _ => "na" -- used to link nouns, pronouns,  relative subject clauses, relative object clauses and adjectival nouns or infinitives (okuzana na okwikruka
        };
        
      s2 =[];
      n  = Pl 
      };




  {-

      TODO: Look at the grammar books by Mpairwe & Kahangi Pg 155
      and investigate or to find out its arguments but for now
      I will assume nari works on all types of 
      ConjArg (Conjunction Arguments)

      nari is the general or
      
      These are candidates for Extra module if they are not specific
      to the type of argument.
      nînga for Runynakore and 

      nainga for rukiga
  -}

    
    or_Conj = {
      s = \\ _ => "nari"; 
      s2 =[];
      n  = Sg 
    };
    
   have_V2 ={s= "ine"; pres=[]; perf =[]; isPresBlank = False;
                        isPerfBlank = False; morphs = mkVerbMorphs; comp = []; isRegular=False};  --: V2 ;

  {-
    All Predeterminers are given here.
    Initial analysis shows that 
     a) They appear after the noun phrase but some may be multi-word expressions for a single word in eblish.
     b) They appear to agree with the noun class particle. But nothing in the literature states about
        their morphological structure.  The stems can be guessed by removing the two letter
        suffix at the begining of the word. However, there are exceptions such as "not" which is not
        inflected according to noun class 
    c) A table of concords must be built to accomodate every instance and this can only be done 
      using an analysis of some of the words in the dictionary(Mapirwe and Kahagi).
      I am incluned to say use of the table of self-standing pronouns is sufficient.
    d) An investigation of the tone systems would also be worthwhile.
    

    Example sentences:
    1. All these chickens
    2. once a day
    3. only the man
  -}
  all_Predet = {s  = "òna"; s2 = []; isMWE = False; isInflected =True};
  only_Predet = {s = "nka"; s2 = []; isMWE = False; isInflected =True};
  {-
  -- how do we deal with superlatives. There seems to be no distinction between countable
  -- and uncountable when it comes to superlatioves
  -}
  most_Predet = {s = "rikukíra"; s2 = "îngi"; isMWE = True; isInflected =True}; 
  not_Predet  = {s = "ti"; s2 = []; isMWE = False; isInflected =False};




  {-Section for Adverbs-}
  always_AdV = {s = "obutóòsha"; agr = AgrNo};
  everywhere_Adv = {s = "hóòna"; agr = AgrNo}; -- adverb of place.
  here_Adv = {s = "hanu"; agr = AgrNo};
  {-End of Adverbs Adverbs-}

  {-Begining of Quantifiers-}
    
          --For DetQuant function to work, we need sample quatitifiers in Runynakore. Proximal, Medial, Distant
          --We need a table to provide all of these.
    
  that_Quant  = {s={s = \\_=>[]; third =\\_,_=>[]; agr = AgrNo}; s2 = mkThat; doesAgree = True; isPron = False}; --: Quant ;
  this_Quant =  {s={s = \\_=>[]; third =\\_,_=>[]; agr = AgrNo}; s2 = mkThis; doesAgree = True; isPron = False}; --: Quant ;
    
  no_Quant = {s ={s=\\_=>"tihariho";third =\\_,_=>[]; agr=AgrNo}; s2 =\\_=> []; doesAgree = False; isPron = False};--: Quant ;
  {-End of Quantifiers-}




  {-Begining of verb-phrase-complement verb VV-} -- A verb whose complement is a verb phrase
   --can8know_VV : VV ; -- can (capacity)





  {-End of verb-phrase-complement verb -}

  {-Beggining of Interrogative Pronoun-}

  whatPl_IP = { s= "ki";  n = IPl;  isVerbSuffix = True; requiresIPPrefix = False; aux=[]; endOfSentence = True} ; -- what (plural)
  whatSg_IP = { s= "ki";  n = ISg;  isVerbSuffix = True; requiresIPPrefix = False; aux=[]; endOfSentence = True}; --: IP ; -- what (singular)
  whoPl_IP  = { s=  "ha";  n = IPl; isVerbSuffix = True; requiresIPPrefix = False; aux="ni"; endOfSentence = True} ;--: IP ;  -- who (plural)
  whoSg_IP =  { s=  "ha"; n = ISg;  isVerbSuffix = True; requiresIPPrefix = False; aux=[]; endOfSentence = True}; --: IP ;  -- who (singular)
  --You may need to use booleans to indicate that you need these tables rather than carrying them.
  how_IAdv = {s ="ta"; requiresSubjPrefix = True; endOfSentence =True};  --: IAdv ;
  --how8much_IAdv = {s ="kwiga"; s2requireSubjPrefix = True};--: IAdv ;

  when_IAdv = {s ="ryari"; requiresSubjPrefix = False; endOfSentence =True}; --: IAdv ;
  where_IAdv = {s ="nkahe"; requiresSubjPrefix = False; endOfSentence =True}; --: IAdv ;
  why_IAdv  = {s ="ahabweki"; requiresSubjPrefix = False; endOfSentence =False};--: IAdv ;
  
  how8many_IDet  ={s ="ngahe"; n =Pl; requiresSubjPrefix = False; endOfSentence = True};--: IDet ;

  which_IQuant ={s =\\_ =>"ha"; requiresSubjPrefix = False};--: IQuant ;


  almost_AdN = {s="hihi"}; --: AdN ; -- what about nika
  at_least_AdN ={s= " hakiri"}; --: AdN ; --need advice from linguist. What about akakye?

  {-
    The following require some reflection from a linguist.
  -}
  someSg_Det =
    {
      s =[]; 
      s2 =\\agr => mkSubjCliticTableSg ! agr ++ BIND++ "mwe"; 
      ntype = Complete;
      num = Sg;
      pos = Pre;
      doesAgree = True
    };
  somePl_Det  =
    {
      s =[]; 
      s2 =\\agr => mkSubjCliticTablePl ! agr ++ BIND++ "mwe"; 
      ntype = Complete;
      num = Pl;
      pos = Pre;
      doesAgree = True
    };--: Det ;
  
   want_VV =  {s = "yend"; pres="da"; perf = "zire"; isPresBlank = False;
                        isPerfBlank = False; morphs=mkVerbMorphs; isRegular=True; inf=[]; whenUsed = VVBoth};
   can8know_VV = {s = "baas"; pres="a"; perf = "ize"; isPresBlank = False;
                        isPerfBlank = False; morphs=mkVerbMorphs; isRegular=True; inf=[]; whenUsed = VVBoth};--: VV ; -- can (capacity)
   can_VV =  {s = "baas"; pres="a"; perf = "ize"; isPresBlank = False;
                        isPerfBlank = False; morphs=mkVerbMorphs; isRegular=True; inf=[]; whenUsed = VVBoth};--: VV ;      -- can (possibility)
   -- must_VV used especially in the perfective mood: see dictionary entry shemerera on Pg 501 of Mpairwe
   -- must has no passive form
   must_VV = {s = "shemere"; pres="ra"; perf = "ire"; isPresBlank = False;
                        isPerfBlank = False; morphs=mkVerbMorphs; isRegular=False; inf=[]; whenUsed = VVPerf}; --VV 
    --somebody_NP = {}; --: NP ;
    --something_NP : NP ;
    --somewhere_Adv : Adv ;

  that_Subj = ss "ngu" ;

  --Adjective modifying Adverbs
  almost_AdA = {s="haihi"; position=Pre}; --: AdA ;
  --quite_Adv ss "kimwe"; --: AdA ; used in the pr
  so_AdA = {s="munônga"; position=Post};--: AdA ;
  too_AdA = {s="munônga"; position=Post}; --: AdA ;
  very_AdA  = {s="munônga"; position=Post}; --: AdA ;

  please_Voc = ss ", nyabura we"; --: Voc ;

  but_PConj = ss "báìtu"; --: PConj ; -- variants béìtu
  otherwise_PConj = ss "okûndi"; --: PConj ;
  therefore_PConj = ss "n'ahabwe'êkyo";  --: PConj ;
  {-
  and_Conj : Conj ;
  both7and_DConj : Conj ; -- both...and
  both7and_DConj : DConj ;
  but_PConj : PConj ;
  either7or_DConj : Conj ; -- either...or
  either7or_DConj : DConj ;
  or_Conj : Conj ;
  otherwise_PConj : PConj ;
  therefore_PConj : PConj ;
  if_then_Conj : Conj ;

  -}

{-	
--1 Structural: Structural Words
--
-- Here we have some words belonging to closed classes and appearing
-- in all languages we have considered.
-- Sometimes more distinctions are needed, e.g. $we_Pron$ in Spanish
-- should be replaced by masculine and feminine variants, found in
-- [``ExtendSpa`` ../spanish/ExtendSpa.gf].

abstract Structural = Cat ** {

  fun

-- This is an alphabetical list of structural words

  above_Prep : Prep ;
  after_Prep : Prep ;
  all_Predet : Predet ;
  almost_AdA : AdA ;
  almost_AdN : AdN ;
  although_Subj : Subj ;
  always_AdV : AdV ;
  and_Conj : Conj ;
  because_Subj : Subj ;
  before_Prep : Prep ;
  behind_Prep : Prep ;
  between_Prep : Prep ;
  both7and_DConj : Conj ; -- both...and
---b  both7and_DConj : DConj ;
  but_PConj : PConj ;
  by8agent_Prep : Prep ; -- by (agent)
  by8means_Prep : Prep ; -- by (means of)
  can8know_VV : VV ; -- can (capacity)
  can_VV : VV ;      -- can (possibility)
  during_Prep : Prep ;
  either7or_DConj : Conj ; -- either...or
---b  either7or_DConj : DConj ;
  every_Det : Det ;
  everybody_NP : NP ;  -- everybody
  everything_NP : NP ;
  everywhere_Adv : Adv ; --ha-ona =hoona
---  first_Ord : Ord ; DEPRECATED
  few_Det : Det ;
  for_Prep : Prep ;
  from_Prep : Prep ;
  he_Pron : Pron ;
  here_Adv : Adv ; --hanu
  

  here7to_Adv : Adv ; -- to here
  here7from_Adv : Adv ;  -- from here
  how_IAdv : IAdv ;
  how8many_IDet : IDet ;
  how8much_IAdv : IAdv ;
  i_Pron : Pron ;
  if_Subj : Subj ;
  in8front_Prep : Prep ; -- in front of
  in_Prep : Prep ;
  it_Pron : Pron ;
  less_CAdv : CAdv ;
  many_Det : Det ;
  more_CAdv : CAdv ;
  most_Predet : Predet ;
  much_Det : Det ;
  must_VV : VV ;
---b  no_Phr : Phr ;
  no_Utt : Utt ;
  on_Prep : Prep ;
---  one_Quant : QuantSg ; DEPRECATED
  only_Predet : Predet ;
  or_Conj : Conj ;
  otherwise_PConj : PConj ;
  part_Prep : Prep ;
  please_Voc : Voc ;
  possess_Prep : Prep ; -- of (possessive)
  quite_Adv : AdA ;
  she_Pron : Pron ;
  so_AdA : AdA ;
  someSg_Det : Det ;
  somePl_Det : Det ;
  somebody_NP : NP ;
  something_NP : NP ;
  somewhere_Adv : Adv ;
  that_Quant : Quant ;
  that_Subj : Subj ;
  there_Adv : Adv ; --hari

  
  there7to_Adv : Adv ; -- to there
  there7from_Adv : Adv ; -- from there
  therefore_PConj : PConj ;
  they_Pron : Pron ;
  this_Quant : Quant ;
  through_Prep : Prep ;
  to_Prep : Prep ;
  too_AdA : AdA ;
  under_Prep : Prep ;
  very_AdA : AdA ;
  want_VV : VV ;
  we_Pron : Pron ;
  whatPl_IP : IP ; -- what (plural)
  whatSg_IP : IP ; -- what (singular)
  when_IAdv : IAdv ;
  when_Subj : Subj ;
  where_IAdv : IAdv ;
  which_IQuant : IQuant ;
  whoPl_IP : IP ;  -- who (plural)
  whoSg_IP : IP ;  -- who (singular)
  why_IAdv : IAdv ;
  with_Prep : Prep ;
  without_Prep : Prep ;
---b  yes_Phr : Phr ;
  yes_Utt : Utt ;
  youSg_Pron : Pron ; -- you (singular)
  youPl_Pron : Pron ; -- you (plural)
  youPol_Pron : Pron ; -- you (polite)

  no_Quant : Quant ;
  not_Predet : Predet ;
  if_then_Conj : Conj ;
  at_least_AdN : AdN ;
  at_most_AdN : AdN ;
  nobody_NP : NP ;
  nothing_NP : NP ;
  except_Prep : Prep ;

  as_CAdv : CAdv ;

  have_V2 : V2 ;

  fun language_title_Utt : Utt ;

-}

}

