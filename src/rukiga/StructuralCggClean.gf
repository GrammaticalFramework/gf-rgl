--# -path=.:../prelude:../abstract:../common

concrete StructuralCgg of Structural = CatCgg ** 
  open ResCgg, ParadigmsCgg, (C = ConstructX), Prelude in {


  	lin
	  --Determiner : Type = {s : Str ; s2: Agreement=>Str; ntype : NounState ; num : Number ; pos : Position; doesAgree: Bool };
	  a_Det = {s =[] ; s2 = \\_ => []; ntype = Complete; num = Sg; pos = PreDeterminer; doesAgree = False};     --: Det ; indefinite singular ---s
	  aPl_Det = {s =[]; s2= \\_ => []; ntype = Complete; num = Pl; pos = PreDeterminer; doesAgree = False}; -- : Det ;indefinite plural   ---s
	  the_Det = {s =[]; s2= \\_ => []; ntype = Complete; num = Sg; pos = PreDeterminer; doesAgree = False};  --: Det ;                   -- definite singular   ---s    thePl_Det = {s =[]; ntype = Complete; num = Pl; pos = PreDeterminer}; --: Det ;definite plural     ---s
	  
	  every_Det = {s ="buri"; s2 = \\_ => []; ntype=Incomplete; num=Sg; pos=PreDeterminer; doesAgree = False} ;
	  few_Det = {s="kye"; s2 = \\_ => []; ntype =Complete; num=Pl; pos=PostDeterminer; doesAgree = False} ;
	  many_Det ={s="ingi"; s2 = \\_ => []; ntype =Complete; num=Pl; pos=PostDeterminer; doesAgree = False} ;

	  behind_Prep ={s="enyuma ya"};
	  between_Prep = {s="hagati ya"};
	  to_Prep ={s="aha"};
	  -- several words depending on use omuri??
	  in_Prep        = mkPrep "omu" "omuri";
	  --aha-ri Kamukuzi??? works for places 
	  on_Prep        = mkPrep "aha" "ahari";

	  with_Prep      = mkPrep "na" []; 

	  from_Prep ={s="kuruga"};
	  under_Prep = {s="hansi ya"};

	  {-
	    --there are several and i.e. 
	    -- na (two nouns, 2 Noun Phrases, 2 Pronouns, 2 relative subject clauses, )
	    --kandi (clauses having a commonality of subjects, object or tense)
	    --the best structure is a table 
	    --mkConjunction "na" "kandi" and_Conj ;
 	  -}
      and_Conj  = {
       s = table { AConj Other => "kandi"; _ => "na"};  
       s2 =[];
       n  = Pl 
      };




	  	{-

	      --TODO: Look at the grammar books by Mpairwe & Kahangi Pg 155
	      --and investigate or to find out its arguments but for now
	      --I will assume nari works on all types of 
	      --ConjArg (Conjunction Arguments)

	      --nari is the general or
	      
	      --These are candidates for Extra module if they are not specific
	      --to the type of argument.
	      --nînga for Runynakore and 

	      --nainga for rukiga
	    -}

    
		or_Conj = {
		  s = \\ _ => "nari"; 
		  s2 =[];
		  n  = Sg 
		};
    
   		have_V2 ={s= "ine"; pres=[]; perf =[]; morphs = mkVerbMorphs; comp = []; isRegular=False};  --: V2 ;

	   i_Pron            = {s = table{Gen => glueGen (AgMUBAP1 Sg); _=> mkSStand (AgMUBAP1 Sg)}; third = \\_,_=>[]; itP3Required=False};--mkPron "nyowe" "nyowe" (AgMUBAP1 Sg);
	   youSg_Pron        = {s = table{Gen => glueGen (AgMUBAP2 Sg); _=>mkSStand (AgMUBAP2 Sg)}; third = \\_,_=>[]; itP3Required=False};--mkPron "iwe" "we" (AgMUBAP2 Sg); 
	   he_Pron, she_Pron = {s = table{Gen => glueGen (AgP3 Sg MU_BA); _=>mkSStand (AgP3 Sg MU_BA)}; third = \\_,_=>[]; itP3Required=False};--mkPron "uwe" "uwe" (AgP3 Sg MU_BA);
	   we_Pron           = {s = table{Gen => glueGen (AgMUBAP1 Pl); _=>mkSStand (AgMUBAP1 Pl)}; third = \\_,_=>[]; itP3Required=False}; --mkPron "itwe" "itwe" (AgMUBAP1 Pl);
	   youPl_Pron        = {s = table{Gen => glueGen (AgMUBAP2 Pl); _=>mkSStand (AgMUBAP2 Pl)}; third = \\_,_=>[]; itP3Required=False};--mkPron "imwe" "imwe" (AgMUBAP2 Pl);    they_Pron         = {s = table{Gen => glueGen  AgP3 Pl MU_BA; _=>mkSStand (AgP3 Pl MU_BA)}; third = \\_,_=>[]; itP3Required=False};--mkPron "bo" "bo" (AgP3 Pl MU_BA);
	  -- default implementation Using KI_BI. Use mkmkGenPrepNoIVClitic and 
	  it_Pron = {s = \\_=>[]; third = table{Gen => \\agr => glueGen agr; _=> \\agr => mkSStand agr}; itP3Required=True}; --mkPron "kyo" "kyo" (AgP3 Sg KI_BI); -- should form an it_Pron_NClass in extra module


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




	  -- Adverbs
	  always_AdV = {s = "obutóòsha"; agr = AgrNo};
	  everywhere_Adv = {s = "hóòna"; agr = AgrNo}; -- adverb of place.
	  here_Adv = {s = "hanu"; agr = AgrNo};
	  

	  --Quantifiers
	    
	  that_Quant  = {s={s=\\_ =>[]; agr=AgrNo}; s2 = mkThat; doesAgree = True; isPron = False}; --: Quant ;
	  this_Quant =  {s={s=\\_ =>[]; agr=AgrNo}; s2 = mkThis; doesAgree = True; isPron = False}; --: Quant ;
	  
	  no_Quant = {s ={s=\\_=>"tihariho";agr=AgrNo}; s2 =\\_=> []; doesAgree = False; isPron = False};--: Quant ;
	  






}