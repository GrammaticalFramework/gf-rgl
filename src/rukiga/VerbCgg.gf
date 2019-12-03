--# -path=.:../prelude:../abstract:../common

concrete VerbCgg of Verb = CatCgg ** open ResCgg, Prelude in {

lin

      UseV v = {
        s = v.s ; 
        pres =v.pres; 
        perf = v.perf; 
        --morphs = v.morphs;
        isPresBlank = v.isPresBlank;
        isPerfBlank = v.isPerfBlank;
        comp =[];
        comp2 = [];
        ap =[];
        isCompApStem = False; 
        agr = AgrNo; 
        isRegular = v.isRegular;
        adv =[];
        containsAdv =False;
        adV =[];
        containsAdV = False
        };  --: V   -> VP; -- sleep --ignoring object agreement

  --  UseComp  : Comp -> VP ; -- be warm means complement of a copula especially adjectival Phrase
  --AdjectivalPhrase : Type = {s : Str ; post : Str; isPre : Bool; isProper : Bool; isPrep: Bool};
      UseComp comp = 
        --let auxBe = mkBecome
        --in
        case comp.source of{
              AdjP => {
                      s = mkBecome.s ++ BIND ++ mkBecome.pres;  --Assuming there is no AP which is prepositional
                      pres =[]; 
                      perf = [];
                      isPresBlank = True;
                      isPerfBlank = True;
                      --morphs=\\form,morphs=>[]; 
                      comp = comp.s;
                      comp2 = [];
                      ap = [];
                      isCompApStem = True; 
                      agr = AgrNo;
                      isRegular = False;
                      adv = [];
                      containsAdv =False;
                      adV =[];
                      containsAdV = False
                    };
              _    => {
                        s = mkBecome.s ++ BIND ++mkBecome.pres++ comp.s;  --Assuming there is no AP which is prepositional
                        pres =[]; 
                        perf = [];
                        --morphs=\\form,morphs=>[]; 
                        isPresBlank = True;
                        isPerfBlank = True;
                        comp = [];
                        comp2 = [];
                        ap = [];
                        isCompApStem = False; 
                        agr = AgrNo;
                        isRegular = False;
                        adv = [];
                        containsAdv =False;
                        adV =[];
                        containsAdV = False
                      }
          }; --its not generating any sentence
                       
                      

--    CompAP   : AP  -> Comp;            -- (be) small
      CompAP ap = {s=ap.s! AgP3 Sg KI_BI; source = AdjP}; -- used a hack.

--    CompNP   : NP  -> Comp ;            -- (be) the man
      CompNP np = {s= np.s ! Acc; source = NounP}; --{s =[] ; post =np.s; isPre = False; isProper = Bool; isPrep: Bool};

--    CompAdv  : Adv -> Comp ;            -- (be) here
      CompAdv adv ={ s= adv.s; source = ADverb};
      {-
          This has been a hack to simply pick the sigular and complete noun.
      -}
      --CompCN   : CN  -> Comp ;            -- (be) a man/men
      CompCN   cn = {s =cn.s ! Sg ! Complete; source = CommonNoun} ;            -- (be) a man/men
--    SlashV2a : V2        -> VPSlash ;  -- love (it)
      SlashV2a v2 ={ 
        s =v2.s;
        pres =v2.pres; 
        perf = v2.perf;
        --morphs = v2.morphs;
        isPresBlank = v2.isPresBlank;
        isPerfBlank = v2.isPerfBlank;
        comp = [];
        comp2 =[];
        ap =[];
        isRegular =v2.isRegular;
        adv=[];
        containsAdv =False;
        adV =[];
        containsAdV = False
      };
      --Slash2V3 : V3  -> NP -> VPSlash ;  -- give it (to her)
      Slash2V3 v3 np ={
        s =v3.s;
        pres =v3.pres; 
        perf = v3.perf;
        --morphs = v3.morphs;
        isPresBlank = v3.isPresBlank;
        isPerfBlank = v3.isPerfBlank;
        comp =  np.s ! Acc;
        comp2 =[];
        ap =[];
        isRegular = v3.isRegular;
        adv = [];
        containsAdv =False;
        adV =[];
        containsAdV = False
      };

      --Slash3V3 : V3  -> NP -> VPSlash ;  -- give (it) to her
      Slash3V3  v3 np ={
        s =v3.s;
        pres =v3.pres; 
        perf = v3.perf;
        --morphs = v3.morphs; 
        isPresBlank = v3.isPresBlank;
        isPerfBlank = v3.isPerfBlank;
        comp = np.s ! Acc; 
        comp2 = np.s ! Acc; -- what is the meaning of this function?
        ap = [];
        isRegular = v3.isRegular;
        adv = [];
        containsAdv =False;
        adV =[];
        containsAdV = False
      };
      --SlashVV    : VV  -> VPSlash -> VPSlash ;       -- want to buy
      SlashVV vv vpslash ={
        s =vv.s;
        pres =vv.pres; 
        perf = vv.perf;
        --morphs = vv.morphs;
        isPresBlank = vv.isPresBlank;
        isPerfBlank = vv.isPerfBlank;
        comp = case vv.isPresBlank of {
                        False => vpslash.s ++ BIND ++ vpslash.pres;
                        _     => vpslash.s
                      };
        comp2 = [];
        ap = [];
        isRegular = vv.isRegular;
        adv = [];
        containsAdv = False;
        adV =[];
        containsAdV = False
      };
      --SlashV2V : V2V -> VP -> VPSlash ;  -- beg (her) to go

    --SlashV2S : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
    --SlashV2Q : V2Q -> QS -> VPSlash ;  -- ask (him) who came
    --SlashV2A : V2A -> AP -> VPSlash ;  -- paint (it) red
--    ComplSlash : VPSlash -> NP -> VP ; -- love it
      ComplSlash vpslash np ={ 
        s =vpslash.s;
        pres =vpslash.pres; 
        perf = vpslash.perf;
        --morphs = vpslash.morphs;
        isPresBlank = vpslash.isPresBlank;
        isPerfBlank = vpslash.isPerfBlank; 
        comp = vpslash.comp ++  np.s ! Acc;
        comp2 =vpslash.comp2; --should be empty
        ap = [];
        isCompApStem = False; 
        agr = AgrYes np.agr;
        isRegular = vpslash.isRegular;
        adv = [];
        containsAdv =False;
        adV =[];
        containsAdV = False
      };
--   AdvVP    : VP -> Adv -> VP ;        -- sleep here
--   VerbPhrase: Type = {s:Str; morphs: VMorphs ; comp:Str ; isCompApStem : Bool; agr : AgrExist};
     AdvVP vp adv =
      {
        s=vp.s; 
        pres =vp.pres; 
        perf = vp.perf; 
        --morphs = vp.morphs;
        isPresBlank = vp.isPresBlank;
        isPerfBlank = vp.isPerfBlank; 
        comp = adv.s;
        comp2 = [];
        ap =[];
        isCompApStem = False; 
        agr = AgrNo;
        isRegular = vp.isRegular;
        adv = [];
        containsAdv =True;
        adV =[];
        containsAdV = False
      };

  -- AdVVP    : AdV -> VP -> VP ;        -- always sleep
  AdVVP adV vp = {
      s=vp.s; 
      pres =vp.pres; 
      perf = vp.perf; 
      --morphs = vp.morphs;
      isPresBlank = vp.isPresBlank;
      isPerfBlank = vp.isPerfBlank;
      comp = vp.comp;
      comp2 =vp.comp2;
      ap = [];
      isCompApStem = False; 
      agr = AgrNo;
      isRegular = vp.isRegular;
      adv = [];
      containsAdv =False;
      adV =adV.s;
      containsAdV = True
      };

    --AdvVPSlash : VPSlash -> Adv -> VPSlash ;  -- use (it) here
    {-
        FUTURE:
        The problem here could rise from the agreement if the adverb agrees.
        We could change the type of adv to be Agreement => Str such that we have NONE.
    -}
    AdvVPSlash vpslash adv ={
        s =vpslash.s;
        pres =vpslash.pres; 
        perf = vpslash.perf;
        --morphs = vpslash.morphs;
        isPresBlank = vpslash.isPresBlank;
        isPerfBlank = vpslash.isPerfBlank; 
        comp = vpslash.comp; 
        comp2 = vpslash.comp2;
        ap = [];
        isRegular = vpslash.isRegular;
        adv = adv.s;
        containsAdv =True;
        adV =[];
        containsAdV = False
      };
    -- Adverb directly attached to verb
    --AdVVPSlash : AdV -> VPSlash -> VPSlash ;  -- always use (it)
    {- NOTE:
      This is a hack mainly because we need a separate field for AdV type

    -}
    AdVVPSlash adV vpslash ={
        s =vpslash.s;
        pres =vpslash.pres; 
        perf = vpslash.perf;
        --morphs = vpslash.morphs;
        isPresBlank = vpslash.isPresBlank;
        isPerfBlank = vpslash.isPerfBlank; 
        comp = vpslash.comp; 
        comp2 = vpslash.comp2;
        ap = [];
        isRegular = vpslash.isRegular;
        adv = [];
        containsAdv =False;
        adV =adV.s;
        containsAdV = True
      };
  -- Verb phrases are constructed from verbs by providing their
  -- complements. There is one rule for each verb category.

  {- NOTE: This is a hack
  --ComplVV  : VV  -> VP -> VP ;  -- want to run
  --This function requires the remodelling of VP to accomodate two Verbs.
  --
  -}
  ComplVV vv vp = let vpPres = "ku" ++ BIND ++ vp.s ++ BIND ++ vp.pres;
                  in case vv.whenUsed of {
                          VVPerf => {
                                        s= vv.s ++ BIND ++ vv.perf ++ vpPres; 
                                        pres = [];--vv.pres; 
                                        perf=  []; -- vv.perf; 
                                        --morphs = vv.morphs;
                                        isPresBlank = True;
                                        isPerfBlank = True; 
                                        comp=vp.comp ;
                                        comp2 = vp.comp2;
                                        ap = [];
                                        isCompApStem = False; agr = AgrNo; 
                                        isRegular = vv.isRegular; 
                                        adv =[]; 
                                        containsAdv =False;
                                        adV =[];
                                        containsAdV = False
                                    };
                          _      => {
                                      s= vv.s ++ BIND ++ vv.pres ++ vpPres; 
                                      pres = [];--vv.pres; 
                                      perf=  [];--vv.perf; 
                                      --morphs = vv.morphs;
                                      isPresBlank = True;
                                      isPerfBlank = True;  
                                      comp=vp.comp ;
                                      comp2 = vp.comp2;
                                      ap = []; 
                                      isCompApStem = False; 
                                      agr = AgrNo; 
                                      isRegular = vv.isRegular; 
                                      adv =[]; 
                                      containsAdv =False;
                                      adV =[];
                                      containsAdV = False
                                    }
                  };

  --ComplVS  : VS  -> S  -> VP ;  -- say that she runs
  ComplVS vs s = {
    s= vs.s; 
    pres =vs.pres; 
    perf=vs.perf; 
    --morphs = vs.morphs;
    isPresBlank = vs.isPresBlank;
    isPerfBlank = vs.isPerfBlank;
    comp=s.s ;
    comp2 = []; 
    ap = [];
    isCompApStem = False; 
    agr = AgrNo; 
    isRegular = vs.isRegular; 
    adv =[]; 
    containsAdv =False;
    adV =[];
    containsAdV = False
  };

  {-
    This function may need revision as I have not met such kinds of questions
  -}
  --ComplVQ  : VQ  -> QS -> VP ;  -- wonder who runs
  ComplVQ vq qs = {
    s= vq.s; 
    pres =vq.pres; 
    perf=vq.perf; 
    --morphs = vq.morphs;
    isPresBlank = vq.isPresBlank;
    isPerfBlank = vq.isPerfBlank;
    comp=qs.s ;
    comp2 = [];
    ap = []; 
    isCompApStem = False; 
    agr = AgrNo; 
    isRegular = vq.isRegular; 
    adv =[]; 
    containsAdv =False;
    adV =[];
    containsAdV = False
  };

  {-
    The adgectival Phrase is comlicated here.

    The VP has to accomodate the whole structure of the Adjectival Phrase

    For the timebeing, we can use the isCompApStem field but we need a separate field
  -}
  --ComplVA  : VA  -> AP -> VP ;  -- they become red
  --AP = {s : Str ; position1 : Res.Position1; isProper : Bool; isPrep: Bool};--Res.AdjectivalPhrase;
  ComplVA va ap = {
    s= va.s; 
    pres =va.pres; 
    perf=va.perf; 
    --morphs = va.morphs;
    isPresBlank = va.isPresBlank;
    isPerfBlank = va.isPerfBlank;
    comp=[] ;
    comp2 = [];
    ap = ap.s! AgP3 Sg KI_BI; 
    isCompApStem = True; 
    agr = AgrNo; 
    isRegular = va.isRegular; 
    adv =[]; 
    containsAdv =False;
    adV =[];
    containsAdV = False
  };

  -- Copula alone
  --UseCopula : VP ;                    -- be
  UseCopula = be_Copula ** {
                            comp=[]; 
                            comp2 = [];
                            ap = [];
                            isCompApStem = False; 
                            agr = AgrNo;
                            adv = []; 
                            containsAdv = False;
                            adV =[];
                            containsAdV = False
                  };
{-
--1 The construction of verb phrases

abstract Verb = Cat ** {

  flags coding = utf8 ;

--2 Complementization rules

-- Verb phrases are constructed from verbs by providing their
-- complements. There is one rule for each verb category.

    ComplVV  : VV  -> VP -> VP ;  -- want to run
    ComplVS  : VS  -> S  -> VP ;  -- say that she runs
    ComplVQ  : VQ  -> QS -> VP ;  -- wonder who runs
    ComplVA  : VA  -> AP -> VP ;  -- they become red

    SlashV2a : V2        -> VPSlash ;  -- love (it)
    Slash2V3 : V3  -> NP -> VPSlash ;  -- give it (to her)
    Slash3V3 : V3  -> NP -> VPSlash ;  -- give (it) to her

    SlashV2V : V2V -> VP -> VPSlash ;  -- beg (her) to go
    SlashV2S : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
    SlashV2Q : V2Q -> QS -> VPSlash ;  -- ask (him) who came
    SlashV2A : V2A -> AP -> VPSlash ;  -- paint (it) red

    ComplSlash : VPSlash -> NP -> VP ; -- love it

    SlashVV    : VV  -> VPSlash -> VPSlash ;       -- want to buy
    SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy

--2 Other ways of forming verb phrases

-- Verb phrases can also be constructed reflexively and from
-- copula-preceded complements.

    ReflVP   : VPSlash -> VP ;         -- love himself
    UseComp  : Comp -> VP ;            -- be warm

-- Passivization of two-place verbs is another way to use
-- them. In many languages, the result is a participle that
-- is used as complement to a copula ("is used"), but other
-- auxiliary verbs are possible (Ger. "wird angewendet", It.
-- "viene usato"), as well as special verb forms (Fin. "käytetään",
-- Swe. "används").
--
-- *Note*. the rule can be overgenerating, since the $V2$ need not
-- take a direct object.

    PassV2   : V2 -> VP ;               -- be loved

-- Adverbs can be added to verb phrases. Many languages make
-- a distinction between adverbs that are attached in the end
-- vs. next to (or before) the verb.

    AdvVP    : VP -> Adv -> VP ;        -- sleep here
    ExtAdvVP : VP -> Adv -> VP ;        -- sleep , even though ...
    AdVVP    : AdV -> VP -> VP ;        -- always sleep

    AdvVPSlash : VPSlash -> Adv -> VPSlash ;  -- use (it) here
    AdVVPSlash : AdV -> VPSlash -> VPSlash ;  -- always use (it)
   
    VPSlashPrep : VP -> Prep -> VPSlash ;  -- live in (it)


-- *Agents of passives* are constructed as adverbs with the
-- preposition [Structural Structural.html]$.8agent_Prep$.


--2 Complements to copula

-- Adjectival phrases, noun phrases, and adverbs can be used.

    CompAP   : AP  -> Comp ;            -- (be) small
    CompNP   : NP  -> Comp ;            -- (be) the man
    CompAdv  : Adv -> Comp ;            -- (be) here
    CompCN   : CN  -> Comp ;            -- (be) a man/men

-- Copula alone

    UseCopula : VP ;                    -- be

-}

}
