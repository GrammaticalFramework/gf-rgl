--# -path=.:../common:../abstract

concrete ExtendMay of Extend = CatMay
  ** ExtendFunctor - [
    VPS           -- finite VP's with tense and polarity
    , ListVPS, BaseVPS, ConsVPS, ConjVPS
    , VPI, MkVPI, ComplVPIVV
    , ListVPI -- infinitive VP's (TODO: with anteriority and polarity)
    , MkVPS
    , PredVPS, RelVPS, QuestVPS, SQuestVPS
    , PassVPSlash, PassAgentVPSlash

    -- excluded because RGL funs needed for them not implemented yet
    , PredAPVP


    ,PresPartAP, PastPartAP
    ,GenModNP, GenNP, GenRP
    ,CompoundN
    ,GerundNP, GerundAdv
    ,ByVP


    -- VPS2 ;        -- have loved (binary version of VPS)
    -- [VPS2] {2} ;  -- has loved, hates"
    -- VPI2 ;        -- to love (binary version of VPI)
    -- [VPI2] {2} ;  -- to love, to hate

]
  with (Grammar=GrammarMay)
  ** open Prelude, Coordination, ResMay, NounMay in {
    lincat
      VPS, VPI = SS ;
      ListVPS, ListVPI = ListX ;
    lin
      -- MkVPS      : Temp -> Pol -> VP -> VPS ;  -- hasn't slept
      MkVPS t p vp = {
        s = t.s ++ p.s ++ vp.s ! Active ! p.p;
        } ;

      -- BaseVPS : VPS -> VPS -> ListVPS ;
      BaseVPS = twoSS ;
      -- ConsVPS : VPS -> ListVPS -> ListVPS ;
      ConsVPS = consrSS ",";
      -- ConjVPS    : Conj -> [VPS] -> VPS ;      -- has walked and won't sleep
      ConjVPS = conjunctDistrSS ;
      -- PredVPS    : NP   -> VPS -> S ;          -- she [has walked and won't sleep]
      PredVPS np vps = {
        s = np.s ! Bare ++ vps.s ;
      } ;
      -- SQuestVPS  : NP   -> VPS -> QS ;         -- has she walked
      SQuestVPS np vps = {s = "adakah" ++ np.s ! Bare ++ vps.s} ;

      -- QuestVPS   : IP   -> VPS -> QS ;         -- who has walked
      QuestVPS ip vps = {s = ip.s ! Bare ++ vps.s} ;

      -- RelVPS     : RP   -> VPS -> RS ;         -- which won't sleep
      RelVPS rp vps = {s = \\person => rp.s ++ vps.s} ;

      -- MkVPI      : VP -> VPI ;                 -- to sleep (TODO: Ant and Pol)
      MkVPI vp = {s = linVP vp} ;


      -- BaseVPI : VPI -> VPI -> ListVPI ;
      BaseVPI vpi vpi2 = twoSS vpi vpi2 ;
      -- ConsVPI : VPI -> ListVPI -> ListVPI ;
      ConsVPI str listvpi vpi = consSS "," listvpi vpi ;

      -- ConjVPI    : Conj -> [VPI] -> VPI ;      -- to sleep and to walk
      -- ComplVPIVV : VV   -> VPI -> VP ;         -- must sleep and walk
      ComplVPIVV vv vpi = useV {
        s = \\vf => vv.s ++ vpi.s
        } ;

      -- PresPartAP    : VP -> AP ;   -- (the man) looking at Mary
      PresPartAP vp = {
        s = linVP vp
      } ;

      PastPartAP vp = {
        s = linVP vp
      } ;

      -- GenModNP    : Num -> NP -> CN -> NP ; -- this man's car(s)
      GenModNP n np cn = variants {};

      -- GenNP       : NP -> Quant ;       -- this man's
      GenNP np = variants {};
      -- GenRP       : Num -> CN -> RP ;   -- whose car
      GenRP n cn = variants {};

      -- CompoundN   : N -> N  -> N ;      -- control system / controls system / control-system
      CompoundN n1 n2 = n2 ** {
        s = \\nf => n1.s ! NF Sg Bare ++ n2.s ! nf
      } ;
      -- GerundNP    : VP -> NP ;          -- publishing the document (by nature definite)
      GerundNP vp = emptyNP ** {
        s = \\_ => linVP vp
      } ;

      GerundAdv vp = ss (linVP vp) ;

      ByVP vp = cc2 by8means_Prep (GerundAdv vp) ;

      -- PassVPSlash : VPS -> VP ;
      -- be begged to sleep
      PassVPSlash vps = vps ** {
        s = \\vf,pol => vps.s ! Passive ! pol ++ vps.adjCompl;
      };

    -- PassAgentVPSlash : VPSlash -> NP -> VP ;  -- be begged by her to go
      PassAgentVPSlash vps np = {
        s = \\vf,pol => vps.s ! Passive ! pol ++ vps.adjCompl ++ (applyPrep by8agent_Prep np);
      };
      --      PassAgentVPSlash vps np = {
      --   s = \\vf,pol => vps.s ! Passive ! pol ++ (applyPrep by8agent_Prep np) ;
      -- };



      -- MkVPS2    : Temp -> Pol -> VPSlash -> VPS2 ;  -- has loved
      -- ConjVPS2  : Conj -> [VPS2] -> VPS2 ;          -- has loved and now hates
      -- ComplVPS2 : VPS2 -> NP -> VPS ;               -- has loved and now hates that person
      -- ReflVPS2  : VPS2 -> RNP -> VPS ;              -- have loved and now hate myself and my car

      -- MkVPI2    : VPSlash -> VPI2 ;                 -- to love
      -- ConjVPI2  : Conj -> [VPI2] -> VPI2 ;          -- to love and hate
      -- ComplVPI2 : VPI2 -> NP -> VPI ;               -- to love and hate that person

} ;
