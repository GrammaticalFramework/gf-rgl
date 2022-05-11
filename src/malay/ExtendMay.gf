--# -path=.:../common:../abstract

concrete ExtendMay of Extend = CatMay
  ** ExtendFunctor - [
    VPS           -- finite VP's with tense and polarity
    , ListVPS
    , VPI
    , ListVPI -- infinitive VP's (TODO: with anteriority and polarity)
    , MkVPS
    , PredVPS



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
      BaseVPS vps vps2 = twoSS vps vps2 ;
      -- ConsVPS : VPS -> ListVPS -> ListVPS ;
      ConsVPS str listvps vps = consSS "," listvps vps ;
      -- ConjVPS    : Conj -> [VPS] -> VPS ;      -- has walked and won't sleep
      ConjVPS conj listvps = conjunctX conj listvps ;
      -- PredVPS    : NP   -> VPS -> S ;          -- she [has walked and won't sleep]
      PredVPS np vps = {
        s = np.s ! Bare ++ vps.s ;
      } ;
      -- SQuestVPS  : NP   -> VPS -> QS ;         -- has she walked
      -- QuestVPS   : IP   -> VPS -> QS ;         -- who has walked
      -- RelVPS     : RP   -> VPS -> RS ;         -- which won't sleep

      -- MkVPI      : VP -> VPI ;                 -- to sleep (TODO: Ant and Pol)
      MkVPI vp = {s = linVP vp} ;


      -- BaseVPI : VPI -> VPI -> ListVPI ;
      BaseVPI vpi vpi2 = twoSS vpi vpi2 ;
      -- ConsVPI : VPI -> ListVPI -> ListVPI ;
      ConsVPI str listvpi vpi = consSS "," listvpi vpi ;


      -- MkVPS2    : Temp -> Pol -> VPSlash -> VPS2 ;  -- has loved
      -- ConjVPS2  : Conj -> [VPS2] -> VPS2 ;          -- has loved and now hates
      -- ComplVPS2 : VPS2 -> NP -> VPS ;               -- has loved and now hates that person
      -- ReflVPS2  : VPS2 -> RNP -> VPS ;              -- have loved and now hate myself and my car

      -- MkVPI2    : VPSlash -> VPI2 ;                 -- to love
      -- ConjVPI2  : Conj -> [VPI2] -> VPI2 ;          -- to love and hate
      -- ComplVPI2 : VPI2 -> NP -> VPI ;               -- to love and hate that person

} ;
