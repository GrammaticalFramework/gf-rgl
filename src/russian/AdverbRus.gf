--# -path=.:../abstract:../common:../../prelude

concrete AdverbRus of Adverb = CatRus ** open ResRus, Prelude, Coordination in {
flags coding=utf8 ;

lin
  -- : A -> Adv ;                 -- warmly - тепло
  PositAdvAdj a = {s = a.sn} ;    -- only qual

  -- : A -> AdA ;                 -- extremely - исключительно
  PositAdAAdj a = {s = a.sn} ;    -- only qual

  -- : CAdv -> A -> NP -> Adv ;   -- more warmly than John - более тепло чем Иван
  ComparAdvAdj cadv a np = {
    s = cadv.s ++ a.sn ++ embedInCommas (cadv.p ++ np.s ! Nom)
    } ;
  -- : CAdv -> A -> S -> Adv ;   -- more warmly than he runs - более тепло чем он бежал
  ComparAdvAdjS cadv a s = {
    s = cadv.s ++ a.sn ++ cadv.p ++ s.s ! Ind
    } ;

  -- : Prep -> NP -> Adv ;        -- in the house - в доме
  PrepNP prep np = ss (applyPrep prep np) ;

  -- : AdA -> Adv -> Adv ;        -- very quickly - очень быстро
  AdAdv = cc2 ;

  -- : Subj -> S -> Adv ;         -- when she sleeps - когда она спит
  SubjS subj s = {s=subj.s ++ s.s ! Ind} ;

  -- : CAdv -> AdN ;              -- less (than five) - менее (пяти)
  AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
}
