--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete AdverbLit of Adverb = CatLit ** open ResLit, Prelude in {
  flags  coding=utf8 ;

  lin
   -- A -> Adv
   -- e.g. "warmly"
   PositAdvAdj a = {s = a.advpos ; advType = AdjT} ;
   
--    ComparAdvAdj  : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdj c a n = {
      s = c.s ++ a.advpos ++ c.p ++ n.nom ;
      advType = OtherT
    } ;

--     ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    ComparAdvAdjS c a s = {
      s = c.s ++ a.advpos ++ c.p ++ s.s ;
      advType = OtherT
    } ;
    
--     AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    AdnCAdv cadv = { s=cadv.sn ++ cadv.pn };
    
    
--    PrepNP prep noun = ss (prep.prep ++ noun.dep ! prep.cas);
    PrepNP prep np =
      case np.nomType of {
        Reg => case prep.cas of {
          GenC => {s = prep.s ++ np.dep ! prep.cas ; advType = GenT} ;
          _ => {s = prep.s ++ np.dep ! prep.cas ; advType = OtherT}  
          } ;
        _ => {s = prep.s ++ np.dep ! prep.cas ; advType = PronT }
       } ;

--    AdAdv = cc2 ;
    -- AdA -> Adv -> Adv
    -- e.g. "very quickly"
    AdAdv ada adv = {s = ada.s ++ adv.s ; advType = adv.advType ; } ; -- but very + PrepAdv will be bad


--    SubjS = cc2 ;
    -- Subj -> S -> Adv
    -- e.g. "when she sleeps"
    SubjS subj s = {s = subj.s ++ s.s ; advType = OtherT} ;
}
