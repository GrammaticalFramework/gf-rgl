concrete ExtendAfr of Extend =
  CatAfr ** ExtendFunctor - [PassVPSlash,PassAgentVPSlash]
  with
    (Grammar = GrammarAfr) **

  open
    ParadigmsAfr, ResAfr in {

-- KA: guessed from PassV2 in Afrikaans and the equivalents in Dutch
lin PassVPSlash vps = 
      insertInf (vps.s.s ! VPerf) (predV word_V) ;
    PassAgentVPSlash vps np = 
      insertAdv (appPrep "door" np.s) (insertInf (vps.s.s ! VPerf) (predV word_V)) ;

}
