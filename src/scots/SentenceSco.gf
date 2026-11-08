concrete SentenceSco of Sentence = SentenceEng - [PredVP,PredSCVP,ImpVP,SlashVP,SlashVS,EmbedVP] **
  open Prelude, ResSco in {

lin
  PredVP np vp = mkClause (np.s ! npNom) np.a vp ;

  PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;

  ImpVP vp = {
    s = \\pol,n =>
      let agr = AgP2 (numImp n) ;
          verb = infVP VVAux vp False Simul CPos agr ;
          dinna = case pol of {
            CNeg True => "dinna" ;
            CNeg False => "dae" ++ "no" ;
            _ => []
            }
      in dinna ++ verb
    } ;

  SlashVP np vp =
    mkClause (np.s ! npNom) np.a vp ** {c2 = vp.c2} ;

  SlashVS np vs slash =
    mkClause (np.s ! npNom) np.a
      (insertObj (\\_ => conjThat ++ slash.s) (predV vs)) **
      {c2 = slash.c2} ;

  EmbedVP vp = {s = infVP VVInf vp False Simul CPos (agrP3 Sg)} ;

}
