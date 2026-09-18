concrete IdiomTel of Idiom = CatTel ** open Prelude, ResTel in {
  lin
    ProgrVP vp = vp ** {
      s = \\pol,form => case form of {
        VPTense VPGenPres agr => vp.s ! pol ! VPTense VPContPres agr ;
        VPTense VPPerf agr => vp.s ! pol ! VPTense VPContPast agr ;
        _ => vp.s ! pol ! form
        }
      } ;

    ExistNP np = {
      s = \\tense,pol => np.s ! NPC Dir ++ existVerb tense pol np.a
      } ;

    ExistNPAdv np adv = {
      s = \\tense,pol => np.s ! NPC Dir ++ adv.s ++ existVerb tense pol np.a
      } ;

    ImpPl1 vp = {
      s = let f = vp.s ! Pos ! VPHort in
        vp.obj.s ++ vp.comp ! (Ag Masc Pl P1) ++ f.neg ++ f.inf ++ f.fin
      } ;

    ImpersCl vp = mkClause {
      s = \\_ => "అది" ;
      a = agrP3 Neutr Sg
      } vp ;

  oper
    existVerb : VPHTense -> Polarity -> Agr -> Str = \tense,pol,agr ->
      case pol of {
        Neg => "లేదు" ;
        Pos => case <tense,agr> of {
          <VPPerf,     Ag Masc  Sg P3> => "ఉండేవాడు" ;
          <VPPerf,     Ag _     Sg P3> => "ఉండేది" ;
          <VPPerf,     Ag Neutr Pl P3> => "ఉండేవి" ;
          <VPPerf,     Ag _     Pl _ > => "ఉండేవారు" ;
          <VPPerfPast, Ag Masc  Sg P3> => "ఉండేవాడు" ;
          <VPPerfPast, Ag _     Sg P3> => "ఉండేది" ;
          <VPPerfPast, Ag Neutr Pl P3> => "ఉండేవి" ;
          <VPPerfPast, Ag _     Pl _ > => "ఉండేవారు" ;
          <VPFut,      Ag Masc  Sg P3> => "ఉంటాడు" ;
          <VPFut,      Ag _     Sg P3> => "ఉంటుంది" ;
          <VPFut,      Ag Neutr Pl P3> => "ఉంటాయి" ;
          <VPFut,      Ag _     Pl _ > => "ఉంటారు" ;
          <_,          Ag Masc  Sg P3> => "ఉన్నాడు" ;
          <_,          Ag _     Sg P3> => "ఉంది" ;
          <_,          Ag Neutr Pl P3> => "ఉన్నాయి" ;
          <_,          Ag _     Pl _ > => "ఉన్నారు" ;
          _ => "ఉంది"
          }
        } ;
}
