concrete CatMkd of Cat = CommonX ** open ResMkd, Prelude in {

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat V, VA, VV, VS, VQ = Verb ;
lincat V2, V2S, V2Q = Verb ** {c2 : Compl} ;
lincat V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
lincat A = Adj ;
lincat A2 = Adj ** {c2 : Compl} ;
lincat Pron = Pron ;
lincat Prep = Compl ;

linref N,N2,N3 = \n -> n.s ! Indef ! Sg ;
linref V, VA, VV, VS, VQ, V2, V2S, V2Q, V3, V2A, V2V = 
  \v -> v.present ! Imperfective ! Sg ! P3 ++
        case v.isRefl of {
          True  => "се" ;
          False => []
        } ;
linref A, A2 = \a -> a.s ! Indef ! GSg Masc ;

}
