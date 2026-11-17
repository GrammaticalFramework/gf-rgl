concrete IdiomHye of Idiom = CatHye ** open Prelude, ResHye in {
  oper
    bareCl : Str -> Cl = \s -> lin Cl {
      s=s; negative=table {_ => "չ" ++ s}; anterior=\\_=>table {_=>s}; conditional=\\_,_ => s;
      converb={imperfective=s;futCon1=s;futCon2=s;negative=s;perfective=s;simultaneous=s};
      passive=s;past=\\_,_ => s;participle=\\_ => s;subjunctive=\\_,_ => s
    } ;
    impersCl : VP -> Cl = \vp -> lin Cl {
      s=vp.converb.imperfective++presentAux P3 Sg;
      negative=table {
        Pres=>negativePresentAux P3 Sg++vp.converb.imperfective;
        Past=>"չ"++BIND++vp.past!P3!Sg;
        Fut=>negativePresentAux P3 Sg++vp.converb.futCon1;
        Cond=>negativePastAux P3 Sg++vp.subjunctive!Non_Past!P3!Sg
      };
      anterior=table {
        Pres=>table {Pos=>vp.converb.perfective++presentAux P3 Sg;
                    Neg=>negativePresentAux P3 Sg++vp.converb.perfective};
        Past=>table {Pos=>vp.converb.perfective++pastAux P3 Sg;
                    Neg=>negativePastAux P3 Sg++vp.converb.perfective};
        Fut=>table {Pos=>vp.converb.perfective++copulaVerb.conditional!Non_Past!P3!Sg;
                   Neg=>negativePresentAux P3 Sg++vp.converb.perfective++copulaVerb.converb.futCon1};
        Cond=>table {Pos=>vp.converb.perfective++copulaVerb.conditional!Perfect!P3!Sg;
                    Neg=>negativePastAux P3 Sg++vp.converb.perfective}
      };
      conditional=\\a,_=>vp.conditional!a!P3!Sg;
      converb=vp.converb;
      passive=vp.passive;
      past=\\_,_=>vp.past!P3!Sg;
      participle=vp.participle;
      subjunctive=\\a,_=>vp.subjunctive!a!P3!Sg
    } ;
  lin
    ImpersCl vp = impersCl vp ;
    GenericCl vp = bareCl ("մարդ" ++ vp.s) ;
    CleftNP np rs = bareCl (np.s ! Nom ++ rs.s) ;
    CleftAdv adv s = bareCl (adv.s ++ s.s) ;
    ExistNP np = bareCl (np.s ! Nom ++ "կա") ;
    ExistIP ip = {s=\\_,_,_=>ip.s ++ BIND ++ "՞" ++ "կա"} ;
    ExistNPAdv np adv = bareCl (adv.s ++ np.s ! Nom ++ "կա") ;
    ExistIPAdv ip adv = {s=\\_,_,_=>adv.s ++ ip.s ++ BIND ++ "՞" ++ "կա"} ;
    ProgrVP vp = vp ;
    ImpPl1 vp = {s="եկեք" ++ vp.s} ;
    ImpP3 np vp = {s="թող" ++ np.s ! Nom ++ vp.s} ;
    SelfAdvVP vp = vp ; SelfAdVVP vp = vp ; SelfNP np = np ;
}
