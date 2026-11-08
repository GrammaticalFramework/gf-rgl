concrete IdiomDan of Idiom = CatDan ** 
  open MorphoDan, ParadigmsDan, IrregDan, Prelude in {

  flags optimize=all_subs ;
    coding=utf8 ;

  lin
    ImpersCl vp = mkClause "det" (agrP3 MorphoDan.neutrum Sg) vp ;
    GenericCl vp = mkClause "man" (agrP3 MorphoDan.utrum Sg) vp ;

    CleftNP np rs = mkClause "det" (agrP3 MorphoDan.neutrum Sg) 
        (insertObj (\\_ => np.s ! rs.c ++ rs.s ! np.a ! RNom) (predV verbBe)) ;

    CleftAdv ad s = mkClause "det" (agrP3 MorphoDan.neutrum Sg) 
      (insertObj (\\_ => ad.s ++ s.s ! Sub) (predV verbBe)) ;

    ExistNP np = 
      mkClause "det" (agrP3 MorphoDan.neutrum Sg) (insertObj 
        (\\_ => np.s ! accusative) (predV (depV finde_V))) ;

    ExistIP ip = {
      s = \\t,a,p => 
            let 
              cls = 
               (mkClause "det" (agrP3 MorphoDan.neutrum Sg) (predV (depV finde_V))).s ! t ! a ! p ;
              who = ip.s ! accusative
            in table {
              QDir   => who ++ cls ! Inv ;
              QIndir => who ++ cls ! Sub
              }
      } ;

    ExistNPAdv np adv =
      mkClause "det" (agrP3 MorphoDan.neutrum Sg) (insertObj
        (\\_ => np.s ! accusative ++ adv.s) (predV (depV finde_V))) ;

    ExistIPAdv ip adv = {
      s = \\t,a,p =>
        let cls = (mkClause "det" (agrP3 MorphoDan.neutrum Sg)
                    (insertAdv adv.s (predV (depV finde_V)))).s ! t ! a ! p ;
            who = ip.s ! accusative
        in table {
          QDir => who ++ cls ! Inv ;
          QIndir => who ++ cls ! Sub
          }
      } ;

    ProgrVP vp = 
      insertObj (\\a => ["ved å"] ++ infVP vp a) (predV verbBe) ;

    ImpPl1 vp = {s = ["lad os"] ++ infVP vp {g = Utr ; n = Pl ; p = P1}} ;

    ImpP3 np vp = {s = "lad" ++ np.s ! accusative ++ infVP vp np.a} ;

    SelfAdvVP vp = insertObj (\\a => selv a.g a.n) vp ;
    SelfAdVVP vp = insertAdVAgr (\\a => selv a.g a.n) vp ;
    SelfNP np = {
      s = \\c => np.s ! c ++ selv np.a.g np.a.n ;
      a = np.a ;
      isPron = False
      } ;

  oper
    selv : Gender -> Number -> Str = \g,n -> case <g,n> of {
      <Utr,Sg> => "selv" ;
      <Neutr,Sg> => "selv" ;
      _ => "selv"
      } ;

}
