concrete RelativeTur of Relative = CatTur ** open ResTur in {

lin
  RelCl cl = {
    s = \\t,a,p,agr => cl.s ! t ! a ! p ++ "olan"
  } ;

  RelVP rp vp = {
    s = \\t,a,p,agr =>
          case a of {
            Simul => rp.s ! agr ++ vp.compl ++
                     case t of {
                       Fut => vp.s ! Perf ! VProspPart p ;
                       _   => vp.s ! Perf ! VImperfPart p
                     } ;
            Anter => vp.s ! Perf ! VFin t a p agr ++ "olan"
          } ;
  } ;

  RelSlash rp cl = {
    s = \\t,a,p,agr => rp.s ! agr ++ cl.s ! t ! a ! p ++ "olan"
  } ;

  FunRP prep np rp = {
    s = \\agr => np.s ! prep.c ++ prep.s ++ rp.s ! agr
  } ;

  IdRP = {s = \\_ => []} ;
  
}
