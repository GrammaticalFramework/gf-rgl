concrete RelativeTur of Relative = CatTur ** open ResTur in {

lin
  RelCl = variants {} ;

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

  RelSlash = variants {} ;

  FunRP = variants {} ;

  IdRP = {s = \\_ => []} ;
  
}
