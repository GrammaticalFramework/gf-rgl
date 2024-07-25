concrete RelativeTur of Relative = CatTur ** open ResTur in {

lin
  RelCl = variants {} ;

  RelVP rp vp = {
    s = \\t,p,agr => rp.s ! agr ++ vp.compl ++
                     case t of {
                       Fut => vp.s ! Perf ! VProspPart p ;
                       _   => vp.s ! Perf ! VImperfPart p
                     }
  } ;

  RelSlash = variants {} ;

  FunRP = variants {} ;

  IdRP = {s = \\_ => []} ;
  
}
