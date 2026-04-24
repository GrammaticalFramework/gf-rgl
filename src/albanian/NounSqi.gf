concrete NounSqi of Noun = CatSqi ** open MorphoSqi, ResSqi in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s = \\c => det.s ! c ! cn.g ++ cn.s ! det.sp ! c ! det.n ;
      a = agrgP3 cn.g det.n
      } ;

    -- UsePron p = p ;

    DetQuant quant num = {
      s  = \\c,g => quant.s ! c ! g ! num.n ++ num.s ;
      n  = num.n ;
      sp = quant.sp
      } ;

    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;

    DefArt = {
      s  = \\c,g,n => [] ;
      sp = Def
      } ;

    IndefArt = {
      s = \\c,g => table Number ["një"; []] ;
      sp = Indef
      } ;

    UseN n = n ;

    AdjCN ap cn = {
      s = \\spec,c,n => cn.s ! spec ! c ! n ++ ap.s ! spec ! c ! cn.g ! n ;
      g = cn.g
      } ;

}
