concrete NounSqi of Noun = CatSqi ** open MorphoSqi, ResSqi in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s = \\c => det.s ! c ! cn.g ++ cn.s ! det.spec ! c ! det.n ;
      a = agrgP3 cn.g det.n
      } ;

    UsePron p = p ;

    DetQuant quant num = {
      s  = \\c,g => quant.s ! c ! g ! num.n ++ num.s ;
      n  = num.n ;
      spec = quant.spec
      } ;

    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;

    DefArt = {
      s  = \\c,g,n => [] ;
      spec = Def
      } ;

    IndefArt = {
      s = \\c,g => table Number ["një"; []] ;
      spec = Indef
      } ;

    UseN n = n ;
    UseN2 n = n ;

    AdjCN ap cn = {
      s = \\spec,c,n => cn.s ! spec ! c ! n ++ ap.s ! spec ! c ! cn.g ! n ;
      g = cn.g
      } ;

}
