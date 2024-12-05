concrete NounSqi of Noun = CatSqi ** open MorphoSqi, ResSqi in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s = \\c => det.s ++ cn.s ! det.spec ! c ! det.n ;
      a = agrgP3 cn.g det.n
      } ;

    DetQuant quant num = {
      s  = quant.s ++ num.s ;
      n  = num.n ;
      spec = quant.spec
      } ;

    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;

    DefArt = {
      s  = [] ;
      spec = Def
      } ;

    IndefArt = {
      s = "një" ;
      spec = Indef
      } ;

    UseN n = n ;
    UseN2 n = n ;

    AdjCN ap cn = {
      s = \\spec,c,n => cn.s ! spec ! c ! n ++ ap.s ! spec ! c ! cn.g ! n ;
      g = cn.g
      } ;

}
