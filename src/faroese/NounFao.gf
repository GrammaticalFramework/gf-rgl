concrete NounFao of Noun = CatFao ** open ResFao in {
lin
  UseN n = n ;
  UsePron p = p ;
  RelNP np rs =
    np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.g ! persNum np.n np.p} ;
  DetCN det cn = {
    s = \\c => det.s ! cn.g ! c ++ cn.s ! det.sp ! det.n ! c ;
    g = cn.g ;
    n = det.n ;
    p = P3
  } ;
  DefArt = {
    s = \\_,_,_ => [] ;
    sp = Def
  } ;
  IndefArt = {
    s = table {
      Masc => table {
        Sg => table {Nom => "ein" ; Acc => "ein" ; Dat => "einum" ; Gen => "eins"} ;
        Pl => \\_ => []
      } ;
      Fem => table {
        Sg => table {Nom => "ein" ; Acc => "eina" ; Dat => "einari" ; Gen => "einar"} ;
        Pl => \\_ => []
      } ;
      Neuter => table {
        Sg => table {Nom => "eitt" ; Acc => "eitt" ; Dat => "einum" ; Gen => "eins"} ;
        Pl => \\_ => []
      }
    } ;
    sp = Indef
  } ;
  DetQuant quant num = {
    s = \\g,c => quant.s ! g ! num.n ! c ++ num.s ! g ! c ;
    n = num.n ;
    sp = quant.sp
  } ;
  NumSg = {
    s = \\_,_ => [] ;
    n = Sg
  } ;
  NumPl = {
    s = \\_,_ => [] ;
    n = Pl
  } ;
  AdjCN ap cn = {
    s = \\sp,n,c => ap.s ! cn.g ! n ! c ++ cn.s ! sp ! n ! c ;
    g = cn.g
  } ;
}
