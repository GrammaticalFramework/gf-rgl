concrete NounHye of Noun = CatHye ** open ResHye in {
  lin AdjCN ap cn = {
        s = \\sp,c,n =>
                case ap.isPre of {
                  True  => ap.s ! Indef ! Nom ! Sg ++ cn.s ! sp ! c ! n;
                  False => cn.s ! sp ! c ! n ++ ap.s ! Indef ! Nom ! Sg
                }
      } ;
  lin AdvCN cn adv = {
        s = \\sp,c,num => adv.s ++ cn.s ! sp ! c ! num
      } ;
  lin DefArt = {s = []; sp = Def} ;
  lin DetCN det cn = {s = \\c => det.s ++ cn.s ! det.sp ! c ! det.n;
                      a = {n=det.n; p=P3}} ;
  lin DetQuant quant num = {s = quant.s ++ num.s; n = num.n; sp=quant.sp} ;
  lin IndefArt = {s = "մի"; sp = Indef} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin MassNP cn = {
        s = \\c => cn.s ! Indef ! c ! Sg;
        a = {n=Sg; p=P3}
      } ;
  lin PossPron pron = {s = pron.empty; sp = Poss pron.a.p} ;
  lin UseN n = {
        s = \\sp,c,num =>
                case <sp,c> of {
                  <Def,Nom> => n.def_nom ! num ;
                  <Def,Dat> => n.def_dat ! num ;
                  <Poss P1,_> => n.poss1 ! c ! num ;
                  <Poss P2,_> => n.poss2 ! c ! num ;
                  _         => n.s ! c ! num
                }
      } ;
  lin UsePron pron = {s = \\c => pron.s; a=pron.a} ;
}
