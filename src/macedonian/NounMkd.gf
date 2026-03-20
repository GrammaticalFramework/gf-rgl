concrete NounMkd of Noun = CatMkd ** open Prelude,ResMkd in {
  lin AdjCN ap cn = {
       s = case ap.isPre of {
             True  => \\s,n => ap.s ! s
                                    ! case n of {
                                        Sg => GSg cn.g;
                                        Pl => GPl
                                      } ++
                               cn.s ! Indef ! n;
             False => \\s,n => cn.s ! s ! n ++
                               ap.s ! Indef
                                    ! case n of {
                                        Sg => GSg cn.g;
                                        Pl => GPl
                                      }
           };
       vocative = \\n => ap.s ! Indef
                              ! case n of {
                                  Sg => GSg cn.g;
                                  Pl => GPl
                                } ++
                         cn.vocative ! n;
       count_form = ap.s ! Indef ! GPl ++ cn.count_form; g = cn.g} ;
  lin AdvCN cn adv = {
        s = \\s,n => cn.s ! s ! n ++ adv.s;
        count_form = cn.count_form ++ adv.s;
        vocative = \\n => cn.vocative ! n ++ adv.s;
        g = cn.g
      } ;
  lin DefArt = {s = []; sp = Def Unspecified} ;
  lin DetCN det cn = {
        s = \\r => det.s ++ cn.s ! det.sp ! det.n;
        vocative = det.s ++ cn.vocative ! det.n;
        g = case det.n of {
              Sg => GSg cn.g;
              Pl => GPl
            } ;
        p = P3;
      } ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin UseN s = s ;
  lin UsePron p = p ** {vocative=p.s ! RSubj} ;
}
