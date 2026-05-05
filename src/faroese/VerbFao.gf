concrete VerbFao of Verb = CatFao ** open Prelude,ResFao in {
  lin UseV v = {Converb = v.Converb ++ v.particle;
                Indicative = \\t,pol,g,p => v.Indicative ! t ! p ++ v.particle ++
                                            case pol of {
                                              Pos => [] ;
                                              Neg => "ikki"
                                            } ;
                Nonfinite = v.Nonfinite ++ v.particle;
                Participle = \\t => v.Participle ! t ++ v.particle} ;
  lin AdvVP vp adv = {Converb = vp.Converb ++ adv.s;
                      Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p  ++ adv.s;
                      Nonfinite = vp.Nonfinite ++ adv.s;
                      Participle = \\t => vp.Participle ! t ++ adv.s} ;
  lin ComplSlash vps np = {Converb = vps.Converb ++ vps.particle
                                      ++ vps.c2.s ++ np.s ! vps.c2.c;
                           Indicative = \\t,pol,g,p => 
                                            vps.Indicative ! t ! p ++ vps.particle ++
                                            case pol of {
                                              Pos => [] ;
                                              Neg => "ikki"
                                            } ++
                                            vps.c2.s ++ np.s ! vps.c2.c ;
                           Nonfinite = vps.Nonfinite ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c;
                           Participle = \\t => vps.Participle ! t ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c} ;
  lin UseComp comp = {
        Converb = copula ! Pres ! PPl ++ comp.s ! Masc ! Sg ;
        Indicative = \\t,pol,g,p => copula ! t ! p ++
                                    case pol of {
                                      Pos => [] ;
                                      Neg => "ikki"
                                    } ++
                                    comp.s ! g ! persNumNumber p ;
        Nonfinite = "vera" ++ comp.s ! Masc ! Sg ;
        Participle = \\t => "verið" ++ comp.s ! Masc ! Sg
      } ;
  lin CompAP ap = {s = \\g,n => ap.s ! g ! n ! Nom} ;
  lin CompNP np = {s = \\_,_ => np.s ! Nom} ;
  lin CompAdv adv = {s = \\_,_ => adv.s} ;
  lin CompCN cn = {s = \\_,n => cn.s ! Indef ! n ! Nom} ;

  lin SlashV2a v = v ;
}
