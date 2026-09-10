concrete VerbFao of Verb = CatFao ** open Prelude,ResFao in {
  lin UseV v = {Converb = v.Converb ++ v.particle;
                Indicative = \\t,pol,g,p => v.Indicative ! t ! p ++ v.particle ++
                                            negStr pol ;
                Nonfinite = v.Nonfinite ++ v.particle;
                Participle = \\t => v.Participle ! t ++ v.particle} ;
  lin ComplVV vv vp = {Converb = vv.Converb ++ vv.particle ++ vp.Nonfinite;
                       Indicative = \\t,pol,g,p =>
                         vv.Indicative ! t ! p ++ vv.particle ++ negStr pol ++ vp.Nonfinite;
                       Nonfinite = vv.Nonfinite ++ vv.particle ++ vp.Nonfinite;
                       Participle = \\t => vv.Participle ! t ++ vv.particle ++ vp.Nonfinite} ;
  lin ComplVS vs s = {Converb = vs.Converb ++ vs.particle ++ "at" ++ s.s;
                      Indicative = \\t,pol,g,p =>
                        vs.Indicative ! t ! p ++ vs.particle ++ negStr pol ++ "at" ++ s.s;
                      Nonfinite = vs.Nonfinite ++ vs.particle ++ "at" ++ s.s;
                      Participle = \\t => vs.Participle ! t ++ vs.particle ++ "at" ++ s.s} ;
  lin ComplVQ vq qs = {Converb = vq.Converb ++ vq.particle ++ qs.s;
                       Indicative = \\t,pol,g,p =>
                         vq.Indicative ! t ! p ++ vq.particle ++ negStr pol ++ qs.s;
                       Nonfinite = vq.Nonfinite ++ vq.particle ++ qs.s;
                       Participle = \\t => vq.Participle ! t ++ vq.particle ++ qs.s} ;
  lin ComplVA va ap = {Converb = va.Converb ++ va.particle ++ ap.s ! Neuter ! Sg ! Nom;
                       Indicative = \\t,pol,g,p =>
                         va.Indicative ! t ! p ++ va.particle ++ negStr pol ++ ap.s ! g ! persNumNumber p ! Nom;
                       Nonfinite = va.Nonfinite ++ va.particle ++ ap.s ! Neuter ! Sg ! Nom;
                       Participle = \\t => va.Participle ! t ++ va.particle ++ ap.s ! Neuter ! Sg ! Nom} ;
  lin AdvVP vp adv = {Converb = vp.Converb ++ adv.s;
                      Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p  ++ adv.s;
                      Nonfinite = vp.Nonfinite ++ adv.s;
                      Participle = \\t => vp.Participle ! t ++ adv.s} ;
  lin ExtAdvVP vp adv = {Converb = vp.Converb ++ "," ++ adv.s;
                         Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p ++ "," ++ adv.s;
                         Nonfinite = vp.Nonfinite ++ "," ++ adv.s;
                         Participle = \\t => vp.Participle ! t ++ "," ++ adv.s} ;
  lin AdVVP adv vp = {Converb = adv.s ++ vp.Converb;
                      Indicative = \\t,pol,g,p => adv.s ++ vp.Indicative ! t ! pol ! g ! p;
                      Nonfinite = adv.s ++ vp.Nonfinite;
                      Participle = \\t => adv.s ++ vp.Participle ! t} ;
  lin ComplSlash vps np = {Converb = vps.Converb ++ vps.particle
                                      ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc;
                           Indicative = \\t,pol,g,p => 
                                            vps.Indicative ! t ! p ++ vps.particle ++
                                            negStr pol ++
                                            vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc ;
                           Nonfinite = vps.Nonfinite ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc;
                           Participle = \\t => vps.Participle ! t ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc} ;
  lin UseComp comp = {
        Converb = copula ! Pres ! PPl ++ comp.s ! Masc ! Sg ;
        Indicative = \\t,pol,g,p => copula ! t ! p ++
                                    negStr pol ++
                                    comp.s ! g ! persNumNumber p ;
        Nonfinite = "vera" ++ comp.s ! Masc ! Sg ;
        Participle = \\t => "verið" ++ comp.s ! Masc ! Sg
      } ;
  lin CompAP ap = {s = \\g,n => ap.s ! g ! n ! Nom} ;
  lin CompNP np = {s = \\_,_ => np.s ! Nom} ;
  lin CompAdv adv = {s = \\_,_ => adv.s} ;
  lin CompCN cn = {s = \\_,n => cn.s ! Indef ! n ! Nom} ;
  lin UseCopula = {
        Converb = "verið" ;
        Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ;
        Nonfinite = "vera" ;
        Participle = \\_ => "verið"
      } ;

  lin SlashV2a v = v ** {sc = []} ;
  lin Slash2V3 v np = v ** {
        Converb = v.Converb ++ v.particle ++ v.c2.s ++ np.s ! v.c2.c ;
        Indicative = \\t,p => v.Indicative ! t ! p ++ v.particle ++ v.c2.s ++ np.s ! v.c2.c ;
        Nonfinite = v.Nonfinite ++ v.particle ++ v.c2.s ++ np.s ! v.c2.c ;
        Participle = \\t => v.Participle ! t ++ v.particle ++ v.c2.s ++ np.s ! v.c2.c ;
        particle = [] ;
        c2 = v.c3 ;
        sc = []
      } ;
  lin Slash3V3 v np = v ** {
        c2 = v.c2 ;
        sc = v.c3.s ++ np.s ! v.c3.c
      } ;
  lin SlashV2A v ap = v ** {
        c2 = v.c2 ;
        sc = ap.s ! Neuter ! Sg ! Nom
      } ;
  lin SlashV2S v s = v ** {
        c2 = v.c2 ;
        sc = "at" ++ s.s
      } ;
  lin SlashV2Q v qs = v ** {
        c2 = v.c2 ;
        sc = qs.s
      } ;
  lin SlashV2V v vp = v ** {
        c2 = v.c2 ;
        sc = vp.Nonfinite
      } ;
  lin SlashVV vv vps = vps ** {
        Converb = vv.Converb ++ vv.particle ++ vps.Nonfinite ;
        Indicative = \\t,p => vv.Indicative ! t ! p ++ vv.particle ++ vps.Nonfinite ;
        Nonfinite = vv.Nonfinite ++ vv.particle ++ vps.Nonfinite ;
        Participle = \\t => vv.Participle ! t ++ vv.particle ++ vps.Nonfinite ;
        particle = []
      } ;
  lin SlashV2VNP v np vps = v ** {
        c2 = v.c2 ;
        sc = v.c3.s ++ np.s ! v.c3.c ++ vps.Nonfinite
      } ;
  lin ReflVP vps =
      let np = mkNP "seg" Masc Sg P3 in {
        Converb = vps.Converb ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc ;
        Indicative = \\t,pol,g,p =>
          vps.Indicative ! t ! p ++ vps.particle ++ negStr pol ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc ;
        Nonfinite = vps.Nonfinite ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc ;
        Participle = \\t => vps.Participle ! t ++ vps.particle ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc
      } ;
  lin PassV2 v = {
        Converb = "verið" ++ v.Participle ! Past ;
        Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ++ v.Participle ! Past ;
        Nonfinite = "vera" ++ v.Participle ! Past ;
        Participle = \\_ => "verið" ++ v.Participle ! Past
      } ;
  lin AdvVPSlash vps adv = vps ** {sc = vps.sc ++ adv.s} ;
  lin AdVVPSlash adv vps = vps ** {
        Converb = adv.s ++ vps.Converb ;
        Indicative = \\t,p => adv.s ++ vps.Indicative ! t ! p ;
        Nonfinite = adv.s ++ vps.Nonfinite ;
        Participle = \\t => adv.s ++ vps.Participle ! t
      } ;
  lin VPSlashPrep vp prep = {
        Converb = vp.Converb ;
        imperative = \\_ => vp.Nonfinite ;
        Indicative = \\t,p => vp.Indicative ! t ! Pos ! Masc ! p ;
        Nonfinite = vp.Nonfinite ;
        Participle = vp.Participle ;
        particle = [] ;
        c2 = prep ;
        sc = []
      } ;
}
