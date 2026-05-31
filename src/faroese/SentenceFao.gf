concrete SentenceFao of Sentence = CatFao ** open Prelude, ResFao, (P = ParamX) in {
  lin PredVP np vp = {Converb = np.s ! Nom ++ vp.Converb;
                      Indicative = \\t,pol =>
                                         let p = persNum np.n np.p
                                         in np.s ! Nom ++ vp.Indicative ! t ! pol ! np.g ! p;
                      Nonfinite = np.s ! Nom ++ vp.Nonfinite;
                      Participle = \\t => np.s ! Nom ++ vp.Participle ! t} ;
  PredSCVP sc vp = {Converb = sc.s ++ vp.Converb;
                    Indicative = \\t,pol => sc.s ++ vp.Indicative ! t ! pol ! Neuter ! PSg P3;
                    Nonfinite = sc.s ++ vp.Nonfinite;
                    Participle = \\t => sc.s ++ vp.Participle ! t} ;
  SlashVP np vps = {
    s = \\t,pol =>
      let p = persNum np.n np.p in
      np.s ! Nom ++ vps.Indicative ! t ! p ++ vps.particle ++ negStr pol ++ vps.sc ;
    c2 = vps.c2
  } ;
  AdvSlash cls adv = cls ** {
    s = \\t,pol => cls.s ! t ! pol ++ adv.s
  } ;
  SlashPrep cl prep = {
    s = cl.Indicative ;
    c2 = prep
  } ;
  SlashVS np vs sslash = {
    s = \\t,pol =>
      let p = persNum np.n np.p in
      np.s ! Nom ++ vs.Indicative ! t ! p ++ vs.particle ++ negStr pol ++ sslash.s ;
    c2 = sslash.c2
  } ;
  ImpVP vp = {
    s = \\pol,n => negStr pol ++ vp.Nonfinite
  } ;
  AdvImp adv imp = {
    s = \\pol,n => adv.s ++ imp.s ! pol ! n
  } ;
  UseCl temp pol cl =
    let
      tense = case temp.t of {
        P.Pres => Pres ;
        P.Past => Past ;
        P.Fut => Pres ;
        P.Cond => Past
      } ;
    in {
      s = temp.s ++ pol.s ++ cl.Indicative ! tense ! pol.p
    } ;
  UseQCl temp pol qcl =
    let
      tense = case temp.t of {
        P.Pres => Pres ;
        P.Past => Past ;
        P.Fut => Pres ;
        P.Cond => Past
      } ;
    in {
      s = temp.s ++ pol.s ++ qcl.s ! tense ! pol.p
    } ;
  UseRCl temp pol rcl =
    let
      tense = case temp.t of {
        P.Pres => Pres ;
        P.Past => Past ;
        P.Fut => Pres ;
        P.Cond => Past
      }
    in {
      s = \\g,pn =>
        temp.s ++ pol.s ++ rcl.s ! tense ! pol.p ! g ! pn
    } ;
  UseSlash temp pol cls =
    let
      tense = case temp.t of {
        P.Pres => Pres ;
        P.Past => Past ;
        P.Fut => Pres ;
        P.Cond => Past
      } ;
    in {
      s = temp.s ++ pol.s ++ cls.s ! tense ! pol.p ;
      c2 = cls.c2
    } ;
  EmbedS s = {s = "at" ++ s.s} ;
  EmbedQS qs = {s = qs.s} ;
  EmbedVP vp = {s = vp.Nonfinite} ;
  AdvS adv s = {s = adv.s ++ s.s} ;
  ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;
  SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;
  RelS s rs = {s = s.s ++ "," ++ rs.s ! Neuter ! PSg P3} ;
}
