concrete SentenceFao of Sentence = CatFao ** open Prelude, ResFao, (P = ParamX) in {
  lin PredVP np vp = {Converb = np.s ! Nom ++ vp.Converb;
                      Indicative = \\t,pol =>
                                         let p = persNum np.n np.p
                                         in np.s ! Nom ++ vp.Indicative ! t ! pol ! np.g ! p;
                      Interrogative = \\t,pol => let p = persNum np.n np.p in
                        vp.Finite ! t ! p ++ np.s ! Nom ++ vp.Remainder ! pol ! np.g ! p ;
                      Future = \\pol => let p = persNum np.n np.p in
                        np.s ! Nom ++ futureAux ! p ++ negStr pol ++ vp.Nonfinite ;
                      FutureInterrogative = \\pol => let p = persNum np.n np.p in
                        futureAux ! p ++ np.s ! Nom ++ negStr pol ++ vp.Nonfinite ;
                      Conditional = \\pol => let p = persNum np.n np.p in
                        np.s ! Nom ++ conditionalAux ! p ++ negStr pol ++ vp.Nonfinite ;
                      ConditionalInterrogative = \\pol => let p = persNum np.n np.p in
                        conditionalAux ! p ++ np.s ! Nom ++ negStr pol ++ vp.Nonfinite ;
                      Anterior = \\t,pol => let p = persNum np.n np.p in
                        np.s ! Nom ++ perfectAux ! t ! p ++ negStr pol ++ vp.Converb ;
                      AnteriorInterrogative = \\t,pol => let p = persNum np.n np.p in
                        perfectAux ! t ! p ++ np.s ! Nom ++ negStr pol ++ vp.Converb ;
                      Nonfinite = np.s ! Nom ++ vp.Nonfinite;
                      Participle = \\t => np.s ! Nom ++ vp.Participle ! t} ;
  PredSCVP sc vp = {Converb = sc.s ++ vp.Converb;
                    Indicative = \\t,pol => sc.s ++ vp.Indicative ! t ! pol ! Neuter ! PSg P3;
                    Interrogative = \\t,pol => vp.Finite ! t ! PSg P3 ++ sc.s ++
                                              vp.Remainder ! pol ! Neuter ! PSg P3;
                    Future = \\pol => sc.s ++ futureAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
                    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ sc.s ++ negStr pol ++ vp.Nonfinite ;
                    Conditional = \\pol => sc.s ++ conditionalAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ;
                    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ sc.s ++ negStr pol ++ vp.Nonfinite ;
                    Anterior = \\t,pol => sc.s ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ vp.Converb ;
                    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ sc.s ++ negStr pol ++ vp.Converb ;
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
    s = \\pol,n => negStr pol ++ vp.Imperative ! n
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
      s = temp.s ++ pol.s ++ case <temp.a,temp.t> of {
        <P.Simul,P.Pres> => cl.Indicative ! Pres ! pol.p ;
        <P.Simul,P.Past> => cl.Indicative ! Past ! pol.p ;
        <P.Simul,P.Fut> => cl.Future ! pol.p ;
        <P.Simul,P.Cond> => cl.Conditional ! pol.p ;
        <P.Anter,P.Pres> => cl.Anterior ! Pres ! pol.p ;
        <P.Anter,P.Past> => cl.Anterior ! Past ! pol.p ;
        <P.Anter,P.Fut> => cl.Future ! pol.p ;
        <P.Anter,P.Cond> => cl.Anterior ! Past ! pol.p
      }
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
      s = temp.s ++ pol.s ++ case <temp.a,temp.t> of {
        <P.Simul,P.Pres> => qcl.s ! Pres ! pol.p ;
        <P.Simul,P.Past> => qcl.s ! Past ! pol.p ;
        <P.Simul,P.Fut> => qcl.future ! pol.p ;
        <P.Simul,P.Cond> => qcl.conditional ! pol.p ;
        <P.Anter,P.Pres> => qcl.anterior ! Pres ! pol.p ;
        <P.Anter,P.Past> => qcl.anterior ! Past ! pol.p ;
        <P.Anter,P.Fut> => qcl.future ! pol.p ;
        <P.Anter,P.Cond> => qcl.anterior ! Past ! pol.p
      }
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
