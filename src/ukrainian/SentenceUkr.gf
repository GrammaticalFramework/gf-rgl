concrete SentenceUkr of Sentence = CatUkr ** open ResUkr, (R = ParamX) in {

lin
  PredVP np vp = {
    s = \\t,pol => np.s ! Nom ++ vp.s ! t ! pol ! np.g ! np.n ! np.p
  } ;
  PredSCVP sc vp = {
    s = \\t,pol => sc.s ++ vp.s ! t ! pol ! Masc ! Sg ! P3
  } ;

  SlashVP np slash = {
    s = \\t,pol => np.s ! Nom ++ slash.s ! t ! pol ! np.g ! np.n ! np.p ++ slash.post ;
    c = slash.c
  } ;
  AdvSlash cls adv = cls ** {
    s = \\t,pol => cls.s ! t ! pol ++ adv.s
  } ;
  SlashPrep cl prep = {
    s = cl.s ;
    c = prep
  } ;
  SlashVS np vs sslash = {
    s = \\t,pol => np.s ! Nom ++ finiteVerb vs t pol np.g np.n np.p ++ sslash.s ;
    c = sslash.c
  } ;

  ImpVP vp = {s = vp.imp} ;
  AdvImp adv imp = {
    s = \\pol,n => adv.s ++ imp.s ! pol ! n
  } ;

  EmbedS s = {s = "що" ++ s.s} ;
  EmbedQS qs = {s = qs.s} ;
  EmbedVP vp = {s = vp.inf} ;

  UseCl temp pol cl = {
    s = temp.s ++ pol.s ++ cl.s ! temp.t ! pol.p
  } ;
  UseQCl temp pol qcl = {
    s = temp.s ++ pol.s ++ qcl.s ! temp.t ! pol.p
  } ;
  UseRCl temp pol rcl = {
    s = \\g,n => temp.s ++ pol.s ++ rcl.s ! g ! n
  } ;
  UseSlash temp pol cls = {
    s = temp.s ++ pol.s ++ cls.s ! temp.t ! pol.p ;
    c = cls.c
  } ;

  AdvS adv s = {s = adv.s ++ s.s} ;
  ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;
  SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;
  RelS s rs = {s = s.s ++ "," ++ rs.s ! Masc ! Sg} ;
}
