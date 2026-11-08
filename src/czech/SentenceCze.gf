concrete SentenceCze of Sentence = CatCze ** open Prelude, ResCze in {
lin
    PredVP np vp = {
      -- A dropped subject still contributes an empty constituent, so PGF
      -- retains the pronoun and constrains its person through agreement.
      subj = case np.isDrop of {True => np.clit ! Nom ; False => np.s ! Nom} ;
      clit = vp.clit ! np.a ; compl = vp.compl ! np.a ; verb = vp.verb ;
      a = np.a ; isDrop = np.isDrop ; clitPresent = vp.clitPresent ;
      finite = table {
        Pres => \\p => verbAgr vp.verb np.a p ;
        Past => \\p => pastPartAgr vp.verb np.a p ;
        Fut => \\p => tenseVerb Fut vp.verb np.a p ;
        Cond => \\p => pastPartAgr vp.verb np.a p
        } ;
      auxiliary = table {
        Pres => [] ; Past => pastAux np.a ; Fut => [] ; Cond => conditionalAux np.a
        }
      } ;

    SlashVP np vp = {
      subj = case np.isDrop of {True => np.clit ! Nom ; False => np.s ! Nom} ;
      clit = vp.clit ! np.a ++ vp.clitAfter ! np.a ;
      compl = vp.compl ! np.a ; ind = vp.ind ! np.a ; verb = vp.verb ; c = vp.c ;
      a = np.a ; isDrop = np.isDrop ; clitPresent = vp.clitPresent ;
      finite = table {
        Pres => \\p => verbAgr vp.verb np.a p ;
        Past => \\p => pastPartAgr vp.verb np.a p ;
        Fut => \\p => tenseVerb Fut vp.verb np.a p ;
        Cond => \\p => pastPartAgr vp.verb np.a p
        } ;
      auxiliary = table {Pres => [] ; Past => pastAux np.a ; Fut => [] ; Cond => conditionalAux np.a}
      } ;

    UseSlash temp pol cl = {
      s = temp.s ++ cl.subj ++ cl.auxiliary ! temp.t ++ cl.clit ++
        pol.s ++ cl.finite ! temp.t ! pol.p ++ cl.compl ++ cl.ind ; c = cl.c
      } ;

    AdvSlash cl adv = cl ** {compl = cl.compl ++ adv.s} ;
    SlashPrep cl prep = cl ** {c = prep ; ind = []} ;

    SlashVS np vs ssl =
      let base = PredVP np {
        verb = vs ; clitPresent = False ; clit = \\_ => vs.refl ;
        compl = \\_ => SOFT_BIND ++ "," ++ ssl.s
        }
      in base ** {c = ssl.c ; ind = []} ;

    UseCl temp pol cl = let v = pol.s ++ cl.finite ! temp.t ! pol.p ;
                            cs = cl.auxiliary ! temp.t ++ cl.clit in
      case cl.isDrop of {
        True => sentence True (temp.s ++ cl.subj ++ v) cs cl.compl ;
        False => sentence True (temp.s ++ cl.subj) cs (v ++ cl.compl)
        } ;

    UseQCl temp pol cl = let v = pol.s ++ cl.finite ! temp.t ! pol.p ;
                             cs = cl.auxiliary ! temp.t ++ cl.clit in {
      s = temp.s ++ case cl.yesNo of {
        True => v ++ cs ++ cl.subj ++ cl.compl ;
        False => cl.q ++ cs ++ v ++ cl.subj ++ cl.compl
        } ;
      ind = temp.s ++ case cl.yesNo of {
        True => "jestli" ++ cs ++ cl.subj ++ v ++ cl.compl ;
        False => cl.q ++ cs ++ v ++ cl.subj ++ cl.compl
        }
      } ;

    UseRCl temp pol rcl = {
      s = \\a => temp.s ++ rcl.subj ! a ++ tenseClitic temp.t a ++ rcl.clit ! a ++
        pol.s ++ tenseVerb temp.t rcl.verb a pol.p ++ rcl.compl ! a
      } ;
    ImpVP vp = {s = \\pos,a =>
      imperativeAgr vp.verb a pos ++ vp.clit ! a ++ vp.compl ! a
      } ;
    AdvImp adv imp = {s = \\pos,a => adv.s ++ imp.s ! pos ! a} ;
    PredSCVP sc vp = PredVP
      (npForms (\\_ => sc.s) (\\_ => sc.s) ** {
        clit = \\_ => sc.s ; a = Ag Neutr Sg P3 ;
        hasClit = False ; isDrop = False ; isPron = False}) vp ;
    EmbedS s = {s = (frontSentence "že" s).s} ;
    EmbedQS qs = {s = qs.ind} ;
    EmbedVP vp = let agr = Ag Neutr Sg P3 in
      {s = vp.verb.inf ++ vp.clit ! agr ++ vp.compl ! agr} ;
    AdvS a s = frontSentence a.s s ;
    ExtAdvS a s = prefixSentence (a.s ++ SOFT_BIND ++ ",") s ;
    SSubjS a subj b = appendSentence a (SOFT_BIND ++ "," ++ (frontSentence subj.s b).s) ;
}
