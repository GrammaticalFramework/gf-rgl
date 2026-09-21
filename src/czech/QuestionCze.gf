concrete QuestionCze of Question = CatCze ** open ResCze, Prelude in {
lin
  QuestCl cl = cl ** {q = [] ; yesNo = True} ;
  QuestIAdv adv cl = cl ** {q = adv.s ; yesNo = False} ;
  QuestIComp comp np = {
    q = comp.s ; subj = case np.isDrop of {True => np.clit ! Nom ; False => np.s ! Nom} ;
    clit,compl = [] ; verb = copulaVerbForms ; a = np.a ; yesNo = False
    } ;
  QuestVP ip vp = {
    q = ip.s ! Nom ; subj = [] ; clit = vp.clit ! ip.a ;
    compl = vp.compl ! ip.a ; verb = vp.verb ; a = ip.a ; yesNo = False
    } ;
  CompIAdv adv = adv ;
  CompIP ip = {s = ip.s ! Nom} ;
  AdvIAdv a b = {s = a.s ++ b.s} ;
}
