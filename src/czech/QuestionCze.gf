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
  IdetCN det cn = {
    s = \\c => det.s ! nounGender cn (numSizeNumber det.size) ! c ++ numSizeForm cn.s det.size c ;
    a = numeralAgr (nounGender cn (numSizeNumber det.size)) det P3
    } ;
  IdetIP det = {s = \\c => det.s ! Neutr ! c ; a = numeralAgr Neutr det P3} ;
  IdetQuant = quantifyNumeral ;
  PrepIP p ip = {s = p.s ++ ip.s ! p.c} ;
  AdvIP ip adv = ip ** {s = \\c => ip.s ! c ++ adv.s} ;
  AdvIAdv a b = {s = a.s ++ b.s} ;
}
