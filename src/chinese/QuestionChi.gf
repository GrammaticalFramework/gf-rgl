concrete QuestionChi of Question = CatChi **
  open ResChi, Prelude in {

  flags optimize=all_subs ;
  flags coding=utf8 ;

  lin

    QuestCl cl = {
      s = \\b,p,a => cl.s ! p ! a ++ question_s ; -- redup question as variant in ExtraChi
     } ;

    QuestVP ip vp = {
      s = \\_,p,a => ip.s ++ vp.prePart ++ useVerb vp.verb ! p ! a ++ vp.compl
      } ;

    QuestSlash ip cls = {s = \\_ => (mkClauseCompl cls.np (insertObj (mkNP (appPrep cls.c2 ip.s)) cls.vp) []).s} ;

    QuestIAdv iadv cl = {s = \\_ => (mkClauseCompl cl.np (insertAdv iadv cl.vp) []).s} ;

    QuestIComp icomp np = {s = \\_,p,a => linNP np ++ icomp.s} ; ---- order

    PrepIP p ip = ss (appPrep p ip.s) ;

    AdvIP ip adv = ss (adv.s ++ possessive_s ++ ip.s) ; ---- adding de

    IdetCN det cn = case det.detType of {
            DTFull Sg => {s = det.s ++ cn.c  ++ cn.s} ;  -- which house
            DTFull Pl => {s = det.s ++ xie_s ++ cn.s} ;  -- which houses
            DTNum     => {s = det.s ++ cn.c  ++ cn.s} ;  -- (which) five houses
            DTPoss    => {s = det.s          ++ cn.s}    -- whose (five) houses
      } ;


    IdetIP idet = idet ;

    IdetQuant iquant num = {
      s = iquant.s ++ num.s ;
      detType = case num.numType of {
        NTFull   => DTNum ;                     -- which five
        NTVoid n => DTFull n   ---- TODO: whose
        }
      } ;


    AdvIAdv i a = ss (a.s ++ i.s) ;

    CompIAdv a = ss (zai_s ++ a.s) ;

    CompIP ip = ss (copula_s ++ ip.s) ;

}

