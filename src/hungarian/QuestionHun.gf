concrete QuestionHun of Question = CatHun ** open
  Prelude, ResHun, ParadigmsHun, (NH=NounHun), (VH=VerbHun) in {

-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.

  lincat
    QVP = VerbPhrase ;

  lin
  -- : Cl -> QCl ;
  QuestCl cl = cl ;

  -- : IP -> VP -> QCl ;
  QuestVP ip vp = predVP ip vp ;

  -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls = {
    s = \\t,a,p => ip.s ! NoPoss ! cls.c2 ++ cls.s ! t ! a ! p
    } ;

  -- : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cls = {
    s = \\t,a,p => iadv.s ++ cls.s ! t ! a ! p
    } ;


  -- : IComp -> NP -> QCl ;   -- where is John?
  QuestIComp icomp np = {
    s = \\_,_,p =>
      icomp.s ++ if_then_Pol p [] "nem" ++ linNP np
    } ;


-- Interrogative pronouns can be formed with interrogative
-- determiners, with or without a noun.

  -- : IDet -> CN -> IP ;       -- which five songs
  IdetCN idet cn = NH.DetCN idet cn ;

  -- : IDet       -> IP ;       -- which five
  IdetIP idet = NH.DetNP idet ;

-- They can be modified with adverbs.
  -- : IP -> Adv -> IP ;        -- who in Paris
  AdvIP = NH.AdvNP ;

-- Interrogative quantifiers have number forms and can take number modifiers.

  -- : IQuant -> Num -> IDet ;  -- which (five)
  IdetQuant = NH.DetQuant ;

-- Interrogative adverbs can be formed prepositionally.
  -- : Prep -> IP -> IAdv ;     -- with whom
  PrepIP prep ip = {s = applyAdp prep ip} ;

-- They can be modified with other adverbs.

  -- : IAdv -> Adv -> IAdv ;    -- where in Paris
  AdvIAdv iadv adv = {
    s = iadv.s ++ adv.s
    } ;

-- Interrogative complements to copulas can be both adverbs and
-- pronouns.

  -- : IAdv -> IComp ;
  CompIAdv iadv = iadv ;            -- where (is it)

  -- : IP -> IComp ;
  CompIP ip = {s = ip.s ! NoPoss ! Nom} ;    -- who (is it)


-- More $IP$, $IDet$, and $IAdv$ are defined in $Structural$.

-- Wh questions with two or more question words require a new, special category.

  ComplSlashIP vps ip = VH.insertObj vps ip ;
  AdvQVP vp iadv = vp ** {adv = vp.adv ++ iadv.s} ;
  AddAdvQVP qvp iadv = qvp ** {adv = qvp.adv ++ iadv.s} ;
  QuestQVP ip qvp = predVP ip qvp ;

}
