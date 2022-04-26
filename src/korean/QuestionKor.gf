concrete QuestionKor of Question = CatKor ** open
  Prelude, ResKor, ParadigmsKor, (VS=VerbKor), (NS=NounKor), (SS=StructuralKor) in {

-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.

{-
  lin
  -- : Cl -> QCl ;
  QuestCl =

  -- : IP -> VP -> QCl ;
  QuestVP ip vp =

  -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls =

  -- : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cls =


  -- : IComp -> NP -> QCl ;   -- where is John?
  QuestIComp icomp np =


-- Interrogative pronouns can be formed with interrogative
-- determiners, with or without a noun.

  -- : IDet -> CN -> IP ;       -- which five songs
  IdetCN idet cn = {contractSTM = False} ** NS.DetCN idet cn ;

  -- : IDet       -> IP ;       -- which five
  IdetIP idet = {contractSTM = False} ** NS.DetNP idet ;

-- They can be modified with adverbs.
  -- : IP -> Adv -> IP ;        -- who in Paris
  --AdvIP = NS.AdvNP ;

-- Interrogative quantifiers have number forms and can take number modifiers.

  -- : IQuant -> Num -> IDet ;  -- which (five)
  IdetQuant = NS.DetQuant ;

-- Interrogative adverbs can be formed prepositionally.
  -- : Prep -> IP -> IAdv ;     -- with whom
  PrepIP prep ip = SS.prepIP prep (ip.s ! Abs) False ;

-- They can be modified with other adverbs.

  -- : IAdv -> Adv -> IAdv ;    -- where in Paris
  --  AdvIAdv iadv adv =

-- Interrogative complements to copulas can be both adverbs and
-- pronouns.

  -- : IAdv -> IComp ;
  CompIAdv iadv = iadv ;            -- where (is it)

  -- : IP -> IComp ;
  CompIP ip = {s = ip.s ! Abs} ;    -- who (is it)


-- More $IP$, $IDet$, and $IAdv$ are defined in $Structural$.

-- Wh questions with two or more question words require a new, special category.

  cat
    QVP ;          -- buy what where
  fun
    ComplSlashIP  : VPSlash -> IP -> QVP ;   -- buys what
    AdvQVP        : VP  ->   IAdv -> QVP ;   -- lives where
    AddAdvQVP     : QVP ->   IAdv -> QVP ;   -- buys what where

    QuestQVP      : IP -> QVP -> QCl ;       -- who buys what where
-}


}
