--# -path=.:../prelude:../abstract:../common

concrete QuestionCgg of Question = CatCgg ** open ResCgg, Prelude in {
--1 Question: Questions and Interrogative Pronouns
-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.

  lin
    --QuestCl     : Cl -> QCl ;            -- does John walk
    QuestCl cl = cl;
    --QuestVP     : IP -> VP -> QCl ;      -- who walks
    --QuestSlash  : IP -> ClSlash -> QCl ; -- whom does John love
    --QuestIAdv   : IAdv -> Cl -> QCl ;    -- why does John walk
    --QuestIComp  : IComp -> NP -> QCl ;   -- where is John

    --IdetCN    : IDet -> CN -> IP ;       -- which five songs
    
    IdetCN idet cn = case idet.requiresSubjPrefix  of {
                            True => {s =  cn.s!idet.n!Complete ++ mkSubjPrefix (mkAgreement cn.gender P3 idet.n) ++ idet.s; other =[]; n = idet.n; isVerbSuffix=False; requiresIPPrefix=True; aux= "ni"; endOfSentence = True};
                            False => { s = cn.s!idet.n!Complete ++ idet.s; isVerbSuffix=False; other =[]; n=idet.n; requiresIPPrefix=True; aux= "ni"; endOfSentence = True}
                        };
    --IdetIP    : IDet       -> IP ;       -- which five
    --Noun Class has been ignored
    IdetIP idet = { s = idet.s ; other = idet.s; isVerbSuffix=False; n=idet.n; requiresIPPrefix=True; aux= "ni"; endOfSentence = True};

    --IdetQuant : IQuant -> Num -> IDet ;  -- which (five)
    --IdetQuant iquant num = { s = iquant.s ! num.n  ; requiresSubjPrefix=True};
{-
--1 Question: Questions and Interrogative Pronouns

abstract Question = Cat ** {

-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.

  fun
    QuestCl     : Cl -> QCl ;            -- does John walk
    QuestVP     : IP -> VP -> QCl ;      -- who walks
    QuestSlash  : IP -> ClSlash -> QCl ; -- whom does John love
    QuestIAdv   : IAdv -> Cl -> QCl ;    -- why does John walk
    QuestIComp  : IComp -> NP -> QCl ;   -- where is John

-- Interrogative pronouns can be formed with interrogative
-- determiners, with or without a noun.

    IdetCN    : IDet -> CN -> IP ;       -- which five songs
    IdetIP    : IDet       -> IP ;       -- which five

-- They can be modified with adverbs.

    AdvIP     : IP -> Adv -> IP ;        -- who in Paris

-- Interrogative quantifiers have number forms and can take number modifiers.

    IdetQuant : IQuant -> Num -> IDet ;  -- which (five)

-- Interrogative adverbs can be formed prepositionally.

    PrepIP    : Prep -> IP -> IAdv ;     -- with whom

-- They can be modified with other adverbs.

    AdvIAdv   : IAdv -> Adv -> IAdv ;    -- where in Paris

-- Interrogative complements to copulas can be both adverbs and
-- pronouns.

    CompIAdv  : IAdv -> IComp ;          -- where (is it)
    CompIP    : IP   -> IComp ;          -- who (is it)

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
