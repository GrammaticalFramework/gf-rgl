concrete QuestionTMP of Question = CatTMP ** open
  Prelude, ResTMP, ParadigmsTMP, (V=VerbTMP), (Noun=NounTMP), (S=StructuralTMP) in {

-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.
-- Interrogative pronouns can be formed with interrogative
-- determiners, with or without a noun.

{-
lin
  -- : IDet -> CN -> IP ;       -- which five songs
  IdetCN idet cn = Noun.DetCN idet cn ** {
    } ;

  -- : IDet       -> IP ;       -- which five
  IdetIP idet = Noun.DetNP idet ** {sp = idet.sp};

  -- : IQuant -> Num -> IDet ;  -- which (five)
  IdetQuant iquant num = iquant ** {
    } ;

  -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls = cls ** {

    } ;

  -- : Cl -> QCl ;
  QuestCl cl = cl ** {
    };


  -- : IP -> VP -> QCl ;
  QuestVP ip cl =  cl ** {
    } ;

  -- : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cls = {
    } ;

  -- : IP -> IComp ;
  CompIP ip = {s = ip.s !  } ;    -- who (is it)

  -- : IComp -> NP -> QCl ;   -- where is John?
  QuestIComp icomp np = {
    } ;


-- Interrogative pronouns can be formed with interrogative
-- determiners, with or without a noun.

  -- : IDet -> CN -> IP ;       -- which five songs
  IdetCN idet cn =

  -- : IDet       -> IP ;       -- which five
  IdetIP idet =

-- They can be modified with adverbs.

  -- : IP -> Adv -> IP ;        -- who in Paris
  AdvIP = Noun.AdvNP ;

-- Interrogative quantifiers have number forms and can take number modifiers.

  -- : IQuant -> Num -> IDet ;  -- which (five)
  IdetQuant = Noun.DetQuant ;

-- Interrogative adverbs can be formed prepositionally.
  -- : Prep -> IP -> IAdv ;     -- with whom
  PrepIP prep ip =

-- They can be modified with other adverbs.

  -- : IAdv -> Adv -> IAdv ;    -- where in Paris
  AdvIAdv iadv adv =

-- Interrogative complements to copulas can be both adverbs and
-- pronouns.

  -- : IAdv -> IComp ;
  CompIAdv iadv = iadv ;            -- where (is it)


-- More $IP$, $IDet$, and $IAdv$ are defined in $Structural$.

-- Wh questions with two or more question words require a new, special category.

  lincat
    -- buy what where
    QVP =
  lin
    -- : VPSlash -> IP -> QVP ;   -- buys what
    ComplSlashIP vps ip =

    -- : VP  ->   IAdv -> QVP ;   -- lives where
    AdvQVP vp iadv =

    -- : QVP ->   IAdv -> QVP ;   -- buys what where
    AddAdvQVP qvp iadv =

    -- : IP -> QVP -> QCl ;       -- who buys what where
    QuestQVP ip qvp =
-}


}
