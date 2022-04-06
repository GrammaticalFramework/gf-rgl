concrete QuestionMay of Question = CatMay ** open
  Prelude, ResMay, ParadigmsMay, (VS=VerbMay), (NM=NounMay), (SS=StructuralMay) in {

-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.

-- Interrogative pronouns can be formed with interrogative
-- determiners, with or without a noun.
lin
  -- : IDet -> CN -> IP ;       -- which five songs
  IdetCN idet cn = NM.DetCN idet cn ** {
    sp = \\nf => idet.sp ! nf ++ cn.s ! nf
  } ;

  -- : IDet       -> IP ;       -- which five
  IdetIP idet = NM.DetNP idet ** {sp = idet.sp};

  -- : IQuant -> Num -> IDet ;  -- which (five)
  IdetQuant iquant num = iquant ** {
    pr = num.s ++ case iquant.isPre of {True => iquant.s ; False => [] } ;
    -- if isPre is True, then: "berapa kucing"
    s = case iquant.isPre of { False => iquant.s ; True => [] };
    -- if isPre is False, use s: "kucing berapa"
    n = num.n
  } ;

  -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls = cls ** {
    pred = \\vf,pol => cls.pred ! vf ! pol ++ ip.s ! Bare
  } ;

  -- : Subj -> Pred -> QCl ;
  -- QuestCl cl = cl ** {
  --   pred = \\vf,pol => cl.pred ! vf ! pol
  -- };
  QuestCl cl = cl ** {
    subj = "adakah" ++ cl.subj;
  } ;

    -- missing record fields: pred type of vp

  -- : IP -> VP -> QCl ;
  --  expected: ParamMay.VForm => ParamX.Polarity => Str
  --   inferred: {s : ParamMay.VForm => ParamX.Polarity => Str}

  QuestVP ip cl =  cl ** {
    pred = \\vf,pol => cl.s ! vf ! pol;
    subj = ip.s ! Bare ;
  };

  -- : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cls = {
    subj = case iadv.isPre of {
      True => iadv.s ++ cls.subj ; False => cls.subj
    } ;
    pred = \\vf,pol => case iadv.isPre of {
      True => cls.pred ! iadv.vf ! pol ;
      False => cls.pred ! iadv.vf ! pol ++ iadv.s
    } ;
  } ;


  -- : IP -> IComp ;
  CompIP ip = {s = ip.s ! Bare } ;    -- who (is it)

  -- : IComp -> NP -> QCl ;   -- where is John?
  QuestIComp icomp np = {
    pred = \\vf,pol => np.s ! Bare ;
    subj = icomp.s ;
  } ;

-- \\vf,pol,posadv =>
  --   {
  --   pred = \\vf,pol => ip.s ++ vp.s ! vf ! pol;
  -- } ;

{- ----
      s = \\t,a,p =>
            let
              cl = oldClause slash ;
              cls : Direct -> Str =
                    \d -> cl.s !        d ! t ! a ! p ! Indic ;
----                \d -> cl.s ! ip.a ! d ! t ! a ! p ! Indic ;
              who = slash.c2.s ++ ip.s ! slash.c2.c
            in table {
              QDir   => who ++ cls DInv ;
              QIndir => who ++ cls DDir
              }
-}

{-
  lin





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
