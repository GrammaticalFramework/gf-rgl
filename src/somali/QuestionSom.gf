concrete QuestionSom of Question = CatSom ** open
  Prelude, ResSom, ParadigmsSom, (VS=VerbSom), (NS=NounSom), (SS=StructuralSom) in {

-- A question can be formed from a clause ('yes-no question') or
-- with an interrogative.

  lin
  -- : Cl -> QCl ;
  QuestCl = cl2qcl PolarQuestion True;

  -- : IP -> VP -> QCl ;
  QuestVP ip vp = -- TODO: if we want to contract baa + subj. pronoun, change ResSom.predVP
    let cls : ClSlash = predVP ip vp ;
        baan : Str = case ip.contractSTM of {True => "aan" ; _ => "baa aan"} ;
        cl : ClSlash = cls ** {
                stm = modSTM "baa" baan cls.stm
        } ;
     in cl2qcl PolarQuestion (notB ip.contractSTM) cl ;

  -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls =
    let baan : Str = case ip.contractSTM of {True => "aan" ; _ => "baa aan"} ;
        clsIPFocus = cls ** {
          subj = cls.subj ** {  -- keep old subject pronoun,
                   noun = ip.s ! Nom -- and place IP first.
                 } ;
          obj = cls.obj ** { -- move old subject noun before object.
                   s = cls.subj.noun ++ cls.obj.s
                 } ;
          stm = modSTM "baa" baan cls.stm
        } ;
     in cl2qclslash (notB ip.contractSTM) clsIPFocus ;

  -- : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cls =
    let clRaw : ClSlash = insertIAdv iadv cls ;
        sbj = clRaw.subj ;
        cl : ClSlash = clRaw ** {
            stm = \\clt,p => case <clt,p> of {
                                -- IAdv is focused with baa, and subject comes after
                                <_,Pos> => case iadv.contractSTM of {
                                             True => [] ; _ => "baa"}
                                        ++ sbj.pron ++ sbj.noun ;
                                _ => case iadv.contractSTM of {
                                      True => [] ; _ => clRaw.stm ! WhQuestion ! p}
                                  ++ sbj.pron ++ sbj.noun } ;
            subj = sbj ** {noun, pron = []}  -- to force subject after baa
        } ;
     in cl2qcl WhQuestion True cl ; -- True because we handle STM placement in cl.stm

  -- : IComp -> NP -> QCl ;   -- where is John?
  QuestIComp icomp np =
    let cls = predVP np (VS.UseComp (icomp2comp icomp)) ;
        -- cl = cls ** { -- TODO: neg. questions
        --       stm : ClType=>Polarity=>Str = \\_,_ => "waa"
        -- }
     in cl2sentence False cls ; -- copula is dropped and STM is waa

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
  --  AdvIAdv iadv adv = iadv ** {s = iadv.s ++ adv.berri} ; -- TODO do we need PrepCombination in IAdv?

-- Interrogative complements to copulas can be both adverbs and
-- pronouns.

  -- : IAdv -> IComp ;
  CompIAdv iadv = iadv ;            -- where (is it)

  -- : IP -> IComp ;
  CompIP ip = {s = ip.s ! Abs} ;    -- who (is it)

{-
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

oper

  icomp2comp : SS -> Complement = \icomp -> icomp ** {
    aComp = \\_ => [] ;
    nComp = icomp.s ;
    compar = [] ;
    stm = Waa NoCopula
  } ;

  -- Question clauses: subject pronoun not included, STM is
  cl2qcl : ClType -> Bool -> ClSlash -> Clause = \cltyp ->
    let hasSubjPron : Bool = False ;
        isRel : Bool = False ;
     in mkClause cltyp isRel hasSubjPron ;

  -- Question clause with wh-word as object: subject pronoun is included
  cl2qclslash : Bool -> ClSlash -> Clause =
    let hasSubjPron : Bool = True ;
        isRel : Bool = False ;
     in mkClause PolarQuestion isRel hasSubjPron ;

}
