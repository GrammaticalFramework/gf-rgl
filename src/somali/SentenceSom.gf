concrete SentenceSom of Sentence = CatSom ** open
  TenseX, ResSom, (AS=AdverbSom), Prelude in {

flags optimize=all_subs ;

lin

--2 Clauses

  -- : NP -> VP -> Cl
  PredVP np vps =
    let vp = case vps.c2 of {
                Passive => complSlash (insertComp vps np) ;
                _ => complSlash vps } ;
        subj = case vps.c2 of {Passive => impersNP ; _ => np} ;
    in { s = \\isQ,t,a,p =>
       let predRaw : {fin : Str ; inf : Str} = vf t a p subj.a vp ;
           pred : {fin : Str ; inf : Str} = case <isQ,p,vp.pred> of {
              <False,Pos,NoCopula> => {fin,inf = []} ;
              _                    => predRaw
           } ;
           subjnoun : Str = if_then_Str np.isPron [] (subj.s ! Nom) ;
           subjpron : Str = if_then_Str np.isPron (subj.s ! Nom) [] ;
           obj : {p1,p2 : Str} =
              let o : {p1,p2 : Str} = vp.comp ! subj.a ;
                  bind : Str = if_then_Str (isP3 vp.obj2.a) [] BIND ;
               in case p of {
                    Pos => o ;
                    Neg => {p2 = [] ; p1 = o.p1 ++ o.p2 ++ bind} -- object pronoun, prepositions and negation all contract
                  } ;
           stm : Str = case  <isQ,p,vp.pred,subj.a> of {
                       <False,Pos,Copula|NoCopula,Sg3 _|Impers> => "waa" ;
                       <True ,Pos,_              ,_           > => "ma" ;
                       _ => case <np.isPron,p> of {
                              <True,Pos> => "waa" ++ subjpron ; -- to force some string from NP to show in the tree
                              <True,Neg> => "ma" ++ subjpron ;
                              <False>    => stmarkerNoContr ! subj.a ! p
                            }
                  } ;

      in vp.berri -- AdV
      ++ subjnoun -- subject if it's a noun
      ++ obj.p1   -- object if it's a noun
      ++ stm      -- sentence type marker + possible subj. pronoun
      ++ obj.p2   -- object if it's a pronoun
      ++ vp.sii   -- restricted set of particles
      ++ vp.dhex  -- restricted set of nouns/adverbials
      ++ vp.secObj   -- "second object"
      ++ pred.inf     -- potential infinitive/participle
      ++ pred.fin     -- the verb inflected
      ++ vp.miscAdv   ---- NB. Only used if there are several adverbs.
                      ---- Primary places for adverbs are obj, sii or dhex.
    } ;
{-
  -- : SC -> VP -> Cl ;         -- that she goes is good
  PredSCVP sc vp = ;

--2 Clauses missing object noun phrases
  -- : NP -> VPSlash -> ClSlash ;
  SlashVP np vps = ;

  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  AdvSlash cls adv = cls ** insertAdv adv cls ;

--    SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks

  -- : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves
--  SlashVS np vs ss = {} ;


  --  : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
  UseSlash t p cls = UseCl t p (PredVP he_Pron cls) ;

--2 Imperatives
  -- : VP -> Imp ;
 ImpVP vp = { s = linVP vp } ;

--2 Embedded sentences


  -- : S  -> SC ;
  EmbedS s = { } ;

  -- : QS -> SC ;
  EmbedQS qs = { } ;

  -- : VP -> SC ;
  EmbedVP vp = { s = linVP vp } ;

--2 Sentences

-}
  -- : Temp -> Pol -> Cl -> S ;
  UseCl t p cl = {s = t.s ++ p.s ++ cl.s ! False ! t.t ! t.a ! p.p} ;

  -- : Temp -> Pol -> QCl -> QS ;
  UseQCl t p cl = {s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p} ;

{-
  -- : Temp -> Pol -> RCl -> RS ;
  UseRCl t p cl = { s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p } ;

-- An adverb can be added to the beginning of a sentence, either with comma ("externally")
-- or without:

  -- : Adv -> S  -> S ;            -- then I will go home
  AdvS = advS ;

  -- : Adv -> S  -> S ;            -- next week, I will go home
  ExtAdvS adv = advS {s = adv.s ++ SOFT_BIND ++ ","}  ;

-- There's an SubjS already in AdverbSom -- should this be deprecated?
  -- : S -> Subj -> S -> S ;
  SSubjS s1 subj s2 = AdvS (AS.SubjS subj s2) s1 ;

-- A sentence can be modified by a relative clause referring to its contents.

  --  : S -> RS -> S ;              -- she sleeps, which is good
  RelS sent rs = advS { s = rs.s ! Sg3 Masc ++ SOFT_BIND ++ ","} sent ;

oper

  advS : Adv -> SS -> SS = \a,s -> {s = a.s ++ s.s} ;
-}
}
