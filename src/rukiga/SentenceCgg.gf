--# -path=.:../prelude:../abstract:../common

concrete SentenceCgg of Sentence = CatCgg **
  open Prelude, ResCgg in {  

lin 
  {-creating a sentence-}
  --UseCl temp pol cl = {
  --    s = temp.s ++ pol.s ++ cl.s !pol.p ! temp.t ! temp.a
  --  } ;
--2 Sentences
  --UseCl    : Temp -> Pol -> Cl  -> S ;   -- she had not slept
  --Temp  = {s : Str ; t : R.Tense ; a : R.Anteriority} ;
  UseCl  temp pol cl = let 
                subj = cl.s;
                vMorphs = mkVerbMorphs;
                clitic = mkSubjClitic cl.subjAgr;
                presSimul =  vMorphs ! VFPres; --this is not delivering the string
                presAnt = vMorphs ! VFPastPart; --this is not delivering the string
                root = cl.root;
                presRestOfVerb = cl.pres;
                pastRestOfVerb = cl.perf; --morphs ! VFPastPart ! RestOfVerb;

                compl = cl.compl
                in 
  case <temp.t,temp.a, pol.p> of {
      <Pres,Simul, Pos> => case cl.isPresBlank of { 
                                  True  => {s = subj ++ clitic ++ root  ++ compl};
                                  False => {s = subj ++ clitic ++ root  ++ Predef.BIND ++ compl}
                          };
      {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
      <Pres,Simul, Neg> => case cl.isPresBlank of { 
                                  True  => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ root ++ compl};
                                  False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++
                                              root ++ Predef.BIND ++ presRestOfVerb ++ compl}
                            };
      <Pres,Anter, Pos> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ clitic ++ root ++ compl};
                                  False => {s = subj ++  clitic ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                            };        
      <Pres,Anter, Neg> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ clitic ++ root  ++ compl};
                                  False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ 
                                              root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                            };


      <Past,Simul, Pos> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ clitic ++ "ka" ++ Predef.BIND ++ root  ++ compl};
                                  False => {s = subj ++ clitic  ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                          };
      {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
      <Past,Simul, Neg> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ root  ++ compl};
                                  False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ 
                                              root ++ pastRestOfVerb ++ compl}
                                };

      <Past,Anter, Pos> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ clitic ++ "kaba" ++Predef.BIND ++ clitic ++
                                               root ++  compl};
                                  False => {s = subj ++ clitic ++ "kaba" ++ clitic ++  "a" ++ Predef.BIND ++ 
                                                root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                              };       
      <Past,Anter, Neg> =>case cl.isPerfBlank of { 
                                  True  => {s = subj ++ clitic ++ "ka" ++Predef.BIND ++ clitic ++
                                               root ++  compl};
                                  False => {s = subj ++ clitic ++ "kaba" ++ clitic ++  "taa" ++ Predef.BIND ++ 
                                                root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                              };

      <Fut,Simul, Pos> => case cl.isPresBlank of { 
                                  True  => {s = subj ++ "ni" ++ Predef.BIND ++clitic ++ "za ku" ++ Predef.BIND ++  --choice of za over ija
                                            root ++ compl};
                                  False => {s = subj ++ "ni" ++ Predef.BIND ++clitic ++ "za ku" ++ Predef.BIND ++  --choice of za over ija
              root ++ Predef.BIND ++ presRestOfVerb ++ compl}
                              };

      
      {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
      <Fut,Simul, Neg> => case cl.isPresBlank of { 
                                  True  => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "kuza ku" ++ Predef.BIND ++ 
                                            root ++ compl};
                                  False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "kuza ku" ++ Predef.BIND ++ 
                                            root ++ BIND ++ presRestOfVerb ++ compl}
                              };
      <Fut,Anter, Pos> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ "ni" ++ Predef.BIND ++clitic ++ "za kuba" ++ Predef.BIND ++ clitic ++ --choice of za over ija
                                            root ++ Predef.BIND ++ "ire" ++ compl};
                                  False => {s = subj ++ "ni" ++ Predef.BIND ++clitic ++ "za kuba" ++ Predef.BIND ++ clitic ++ --choice of za over ija
                                            root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                              };
             
      <Fut,Anter, Neg> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ "ni" ++ Predef.BIND ++ clitic ++ "za kuba" ++ clitic  ++ Predef.BIND ++ 
                                              root ++ "ire" ++ compl};
                                  False => {s = subj ++ "ni" ++ Predef.BIND ++ clitic ++ "za kuba" ++ clitic ++ "taka" ++ Predef.BIND ++ 
                                              root ++ pastRestOfVerb ++ compl}
                              };
      <Cond,Simul, Pos> => case cl.isPresBlank of { 
                                  True  => {s = subj ++ clitic ++ "kaa" ++Predef.BIND ++ root ++ compl};
                                  False => {s = subj ++ clitic ++ "kaa" ++Predef.BIND ++ 
                                              root ++ Predef.BIND ++ presRestOfVerb ++ compl}
                              };
      {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
      <Cond,Simul, Neg> =>case cl.isPresBlank of { 
                                  True  => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "kaa" ++ Predef.BIND ++ 
                                              root ++ "ire" ++ compl};
                                  False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "kaa" ++ Predef.BIND ++ 
                                              root ++ presRestOfVerb ++ compl}
                              }; 

      <Cond,Anter, Pos> => case cl.isPerfBlank of { 
                                  True  => {s = subj ++ clitic ++ "kaa" ++ root ++  compl};
                                  False => {s = subj ++ clitic ++ "kaa" ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                              };

      <Cond,Anter, Neg> =>case cl.isPerfBlank of { 
                                  True  => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++  "kaa" ++Predef.BIND 
                   ++ root ++ Predef.BIND ++ "ire" ++ compl};
                                  False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++  "kaa" ++Predef.BIND 
                   ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                              }
    };  --: Temp -> Pol -> QCl  -> QS ; -- has John walked

  -- These are the 2 x 4 x 4 = 16 forms generated by different
  -- combinations of tense, polarity, and
  -- anteriority, which are defined in [``Common`` Common.html].
  UseQCl   = UseCl; -- : Temp -> Pol -> Cl   -> S ;  -- John has not walked
  

  --UseRCl   : Temp -> Pol -> RCl -> RS ;  -- that had not slept
  UseRCl temp pol rcl = let 
                            subj = rcl.s; -- this could be empty
                            vMorphs = mkVerbMorphs;
                            subjClitic  = case rcl.agr of {
                                                AgrYes a => mkSubjClitic a;
                                                _        => mkSubjClitic (AgP3 Sg MU_BA)
                                              };
                            rsubjClitic = case rcl.agr of {
                                              AgrYes a => mkRPs!RSubj! a;
                                              _        => mkRPs!RSubj! AgP3 Sg MU_BA
                                      };
                            robjClitic = case rcl.agr of {
                                              AgrYes a => mkRPs!RObj! a;
                                              _        => mkRPs!RObj! AgP3 Sg MU_BA
                                      };
                            presSimul =  vMorphs ! VFPres; --this is not delivering the string
                            presAnt = vMorphs ! VFPastPart; --this is not delivering the string
                            root = rcl.root;
                            presRestOfVerb = rcl.pres;
                            pastRestOfVerb = rcl.perf; --morphs ! VFPastPart ! RestOfVerb;

                            compl = rcl.compl
                      in {- will these strings I am introducing allow back translation? Yes, it simply depends on functions-}
                        case <temp.t,temp.a, pol.p> of {
                            <Pres,Simul, Pos> => {s = table { 
                                                      RF RSubj  => subj ++ rsubjClitic ++  root ++ Predef.BIND ++ presRestOfVerb ++ compl;
                                                      RF RObj   => subj ++ robjClitic ++  root ++ Predef.BIND ++ presRestOfVerb ++ compl;
                                                      Such_That => "kugira ngu" ++ subjClitic ++  root ++ Predef.BIND ++ presRestOfVerb ++ compl
                                                    }
                                                  };
                            {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
                            <Pres,Simul, Neg> => {
                                                  s = table { 
                                                              RF RSubj  => subj ++ "ti" ++ Predef.BIND ++ rsubjClitic ++ root ++ presRestOfVerb ++ compl;
                                                              RF RObj   => subj ++ "ti" ++ Predef.BIND ++ robjClitic ++ root ++ presRestOfVerb ++ compl;
                                                              Such_That => "kugira ngu" ++ subj ++ "ti" ++ Predef.BIND ++ subjClitic ++ root ++ presRestOfVerb ++ compl
                                                            }
                                                          };
                            <Pres,Anter, Pos> => {
                                                    s = table{
                                                            RF RSubj => subj ++ rsubjClitic ++ vMorphs!VFPresAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                            RF RObj  => subj ++ robjClitic  ++ vMorphs!VFPresAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                            Such_That => "kugira ngu" ++ subj ++ subjClitic ++ vMorphs!VFPresAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl
                                                            }
                                                          }; 
                            <Pres,Anter, Neg> =>{
                                                  s = table {
                                                            RF RSubj  => subj ++ "ti" ++ Predef.BIND ++ rsubjClitic ++ vMorphs!VFPresAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                            RF RObj   => subj ++ "ti" ++ Predef.BIND ++ robjClitic ++ vMorphs!VFPresAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                            Such_That => "kugira ngu" ++  "ti" ++ Predef.BIND ++ rsubjClitic ++ vMorphs!VFPresAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl
                                                          }
                                                        };

                            <Past,Simul, Pos> => {
                                                    s = table { 
                                                                RF RSubj  => subj ++ rsubjClitic ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                                RF RObj   => subj ++ robjClitic ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                                Such_That => "kugira ngu" ++ subjClitic ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl
                                                              }
                                                            };
                            {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
                            <Past,Simul, Neg> => {
                                                    s = table {
                                                                RF RSubj  => subj ++ "ti" ++ Predef.BIND ++ rsubjClitic ++root ++ pastRestOfVerb ++ compl;
                                                                RF RObj   => subj ++ "ti" ++ Predef.BIND ++ robjClitic ++root ++ pastRestOfVerb ++ compl;
                                                                Such_That => "kugira ngu" ++ "ti" ++ Predef.BIND ++ subjClitic ++ root ++ pastRestOfVerb ++ compl
                                                              }
                                                            };
                            <Past,Anter, Pos> => {
                                                  s = table {
                                                      RF RSubj  => subj ++ rsubjClitic ++ "bire" ++ rsubjClitic ++ vMorphs!VFPastAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                      RF RObj   => subj ++ robjClitic ++ "bire" ++ subjClitic ++ vMorphs!VFPastAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                      Such_That => "kugira ngu" ++ "bire" ++ subjClitic ++ vMorphs!VFPastAnt!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl
                                                      }
                                                    };    
                            <Past,Anter, Neg> =>{
                                                  s = table  {
                                                        RF RSubj  => subj ++  rsubjClitic ++ "bire" ++ subjClitic ++ "ta" ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                        RF RObj   => subj ++  robjClitic ++ "bire" ++ subjClitic ++ "ta" ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                        Such_That => "kugira ngu" ++ "bire" ++ subjClitic ++ "ta" ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl
                                                      }
                                                    };
                            
                            <Fut,Simul, Pos> => {
                                                    s = table {
                                                        RF RSubj  => subj ++ "ni" ++ Predef.BIND ++ rsubjClitic ++ "za ku" ++ Predef.BIND ++  --choice of za over ija
                                                                      root ++ Predef.BIND ++ presRestOfVerb ++ compl;
                                                        RF RObj   => subj ++ "ni" ++ Predef.BIND ++ robjClitic ++ "za ku" ++ Predef.BIND ++  --choice of za over ija
                                                                      root ++ Predef.BIND ++ presRestOfVerb ++ compl;
                                                        Such_That => "kugira ngu" ++ "ni" ++ Predef.BIND ++ robjClitic ++ "za ku" ++ Predef.BIND ++  --choice of za over ija
                                                                      root ++ Predef.BIND ++ presRestOfVerb ++ compl
                                                  }

                                                };
                            
                            {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
                            <Fut,Simul, Neg> => {
                                                    s = table {
                                                        RF RSubj  =>subj ++ "ti" ++ Predef.BIND ++ rsubjClitic ++ "kuza ku" ++ Predef.BIND ++ root ++ presRestOfVerb ++ compl;
                                                        RF RObj   =>subj ++ "ti" ++ Predef.BIND ++ robjClitic ++ "kuza ku" ++ Predef.BIND ++ root ++ presRestOfVerb ++ compl;
                                                        Such_That => "Kugira ngu" ++ "ti" ++ Predef.BIND ++ subjClitic ++ "kuza ku" ++ Predef.BIND ++ root ++ presRestOfVerb ++ compl
                                                  }

                                                };
                            <Fut,Anter, Pos> => {
                                                    s = table {
                                                        RF RSubj  => subj ++ "ni" ++ Predef.BIND ++ rsubjClitic ++ "za kuba" ++ Predef.BIND ++  --choice of za over ija
                                    root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                        RF RObj   => subj ++ "ni" ++ Predef.BIND ++ robjClitic ++ "za kuba" ++ Predef.BIND ++  --choice of za over ija
                                    root ++ Predef.BIND ++ pastRestOfVerb ++ compl;
                                                        Such_That => "kugira ngu" ++ "ni" ++ Predef.BIND ++ robjClitic ++ "za kuba" ++ Predef.BIND ++  --choice of za over ija
                                    root ++ Predef.BIND ++ pastRestOfVerb ++ compl
                                                  }

                                                };    
                            <Fut,Anter, Neg> => {
                                                    s = table {
                                                        RF RSubj  => subj ++ "ni" ++ Predef.BIND ++ rsubjClitic ++ "za kuba" ++ subjClitic ++ "taka" ++ Predef.BIND ++ 
                                      root ++ pastRestOfVerb ++ compl;
                                                        RF RObj   => subj ++ "ni" ++ Predef.BIND ++ robjClitic ++ "za kuba" ++ subjClitic ++ "taka" ++ Predef.BIND ++ 
                                      root ++ pastRestOfVerb ++ compl;
                                                        Such_That => "kugira ngu" ++ "ni" ++ Predef.BIND ++ subjClitic ++ "za kuba" ++ subjClitic ++ "taka" ++ Predef.BIND ++ 
                                      root ++ pastRestOfVerb ++ compl
                                                  }

                                                };

                            <Cond,Simul, Pos> => {
                                                    s = table {
                                                        RF RSubj  => subj ++ subjClitic ++ "kaa" ++Predef.BIND ++ root ++ Predef.BIND ++ presRestOfVerb ++ compl;
                                                        RF RObj   => subj ++ subjClitic ++ "kaa" ++Predef.BIND ++ root ++ Predef.BIND ++ presRestOfVerb ++ compl;
                                                        Such_That => "kugira ngu" ++ subjClitic ++ "kaa" ++Predef.BIND ++ root ++ Predef.BIND ++ presRestOfVerb ++ compl
                                                  }

                                                };
                            {-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
                            <Cond,Simul, Neg> => { s =  \\_ => subj ++ "ti" ++ Predef.BIND ++ subjClitic ++ "kaa" ++ Predef.BIND ++ root ++ presRestOfVerb ++ compl
                                                        --RF RSubj  => subj ++ "ti" ++ Predef.BIND ++ subjClitic ++ "kaa" ++ Predef.BIND ++ root ++ presRestOfVerb ++ compl;
                                                        --RF RObj   =>
                                                        --Such_That =>

                                                };
                            <Cond,Anter, Pos> => { s =  \\_ => subj ++ subjClitic ++ "kaa" ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl};
      
                            <Cond,Anter, Neg> => {s = \\_ => subj ++ "ti" ++ Predef.BIND ++ subjClitic ++  "kaa" ++Predef.BIND 
                                         ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}  --: Temp -> Pol -> QCl  -> QS ; -- has John walked
                      };

  
  PredVP np vp = case vp.isCompApStem of{
              False    => {
                        s = np.s ! Nom;   --: NP -> VP -> Cl ;            -- John walks / John does not walk
                        subjAgr = np.agr;
                        pres = vp.pres;
                        perf = vp.perf;
                        root = vp.s;
                        --morphs = vp.morphs;
                        isPresBlank = vp.isPresBlank;
                        isPerfBlank = vp.isPerfBlank;
                        {-
                        inf  = mkVerbInrf vp.root;
                      pres  = mkVerbPres vp.root; 
                      past  = mkVerbPast vp.root; 
                      presPart  = mkVerbPresPart vp.root; 
                      pastPart  = mkVerbPastPart vp.root;                              -- subject
                      -}
                      --root = vp.root ;
                        compl = vp.comp
                        };
              True    =>  {
                        s = np.s ! Nom;   --: NP -> VP -> Cl ;            -- John walks / John does not walk
                        subjAgr = np.agr;
                        pres = vp.pres;
                        perf = vp.perf;
                        root = vp.s;
                        --morphs = vp.morphs;
                        isPresBlank = vp.isPresBlank;
                        isPerfBlank = vp.isPerfBlank;
                        {-
                        inf  = mkVerbInrf vp.root;
                      pres  = mkVerbPres vp.root; 
                      past  = mkVerbPast vp.root; 
                      presPart  = mkVerbPresPart vp.root; 
                      pastPart  = mkVerbPastPart vp.root;                              -- subject
                      -}
                      --root = vp.root ;
                        compl = mkSubjClitic np.agr   ++ vp.comp --mkSubjClitic np.agr ++ Predef.BIND ++ vp.comp
                      }
        };--: NP -> VP -> Cl ; -- John walks / John does not walk
    

  
    {-
    Note: It seems mkSubjClitic comes with a Predef.BIND already
    prepared for the next token to bind.
    Reason: When I add a BIND command, I get two bind tokens in the linearizations
    -}

  ImpVP  vp = let vMorphs = mkVerbMorphs in {
        s =table{
          ImpPos  => vp.s ++ Predef.BIND ++ vMorphs!VFInf!RestOfVerb ++ vp.comp;
          ImpNeg =>  case vp.isCompApStem of {   -- How do I make the number dynamic use case?
                  True =>vMorphs!VFPres!SecNegM ++ Predef.BIND ++ vp.s ++ Predef.BIND ++ 
                        vMorphs!VFInf!RestOfVerb ++ (mkAdjPronNoIVClitic (AgMUBAP2 Sg)) ++ vp.ap;
                  False  => vMorphs!VFPres!SecNegM ++ Predef.BIND ++ vp.s ++ Predef.BIND ++ 
                        vMorphs!VFInf!RestOfVerb ++ vp.comp
              }
        } 
  };  --: VP -> Imp ;                 -- walk / do not walk

  -- A sentence can be modified by a relative clause referring to its contents.

    --RelS     : S -> RS -> S ;              -- she sleeps, which is good

--2 Clauses missing object noun phrases

-- This category is a variant of the 'slash category' $S/NP$ of
-- GPSG and categorial grammars, which in turn replaces
-- movement transformations in the formation of questions
-- and relative clauses. Except $SlashV2$, the construction 
-- rules can be seen as special cases of function composition, in
-- the style of CCG.
-- *Note* the set is not complete and lacks e.g. verbs with more than 2 places.

    --SlashVP   : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    SlashVP np vpslash =let complTp = case <vpslash.containsAdv, vpslash.containsAdV> of {
                                              <True, False> => Adverbial;
                                              <False, True> => AdverbialVerb;
                                              <False, False> => Ap;
                                              <True, True>   => Empty
                                            };
                        in 
                            {
                              s = np.s ! Nom;
                              subjAgr     = np.agr;
                              root        = vpslash.s;
                              pres        = vpslash.pres;
                              perf        = vpslash.perf;
                              --morphs      = vpslash.morphs;
                              isPresBlank = vpslash.isPresBlank;
                              isPerfBlank = vpslash.isPerfBlank;
                              ap          = vpslash.ap;
                              isRegular   = vpslash.isRegular;
                              adv         = vpslash.adv;
                              
                              adV         = vpslash.adV;
                              complType   = complTp
                            };

    --AdvSlash  : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
    --SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
    --SlashVS   : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves





{-
--1 Sentence: Sentences, Clauses, and Imperatives

abstract Sentence = Cat ** {

--2 Clauses

-- The $NP VP$ predication rule form a clause whose linearization
-- gives a table of all tense variants, positive and negative.
-- Clauses are converted to $S$ (with fixed tense) with the
-- $UseCl$ function below.

  data
    PredVP    : NP -> VP -> Cl ;         -- John walks

-- Using an embedded sentence as a subject is treated separately.
-- This can be overgenerating. E.g. "whether you go" as subject
-- is only meaningful for some verb phrases.

    PredSCVP  : SC -> VP -> Cl ;         -- that she goes is good

--2 Clauses missing object noun phrases

-- This category is a variant of the 'slash category' $S/NP$ of
-- GPSG and categorial grammars, which in turn replaces
-- movement transformations in the formation of questions
-- and relative clauses. Except $SlashV2$, the construction 
-- rules can be seen as special cases of function composition, in
-- the style of CCG.
-- *Note* the set is not complete and lacks e.g. verbs with more than 2 places.

    SlashVP   : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    AdvSlash  : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
    SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
    SlashVS   : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves

--2 Imperatives

-- An imperative is straightforwardly formed from a verb phrase.
-- It has variation over positive and negative, singular and plural.
-- To fix these parameters, see [Phrase Phrase.html].

    ImpVP     : VP -> Imp ;              -- love yourselves

--2 Embedded sentences

-- Sentences, questions, and infinitival phrases can be used as
-- subjects and (adverbial) complements.

    EmbedS    : S  -> SC ;               -- that she goes
    EmbedQS   : QS -> SC ;               -- who goes
    EmbedVP   : VP -> SC ;               -- to go

--2 Sentences

-- These are the 2 x 4 x 4 = 16 forms generated by different
-- combinations of tense, polarity, and
-- anteriority, which are defined in [``Common`` Common.html].

  fun
    UseCl    : Temp -> Pol -> Cl  -> S ;   -- she had not slept
    UseQCl   : Temp -> Pol -> QCl -> QS ;  -- who had not slept
    UseRCl   : Temp -> Pol -> RCl -> RS ;  -- that had not slept
    UseSlash : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen

-- An adverb can be added to the beginning of a sentence, either with comma ("externally")
-- or without:

    AdvS     : Adv -> S  -> S ;            -- then I will go home
    ExtAdvS  : Adv -> S  -> S ;            -- next week, I will go home

-- This covers subjunctive clauses, but they can also be added to the end.

    SSubjS   : S -> Subj -> S -> S ;       -- I go home if she comes

-- A sentence can be modified by a relative clause referring to its contents.

    RelS     : S -> RS -> S ;              -- she sleeps, which is good

---- A sentence can also be post-modified by a subjunct sentence.

----    ModSubjS : S -> Subj -> S -> S ;       -- she sleeps, because she is old 
---- cf. Adverb.SubjS

--.

-- Examples for English $S$/$Cl$:

  Pres  Simul  Pos  ODir  : he sleeps
  Pres  Simul  Neg  ODir  : he doesn't sleep
  Pres  Anter  Pos  ODir  : he has slept
  Pres  Anter  Neg  ODir  : he hasn't slept
  Past  Simul  Pos  ODir  : he slept
  Past  Simul  Neg  ODir  : he didn't sleep
  Past  Anter  Pos  ODir  : he had slept
  Past  Anter  Neg  ODir  : he hadn't slept
  Fut   Simul  Pos  ODir  : he will sleep
  Fut   Simul  Neg  ODir  : he won't sleep
  Fut   Anter  Pos  ODir  : he will have slept
  Fut   Anter  Neg  ODir  : he won't have slept
  Cond  Simul  Pos  ODir  : he would sleep
  Cond  Simul  Neg  ODir  : he wouldn't sleep
  Cond  Anter  Pos  ODir  : he would have slept
  Cond  Anter  Neg  ODir  : he wouldn't have slept

-}

}
