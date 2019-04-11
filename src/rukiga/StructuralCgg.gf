--# -path=.:../prelude:../abstract:../common

concrete StructuralCgg of Structural = CatCgg ** 
  open ResCgg, ParadigmsCgg, (C = ConstructX), Prelude in {

lin

  every_Det = {s = "buri"; ntype=Incomplete; num=Sg; pos=PreDeterminer} ;
  few_Det = {s="kye"; ntype =Complete; num=Pl; pos=PostDeterminer} ;
  many_Det ={s="ingi"; ntype =Complete; num=Pl; pos=PostDeterminer} ;

  i_Pron = {s = table {Nom = "nyowe"; Acc = "nyowe"}; agr = AgMUBAP1 Sg};
  youSg_Pron {s = table {Nom = "iwe"; Acc = "we"}; agr=AgMUBAP2 Sg };
  she_Pron = {s = table {Nom = "uwe"; Acc = "uwe"}; agr=AgP3 Sg MU_BA};
  he_Pron = {s= table {Nom = "uwe"; Acc = "uwe"}; agr=AgP3 Sg MU_BA};
  it_Pron = {s = table {Nom = "kyo"; Acc = "kyo"}; agr=AgP3 Sg KI_BI}; -- should form an it_Pron_NClass in extra module
  we_Pron = {s = table {Nom = "itwe"; Acc = "itwe"}; agr = AgMUBAP1 Pl};
  youPl_Pron ={s = table {Nom = "imwe"; Acc = "imwe"}; agr = AgMUBAP2 Pl};
  they_Pron = {s= table {Nom = "bo"; Acc = "bo"}; agr=AgP3 Pl MU_BA};--But there are lots of other representations from other noun classes in extra module
  behind_Prep ={s="enyuma ya"};
  between_Prep = {s="hagati ya"};
  to_Prep ={s="aha"};
  in_Prep ={s="omu"};
  on_Prep ={s="aha"};
  from_Prep ={s="kuruga"};
  under_Prep = {s="hansi ya"};



{-variants
    NOTE: Please add them to the abstract syntax, ask aarne 
    or creat you own abstract Lexicon which inherits from the 
    standard one. See how english does it. i.e. use DictCggAbs.gf for the funs.
    and DictCgg.gf for the lins.

    Actually use and extend module for Structural
-}

  {-
        For DetQuant function to work, we need sample quatitifiers in Runynakore. Roximal, Medial, Distant
        We need a table to provide all of these.
  -}
{-	
--1 Structural: Structural Words
--
-- Here we have some words belonging to closed classes and appearing
-- in all languages we have considered.
-- Sometimes more distinctions are needed, e.g. $we_Pron$ in Spanish
-- should be replaced by masculine and feminine variants, found in
-- [``ExtendSpa`` ../spanish/ExtendSpa.gf].

abstract Structural = Cat ** {

  fun

-- This is an alphabetical list of structural words

  above_Prep : Prep ;
  after_Prep : Prep ;
  all_Predet : Predet ;
  almost_AdA : AdA ;
  almost_AdN : AdN ;
  although_Subj : Subj ;
  always_AdV : AdV ;
  and_Conj : Conj ;
  because_Subj : Subj ;
  before_Prep : Prep ;
  behind_Prep : Prep ;
  between_Prep : Prep ;
  both7and_DConj : Conj ; -- both...and
---b  both7and_DConj : DConj ;
  but_PConj : PConj ;
  by8agent_Prep : Prep ; -- by (agent)
  by8means_Prep : Prep ; -- by (means of)
  can8know_VV : VV ; -- can (capacity)
  can_VV : VV ;      -- can (possibility)
  during_Prep : Prep ;
  either7or_DConj : Conj ; -- either...or
---b  either7or_DConj : DConj ;
  every_Det : Det ;
  everybody_NP : NP ;  -- everybody
  everything_NP : NP ;
  everywhere_Adv : Adv ; --ha-ona =hoona
---  first_Ord : Ord ; DEPRECATED
  few_Det : Det ;
  for_Prep : Prep ;
  from_Prep : Prep ;
  he_Pron : Pron ;
  here_Adv : Adv ; --hanu
  

  here7to_Adv : Adv ; -- to here
  here7from_Adv : Adv ;  -- from here
  how_IAdv : IAdv ;
  how8many_IDet : IDet ;
  how8much_IAdv : IAdv ;
  i_Pron : Pron ;
  if_Subj : Subj ;
  in8front_Prep : Prep ; -- in front of
  in_Prep : Prep ;
  it_Pron : Pron ;
  less_CAdv : CAdv ;
  many_Det : Det ;
  more_CAdv : CAdv ;
  most_Predet : Predet ;
  much_Det : Det ;
  must_VV : VV ;
---b  no_Phr : Phr ;
  no_Utt : Utt ;
  on_Prep : Prep ;
---  one_Quant : QuantSg ; DEPRECATED
  only_Predet : Predet ;
  or_Conj : Conj ;
  otherwise_PConj : PConj ;
  part_Prep : Prep ;
  please_Voc : Voc ;
  possess_Prep : Prep ; -- of (possessive)
  quite_Adv : AdA ;
  she_Pron : Pron ;
  so_AdA : AdA ;
  someSg_Det : Det ;
  somePl_Det : Det ;
  somebody_NP : NP ;
  something_NP : NP ;
  somewhere_Adv : Adv ;
  that_Quant : Quant ;
  that_Subj : Subj ;
  there_Adv : Adv ; --hari

  
  there7to_Adv : Adv ; -- to there
  there7from_Adv : Adv ; -- from there
  therefore_PConj : PConj ;
  they_Pron : Pron ;
  this_Quant : Quant ;
  through_Prep : Prep ;
  to_Prep : Prep ;
  too_AdA : AdA ;
  under_Prep : Prep ;
  very_AdA : AdA ;
  want_VV : VV ;
  we_Pron : Pron ;
  whatPl_IP : IP ; -- what (plural)
  whatSg_IP : IP ; -- what (singular)
  when_IAdv : IAdv ;
  when_Subj : Subj ;
  where_IAdv : IAdv ;
  which_IQuant : IQuant ;
  whoPl_IP : IP ;  -- who (plural)
  whoSg_IP : IP ;  -- who (singular)
  why_IAdv : IAdv ;
  with_Prep : Prep ;
  without_Prep : Prep ;
---b  yes_Phr : Phr ;
  yes_Utt : Utt ;
  youSg_Pron : Pron ; -- you (singular)
  youPl_Pron : Pron ; -- you (plural)
  youPol_Pron : Pron ; -- you (polite)

  no_Quant : Quant ;
  not_Predet : Predet ;
  if_then_Conj : Conj ;
  at_least_AdN : AdN ;
  at_most_AdN : AdN ;
  nobody_NP : NP ;
  nothing_NP : NP ;
  except_Prep : Prep ;

  as_CAdv : CAdv ;

  have_V2 : V2 ;

  fun language_title_Utt : Utt ;

-}

}

