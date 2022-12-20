--# -path=.:../abstract:../common:../prelude

-- Ilona Nowak Wintersemester 2007/08  

-- Adam Slaski, 2009, 2010 <adam.slaski@gmail.com>

-- In Polish language they aren't determiners like in english or german.

concrete StructuralLit of Structural = CatLit ** 
  open ResLit, ParadigmsLit, Prelude in {


  flags optimize=all; coding=utf8;

lin

  above_Prep = mkPrep "virš" Gen;
  after_Prep = mkPrep "po" Gen;

-- Revoir difference s et np
  all_Predet = { s=visas; np=viskas; adj=True };
  almost_AdA, almost_AdN = ss "beveik";
  although_Subj = ss "nors";
  always_AdV = ss "visada";
  and_Conj = {s1=""; s2 = "ir";  sent1=""; sent2=["ir"]};
  at_least_AdN = ss "mažiausiai";
  at_most_AdN = ss "daugiausiai";
  because_Subj = ss "todėl, kad";
  before_Prep = mkPrep "prieš" Ins;
  behind_Prep = mkPrep "už" Acc;
  between_Prep = mkPrep "tarp" Gen;
  both7and_DConj = {s1="lygiai"; s2=["ir"]; sent1="lygiai"; sent2=[", ir"]};
  but_PConj = ss "bet";
  by8agent_Prep = mkPrep "" Ins; -- possessive for passive voice (bet mano - not manęs...)
  by8means_Prep = mkPrep "naudojant" Acc; -- Is it the best option?
  can8know_VV = mkV "mokėti" "moka" "mokėjo"; -- Also (su)gebėti
  can_VV  = mkV "galėti" "gali" "galėjo";
  during_Prep  = mkPrep "per" Acc; 
  either7or_DConj = {s1="arba"; s2="arba";  sent1="arba"; sent2=[", arba"]};
  every_Det  = kiekvienasDet;
  everybody_NP = visi;
  everything_NP  = viskas;
  everywhere_Adv = { s = "visur"; advType = PronT };
  except_Prep = mkPrep "išskyrus" Acc;
  few_Det = keletasDet;
  for_Prep = mkPrep [] Dat;
  have_V2 = dirV2 (mkV "turėti" "turi" "turėjo"); 
  he_Pron  = pronJis;
  here_Adv = { s = "čia"; advType = PronT };
  here7to_Adv = { s = "čia"; advType = PronT };
  here7from_Adv = { s = "iš čia"; advType = PronT };
  how_IAdv  = ss "kaip";
  how8many_IDet = koksKiekisDet; 
  i_Pron   = pronAs MascSg;
  if_Subj    = ss "jei"; -- jeigu
  if_then_Conj = {s1="jei"; s2=[", tada"];  sent1="jei"; sent2=[", tada"]};
  in8front_Prep  = mkPrep "prieš" Acc;
  in_Prep  =  mkPrep [] Loc;
  it_Pron    = pronTai;
  language_title_Utt = ss "lietuvių kalba";
  less_CAdv = {s,sn = "ne taip" ; p,pn = "kaip" } ;
  many_Det  = daugybeDet;
  -- to replace by a rule in AdvLit
  more_CAdv = {s = "daugiau" ; pn,p = "negu"; sn="daugiau"} ;
  most_Predet = { s=visas; np={nom="dauguma"; voc="dauguma"; 
    dep=table{AccC =>"daugumą"; GenC=>"daugumos"; InsC=>"dauguma"; DatC=>"daugumai"; LocC =>"daugumoje"}; 
    p=P3; gn=FemSg ; nomType = Reg}; adj=False };
  much_Det   = daugDet;
  must_VV = mkV "privalėti" "privalo" "privalėjo"; -- Also turėti
  no_Quant = joksQuant ** { detType = NormalDet };
  no_Utt  = ss "ne";
  nobody_NP = niekasAnimNP;
  not_Predet = { s=joksQuant.s; np=viskas; adj=True };
  nothing_NP = niekasNP;
  on_Prep = mkPrep "ant" Gen; 
  only_Predet = { s=\\_=>"tik"; np=viskas; adj=True };
  or_Conj  = {s1=""; s2="ar";  sent1=""; sent2=["ar"]};
  otherwise_PConj  = ss "priešingu atveju"; -- often works better than 'kitaip' 
  part_Prep = mkPrep "iš" Gen; 
  please_Voc = ss ", prašom"; -- Or 'prašau', but 'prašom' avoids pb in the plural...
  possess_Prep  = mkPrep "" Gen; --overgenerating with pronouns
  quite_Adv = ss "gana";
  she_Pron   = pronJi;
  so_AdA = ss "taip";
  somebody_NP = kazkasAnim;
  someSg_Det = kazkiekDet;
  somePl_Det = keliDet;
  something_NP  = kas ;
  somewhere_Adv  = { s = "kažkur"; advType = PronT }; -- kur nors
  that_Quant = mkPronXas "tas" ** { detType = NormalDet };
  there_Adv = { s = "ten"; advType = PronT };
  there7to_Adv = { s = "ten"; advType = PronT };
  there7from_Adv = { s = "iš ten"; advType = PronT };
  therefore_PConj  = ss "taigi";
  they_Pron = pronJie;
  this_Quant = mkPronXis "šis" ** { detType = NormalDet };
  through_Prep  = mkPrep "per" Acc;
  to_Prep = mkPrep [] Dat; 
  too_AdA = ss "per daug"; -- per
  under_Prep = mkPrep "po" Ins;
  very_AdA = ss "labai";
  want_VV = mkV "norėti" "nori" "norėjo";
  we_Pron = pronMes;
  -- No difference Sg/Plur
  whatPl_IP = kas;
  whatSg_IP = kas;
  when_IAdv = ss "kada";
  when_Subj = ss "kai";
  where_IAdv = ss "kur";
  which_IQuant = { s = kuris.s };
  -- No difference Sg/Plur
  whoPl_IP = kasAnim;
  whoSg_IP = kasAnim;
  why_IAdv = ss "kodėl";
  with_Prep = mkPrep "su" Ins; -- or Ins without nothing (difference animate/inanimate...)
  without_Prep = mkPrep "be" Gen;
  youPl_Pron = pronJus;
  yes_Utt = ss "taip";
  youSg_Pron = pronTu MascSg;
  youPol_Pron =  pronJus;

  as_CAdv = { s,sn="taip"; p,pn="kaip"} ;

};
