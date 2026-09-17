concrete StructuralSqi of Structural = CatSqi ** open ResSqi, ParadigmsSqi in {

lin i_Pron = mkPron "unë" "mua" "mua" "meje" "më" "më" (GSg Masc) P1 ;
lin youSg_Pron = mkPron "ti" "ty" "ty" "teje" "të" "të" (GSg Masc) P2 ;
lin he_Pron = mkPron "ai" "atë" "atij" "atij" "e" "i" (GSg Masc) P3 ;
lin she_Pron = mkPron "ajo" "atë" "asaj" "asaj" "e" "i" (GSg Fem) P3 ;
lin it_Pron = mkPron "ai" "atë" "atij" "atij" "e" "i" (GSg Masc) P3 ;
lin we_Pron = mkPron "ne" "ne" "neve" "nesh" "na" "na" GPl P1 ;
lin youPl_Pron = mkPron "ju" "ju" "juve" "jush" "ju" "ju" GPl P2 ;
lin they_Pron = mkPron "ata" "ata" "atyre" "atyre" "i" "u" GPl P3 ;
lin this_Quant = mkQuant "ky"    "këta"     "kjo"   "këto"
                         "këtë"  "këtyre"   "këtë"  "këtyre"
                         "këtij" "këtyre"   "kësaj" "këtyre"
                         "këtij" "këtyre"   "kësaj" "këtyre" ;
lin that_Quant = mkQuant "ai"   "ata"   "ajo"  "ato"
                         "atë"  "ata"   "atë"  "ato"
                         "atij" "atyre" "asaj" "atyre"
                         "atij" "atyre" "asaj" "atyre" ;

lin
  youPol_Pron = youPl_Pron ;

  and_Conj = mkConj "dhe" ;
  or_Conj = mkConj "ose" ;
  if_then_Conj = mkConj "nëse" ;

  above_Prep = mkPrep "mbi" ;
  after_Prep = mkPrep "pas" ;
  before_Prep = mkPrep "para" ;
  behind_Prep = mkPrep "prapa" ;
  between_Prep = mkPrep "midis" ;
  by8agent_Prep = mkPrep "nga" ;
  by8means_Prep = mkPrep "me" ;
  during_Prep = mkPrep "gjatë" ;
  except_Prep = mkPrep "përveç" ;
  for_Prep = mkPrep "për" ;
  from_Prep = mkPrep "nga" ;
  in8front_Prep = mkPrep "përpara" ;
  in_Prep = mkPrep "në" ;
  on_Prep = mkPrep "mbi" ;
  part_Prep = mkPrep "nga" ;
  possess_Prep = mkPrep "i" ;
  through_Prep = mkPrep "përmes" ;
  to_Prep = mkPrep "te" ;
  under_Prep = mkPrep "nën" ;
  with_Prep = mkPrep "me" ;
  without_Prep = mkPrep "pa" ;

  although_Subj = mkSubj "megjithëse" ;
  because_Subj = mkSubj "sepse" ;
  if_Subj = mkSubj "nëse" ;
  that_Subj = mkSubj "që" ;
  when_Subj = mkSubj "kur" ;

  always_AdV = mkAdV "gjithmonë" ;
  almost_AdA = mkAdA "pothuajse" ;
  so_AdA = mkAdA "kaq" ;
  too_AdA = mkAdA "tepër" ;
  very_AdA = mkAdA "shumë" ;
  almost_AdN = mkAdN "pothuajse" ;
  at_least_AdN = mkAdN "të paktën" ;
  at_most_AdN = mkAdN "më së shumti" ;
  more_CAdv = {s="më";p="se"} ;
  less_CAdv = {s="më pak";p="se"} ;
  as_CAdv = {s="po aq";p="sa"} ;

  here_Adv = mkAdv "këtu" ;
  there_Adv = mkAdv "atje" ;
  everywhere_Adv = mkAdv "kudo" ;
  somewhere_Adv = mkAdv "diku" ;
  here7from_Adv = mkAdv "nga këtu" ;
  here7to_Adv = mkAdv "deri këtu" ;
  there7from_Adv = mkAdv "nga atje" ;
  there7to_Adv = mkAdv "deri atje" ;
  quite_Adv = mkAdv "mjaft" ;

  how_IAdv = mkIAdv "si" ;
  how8much_IAdv = mkIAdv "sa" ;
  when_IAdv = mkIAdv "kur" ;
  where_IAdv = mkIAdv "ku" ;
  why_IAdv = mkIAdv "pse" ;
  which_IQuant = {s=table {Masc=>"cili";Fem=>"cila"}} ;
  how8many_IDet = {s=\\_=>"sa";n=Pl} ;
  whatSg_IP = {s="çfarë";a={gn=GSg Masc;p=P3}} ;
  whatPl_IP = {s="çfarë";a={gn=GPl;p=P3}} ;
  whoSg_IP = {s="kush";a={gn=GSg Masc;p=P3}} ;
  whoPl_IP = {s="cilët";a={gn=GPl;p=P3}} ;

  all_Predet = {s="të gjithë"} ;
  most_Predet = {s="shumica e"} ;
  not_Predet = {s="jo"} ;
  only_Predet = {s="vetëm"} ;

  every_Det = {s=\\_,_=>"çdo";post=\\_,_,_=>[];sp=Indef;n=Sg} ;
  few_Det = {s=\\_,_=>"pak";post=\\_,_,_=>[];sp=Indef;n=Pl} ;
  many_Det = {s=\\_,_=>"shumë";post=\\_,_,_=>[];sp=Indef;n=Pl} ;
  much_Det = {s=\\_,_=>"shumë";post=\\_,_,_=>[];sp=Indef;n=Sg} ;
  someSg_Det = {s=\\_,_=>"një";post=\\_,_,_=>[];sp=Indef;n=Sg} ;
  somePl_Det = {s=\\_,_=>"disa";post=\\_,_,_=>[];sp=Indef;n=Pl} ;
  no_Quant = mkQuant "asnjë" ;

  everybody_NP = {s=\\_=>"të gjithë";a={gn=GPl;p=P3}} ;
  everything_NP = {s=\\_=>"gjithçka";a={gn=GSg Masc;p=P3}} ;
  nobody_NP = {s=\\_=>"askush";a={gn=GSg Masc;p=P3}} ;
  nothing_NP = {s=\\_=>"asgjë";a={gn=GSg Masc;p=P3}} ;
  somebody_NP = {s=\\_=>"dikush";a={gn=GSg Masc;p=P3}} ;
  something_NP = {s=\\_=>"diçka";a={gn=GSg Masc;p=P3}} ;

  can_VV = mkVV (mkV "mund") ;
  can8know_VV = mkVV (irregV "di" "di" "di" "dimë" "dini" "dinë" "di" "dini" "ditur") ;
  must_VV = mkVV (mkV "duhet") ;
  want_VV = mkVV (irregV "dua" "do" "do" "duam" "doni" "duan" "duaj" "doni" "dashur") ;
  have_V2 = mkV2 (irregV "kam" "ke" "ka" "kemi" "keni" "kanë" "ki" "kini" "pasur") ;

  but_PConj = {s="por"} ;
  otherwise_PConj = {s="përndryshe"} ;
  therefore_PConj = {s="prandaj"} ;
  please_Voc = {s="ju lutem"} ;
  yes_Utt = {s="po"} ;
  no_Utt = {s="jo"} ;
  language_title_Utt = {s="shqip"} ;
}
