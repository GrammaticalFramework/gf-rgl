resource MorphoRus = ResRus ** open Prelude in {
flags coding=utf8 ;
oper
  to_exist = guessVerbForms Imperfective Intransitive "существовать" "существую" "существует";
  everybody = pronFormsPronoun vse ;
  everything = pronFormsPronoun vse_ina ;
  what_sg = doChPron "ч" (Ag (GSg Neut) P3) Inanimate ;
  what_pl = doChPron "ч" (Ag GPl P3) Inanimate ;
  something = pronFormsPronoun ((appendToIP what_sg (BIND ++ "-то")) ** {nPrefix=False}) ;
  nothing = pronFormsPronoun ((doChPron "нич" (Ag (GSg Neut) P3) Inanimate) ** {nPrefix=False}) ;
  nechto = pronFormsPronoun ((doChPron "неч" (Ag (GSg Neut) P3) Inanimate) ** {nPrefix=False}) ;
  who_sg = doKPron "к" (Ag (GSg Masc) P3) Animate ;
  who_pl = doKPron "к" (Ag GPl P3) Animate ;
  somebody = pronFormsPronoun ((appendToIP who_sg (BIND ++ "-то")) ** {nPrefix=False}) ;
  nobody = pronFormsPronoun ((doKPron "ник" (Ag (GSg Masc) P3) Animate) ** {nPrefix=False}) ;
  anybody = pronFormsPronoun ((doKPron "нек" (Ag (GSg Masc) P3) Animate) ** {nPrefix=False}) ;
  such = adjFormsAdjective (makeAdjectiveForms "такой" "" "3b" PreferFull) ;

  poltora : DetTable
    = \\g, anim, cas =>
      case <cas, g> of {
        <(Nom|VocRus|Acc), Fem> => "полторы" ;
        <(Nom|VocRus|Acc), _> => "полтора" ;
        _ => "полутора"
      } ;

  poltorasta : DetTable
    = \\g, anim, cas =>
      case cas of {
        (Nom|VocRus|Acc) => "полтораста" ;
        _ => "полутораста"
      } ;

  -- collective numerals

  oba : DetTable
    = \\g, anim, cas => case g of {
      Fem => case <cas, anim> of {
        <Acc, Animate> => "обеих" ;
        <Nom|Acc|VocRus, _> => "обе" ;
        <Gen|Ptv|Loc|Pre, _> => "обеих" ;
        <Dat, _> => "обеим" ;
        <Ins, _> => "обеими"
        } ;
      _ => case <cas, anim> of {
        <Acc, Animate> => "обоих" ;
        <Nom|Acc|VocRus, _> => "оба" ;
        <Gen|Ptv|Loc|Pre, _> => "обоих" ;
        <Dat, _> => "обоим" ;
        <Ins, _> => "обоими"
        }
      } ;

  colnum2_3 : Str -> DetTable
    = \word ->
      let stem = Predef.tk 1 word in
      \\g, anim, cas => case <cas, anim> of {
        <Acc, Animate> => stem + "их" ;
        <Nom|Acc|VocRus, _> => word ;
        <Gen|Ptv|Loc|Pre, _> => stem + "их" ;
        <Dat, _> => stem + "им" ;
        <Ins, _> => stem + "ими"
        } ;

  colnum4_10 : Str -> DetTable
    = \word ->
      let stem = Predef.tk 1 word in
      \\g, anim, cas => case <cas, anim> of {
        <Acc, Animate> => stem + "ых" ;
        <Acc, Inanimate> => word ;
        <Nom|VocRus, _> => word ;
        <Gen|Ptv|Loc|Pre, _> => stem + "ых" ;
        <Dat, _> => stem + "ым" ;
        <Ins, _> => stem + "ыми"
        } ;

  dvoe : DetTable
    = colnum2_3 "двое" ;

  troe : DetTable
    = colnum2_3 "трое" ;

  chetvero : DetTable
    = colnum4_10 "четверо" ;

  pjatero : DetTable
    = colnum4_10 "пятеро" ;

  shestero : DetTable
    = colnum4_10 "шестеро" ;

  semero : DetTable
    = colnum4_10 "семеро" ;

  vosqmero : DetTable
    = colnum4_10 "восьмеро" ;

  devjatero : DetTable
    = colnum4_10 "девятеро" ;

  desjatero : DetTable
    = colnum4_10 "десятеро" ;

  stolqko : DetTable
    = colnum2_3 "столько" ;

  skolqko : DetTable
    = colnum2_3 "сколько" ;

  neskolqko : DetTable
    = colnum2_3 "несколько" ;


-- Situations, when prepositions are modified (approximate, full rules may be much more complex)
  sconsonant : pattern Str = #(("с"|"з"|"ж"|"ш"|"С"|"З"|"Ж"|"Ш") +
               ("б"|"в"|"г"|"д"|"ж"|"з"|"й"|"к"|"л"|"м"|"н"|"п"|"р"|"с"|"т"|"ф"|"х"|"ц"|"ч"|"ш"|"щ" |
                "Б"|"В"|"Г"|"Д"|"Ж"|"З"|"Й"|"К"|"Л"|"М"|"Н"|"П"|"Р"|"С"|"Т"|"Ф"|"Х"|"Ц"|"Ч"|"Ш"|"Щ")) ;
  vconsonant : pattern Str = #(("в"|"ф"|"В"|"Ф") +
               ("б"|"в"|"г"|"д"|"ж"|"з"|"й"|"к"|"л"|"м"|"н"|"п"|"р"|"с"|"т"|"ф"|"х"|"ц"|"ч"|"ш"|"щ" |
                "Б"|"В"|"Г"|"Д"|"Ж"|"З"|"Й"|"К"|"Л"|"М"|"Н"|"П"|"Р"|"С"|"Т"|"Ф"|"Х"|"Ц"|"Ч"|"Ш"|"Щ")) ;
  s_prep_mod : Str = pre {
    #sconsonant => "со" ;
    ("льв"|"льд"|"льн"|"лба"|"мха"|"рва"|"рта"|"лбом"|"мхом"|"рвом"|"ртом"|"мной"|"мною") => "со" ;
    ("вш"|"всем"|"всех"|"всей"|"всею"|"всег"|"всяк"|"всяческ"|"вторник"|"втор"|"многи"|"многом"|"многог") => "со" ;
    ("щ"|"Щ") => "со" ;
    _ => "с"
    } ;
  v_prep_mod : Str = pre {#vconsonant => "во" ; ("мног"|"множ"|"все"|"весь"|"имя"|"мне") => "во" ; _ => "в"} ;
  k_prep_dat_mod : Str = pre {
    ("льву"|"льду"|"льну"|"лбу"|"мху"|"рву"|"ржи"|"рту"|"всякому"|"всему"|"всяческому"|"вторнику"|"второму"|"многому") => "ко" ;
    _ => "к"
    } ;
  ot_prep_gen_mod : Str = pre {
    ("льва"|"льда"|"льна"|"лба"|"мха"|"рва"|"ржи"|"рта") => "ото" ;
    _ => "от"
    } ;
  pod_prep_mod : Str = pre {
    ("львом"|"льдом"|"льном"|"лбом"|"мхом"|"рвом"|"ртом") => "подо" ;
    _ => "под"
    } ;
  above_prep_ins_mod : Str = pre {
    ("львом"|"льдом"|"льном"|"лбом"|"мхом"|"рвом"|"ртом") => "надо" ;
    _ => "над"
    } ;
  o_prep_pre_mod : Str = pre { -- rules are approximate
    ("всем"|"всём"|"мне"|"всех"|"всей") => "обо" ;
    ("а"|"и"|"о"|"у"|"э"|"А"|"И"|"О"|"У"|"Э") => "об" ;
    _ => "о"} ;
}
