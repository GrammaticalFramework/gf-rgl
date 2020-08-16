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
