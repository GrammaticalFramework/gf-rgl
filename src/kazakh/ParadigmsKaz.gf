resource ParadigmsKaz = MorphoKaz  ** open Predef, Prelude, CatKaz, ResKaz in {
oper
  regN : Str -> N   -- s;Nom;Sg
    = \form -> case form of {
		_ + "дас" => mkN001 form;
		_ + "нас" => mkN001 form;
		_ + "йыс" => mkN001 form;
		_ + "дыс" => mkN020 form;
		_ + "ніс" => mkN009 form;
		_ + "гіс" => mkN009 form;
		_ + "тіс" => mkN022 form;
		_ + "мес" => mkN009 form;
		_ + "ғат" => mkN020 form;
		_ + "нат" => mkN001 form;
		_ + "пат" => mkN001 form;
		_ + "жат" => mkN001 form;
		_ + "зат" => mkN020 form;
		_ + "ұлт" => mkN001 form;
		_ + "ілт" => mkN009 form;
		_ + "уыт" => mkN001 form;
		_ + "ірт" => mkN009 form;
		_ + "орт" => mkN020 form;
		_ + "ырт" => mkN020 form;
		_ + "шот" => mkN001 form;
		_ + "ант" => mkN001 form;
		_ + "ент" => mkN009 form;
		_ + "хит" => mkN001 form;
		_ + "нөт" => mkN009 form;
		_ + "тет" => mkN009 form;
		_ + "лет" => mkN022 form;
		_ + "ует" => mkN022 form;
		_ + "шіт" => mkN009 form;
		_ + "гіт" => mkN009 form;
		_ + "ныш" => mkN001 form;
		_ + "лыш" => mkN001 form;
		_ + "ңеш" => mkN022 form;
		_ + "піш" => mkN009 form;
		_ + "ніш" => mkN009 form;
		_ + "ліш" => mkN009 form;
		_ + "ріш" => mkN022 form;
		_ + "шам" => mkN003 form;
		_ + "нам" => mkN002 form;
		_ + "йым" => mkN003 form;
		_ + "рым" => mkN003 form;
		_ + "жым" => mkN003 form;
		_ + "ным" => mkN002 form;
		_ + "тым" => mkN002 form;
		_ + "нім" => mkN016 form;
		_ + "рім" => mkN035 form;
		_ + "дем" => mkN035 form;
		_ + "тан" => mkN003 form;
		_ + "қан" => mkN003 form;
		_ + "ран" => mkN003 form;
		_ + "жан" => mkN003 form;
		_ + "оян" => mkN003 form;
		_ + "мән" => mkN016 form;
		_ + "сін" => mkN016 form;
		_ + "кін" => mkN016 form;
		_ + "гін" => mkN035 form;
		_ + "лен" => mkN016 form;
		_ + "рен" => mkN016 form;
		_ + "йың" => mkN003 form;
		_ + "заң" => mkN003 form;
		_ + "пап" => mkN043 form;
		_ + "лып" => mkN043 form;
		_ + "қып" => mkN020 form;
		_ + "сық" => mkN015 form;
		_ + "шық" => mkN015 form;
		_ + "зақ" => mkN015 form;
		_ + "уақ" => mkN015 form;
		_ + "қақ" => mkN015 form;
		_ + "ыла" => mkN024 form;
		_ + "ола" => mkN024 form;
		_ + "рда" => mkN024 form;
		_ + "ұра" => mkN024 form;
		_ + "йра" => mkN024 form;
		_ + "аға" => mkN024 form;
		_ + "ұма" => mkN024 form;
		_ + "йна" => mkN024 form;
		_ + "хна" => mkN024 form;
		_ + "ина" => mkN024 form;
		_ + "зба" => mkN024 form;
		_ + "шқа" => mkN024 form;
		_ + "сқа" => mkN024 form;
		_ + "ьша" => mkN024 form;
		_ + "ыша" => mkN024 form;
		_ + "опа" => mkN024 form;
		_ + "ашы" => mkN005 form;
		_ + "лшы" => mkN005 form;
		_ + "яшы" => mkN005 form;
		_ + "юшы" => mkN005 form;
		_ + "озы" => mkN005 form;
		_ + "аты" => mkN024 form;
		_ + "ция" => mkN024 form;
		_ + "бар" => mkN006 form;
		_ + "уар" => mkN006 form;
		_ + "сар" => mkN006 form;
		_ + "пар" => mkN006 form;
		_ + "мыр" => mkN025 form;
		_ + "сыр" => mkN006 form;
		_ + "тыр" => mkN006 form;
		_ + "жыр" => mkN006 form;
		_ + "ңір" => mkN018 form;
		_ + "қай" => mkN006 form;
		_ + "най" => mkN025 form;
		_ + "уру" => mkN006 form;
		_ + "іру" => mkN018 form;
		_ + "жау" => mkN006 form;
		_ + "ғау" => mkN006 form;
		_ + "ұну" => mkN006 form;
		_ + "діл" => mkN018 form;
		_ + "тіл" => mkN018 form;
		_ + "ріл" => mkN018 form;
		_ + "пал" => mkN025 form;
		_ + "сал" => mkN025 form;
		_ + "иыл" => mkN025 form;
		_ + "қыл" => mkN025 form;
		_ + "жол" => mkN025 form;
		_ + "дек" => mkN009 form;
		_ + "жек" => mkN009 form;
		_ + "тек" => mkN009 form;
		_ + "рік" => mkN033 form;
		_ + "жік" => mkN009 form;
		_ + "сік" => mkN033 form;
		_ + "шік" => mkN033 form;
		_ + "пік" => mkN033 form;
		_ + "зік" => mkN033 form;
		_ + "біз" => mkN010 form;
		_ + "көз" => mkN010 form;
		_ + "сөз" => mkN010 form;
		_ + "быз" => mkN013 form;
		_ + "кше" => mkN023 form;
		_ + "рпе" => mkN023 form;
		_ + "еде" => mkN023 form;
		_ + "йде" => mkN023 form;
		_ + "өбе" => mkN023 form;
		_ + "ңге" => mkN023 form;
		_ + "кте" => mkN023 form;
		_ + "мле" => mkN023 form;
		_ + "кші" => mkN023 form;
		_ + "нші" => mkN023 form;
		_ + "зші" => mkN023 form;
		_ + "иші" => mkN023 form;
		_ + "лші" => mkN023 form;
		_ + "пші" => mkN023 form;
		_ + "ңкі" => mkN023 form;
		_ + "іс" => mkN009 form;
		_ + "ес" => mkN009 form;
		_ + "үс" => mkN009 form;
		_ + "өс" => mkN009 form;
		_ + "ит" => mkN009 form;
		_ + "өт" => mkN009 form;
		_ + "ет" => mkN009 form;
		_ + "іт" => mkN009 form;
		_ + "ст" => mkN009 form;
		_ + "ят" => mkN020 form;
		_ + "үт" => mkN022 form;
		_ + "аш" => mkN001 form;
		_ + "еш" => mkN009 form;
		_ + "іш" => mkN009 form;
		_ + "үш" => mkN009 form;
		_ + "ам" => mkN002 form;
		_ + "ым" => mkN003 form;
		_ + "ұм" => mkN003 form;
		_ + "әм" => mkN016 form;
		_ + "ем" => mkN016 form;
		_ + "зм" => mkN035 form;
		_ + "ын" => mkN003 form;
		_ + "он" => mkN003 form;
		_ + "ән" => mkN016 form;
		_ + "ін" => mkN016 form;
		_ + "үн" => mkN016 form;
		_ + "ен" => mkN016 form;
		_ + "ың" => mkN003 form;
		_ + "аң" => mkN003 form;
		_ + "оң" => mkN003 form;
		_ + "өп" => mkN022 form;
		_ + "іп" => mkN009 form;
		_ + "оп" => mkN020 form;
		_ + "еп" => mkN022 form;
		_ + "ық" => mkN020 form;
		_ + "оқ" => mkN020 form;
		_ + "ңқ" => mkN020 form;
		_ + "та" => mkN024 form;
		_ + "ша" => mkN024 form;
		_ + "ва" => mkN024 form;
		_ + "ка" => mkN024 form;
		_ + "уа" => mkN024 form;
		_ + "са" => mkN024 form;
		_ + "жа" => mkN024 form;
		_ + "қы" => mkN005 form;
		_ + "жы" => mkN005 form;
		_ + "йы" => mkN024 form;
		_ + "вр" => mkN006 form;
		_ + "ур" => mkN006 form;
		_ + "ор" => mkN006 form;
		_ + "ар" => mkN006 form;
		_ + "ыр" => mkN006 form;
		_ + "яр" => mkN006 form;
		_ + "ір" => mkN036 form;
		_ + "ұр" => mkN025 form;
		_ + "әр" => mkN036 form;
		_ + "ий" => mkN018 form;
		_ + "ей" => mkN018 form;
		_ + "ау" => mkN006 form;
		_ + "қу" => mkN006 form;
		_ + "еу" => mkN018 form;
		_ + "ел" => mkN007 form;
		_ + "іл" => mkN007 form;
		_ + "үл" => mkN007 form;
		_ + "өл" => mkN007 form;
		_ + "ек" => mkN033 form;
		_ + "үк" => mkN009 form;
		_ + "із" => mkN010 form;
		_ + "үз" => mkN010 form;
		_ + "өз" => mkN010 form;
		_ + "ез" => mkN010 form;
		_ + "аз" => mkN013 form;
		_ + "әж" => mkN010 form;
		_ + "пе" => mkN023 form;
		_ + "ке" => mkN023 form;
		_ + "ме" => mkN023 form;
		_ + "зе" => mkN023 form;
		_ + "же" => mkN023 form;
		_ + "рі" => mkN023 form;
		_ + "ні" => mkN023 form;
		_ + "сі" => mkN023 form;
		_ + "с" => mkN001 form;
		_ + "т" => mkN001 form;
		_ + "ш" => mkN001 form;
		_ + "м" => mkN016 form;
		_ + "н" => mkN003 form;
		_ + "ң" => mkN016 form;
		_ + "п" => mkN043 form;
		_ + "д" => mkN002 form;
		_ + "қ" => mkN015 form;
		_ + "х" => mkN001 form;
		_ + "а" => mkN005 form;
		_ + "ы" => mkN024 form;
		_ + "я" => mkN024 form;
		_ + "р" => mkN018 form;
		_ + "и" => mkN006 form;
		_ + "й" => mkN006 form;
		_ + "ю" => mkN006 form;
		_ + "у" => mkN025 form;
		_ + "л" => mkN025 form;
		_ + "к" => mkN022 form;
		_ + "з" => mkN013 form;
		_ + "ж" => mkN013 form;
		_ + "е" => mkN023 form;
		_ + "і" => mkN023 form;
		_ + "ә" => mkN023 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2N : Str -> Str -> N   -- s;Nom;Sg  poss;Sg;P3;Pl
    = \form1, form2 -> case <form1, form2> of {
		<_ + "ғат", _ + "ы"> => mkN001 form1;
		<_ + "нат", _ + "ы"> => mkN001 form1;
		<_ + "уыт", _ + "ы"> => mkN001 form1;
		<_ + "пат", _ + "ы"> => mkN001 form1;
		<_ + "дас", _ + "ы"> => mkN001 form1;
		<_ + "йым", _ + "ы"> => mkN002 form1;
		<_ + "тан", _ + "ы"> => mkN003 form1;
		<_ + "қан", _ + "ы"> => mkN003 form1;
		<_ + "ран", _ + "ы"> => mkN003 form1;
		<_ + "сық", _ + "ы"> => mkN015 form1;
		<_ + "шық", _ + "ы"> => mkN015 form1;
		<_ + "зба", _ + "ы"> => mkN005 form1;
		<_ + "мыр", _ + "ы"> => mkN006 form1;
		<_ + "дек", _ + "і"> => mkN033 form1;
		<_ + "тек", _ + "і"> => mkN033 form1;
		<_ + "көз", _ + "і"> => mkN010 form1;
		<_ + "ым", _ + "ы"> => mkN002 form1;
		<_ + "ын", _ + "ы"> => mkN003 form1;
		<_ + "аң", _ + "ы"> => mkN003 form1;
		<_ + "ық", _ + "ы"> => mkN015 form1;
		<_ + "та", _ + "ы"> => mkN005 form1;
		<_ + "ша", _ + "ы"> => mkN005 form1;
		<_ + "ор", _ + "ы"> => mkN006 form1;
		<_ + "ар", _ + "ы"> => mkN006 form1;
		<_ + "ыр", _ + "ы"> => mkN006 form1;
		<_ + "үл", _ + "і"> => mkN007 form1;
		<_ + "өл", _ + "і"> => mkN007 form1;
		<_ + "іш", _ + "і"> => mkN009 form1;
		<_ + "ір", _ + "і"> => mkN018 form1;
		<_ + "оп", _ + "ы"> => mkN043 form1;
		<_ + "т", _ + "і"> => mkN009 form1;
		<_ + "ш", _ + "ы"> => mkN001 form1;
		<_ + "ы", _ + "ы"> => mkN005 form1;
		<_ + "я", _ + "ы"> => mkN005 form1;
		<_ + "й", _ + "ы"> => mkN006 form1;
		<_ + "й", _ + "і"> => mkN018 form1;
		<_ + "у", _ + "ы"> => mkN006 form1;
		<_ + "к", _ + "і"> => mkN033 form1;
		<_ + "з", _ + "ы"> => mkN013 form1;
		<_ + "м", _ + "і"> => mkN016 form1;
		<_ + "е", _ + "і"> => mkN023 form1;
		<_ + "і", _ + "і"> => mkN023 form1;
		_ => regN form1
  } ;

  regV : Str -> V   -- Infinitive
    = \form -> case form of {
		_ + "ану" => mkV003 form;
		_ + "ыну" => mkV003 form;
		_ + "ону" => mkV003 form;
		_ + "ұну" => mkV003 form;
		_ + "азу" => mkV003 form;
		_ + "ызу" => mkV003 form;
		_ + "озу" => mkV003 form;
		_ + "ұзу" => mkV003 form;
		_ + "арту" => mkV001 form;
		_ + "ырту" => mkV001 form;
		_ + "орту" => mkV001 form;
		_ + "ұрту" => mkV001 form;
		_ + "лту" => mkV005 form;
		_ + "рту" => mkV007 form;
		_ + "ету" => mkV007 form;
		_ + "іту" => mkV007 form;
		_ + "үту" => mkV010 form;
		_ + "тау" => mkV006 form;
		_ + "сау" => mkV006 form;
		_ + "нау" => mkV011 form;
		_ + "қау" => mkV011 form;
		_ + "рау" => mkV026 form;
		_ + "ару" => mkV003 form;
		_ + "ыру" => mkV003 form;
		_ + "іру" => mkV023 form;
		_ + "ту" => mkV001 form;
		_ + "ау" => mkV002 form;
		_ + "лу" => mkV033 form;
		_ + "шу" => mkV010 form;
		_ + "уу" => mkV006 form;
		_ + "еу" => mkV009 form;
		_ + "су" => mkV010 form;
		_ + "бу" => mkV020 form;
		_ + "ңу" => mkV023 form;
		_ + "у" => mkV016 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2V : Str -> Str -> V   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg
    = \form1, form2 -> case <form1, form2> of {
		<_ + "ау", _ + "ін"> => mkV011 form1;
		_ => regV form1
  } ;

  reg3V : Str -> Str -> Str -> V   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg  Indicative;Pres;Progressive;Pos;P1;Pl
    = \form1, form2, form3 -> case <form1, form2, form3> of {
		_ => reg2V form1 form2
  } ;

mkN = overload {
  mkN : Str -> N = regN;   -- s;Nom;Sg
  mkN : Str -> Str -> N = reg2N   -- s;Nom;Sg  poss;Sg;P3;Pl
} ;

mkN2 = overload {
  mkN2 : N -> N2 = \n -> lin N2 n ** {c2=genPrep};
  mkN2 : N -> Prep -> N2 = \n,p -> lin N2 n ** {c2=p};
} ;

mkPN : Str -> PN = \s -> lin PN {s=s} ;
mkLN : Str -> LN = \s -> lin LN {s=s} ;
mkGN : Str -> GN = \s -> lin GN {s=s} ;
mkSN : Str -> SN = \s -> lin SN {s=s} ;
mkPron : (nom,acc,dat,loc,gen,instr,ablat : Str) -> Person -> Number -> Pron =
  \nom,acc,dat,loc,gen,instr,ablat,p,n -> lin Pron {
    s = table {Nom=>nom; Acc=>acc; Dat=>dat; Loc=>loc; Gen=>gen; Instr=>instr; Ablat=>ablat};
    a = {p=p; n=n}
  } ;

mkV = overload {
  mkV : Str -> V = regV;   -- Infinitive
  mkV : Str -> Str -> V = reg2V;   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg
  mkV : Str -> Str -> Str -> V = reg3V   -- Infinitive  Indicative;Pres;Progressive;Pos;P1;Sg  Indicative;Pres;Progressive;Pos;P1;Pl
} ;

mkV2 = overload {
  mkV2 : V -> V2 = \v -> lin V2 v ** {c2=accPrep} ;
  mkV2 : V -> Prep -> V2 = \v,p -> lin V2 v ** {c2=p} ;
} ;

mkVV : V -> VV = \v -> lin VV v ;
mkVS : V -> VS = \v -> lin VS v ;
mkVQ : V -> VQ = \v -> lin VQ v ;
mkVA : V -> VA = \v -> lin VA v ;

mkV2V = overload {
  mkV2V : V -> V2V = \v -> lin V2V v ** {c2=accPrep; c3=noPrep} ;
  mkV2V : V -> Prep -> Prep -> V2V = \v,p2,p3 -> lin V2V v ** {c2=p2; c3=p3} ;
} ;

mkV2S = overload {
  mkV2S : V -> V2S = \v -> lin V2S v ** {c2=accPrep; c3=noPrep} ;
  mkV2S : V -> Prep -> Prep -> V2S = \v,p2,p3 -> lin V2S v ** {c2=p2; c3=p3} ;
} ;

mkV2Q = overload {
  mkV2Q : V -> V2Q = \v -> lin V2Q v ** {c2=accPrep; c3=noPrep} ;
  mkV2Q : V -> Prep -> Prep -> V2Q = \v,p2,p3 -> lin V2Q v ** {c2=p2; c3=p3} ;
} ;

mkV2A = overload {
  mkV2A : V -> V2A = \v -> lin V2A v ** {c2=accPrep; c3=noPrep} ;
  mkV2A : V -> Prep -> Prep -> V2A = \v,p2,p3 -> lin V2A v ** {c2=p2; c3=p3} ;
} ;

mkV3 = overload {
  mkV3 : V -> V3 = \v -> lin V3 v ** {c2=datPrep; c3=accPrep} ;
  mkV3 : V -> Prep -> Prep -> V3 = \v,p2,p3 -> lin V3 v ** {c2=p2; c3=p3} ;
} ;

mkA : Str -> A = \s -> lin A {s=s} ;
mkA2 : A -> A2 = \a -> lin A2 a ** {c2=datPrep} ;

mkAdv : Str -> Adv = \s -> lin Adv {s=s} ;
mkAdV : Str -> AdV = \s -> lin AdV {s=s} ;
mkAdA : Str -> AdA = \s -> lin AdA {s=s} ;
mkAdN : Str -> AdN = \s -> lin AdN {s=s} ;

mkInterj : Str -> Interj = \s -> lin Interj {s=s} ;

mkVoc : Str -> Voc = \s -> lin Voc {s=s} ;

mkPrep = overload {
  mkPrep : Str -> Prep = \s -> lin Prep {s=s; c=Nom} ;
  mkPrep : Str -> Case -> Prep = \s,c -> lin Prep {s=s; c=c}
} ;
noPrep : Prep = lin Prep {s=""; c=Nom} ;
accPrep : Prep = lin Prep {s=""; c=Acc} ;
datPrep : Prep = lin Prep {s=""; c=Dat} ;
genPrep : Prep = lin Prep {s=""; c=Gen} ;

}
