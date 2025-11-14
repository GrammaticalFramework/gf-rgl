resource ParadigmsFao = MorphoFao  ** open Predef, Prelude, CatFao, ResFao in {
oper
  regN : Str -> N   -- s;Indef;Sg;Nom
    = \form -> case form of {
		_ + "aður" => mkN032 form;
		_ + "eki" => mkN025 form;
		_ + "ski" => mkN014 form;
		_ + "ýki" => mkN002 form;
		_ + "rki" => mkN014 form;
		_ + "lki" => mkN014 form;
		_ + "øki" => mkN014 form;
		_ + "øri" => mkN002 form;
		_ + "vri" => mkN021 form;
		_ + "lri" => mkN031 form;
		_ + "yri" => mkN079 form;
		_ + "ldi" => mkN031 form;
		_ + "dni" => mkN002 form;
		_ + "gni" => mkN002 form;
		_ + "vni" => mkN021 form;
		_ + "avi" => mkN021 form;
		_ + "rvi" => mkN031 form;
		_ + "yvi" => mkN031 form;
		_ + "ugi" => mkN021 form;
		_ + "ggi" => mkN040 form;
		_ + "rgi" => mkN014 form;
		_ + "øpi" => mkN002 form;
		_ + "ýpi" => mkN031 form;
		_ + "ppi" => mkN031 form;
		_ + "mli" => mkN021 form;
		_ + "pli" => mkN031 form;
		_ + "æli" => mkN031 form;
		_ + "iði" => mkN031 form;
		_ + "æði" => mkN031 form;
		_ + "ýði" => mkN031 form;
		_ + "eði" => mkN104 form;
		_ + "ysi" => mkN002 form;
		_ + "lsi" => mkN031 form;
		_ + "esi" => mkN031 form;
		_ + "æmi" => mkN002 form;
		_ + "ami" => mkN021 form;
		_ + "ømi" => mkN031 form;
		_ + "rmi" => mkN079 form;
		_ + "sur" => mkN003 form;
		_ + "øur" => mkN009 form;
		_ + "ýur" => mkN009 form;
		_ + "par" => mkN008 form;
		_ + "gar" => mkN015 form;
		_ + "tar" => mkN019 form;
		_ + "mar" => mkN159 form;
		_ + "fer" => mkN008 form;
		_ + "ter" => mkN019 form;
		_ + "tør" => mkN015 form;
		_ + "dir" => mkN133 form;
		_ + "ørr" => mkN139 form;
		_ + "arn" => mkN004 form;
		_ + "ørn" => mkN018 form;
		_ + "agn" => mkN008 form;
		_ + "ogn" => mkN007 form;
		_ + "egn" => mkN008 form;
		_ + "pan" => mkN007 form;
		_ + "ran" => mkN008 form;
		_ + "ian" => mkN008 form;
		_ + "ton" => mkN008 form;
		_ + "lon" => mkN019 form;
		_ + "ein" => mkN019 form;
		_ + "min" => mkN045 form;
		_ + "vín" => mkN019 form;
		_ + "ekn" => mkN019 form;
		_ + "gun" => mkN024 form;
		_ + "ødn" => mkN018 form;
		_ + "jún" => mkN115 form;
		_ + "lak" => mkN008 form;
		_ + "bak" => mkN049 form;
		_ + "ark" => mkN004 form;
		_ + "ørk" => mkN018 form;
		_ + "tsk" => mkN007 form;
		_ + "pik" => mkN008 form;
		_ + "eik" => mkN015 form;
		_ + "økk" => mkN018 form;
		_ + "ekk" => mkN068 form;
		_ + "ikk" => mkN068 form;
		_ + "úkk" => mkN163 form;
		_ + "røk" => mkN121 form;
		_ + "øgg" => mkN007 form;
		_ + "agg" => mkN091 form;
		_ + "org" => mkN015 form;
		_ + "log" => mkN049 form;
		_ + "ald" => mkN004 form;
		_ + "old" => mkN007 form;
		_ + "rgd" => mkN007 form;
		_ + "und" => mkN019 form;
		_ + "and" => mkN096 form;
		_ + "ond" => mkN117 form;
		_ + "ødd" => mkN088 form;
		_ + "arð" => mkN004 form;
		_ + "urð" => mkN006 form;
		_ + "ørð" => mkN018 form;
		_ + "lið" => mkN027 form;
		_ + "nið" => mkN027 form;
		_ + "ráð" => mkN027 form;
		_ + "jal" => mkN004 form;
		_ + "gal" => mkN038 form;
		_ + "eil" => mkN006 form;
		_ + "fil" => mkN099 form;
		_ + "sól" => mkN007 form;
		_ + "egl" => mkN015 form;
		_ + "øll" => mkN078 form;
		_ + "ell" => mkN078 form;
		_ + "lat" => mkN004 form;
		_ + "ikt" => mkN007 form;
		_ + "átt" => mkN137 form;
		_ + "itt" => mkN087 form;
		_ + "ýtt" => mkN087 form;
		_ + "att" => mkN087 form;
		_ + "uft" => mkN007 form;
		_ + "bót" => mkN007 form;
		_ + "ist" => mkN008 form;
		_ + "øst" => mkN008 form;
		_ + "jøt" => mkN008 form;
		_ + "eit" => mkN015 form;
		_ + "rát" => mkN008 form;
		_ + "pet" => mkN015 form;
		_ + "álp" => mkN007 form;
		_ + "upp" => mkN015 form;
		_ + "alv" => mkN004 form;
		_ + "eyv" => mkN006 form;
		_ + "úgv" => mkN132 form;
		_ + "yga" => mkN044 form;
		_ + "oka" => mkN012 form;
		_ + "ina" => mkN012 form;
		_ + "mla" => mkN012 form;
		_ + "vja" => mkN012 form;
		_ + "tsj" => mkN015 form;
		_ + "tos" => mkN026 form;
		_ + "jús" => mkN026 form;
		_ + "lús" => mkN047 form;
		_ + "mús" => mkN047 form;
		_ + "bus" => mkN026 form;
		_ + "ins" => mkN026 form;
		_ + "fræ" => mkN027 form;
		_ + "omb" => mkN117 form;
		_ + "ði" => mkN025 form;
		_ + "fi" => mkN002 form;
		_ + "ai" => mkN015 form;
		_ + "ar" => mkN004 form;
		_ + "ðr" => mkN006 form;
		_ + "er" => mkN046 form;
		_ + "úr" => mkN007 form;
		_ + "or" => mkN008 form;
		_ + "yr" => mkN008 form;
		_ + "ør" => mkN008 form;
		_ + "ár" => mkN019 form;
		_ + "ór" => mkN019 form;
		_ + "ýr" => mkN019 form;
		_ + "ir" => mkN053 form;
		_ + "ír" => mkN019 form;
		_ + "ær" => mkN034 form;
        _ + "æv" => mkN034 form;
		_ + "rr" => mkN080 form;
		_ + "rn" => mkN008 form;
		_ + "vn" => mkN004 form;
		_ + "gn" => mkN018 form;
		_ + "tn" => mkN004 form;
		_ + "in" => mkN008 form;
		_ + "ín" => mkN008 form;
		_ + "yn" => mkN008 form;
		_ + "nn" => mkN051 form;
		_ + "ún" => mkN019 form;
		_ + "ýn" => mkN019 form;
		_ + "sn" => mkN019 form;
		_ + "án" => mkN019 form;
		_ + "pn" => mkN019 form;
		_ + "ak" => mkN004 form;
		_ + "ík" => mkN006 form;
		_ + "sk" => mkN008 form;
		_ + "ðk" => mkN008 form;
		_ + "kk" => mkN015 form;
		_ + "øk" => mkN018 form;
		_ + "ók" => mkN061 form;
		_ + "ag" => mkN004 form;
		_ + "gg" => mkN063 form;
		_ + "óg" => mkN015 form;
		_ + "rg" => mkN019 form;
		_ + "ig" => mkN019 form;
		_ + "og" => mkN027 form;
		_ + "ld" => mkN019 form;
		_ + "dd" => mkN076 form;
		_ + "vd" => mkN118 form;
		_ + "að" => mkN004 form;
		_ + "rð" => mkN019 form;
		_ + "oð" => mkN027 form;
		_ + "al" => mkN008 form;
		_ + "il" => mkN045 form;
		_ + "ll" => mkN085 form;
		_ + "ul" => mkN024 form;
		_ + "yl" => mkN046 form;
		_ + "øl" => mkN075 form;
		_ + "el" => mkN092 form;
		_ + "at" => mkN015 form;
		_ + "kt" => mkN015 form;
		_ + "tt" => mkN015 form;
		_ + "ft" => mkN112 form;
		_ + "ót" => mkN061 form;
		_ + "lt" => mkN008 form;
		_ + "nt" => mkN008 form;
		_ + "øt" => mkN015 form;
		_ + "mt" => mkN008 form;
		_ + "yt" => mkN015 form;
		_ + "vt" => mkN015 form;
		_ + "ít" => mkN015 form;
		_ + "rp" => mkN004 form;
		_ + "lp" => mkN015 form;
		_ + "pp" => mkN108 form;
		_ + "rv" => mkN004 form;
		_ + "lv" => mkN008 form;
		_ + "yv" => mkN008 form;
		_ + "av" => mkN008 form;
		_ + "øv" => mkN018 form;
		_ + "ív" => mkN019 form;
		_ + "gv" => mkN041 form;
		_ + "ev" => mkN046 form;
		_ + "lf" => mkN008 form;
		_ + "ím" => mkN008 form;
		_ + "am" => mkN008 form;
		_ + "mm" => mkN054 form;
		_ + "po" => mkN008 form;
		_ + "no" => mkN015 form;
		_ + "sj" => mkN026 form;
		_ + "ós" => mkN015 form;
		_ + "ks" => mkN015 form;
		_ + "as" => mkN103 form;
		_ + "ás" => mkN100 form;
		_ + "es" => mkN092 form;
		_ + "øs" => mkN139 form;
		_ + "i" => mkN001 form;
		_ + "r" => mkN010 form;
		_ + "n" => mkN015 form;
		_ + "k" => mkN019 form;
		_ + "g" => mkN006 form;
		_ + "d" => mkN015 form;
		_ + "ð" => mkN015 form;
		_ + "l" => mkN019 form;
		_ + "t" => mkN019 form;
		_ + "p" => mkN019 form;
		_ + "a" => mkN005 form;
		_ + "y" => mkN008 form;
		_ + "ó" => mkN007 form;
		_ + "ý" => mkN008 form;
		_ + "ø" => mkN008 form;
		_ + "f" => mkN077 form;
		_ + "m" => mkN019 form;
		_ + "o" => mkN019 form;
		_ + "j" => mkN105 form;
		_ + "s" => mkN028 form;
		_ + "á" => mkN015 form;
		_ + "í" => mkN019 form;
		_ + "u" => mkN019 form;
		_ + "e" => mkN027 form;
		_ + "æ" => mkN022 form;
		_ + "b" => mkN096 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2N : Str -> Str -> N   -- s;Indef;Sg;Nom  s;Indef;Pl;Dat
    = \form1, form2 -> case <form1, form2> of {
		<_ + "ski", _ + "kum"> => mkN001 form1;
		<_ + "ugi", _ + "gum"> => mkN001 form1;
		<_ + "ggi", _ + "gum"> => mkN001 form1;
		<_ + "sur", _ + "num"> => mkN058 form1;
		<_ + "ald", _ + "num"> => mkN008 form1;
		<_ + "eyv", _ + "num"> => mkN008 form1;
		<_ + "ist", _ + "tum"> => mkN015 form1;
		<_ + "ður", _ + "ðum"> => mkN009 form1;
		<_ + "ður", _ + "rum"> => mkN127 form1;
		<_ + "rki", _ + "num"> => mkN021 form1;
		<_ + "agg", _ + "num"> => mkN063 form1;
		<_ + "ekk", _ + "num"> => mkN163 form1;
		<_ + "eki", _ + "m"> => mkN001 form1;
		<_ + "agn", _ + "m"> => mkN004 form1;
		<_ + "pan", _ + "m"> => mkN015 form1;
		<_ + "álp", _ + "m"> => mkN015 form1;
		<_ + "ogn", _ + "m"> => mkN015 form1;
		<_ + "sól", _ + "m"> => mkN015 form1;
		<_ + "átt", _ + "i"> => mkN007 form1;
		<_ + "ist", _ + "i"> => mkN007 form1;
		<_ + "rki", _ + "i"> => mkN025 form1;
		<_ + "øll", _ + "m"> => mkN018 form1;
		<_ + "lið", _ + "m"> => mkN019 form1;
		<_ + "iði", _ + "i"> => mkN025 form1;
		<_ + "ði", _ + "num"> => mkN021 form1;
		<_ + "ar", _ + "num"> => mkN008 form1;
		<_ + "al", _ + "lum"> => mkN004 form1;
		<_ + "lt", _ + "tum"> => mkN019 form1;
		<_ + "sk", _ + "kum"> => mkN015 form1;
		<_ + "ót", _ + "num"> => mkN008 form1;
		<_ + "il", _ + "jum"> => mkN092 form1;
		<_ + "ir", _ + "rum"> => mkN019 form1;
		<_ + "gv", _ + "vum"> => mkN148 form1;
		<_ + "gv", _ + "um"> => mkN102 form1;
		<_ + "ði", _ + "m"> => mkN001 form1;
		<_ + "rð", _ + "i"> => mkN007 form1;
		<_ + "tt", _ + "i"> => mkN007 form1;
		<_ + "ft", _ + "i"> => mkN007 form1;
		<_ + "ld", _ + "i"> => mkN075 form1;
		<_ + "a", _ + "aum"> => mkN019 form1;
		<_ + "y", _ + "yum"> => mkN006 form1;
		<_ + "ð", _ + "num"> => mkN049 form1;
		<_ + "t", _ + "num"> => mkN008 form1;
		<_ + "r", _ + "rum"> => mkN042 form1;
		<_ + "r", _ + "jum"> => mkN150 form1;
		<_ + "æ", _ + "æum"> => mkN027 form1;
		<_ + "s", _ + "num"> => mkN026 form1;
		<_ + "i", _ + "i"> => mkN025 form1;
		<_ + "a", _ + "i"> => mkN012 form1;
		<_ + "g", _ + "i"> => mkN007 form1;
		<_ + "d", _ + "i"> => mkN007 form1;
		<_ + "ð", _ + "i"> => mkN007 form1;
		<_ + "k", _ + "i"> => mkN007 form1;
		_ => regN form1
  } ;

  regA : Str -> A   -- s;Masc;Sg;Nom
    = \form -> case form of {
		_ + "dur" => mkA001 form;
		_ + "tur" => mkA003 form;
		_ + "ður" => mkA010 form;
		_ + "pur" => mkA004 form;
		_ + "sur" => mkA008 form;
		_ + "mur" => mkA016 form;
		_ + "áur" => mkA017 form;
		_ + "íur" => mkA017 form;
		_ + "óur" => mkA017 form;
		_ + "jur" => mkA033 form;
		_ + "il" => mkA035 form;
		_ + "in" => mkA009 form;
		_ + "ur" => mkA007 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2A : Str -> Str -> A   -- s;Masc;Sg;Nom  s;Masc;Sg;Dat
    = \form1, form2 -> case <form1, form2> of {
		_ => regA form1
  } ;

  regV : Str -> V   -- Nonfinite
    = \form -> case form of {
		_ + "erja" => mkV011 form;
		_ + "ala" => mkV046 form;
		_ + "øla" => mkV009 form;
		_ + "gla" => mkV009 form;
		_ + "æla" => mkV009 form;
		_ + "íla" => mkV020 form;
		_ + "ula" => mkV088 form;
		_ + "lda" => mkV056 form;
		_ + "úka" => mkV044 form;
		_ + "aka" => mkV054 form;
		_ + "eka" => mkV028 form;
		_ + "mba" => mkV009 form;
		_ + "íða" => mkV014 form;
		_ + "rða" => mkV108 form;
		_ + "nna" => mkV039 form;
		_ + "vna" => mkV009 form;
		_ + "ina" => mkV009 form;
		_ + "ína" => mkV014 form;
		_ + "yna" => mkV009 form;
		_ + "æna" => mkV020 form;
		_ + "ýna" => mkV009 form;
		_ + "øna" => mkV020 form;
		_ + "una" => mkV021 form;
		_ + "iga" => mkV086 form;
		_ + "ega" => mkV075 form;
		_ + "íga" => mkV014 form;
		_ + "uga" => mkV029 form;
		_ + "nga" => mkV047 form;
		_ + "ðja" => mkV012 form;
		_ + "kja" => mkV077 form;
		_ + "lja" => mkV042 form;
		_ + "mja" => mkV042 form;
		_ + "lsa" => mkV020 form;
		_ + "æsa" => mkV020 form;
		_ + "ysa" => mkV020 form;
		_ + "ýsa" => mkV020 form;
		_ + "ósa" => mkV050 form;
		_ + "esa" => mkV071 form;
		_ + "sta" => mkV037 form;
		_ + "tta" => mkV037 form;
		_ + "áta" => mkV053 form;
		_ + "íta" => mkV014 form;
		_ + "óta" => mkV019 form;
		_ + "yta" => mkV020 form;
		_ + "ýta" => mkV020 form;
		_ + "eta" => mkV023 form;
		_ + "øta" => mkV020 form;
		_ + "fta" => mkV037 form;
		_ + "øra" => mkV009 form;
		_ + "ýra" => mkV009 form;
		_ + "yra" => mkV009 form;
		_ + "æra" => mkV009 form;
		_ + "íra" => mkV009 form;
		_ + "gva" => mkV040 form;
		_ + "ava" => mkV058 form;
		_ + "ova" => mkV099 form;
		_ + "eva" => mkV028 form;
		_ + "íva" => mkV014 form;
		_ + "yva" => mkV009 form;
		_ + "øva" => mkV009 form;
		_ + "rpa" => mkV018 form;
		_ + "ópa" => mkV021 form;
		_ + "epa" => mkV028 form;
		_ + "úpa" => mkV044 form;
		_ + "ema" => mkV018 form;
		_ + "oma" => mkV062 form;
		_ + "ða" => mkV006 form;
		_ + "pa" => mkV020 form;
		_ + "ma" => mkV009 form;
		_ + "áa" => mkV034 form;
		_ + "fa" => mkV106 form;
		_ + "øa" => mkV043 form;
		_ + "a" => mkV001 form;
		_ + "t" => mkV005 form;
		_ => error "Cannot find an inflection rule"
  } ;

  reg2V : Str -> Str -> V   -- Nonfinite  Indicative;Pres;('PSg', P2)
    = \form1, form2 -> case <form1, form2> of {
		<_ + "gva", _ + "ørt"> => mkV055 form1;
		<_ + "era", _ + "ert"> => mkV008 form1;
		<_ + "ala", _ + "ar"> => mkV001 form1;
		<_ + "ala", _ + "ir"> => mkV009 form1;
		<_ + "úka", _ + "ar"> => mkV001 form1;
		<_ + "aka", _ + "ar"> => mkV001 form1;
		<_ + "nna", _ + "ar"> => mkV001 form1;
		<_ + "íða", _ + "ar"> => mkV001 form1;
		<_ + "íða", _ + "ir"> => mkV006 form1;
		<_ + "rja", _ + "ar"> => mkV001 form1;
		<_ + "sta", _ + "ar"> => mkV001 form1;
		<_ + "sta", _ + "ur"> => mkV018 form1;
		<_ + "gva", _ + "ar"> => mkV001 form1;
		<_ + "rða", _ + "ar"> => mkV001 form1;
		<_ + "tta", _ + "ar"> => mkV001 form1;
		<_ + "tta", _ + "ur"> => mkV024 form1;
		<_ + "ava", _ + "ar"> => mkV001 form1;
		<_ + "iga", _ + "ar"> => mkV001 form1;
		<_ + "iga", _ + "ir"> => mkV007 form1;
		<_ + "ova", _ + "ar"> => mkV001 form1;
		<_ + "lda", _ + "ar"> => mkV001 form1;
		<_ + "lda", _ + "ir"> => mkV010 form1;
		<_ + "áta", _ + "ar"> => mkV001 form1;
		<_ + "vna", _ + "ar"> => mkV001 form1;
		<_ + "eka", _ + "ar"> => mkV001 form1;
		<_ + "mba", _ + "ar"> => mkV001 form1;
		<_ + "eva", _ + "ar"> => mkV001 form1;
		<_ + "ita", _ + "ir"> => mkV020 form1;
		<_ + "ína", _ + "ir"> => mkV009 form1;
		<_ + "íva", _ + "ir"> => mkV009 form1;
		<_ + "yta", _ + "ur"> => mkV041 form1;
		<_ + "ysa", _ + "ur"> => mkV041 form1;
		<_ + "eta", _ + "ir"> => mkV020 form1;
		<_ + "kja", _ + "ur"> => mkV100 form1;
		<_ + "nna", _ + "t"> => mkV076 form1;
		<_ + "rja", _ + "t"> => mkV093 form1;
		<_ + "gva", _ + "t"> => mkV022 form1;
		<_ + "ega", _ + "r"> => mkV001 form1;
		<_ + "ita", _ + "t"> => mkV109 form1;
		<_ + "lja", _ + "t"> => mkV112 form1;
		<_ + "ða", _ + "ar"> => mkV001 form1;
		<_ + "ða", _ + "ur"> => mkV066 form1;
		<_ + "pa", _ + "ar"> => mkV001 form1;
		<_ + "ma", _ + "ar"> => mkV001 form1;
		<_ + "fa", _ + "ar"> => mkV001 form1;
		<_ + "áa", _ + "r"> => mkV001 form1;
		<_ + "a", _ + "ært"> => mkV084 form1;
		<_ + "a", _ + "t"> => mkV025 form1;
		_ => regV form1
  } ;

  mkN = overload {
    mkN : Str -> N = regN;   -- s;Indef;Sg;Nom
    mkN : Str -> Str -> N = reg2N   -- s;Indef;Sg;Nom  s;Indef;Pl;Dat
  } ;

  mkN2 = overload {
     mkN2 : N -> N2 = \n -> lin N2 (n ** {c2 = noPrep}) ;
     mkN2 : N -> Prep -> N2 = \n,p -> lin N2 (n ** {c2 = p}) ;
  } ;

  mkN3 = overload {
     mkN3 : N -> N3 = \n -> lin N3 (n ** {c2 = noPrep; c3 = noPrep}) ;
     mkN3 : N -> Prep -> Prep -> N3 = \n,p1,p2 -> lin N3 (n ** {c2 = p1; c3 = p2}) ;
  } ;

  mkA = overload {
    mkA : Str -> A = regA;   -- s;Masc;Sg;Nom
    mkA : Str -> Str -> A = reg2A   -- s;Masc;Sg;Nom  s;Masc;Sg;Dat
  } ;

  mkA2 = overload {
     mkA2 : A -> A2 = \a -> lin A2 (a ** {c2 = noPrep}) ;
     mkA2 : A -> Prep -> A2 = \a,p -> lin A2 (a ** {c2 = p}) ;
  } ;

  mkV = overload {
    mkV : Str -> V = regV;   -- Nonfinite
    mkV : Str -> Str -> V = reg2V   -- Nonfinite  Indicative;Pres;('PSg', P2)
  } ;

  mkVV : V -> VV = \v -> lin VV v ;
  mkVS : V -> VS = \v -> lin VS v ;
  mkVQ : V -> VQ = \v -> lin VQ v ;
  mkVA : V -> VA = \v -> lin VA v ;

  mkV2 = overload {
     mkV2 : V -> V2 = \v -> lin V2 (v ** {c2 = noPrep}) ;
     mkV2 : V -> Prep -> V2 = \v,p -> lin V2 (v ** {c2 = p}) ;
  } ;

  mkV3 = overload {
     mkV3 : V -> V3 = \v -> lin V3 (v ** {c2 = noPrep; c3 = noPrep}) ;
     mkV3 : V -> Prep -> Prep -> V3 = \v,p1,p2 -> lin V3 (v ** {c2 = p1; c3 = p2}) ;
  } ;

  mkV2A = overload {
     mkV2A : V -> V2A = \v -> lin V2A (v ** {c2 = noPrep; c3 = noPrep}) ;
     mkV2A : V -> Prep -> Prep -> V2A = \v,p1,p2 -> lin V2A (v ** {c2 = p1; c3 = p2}) ;
  } ;

  mkV2S = overload {
     mkV2S : V -> V2S = \v -> lin V2S (v ** {c2 = noPrep; c3 = noPrep}) ;
     mkV2S : V -> Prep -> Prep -> V2S = \v,p1,p2 -> lin V2S (v ** {c2 = p1; c3 = p2}) ;
  } ;

  mkV2Q = overload {
     mkV2Q : V -> V2Q = \v -> lin V2Q (v ** {c2 = noPrep; c3 = noPrep}) ;
     mkV2Q : V -> Prep -> Prep -> V2Q = \v,p1,p2 -> lin V2Q (v ** {c2 = p1; c3 = p2}) ;
  } ;

  mkV2V = overload {
     mkV2V : V -> V2V = \v -> lin V2V (v ** {c2 = noPrep; c3 = noPrep}) ;
     mkV2V : V -> Prep -> Prep -> V2V = \v,p1,p2 -> lin V2V (v ** {c2 = p1; c3 = p2}) ;
  } ;

  mkAdv : Str -> Adv = \s -> lin Adv {s=s} ;
  mkAdV : Str -> AdV = \s -> lin AdV {s=s} ;
  mkAdA : Str -> AdA = \s -> lin AdA {s=s} ;
  mkAdN : Str -> AdN = \s -> lin AdN {s=s} ;
  mkCAdv : Str -> CAdv = \s -> lin CAdv {s=s; p=""} ;
  mkInterj : Str -> Interj = \s -> lin Interj {s=s} ;
  mkMU : Str -> MU = \s -> lin MU {s=s; isPre=False} ;

  mkPrep : Str -> Prep = \s -> lin Prep {s=s; c=Acc} ;

  mkIAdv : Str -> IAdv = \s -> lin IAdv {s=s} ;
  mkIP : Str -> IP = \s -> lin IP {s=s} ;
  mkIQuant : Str -> IQuant = \s -> lin IQuant {s=s} ;
  mkIDet : Str -> IDet = \s -> lin IDet {s=s} ;
  mkSubj : Str -> Subj = \s -> lin Subj {s=s} ;
  mkQuant : Str -> Quant = \s -> lin Quant {s=s} ;
  mkPredet : Str -> Predet = \s -> lin Predet {s=s} ;
  mkDet : Str -> Det = \s -> lin Det {s=s} ;
  mkCard : Str -> Card = \s -> lin Card {s=s} ;
  mkConj : Str -> Conj = \s -> lin Conj {s=s} ;
  mkPConj : Str -> PConj = \s -> lin PConj {s=s} ;
  mkVoc : Str -> Voc = \s -> lin Voc {s=s} ;

  mkLN : Str -> LN = \s -> lin LN {s=s} ;
  mkGN : Str -> GN = \s -> lin GN {s=s} ;
  mkSN : Str -> SN = \s -> lin SN {s=s} ;
  mkPN : Str -> PN = \s -> lin PN {s=s} ;

}
