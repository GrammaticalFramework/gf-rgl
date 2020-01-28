resource ResSom = ParamSom ** open Prelude, Predef, ParamSom in {

--------------------------------------------------------------------------------
-- Nouns
oper

  Noun : Type = {
    s : NForm => Str ;
    gda : GenderDefArt ;
    shortPoss : Bool ;
    } ;
  Noun2 : Type = Noun ; -- TODO eventually more parameters?
  Noun3 : Type = Noun ;

  CNoun : Type = Noun ** {
    mod : State -- for conjunctions: oo for indef, ee for def
       => Number => Case => Str ;
    modtype : ModType ;
    isPoss : Bool -- to prevent impossible forms in ComplN2 with Ns that have short possessive, e.g. "father"
    } ;

  cn2str : Number -> Case -> CNoun -> Str = \n,c,cn ->
    cn.s ! Indef n ++ cn.mod ! Indefinite ! n ! c ;

  PNoun : Type = {s : Str ; a : Agreement} ;

  mkPNoun : Str -> Agreement -> PNoun = \str,agr -> {s = str ; a = agr} ;

  mkNoun : (x1,_,_,x4 : Str) -> Gender -> Noun = \wiil,wiilka,wiilal,wiilasha,gender -> {
    s = table {
          Def Sg => hooya ; Def Pl => gury ;
          Indef Sg => wiil ; Indef Pl => wiilal ;
          -- Special forms for feminine nouns
          NomSg => bisadi ; Numerative => bisadood
          } ;
    gda = defAllomorph wiilka wiilasha ;
    shortPoss = False
    } where {
        hooya : Str = case wiilka of {
                        aabb + "aha" => aabb ;
                        hooya + "da" => hooya ;
                        wiila + "sha" => wiila ;
                        _ => wiil} ;
        gury : Str = case wiilasha of {
                        gury + "aha" => gury ;
                        magacya + "da" => magacya ;
                        wiila + "sha" => wiila ;
                        _ => wiilal} ;
        bisadi : Str = case gender of {
                        Fem  => case wiil of {
                                  _ + #c => wiil+"i" ;
                                  _      => wiil } ;
                        Masc => wiil } ;
        bisadood : Str = case gender of {
                        Fem  => case wiilal of {
                                  _ + "o" => wiilal+"od" ;
                                  _ => wiil } ;
                        Masc => wiil }

    } ;
-------------------------
-- Regular noun paradigms
  nHooyo, nAabbe, nMas, nUl, nGuri, nXayawaan : Str -> Noun ;

  --1) Feminine nouns that end in -o
  nHooyo hooyo =
    mkNoun hooyo (init hooyo + "ada") (hooyo + "oyin") (hooyo + "oyinka") Fem ;

  --2) Masculine nouns that end in -e
  nAabbe aabbe = let aabb = init aabbe in
    mkNoun aabbe (aabb + "aha") (aabb + "ayaal") (aabb + "ayaasha") Masc ;

  -- 3) Masculine, plural with duplication
  nMas mas = let s = last mas ;
                 a = last (init mas) ;
                 ka = allomorph mKa mas in
    mkNoun mas (mas + ka) (mas + a + s) (mas + a + s + ka) Masc ;

  -- Irregular one-syllable masculine word
  nWiil : (_,_ : Str) -> Noun = \wiil,wiilal ->
    let ka = allomorph mKa wiil ;
        sha = allomorph mTa wiilal ;
        wiila : Str = case wiilal of {wiila + "l" => wiila ; _ => wiilal} in
    mkNoun wiil (wiil + ka) wiilal (wiila + sha) Masc ;

  -- 4a) Feminine, plural with ó
  nUl ul = let o  = case last ul of { "i" => "yo" ; _ => "o" } ;
               u  = case last ul of { "l" => init ul ; _ => ul } ;
               sha = allomorph mTa ul in
    mkNoun ul (u + sha) (ul + o) (ul + "aha") Fem ;

  -- 4b) Masculine, plural with ó, 2 syllables
  nGuri guri = let ga = allomorph mKa guri ;
                   gury = case last guri of { -- TODO does this generalise? Or just exception?
                                 "i" => init guri + "y" ;
                                 _   => guri } ;
                    o = allomorph mO gury in
    mkNoun guri (guri + ga) (gury + o) (gury + "aha") Masc ;

  -- 4c) Masculine, plural with -ó, 3 syllables or longer
  nXayawaan x = let ka = allomorph mKa x ;
                    o = allomorph mO x ;
                    xo = x + o in
    mkNoun x (x + ka) xo (init xo + "ada") Masc ;

  nMaalin : (_,_ : Str) -> Gender -> Noun = \maalin,maalmo,g ->
   let ta = case g of { Masc => allomorph mKa maalin ;
                        Fem  => allomorph mTa maalin } ;
       aha = case g of { Masc|Fem  => "aha" } ; ---- ?
   in mkNoun maalin (maalin + ta) maalmo (init maalmo + aha) g ;

-------------------------
-- Smart paradigms

  mkN1 : Str -> Noun = \n -> case n of {
      _ + ("ad"|"adh") => nUl n ;
      _ + "o"          => nHooyo n ;
      _ + "e"          => nAabbe n ;
      _ + "ri"         => nGuri n ;
      (#c + #v + #v + #c) -- One syllable words
       | (#v + #v + #c)
       | (#c + #v + #c)
       | (#v + #c)     => nMas n ;
      _                => nXayawaan n } ;

  mk2N : Str -> Str -> Noun = nWiil ;

  mkNg : Str -> Gender -> Noun = \n,g -> case n of {
      _ -- + ("r"|"n"|"l"|"g")
          => case g of {
                  Fem  => nUl n ;
                  Masc => mkN1 n } ;
      _   => mkN1 n
   } ; -- TODO: add more exceptional cases

---------------------------------------------
-- NP

  BaseNP : Type = {
    a : Agreement ;
    isPron : Bool ;
    st : State ;
    empty : Str ;
    } ;

  NounPhrase : Type = BaseNP ** {s : Case => Str} ;

  NPLite : Type = {s : Str ; a : PrepAgr} ; -- Used in Adv and as an object in VP

  nplite : NounPhrase -> NPLite = \np ->
    let pagr : PrepAgr = agr2pagr np.a in
    case <np.isPron,isP3 np.a> of {
      <False,_>   => {s = np.s ! Abs ; a = pagr} ;
      -- <True,True> => {s = objpron np ! Abs ; a = pagr} ; -- uncomment if you want to add long object pronoun for 3rd person object
      _ => {s = np.empty ; a = pagr} } ; -- no long object for other pronouns

  objpron : NounPhrase -> Case => Str = \np -> case np.isPron of {
    True => \\c => np.empty ++ (pronTable ! np.a).sp ! c ;
    False => np.s} ;

  useN : Noun -> CNoun ** BaseNP = \n -> n **
    { mod = \\_,_,_ => [] ; modtype = NoMod ;
      a = Sg3 (gender n) ; isPron,isPoss = False ;
      empty = [] ; st = Indefinite
    } ;

  emptyNP : NounPhrase = {
    s = \\_ => [] ;
    a = Sg3 Masc ;
    isPron = False ;
    empty = [] ;
    st = Indefinite
    } ;

  impersNP : NounPhrase = pronTable ! Impers ;

  indeclNP : Str -> NounPhrase = \s -> emptyNP ** {s = \\c => s} ;

--------------------------------------------------------------------------------
-- Pronouns

  Pronoun : Type = NounPhrase ** {
    poss : { -- for PossPron : Pron -> Quant
      s : DefArticle => Str ;
      sp : GenNum => Str ; -- independent forms, e.g. M:kayga F:tayda Pl:kuwayga
      short : DefArticle => Str -- short possessive suffix: e.g. family members, my/your name
      } ;
    sp : Case => Str ;
    } ;

  {- Saeed p.115: "This combination of possessive and article [kay-ga, tay-da]
      is the basic form but possessives occur with the full range of determiners,
      with associated meanings, for example:
      remote article kii/tii:    gurigaagii 'your house (remote)'
      demonstrative kaas/taas:   gurigaagaas 'that house of yours'
      interrogative kee/tee:     gurigaagee? 'which house of yours?'"

     Since RGL abstract syntax doesn't allow combining two Quants, the way to go is
     to have another Pron -> Quant function in Extra, which forms Quants such as
     -gaagii, -gaagaas, -gaagee.
  -}

  pronTable : Agreement => Pronoun = table {
    Sg1 => {
      s = table {Nom => "aan" ; Abs => "i"} ;
      a = Sg1 ; isPron = True ; sp = table {Nom => "anigu" ; _ =>"aniga"} ;
      empty = [] ; st = Definite ;
      poss = {s = quantTable "ayg" "ayd" ; short = quantTable "ay" ; sp = gnTable "ayg" "ayd" "uwayg"}
      } ;
    Sg2 => {
      s = table {Nom => "aad" ; Abs => "ku"} ;
      a = Sg2 ; isPron = True ; sp = table {Nom => "adigu" ; _ => "adiga"} ;
      empty = [] ; st = Definite ;
      poss = {s = quantTable "aag" "aad" ; short = quantTable "aa" ; sp = gnTable "aag" "aad" "uwaag"}
      } ;
    Sg3 Masc => {
      s = table {Nom => "uu" ; Abs => []} ;
      a = Sg3 Masc ; isPron = True ; sp = table {Nom => "isagu" ; _ => "isaga"} ;
      empty = [] ; st = Definite ;
      poss = {s, short = quantTable "iis" ; sp = gnTable "iis" "iis" "uwiis"}
      } ;
    Sg3 Fem => {
      s = table {Nom => "ay" ; Abs => []} ;
      a = Sg3 Fem ; isPron = True ; sp = table {Nom => "iyadu" ; _ => "iyada"} ;
      empty = [] ; st = Definite ;
      poss = {s, short = quantTable "eed" ; sp = gnTable "eed" "eed" "uweed"}
      } ;
    Pl1 Excl => {
      s = table {Nom => "aan" ; Abs => "na"} ;
      a = Pl1 Excl ; isPron = True ; sp = table {Nom => "annagu" ; _ => "annaga"} ;
      empty = [] ; st = Definite ;
      poss = {s = quantTable "eenn" ; short = quantTable "een" ; sp = gnTable "eenn" "eenn" "uweenn"}
      } ;
    Pl1 Incl => {
      s = table {Nom => "aynu" ; Abs => "ina"} ;
      a = Pl1 Incl ; isPron = True ; sp = table {Nom => "innagu" ; _ => "innaga"} ;
      empty = [] ; st = Definite ;
      poss = {s = quantTable "eenn" ; short = quantTable "een" ; sp = gnTable "eenn" "eenn" "uweenn"}
      } ;
    Pl2 => {
      s = table {Nom => "aad" ; Abs => "idin"} ;
      a =  Pl2 ; isPron = True ; sp = table {Nom => "idinku" ; _ => "idinka"} ;
      empty = [] ; st = Definite ;
      poss = {s = quantTable "iinn" ; short = quantTable "iin" ; sp = gnTable "iinn" "iinn" "uwiinn"}
      } ;
    Pl3 => {
      s = table {Nom => "ay" ; Abs => []} ;
      a = Pl3 ; isPron = True ; sp = table {Nom => "iyagu" ; _ => "iyaga"} ;
      empty = [] ; st = Definite ;
      poss = {s, short = quantTable "ood" ; sp = gnTable "ood" "ood" "uwood"}
      } ;
    Impers => {
      s = \\_ => [] ; -- the string `la' comes from Passive (: PrepCombination)
      a = Impers ; isPron = True ; sp = \\_ => "" ;
      empty = [] ; st = Definite ;
      poss = {s, short = quantTable "iis" ; sp = gnTable "iis" "iis" "uwiis"}
      }
    } ;

  secondObject : PrepAgr => Str = table {
    Sg1_Prep      => "kay" ;
    Sg2_Prep      => "kaa" ;
    Pl1_Prep Excl => "kayo" ;
    Pl1_Prep Incl => "keen" ;
    Pl2_Prep      => "kiin" ;
    _             => []
    } ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  BaseQuant : Type = {
    s : DefArticle => Case => Str ;
    isPoss : Bool ;
    shortPoss : DefArticle => Str ; -- short form of possessive, e.g. family members
    st : State ;
    } ;

  Determiner : Type = BaseQuant ** {
    sp : Gender => Case => Str ;
    n : Number ;
    numtype : NumType ;  -- placement in NP + whether to choose Numerative from CN
    } ;

  Quant : Type = BaseQuant ** {
    sp : GenNum => Case => Str ;
    } ;

  BaseNum : Type = {
    s : DForm => Str ; -- independent or attribute
    thousand : Str ; -- TODO check where possessive suffix goes
    hasThousand : Bool ;
    da : DefArticle ;
    n : Number
    } ;

  baseNum : Num = {
    s = \\_ => [] ;
    thousand = [] ;
    hasThousand = False ;
    da = M KA ;
    n = Sg ;
    numtype = NoNum
    } ;

  Num : Type = BaseNum ** {
    numtype : NumType -- whether to choose Numerative as the value of NForm
    } ;

  Numeral : Type = BaseNum ** {
    ord : Str
    } ;

  baseQuant : BaseQuant = {
    s = \\alm,c => [] ;
    isPoss = False ;
    shortPoss = \\_ => [] ;
    st = Indefinite
  } ;

  defQuant = defQuantBind True ;

  defQuantBind : (bind : Bool) -> (s, kan, tan, kuwan : Str) -> (vowIsI : Bool) -> Quant = \b,s,spm,spf,spp,v ->
    let bind : Str -> Str = \x -> case b of {False => x ; True => BIND ++ x} ;
    in baseQuant ** {
        s = \\allomorph,c =>
           let nom = case v of {False => "u" ; _ => s + "i"}
            in case c of {
                Nom => bind (quantTable nom ! allomorph) ;
                _   => bind (quantTable s ! allomorph) } ;
        sp = \\gn,c =>
           let i = case c of {Nom => "i"; _ => []}
           in gnTable (spm + i) (spf + i) (spp + i) ! gn ;
        st = Definite ;
        } ;

  defIQuant : Str -> Quant = \ee ->
    let quantRaw = defQuant ee ("k"+ee) ("t"+ee) ("kuw"+ee) False
     in quantRaw ** {
          s = \\da,c => quantRaw.s ! da ! Abs ;
          sp = \\gn,c => quantRaw.sp ! gn ! Abs } ;

  gnTable : (m,f,p : Str) -> (GenNum => Str) = \m,f,p ->
    table {SgMasc => m ; SgFem => f ; _ => p} ;

  indefQuant : Quant = baseQuant ** {
    s = \\da,c => [] ;
    sp = \\gn,c => [] ;
    st = Indefinite ;
    } ;

--------------------------------------------------------------------------------
-- Prepositions

  Prep : Type = {
    s : PrepAgr => Str ;
    c2 : Preposition ;
    isPoss : Bool ;
    berri, sii, dhex : Str ;
    hoostiisa : Agreement => Str } ;

  mkPrep : (x1,_,_,_,_,x6 : Str) -> {s : PrepAgr => Str} = \ku,ii,kuu,noo,idiin,isku -> {
    s = table {
          P3_Prep       => ku ;
          Sg1_Prep      => ii ;
          Sg2_Prep      => kuu ;
          Pl2_Prep      => idiin ;
          Pl1_Prep Excl => noo ;
          Pl1_Prep Incl => "i" + noo ;
          Reflexive_Prep => isku
        }
    } ;
  prep : Preposition -> {s : PrepAgr => Str ; c2 : Preposition} = \p ->
    prepTable ! p ** {c2 = p} ;

  prepTable : Preposition => {s : PrepAgr => Str} = table {
    Ku => mkPrep "ku" "igu" "kugu" "nagu" "idinku" "isku" ;
    Ka => mkPrep "ka" "iga" "kaa"  "naga" "idinka" "iska" ;
    La => mkPrep "la" "ila" "kula" "nala" "idinla" "isla" ;
    U  => mkPrep "u" "ii" "kuu" "noo" "idiin" "isu" ;
    _  => mkPrep []  "i"  "ku"  "na"  "idin"  "is"
  } ;

  prepCombTable : PrepAgr => PrepCombination => Str = table {
    Sg1_Prep => table {
                   Ugu => "iigu" ; Uga => "iiga" ; Ula => "iila" ;
                   Kaga => "igaga" ; Kula => "igula" ; Kala => "igala" ;
                   Passive => "la i" ; Loo => "la ii" ; Lala => "la ila" ;
                   Lagu => "laygu" ; Laga => "layga" ;
                   Single p => (prepTable ! p).s ! Sg1_Prep } ;
    Sg2_Prep => table { Ugu => "kuugu" ; Uga => "kaaga" ; Ula => "kuula" ;
                   Kaga => "kaaga" ; Kula => "kugula" ; Kala => "kaala" ;
                   Passive => "lagu" ; Loo => "laguu" ; Lala => "lagula" ;
                   Lagu => "lagugu" ; Laga => "lagaa" ;
                   Single p => (prepTable ! p).s ! Sg2_Prep } ;
    Pl1_Prep Excl =>
           table { Ugu => "noogu" ; Uga => "nooga" ; Ula => "noola" ;
                   Kaga => "nagaga" ; Kula => "nagula" ; Kala => "nagala" ;
                   Passive => "nala" ; Loo => "???" ; Lala => "???" ;
                   Lagu => "nalagu" ; Laga => "nalaga" ;
                   Single p => (prepTable ! p).s ! Pl1_Prep Excl } ;
    Pl1_Prep Incl =>
           table { Ugu => "inoogu" ; Uga => "inooga" ; Ula => "inoola" ;
                   Kaga => "inagaga" ; Kula => "inagula" ; Kala => "inagala" ;
                   Passive => "inala" ; Loo => "???" ; Lala => "???" ;
                   Lagu => "inalagu" ; Laga => "inalaga" ;
                   Single p => (prepTable ! p).s ! Pl1_Prep Incl } ;
    Pl2_Prep => table { Ugu => "idiinku" ; Uga => "idiinka" ; Ula => "idiinla" ;
                   Kaga => "idinkaga" ; Kula => "idinkula" ; Kala => "idinkala" ;
                   Passive => "laydin" ; Loo => "laydiin" ; Lala => "laydinla" ;
                   Lagu => "laydinku" ; Laga => "laydinka" ;
                   Single p => (prepTable ! p).s ! Pl2_Prep } ;
    Reflexive_Prep => -- TODO check every form
           table { Ugu => "isugu" ; Uga => "isuga" ; Ula => "isula" ;
                   Kaga => "iskaga" ; Kula => "iskula" ; Kala => "iskala" ;
                   Passive => "lays" ; Loo => "???" ; Lala => "???" ;
                   Lagu => "laysku" ; Laga => "layska" ;
                   Single p => (prepTable ! p).s ! Reflexive_Prep } ;
    a   => table { Ugu => "ugu" ; Uga => "uga" ; Ula => "ula" ;
                   Kaga => "kaga" ; Kula => "kula" ; Kala => "kala" ;
                   Passive => "la" ; Loo => "loo" ; Lala => "lala" ;
                   Lagu => "lagu" ; Laga => "laga" ;
                   Single p => (prepTable ! p).s ! a }
  } ;

--------------------------------------------------------------------------------
-- Adjectives

-- Sequences of adjectives follow the rules for restrictive relatives clauses, i.e. are linked by oo 'and' on an indefinite head NounPhrase and by ee 'and' on a definite NounPhrase (8.1).

  Adjective : Type = {s : AForm => Str} ;
  Adjective2 : Type = Adjective ** {c2 : Preposition} ;

  duplA : Str -> Adjective = \yar ->
    let yaryar = duplicate yar
    in mkAdj yar yaryar ;

  mkAdj : (str,pl : Str) -> Adjective = \sg,pl -> {
    s = table {
          AF Sg Nom => sg + "i" ;
          AF Pl Nom => pl + "i" ;
          AF Sg _ => sg ;
          AF Pl _ => pl }
    } ;

  duplicate : Str -> Str = \sg -> case sg of {
    -- some hard-coded cases; in general, better to
    -- use 2-paradigm mkAdj for irregular adjectives.
    "dheer" => "dhaadheer" ;
    "weyn"  => "waaweyn" ;

    -- general patterns
    a@#v + d@#c + ? + ?  -- 4 letters of form CVXX
        => a + d + sg ; -- ad+adag
    g@#c + aa@#vv + _
        => g + aa + sg ; -- gaa+gaaban
    y@#c + a@#v + r@#c + _
        => y + a + r + sg ; -- yar+yar ; fud+fudud
    d@#c + h@#c + uu@#vv + _
        => d + h + uu + sg ; -- dhuu+dhuuban
    q@#c + a@#v + y@#vstar + b@#c + _
        => q + a + y + b + sg ; --qayb+qaybsan, fiic+fiican
    _   => sg + sg } ;

  AdjPhrase : Type = Adjective ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  BaseVerb : Type = {
    s : VForm => Str ;
    } ;
  Verb : Type = BaseVerb ** {
    sii : Str ; -- closed class of particles: sii, soo, kala, wada (Saeed 171)
    dhex : Str ; -- closed class of adverbials: hoos, kor, dul, dhex, …
    isCopula : Bool ;
    } ;
  Verb2 : Type = Verb ** {c2 : Preposition} ;
  Verb3 : Type = Verb2 ** {c3 : Preposition} ;

  VV : Type = Verb ** {vvtype : VVForm} ;

  -- Saeed page 79:
  -- "… the reference form is the imperative singular form
  -- since it corresponds to the form of the basic root."
  mkVerb : (imperative,sg1,pl2 : Str) -> Verb = \qaado,qaat,ark ->
    let stems : {p1 : Str ; p2 : Str} = case ark of {
          a + r@#c + k@#c -- two consonants need a vowel in between
            => <ark + "i", a + r + a + voiced k> ;
          _ + #c -- if the pl2 root ends in consonant, infinitive needs a vowel
            => <ark + "i", ark> ;
          yar + "ee"  -- double e turns into ey
            => <ark + "n", yar + "ey"> ;
          _ => <ark + "n", ark> -- no changes, just add n for infinitive
        } ;
        arki = stems.p1 ;
        arag = stems.p2 ;
        arkin = case last arki of { -- The negative past tense ends in n:
                  "n" => arki ;         -- if infinitive ends in n, no change;
                   _  => arki + "n" } ; -- otherwise add n.


        progr : Str = case qaat of { -- Progressive
               _ + "eey"     => stems.p2 + "nay" ; -- bireey -> bireynay
               _ + ("y"|"n") => init qaat + "nay" ; -- akhriy -> akhrinay ; gashad -> gashanay
               _ + #v + "t"  => qaat + "ay" ;
               _ + #c + "t"  => init qaat + "anay" ;
               _             => qaat + "ay" } ;

        -- Some predictable sound changes
        t : Str = case arag of { -- kari+seen, bixi noq+deen, (sug|joogsa|qaada)+teen,
               _ + ("i"|"y") => "s" ;     -- t changes into s in front of i/y
               _ + ("x"|"q"|"c") => "d" ; -- t changes into d in front of x/q/c
               _             => "t" } ;
        ay : Str = case ark of {
               _ + ("i"|"e") => "ey" ;
               _             => "ay" } ;
        n : Str = case arag of {
               _ + #v  => "nn" ; -- n duplicates after vowel
               _ + "r" => "r" ; -- Saeed p. 35: agreement marker n (1PL)
               _ + "l" => "l" ; -- assimilates to stem final r or.
               _       => "n" } ;
        an : Str = case qaado of {
               _ + "o" => "an" ; -- Allomorph for imperatives
               _       => "in" } ;

   in { s = table {
          VPres Simple Sg1_Sg3Masc pol
                                    => qaat     + if_then_Pol pol "aa" "o" ;
          VPres Simple Sg2_Sg3Fem pol
                                    => arag + t + if_then_Pol pol "aa" "o" ;
          VPres Simple Pl1_ pol     => arag + n + if_then_Pol pol "aa" "o"  ;
          VPres Simple Pl2_ pol     => arag + t + "aan" ;
          VPres Simple Pl3_ pol     => qaat     + "aan" ;

          VPres Progressive Sg1_Sg3Masc pol
                                     => progr + if_then_Pol pol "aa" "o" ;
          VPres Progressive Sg2_Sg3Fem pol
                                     => progr + if_then_Pol pol "saa" "so" ;
          VPres Progressive Pl1_ pol
                                     => progr + if_then_Pol pol "naa" "no"  ;
          VPres Progressive Pl2_ pol => progr + "saan" ;
          VPres Progressive Pl3_ pol => progr + "aan" ;

          VPast Simple Sg1_Sg3Masc
                                  => qaat     + ay ;
          VPast Simple Sg2_Sg3Fem => arag + t + ay ; -- t, d or s
          VPast Simple Pl1_       => arag + n + ay ;
          VPast Simple Pl2_       => arag + t + "een" ; -- t, d or s
          VPast Simple Pl3_       => qaat     + "een" ;

          VPast Progressive Sg1_Sg3Masc
                                          => progr + "ey" ;
          VPast Progressive Sg2_Sg3Fem => progr + "sey" ;
          VPast Progressive Pl1_       => progr + "ney" ;
          VPast Progressive Pl2_       => progr + "seen" ;
          VPast Progressive Pl3_       => progr + "een" ;

          VNegPast Simple      => arkin ;
          VNegPast Progressive => progr + "n" ;

          -- TODO check conjugations 2 and 3
          VNegCond PlInv  => arag + n + "een" ;
          VNegCond SgMasc => qaat     + "een" ; -- for most verbs same as VPast Simple Pl3_
          VNegCond SgFem  => arag + t + "een" ; -- for most verbs same as VPast Simple Pl2_

          VImp Sg Pos   => arag ;
          VImp Pl Pos   => qaat + "a" ;
          VImp Sg Neg   => arag + an ;
          VImp Pl Neg   => qaat + "ina" ;

          VInf          => arki ;
          VRel SgMasc   => qaat + "a" ;
          VRel SgFem    => arag + t + "a" ;
          VRel PlInv    => arag + "na" ;
          VRelNeg       => qaat + "o"  -- TODO check

           } ;
        sii, dhex = [] ;
        isCopula = False ;
      } ;

-------------------------
-- Regular verb paradigms

  cSug, cKari, cYaree, cJoogso, cQaado : Str -> Verb ;

  -- 1: Root verbs with no lexical affixes, e.g. sug TR 'wait for', kar INTR 'boil, cook';
  cSug sug =
    let cabb : Str = case sug of {
          _ + "b" => sug + "b" ; -- TODO: more duplication patterns
          _       => sug }
     in mkVerb sug cabb sug ;

  -- 2A: Verbs derived from root verbs by the causative affix -i/-is, e.g. kari TR 'cook' (from conjugation 1 kar INTR 'boil, cook');
  -- 2B: Verbs derived from nouns and adjectives by the causative/factitive affix -eel-ayn, e.g. yaree 'make small' (from yar ADJ 'small');
  cKari, cYaree = \kari -> mkVerb kari (kari+"y") kari ;

  -- 3A: Verbs derived from verbal stems by the middle voice affix -ol­/at
  -- e.g. karsó 'cook for oneself (from conjugation 2 kâri TR 'cook');
  cJoogso joogso =
    let joogsa = init joogso + "a" ;
     in mkVerb joogso (joogsa + "d") joogsa ;

  -- 3B: As conjugation 3A but verbs whose syllable structure triggers
  -- stem contraction and subsequent sandhi rules, e.g. qaadó 'take for oneself
  -- (from conjugation 1 qàad TR 'take').
  cQaado qaado =
    let qaa = init (init qaado)
     in mkVerb qaado  -- Imperative sg, with the vowel
              (qaa + "t")    -- Per1 Sg, Per3 Pl and Per3 Sg Masc
              (qaa + "da") ; -- Per2 Pl and others

  -- Constructs verbs like u baahan+ahay
  prefixV : Str -> Verb -> Verb = \s,v -> v ** {
    s = \\vf => s + v.s ! vf
  } ;

------------------
-- Irregular verbs
  presCopula : {agr:Agreement ; pol:Polarity} => Str =
    \\a => case <a.agr,a.pol> of {
        <Sg1,pol>    => if_then_Pol pol "ahay" "ihi" ;
        <Sg2,pol>    => if_then_Pol pol "tahay" "ihid" ;
        <(Sg3 Masc|Impers),pol> => if_then_Pol pol "yahay" "aha" ;
        <(Sg3 Fem),pol> => if_then_Pol pol "tahay" "aha" ;
        <(Pl1 _),pol> => if_then_Pol pol "nahay" "ihin" ;
        <Pl2,pol>     => if_then_Pol pol "tihiin" "ihidin" ;
        <Pl3,pol>     => if_then_Pol pol "yihiin" "aha"
  } ;

  copula : Verb = {
    s = table {
          VPast _ Sg1_Sg3Masc
                          => "ahaa" ;
          VPast _ Sg2_Sg3Fem
                          => "ahayd" ;
          VPast _ Pl1_    => "ahayn" ;
          VPast _ Pl2_    => "ahaydeen" ;
          VPast _ Pl3_    => "ahaayeen" ;
          VNegPast _      => "ahi" ;
          VNegCond SgMasc => "ahaadeen" ; -- 1SG/3 SG M/3PL
          VNegCond SgFem  => "ahaateen" ; -- 2SG/3 SG F/2PL
          VNegCond PlInv  => "ahaanneen" ; -- 1PL
          VRel _          => "ah" ; -- All persons: see Nilsson p. 78. TODO check Saeed p. 103
          VRelNeg         => "ahayni" ; -- Saeed
          VInf            => "ahaan" ;
          VImp Sg pol     => if_then_Pol pol "ahaw" "ahaanin" ;
          VImp Pl pol     => if_then_Pol pol "ahaada" "ahaanina" ;
          VPres _ _ _     => nonExist -- use presCopula instead
          } ;
      sii, dhex = [] ;
      isCopula = True
     } ;

  have_V : Verb =
   let hold_V = mkVerb "hayso" "haysat" "haysa" in hold_V ** {
    s = table {
          VPres _ Sg1_Sg3Masc Pos => "leeyahay" ;
          VPres _ Sg2_Sg3Fem  Pos => "leedahay" ;
          VPres _ Pl1_        Pos => "leenahay" ;
          VPres _ Pl2_        Pos => "leedihiin" ;
          VPres _ Pl3_        Pos => "leeyihiin" ;

          VPres _ Sg1_Sg3Masc Neg => "lihi" ;
          VPres _ Sg2_Sg3Fem  Neg => "lihid" ;
          VPres _ Pl1_        Neg => "lihin" ;
          VPres _ Pl2_        Neg => "lihidin" ;
          VPres _ Pl3_        Neg => "laha" ;

          VNegPast _              => "lahayn" ;

          VPast asp agr           => "l" + copula.s ! VPast asp agr ;
          VNegCond agr            => "l" + copula.s ! VNegCond agr ;
          VRel _                => "leh" ; -- All persons: see Nilsson p. 78
          VRelNeg               => "lahayn" ;
          x                     => hold_V.s ! x }
    } ;

  fail_V : Verb =
   let waa_V : Verb = cSug "waay" in waa_V ** {
    s = table {
      VPres _ Sg2_Sg3Fem _
                      => "waayday" ;
      VPast _ Sg1_Sg3Masc
                      => "waayey" ;
      VPast _ Sg2_Sg3Fem
                      => "weydey" ;
      VPast _ Pl1_    => "weyney" ;
      VPast _ Pl2_    => "weydeen" ;
      VPast _ Pl3_    => "waayeen" ;
      VInf => "waayi" ;
      x => waa_V.s ! x -- TODO actual forms
    }
  } ;

------------------
-- Adv

  BaseAdv : Type = {
    sii, -- sii, soo, wala, kada go inside VP.
    dhex, -- dhex, hoos, koor, dul, … go inside VP.
    berri, -- AdV, e.g. "tomorrow"; goes before VP.
    miscAdv : Str -- dump for any other kind of adverbial.
    } ;

  Adverb : Type = BaseAdv ** {
    c2 : Preposition ; -- adverbs can contribute to preposition contraction.
    np : NPLite ; -- NP from PrepNP can be promoted into a core argument.
    } ;

  IAdv : Type = Adverb ** {
    contractSTM : Bool ;
    s : Str -- alone, in one-word question, e.g. Waayo? 'Why?'
    } ;

  prepNP : Prep -> NounPhrase -> Adverb = \prep,np -> prep ** {
    np = case prep.isPoss of {
           True  => nplite emptyNP ;
           False => nplite np } ;
    miscAdv = case prep.isPoss of {
           True  => np.s ! Abs ++ prep.hoostiisa ! np.a ;
           False => prep.hoostiisa ! Sg3 Masc }
    } ;

------------------
-- VP

  Complement : Type = {
    aComp : Agreement => Str ;
    nComp : Str ;
    compar : Str ; -- comparative is discontinuous
    stm : STM ; -- to choose right sentence type marker
    } ;

  VerbPhrase : Type = BaseVerb ** Complement ** BaseAdv ** {
    c2 : PrepCombination ; -- Prepositions can combine together and with object pronoun.
    obj : NPLite ; -- {s : Str ; a : PrepAgr}
    obj2 : Str ; -- if two overt pronoun objects
    vComp : {subjunc : Str ; -- "waa in" or subjunctive construction: "in" is placed here
              inf : Str ; -- auxiliary VV with infinitive argument
              subcl : Agreement => Str} -- VV complement if it's a subordinate clause
    } ;

  VPSlash : Type = VerbPhrase ;

  useV : Verb -> VerbPhrase = \v -> v ** {
    stm = case v.isCopula of { -- can change into Waxa in ComplVV
            True  => Waa Copula ;
            False => Waa NoPred
          } ;
    compar = [] ;
    nComp = [] ;
    aComp = \\_ => [] ;
    vComp = {subjunc, inf = [] ;
             subcl = \\_ => []} ;
    berri,miscAdv = [] ;
    c2 = Single NoPrep ;
    obj = {s = [] ; a = P3_Prep} ;
    obj2 = []
    } ;

  useVc : Verb2 -> VPSlash = \v2 -> useV v2 ** {
    c2 = Single v2.c2
    } ;

  useVc3 : Verb3 -> VPSlash = \v3 -> useV v3 ** {
    c2 = combine v3.c2 v3.c3 ;
    } ;

  passV2 : Verb2 -> VerbPhrase = \v2 -> passVP (useVc v2) ;

  passVP : VerbPhrase -> VerbPhrase = \vp -> vp ** {
    c2 = case vp.c2 of {
      Single p => combinePassive p ;
      _             => vp.c2 }
    } ;

  insertRefl : VPSlash -> VPSlash = \vps -> vps ** {
    obj = vps.obj ** {a = Reflexive_Prep} ;

    -- If old obj was something else than P3, it is now shown in obj2
    obj2 = vps.obj2 ++ secondObject ! vps.obj.a ;
    } ;

  insertComp : VPSlash -> NounPhrase -> VerbPhrase = \vp,np ->
    vp ** insertCompLite vp (nplite np) ;

  insertCompCl : ClSlash -> NounPhrase -> ClSlash = \cls,np ->
    cls ** insertCompLite cls (nplite np) ;

  insertAdv : VerbPhrase -> Adverb -> VerbPhrase = \vp,adv ->
    vp ** insertAdvLite vp adv ;

  insertIAdv : Adverb -> ClSlash -> ClSlash = \adv,cls ->
    cls ** insertAdvLite cls adv ;

  -- To generalise insertAdv and insertComp
  VPLite : Type = {
    c2 : PrepCombination ;
    obj : NPLite ;
    sii,dhex,berri,miscAdv,obj2 : Str} ;

  insertCompLite : VPLite -> NPLite -> VPLite = \vp,nplite ->
    case vp.obj.a of {
      -- If the old object is 3rd person (or nonexistent), we replace its agreement.
      -- We keep both old and new string (=noun, if there was one) in obj.s.
      P3_Prep =>
        vp ** {obj = nplite ** {
                  s = nplite.s ++ vp.obj.s}
                  } ; -- no obj2, because there's ≤1 non-3rd-person pronoun.

      -- If old object was non-3rd person, we keep its agreement.
      -- The new object is put in the secondObject field.
      _ =>
          vp ** {obj = vp.obj ** {
                    s = nplite.s ++ vp.obj.s
                    } ;
                 obj2 = vp.obj2 ++ secondObject ! nplite.a}
    } ;

  insertAdvLite : VPLite -> Adverb -> VPLite = \vp,adv ->
    case adv.c2 of {
      NoPrep => vp ** adv'' ; -- the adverb is not formed with PrepNP, e.g. "tomorrow"
      _ => case vp.c2 of {
             -- if free complement slots, introduce adv.np with insertComp
             Single NoPrep => insertCompLite (vp ** {c2 = Single adv.c2}) adv.np ** adv' ;
             Single p => insertCompLite (vp ** {c2 = combine p adv.c2}) adv.np ** adv' ;
             Passive => insertCompLite (vp ** {c2 = combinePassive adv.c2}) adv.np ** adv' ;

             -- if complement slots are full, just insert strings.
             _ => vp ** adv''
            }
    } where {
        adv' : {sii,dhex,berri,miscAdv : Str} = { -- adv.np done with insertComp
          sii = vp.sii ++ adv.sii ;
          dhex = vp.dhex ++ adv.dhex ;
          berri = vp.berri ++ adv.berri ;
          miscAdv = vp.miscAdv ++ adv.miscAdv} ;
        adv'' : {sii,dhex,berri,miscAdv : Str} -- adv.np inserted into miscAdv
          = adv' ** {dhex = (prepTable ! adv.c2).s ! adv.np.a ++ adv.dhex ;
                    miscAdv = adv.miscAdv ++ adv.np.s}
        } ;
--------------------------------------------------------------------------------
-- Sentences etc.


  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = BaseAdv ** {
    -- Fixed in Cl
    subj : {noun, pron : Str ; isP3 : Bool} ; -- noun and subject pronoun if applicable
    obj : NPLite ;
    obj2 : Str ;
    c2 : PrepCombination ; -- NB. QuestIAdv can add more prepositions
    aComp : Str ;
    nComp : Str ;
    vComp : {inf,subcl,subjunc : Str} ;

    -- Still open
    pred : ClType => Tense => Anteriority => Polarity => Str ;
    stm : ClType => Polarity => Str
    } ;

  Clause : Type = {s : Tense => Anteriority => Polarity => Str} ;
  QClause = Clause ;
  RClause : Type = {s : GenNum => Case => Tense => Anteriority => Polarity => Str} ;
  Sentence : Type = {s : Bool {-is subordinate-} => Str} ;

  predVPslash = predVP ; -- Cl and ClSlash are the same category

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vps -> vp ** {
    subj = {noun = subjnoun ; pron = subjpron ; isP3 = isP3 subj.a} ;
    obj = vp.obj ** {s = vp.obj.s ++ vp.compar} ;
    pred = \\cltyp,t,a,p =>
            let predRaw = vf cltyp t a p subj.a vp ;
             in case <cltyp, p, t, vp.stm, subj.a> of {
                <Statement, Pos, Pres, Waa NoCopula, Sg3 _|Pl3> -- VP comes from CompNP/CompCN + P3 subject
                  => [] ;
                <_, _, Pres, Waa (Copula|NoCopula), _> -- Comp* present tense + any subject
                  => presCopula ! {agr=subj.a ; pol=p} ;

                _ => predRaw -- Any other verb
           } ;

    stm = mkStm subj.a vp.stm ;
    aComp = vp.aComp ! subj.a ;
    vComp = vp.vComp ** {
              subcl = vp.vComp.subcl ! subj.a
            }
  } where {
      vp : VerbPhrase = case isPassive vps of {
               True => insertComp vps np ;
               _    => vps } ;
      subj : NounPhrase = case isPassive vps of {
               True => impersNP ;
               _    => np } ;
      subjnoun : Str = case np.isPron of {
                          True  => np.empty ;
                          False => subj.s ! Nom
                       } ;
      subjpron : Str = case <vp.stm,subj.a> of {
                          <Waa (Copula|NoCopula),Pl3|Sg3 _|Impers>
                            => np.empty ;
                          _ => (pronTable ! subj.a).s ! Nom
                       }
      } ;

  -- Sentence: include subject pronoun and STM.
  -- When subordinate, include "in".
  cl2sentence : Bool -> ClSlash -> Clause = \isSubord,cls -> {
    s = \\t,a,p =>
    let cltyp : ClType = case isSubord of {
                           True  => Subord ;
                           False => Statement } ;
        cl : ClSlash = case isSubord of { -- add "in" to the clause if used as subordinate
                           True  => cls ** {
                                      vComp = cls.vComp ** {subjunc = "in"}
                                    } ;
                           False => cls } ;
        isRel = False ;
        hasSubjPron = True ;
        hasSTM = True ;
        sent = mkClause cltyp isRel hasSubjPron hasSTM cl
     in sent.s ! t ! a ! p
    } ;

  mkClause : ClType -> (rel,sp,stm : Bool) -> ClSlash -> Clause =
    \cltyp,isRel,hasSubjPron,hasSTM,cl -> {
    s = \\t,a,p =>
      let -- Put all arguments in their right place
          --cl : ClSlash = complCl incomplCl ;
          prepComb = prepCombTable ! cl.obj.a ! cl.c2 ;

          -- Contractions
          bind : Str = case <isPassive cl, cl.obj.a, cl.c2> of {
            <False,P3_Prep,Single NoPrep> => [] ; -- nothing to attach to the STM
            _                             => BIND } ; -- something to attach, use BIND
          prepCombNeg : Str = case <cltyp,p> of {
             <Statement,Neg> => prepComb ++ bind ;
             _ => []
          } ;
          prepCombPos : Str = case <cltyp,p> of {
             <Statement,Neg> => [] ;
             _ => prepComb
          } ;

          -- Placement of object noun varies depending on type of clause
          statementNounObj = case cltyp of {
                                Statement => cl.obj.s ;
                                _         => [] } ;
          statementNounComp = case cltyp of {
                                Statement => cl.nComp ;
                                _         => [] } ;

          -- for subord and question, NP predicatives and objects behave the same
          subordNounObj = case cltyp of {
                                Subord => cl.obj.s ++ cl.nComp ;
                                _      => [] } ;
          questionNounObj = case cltyp of {
                                PolarQuestion|WhQuestion
                                       => cl.obj.s ++ cl.nComp ;
                                _      => [] } ;

          -- Control whether to include subject pronoun and STM
          subjpron : Str = case <hasSubjPron,p,cl.subj.isP3,isRel> of {
                              <True,Pos,True,True> => [] ;
                              <True,Pos,_,_>       => cl.subj.pron ;
                               _                   => [] } ;
          stm : Str = case <hasSTM,p> of {
                               <True,_> => cl.stm ! cltyp ! p ;
                               <_,Neg>  => cl.stm ! cltyp ! p ; -- negation overrides hasSTM=False. To override the override, set STM to [] in the function that calls this. /IL
                               _          => [] }
    in cl.berri      -- AdV
    ++ cl.subj.noun -- subject if it's a noun
    ++ statementNounObj -- noun object if it's a statement

    ++ prepCombNeg  -- prepositions and pron. objects in negative statement
    ++ stm

    ++ cl.vComp.subjunc  -- "waa in" construction /
    ++ subjpron          -- subject pronoun

    ++ subordNounObj -- noun object if it's subordinate clause: "timir aan /laf/ lahayn" (Saeed p. 210-211)
    ++ cl.aComp          -- AP complement, regardless of cltype
    ++ statementNounComp -- NP complement if it's direct statement

    ++ prepCombPos -- prepositions + pron. objects in positive sentence

    ++ cl.sii   -- restricted set of particles
    ++ cl.dhex  -- restricted set of nouns/adverbials
    ++ cl.obj2   -- "second object"
    ++ cl.vComp.inf  -- VV complement, if it's infinitive
    ++ cl.pred ! cltyp ! t ! a ! p  -- the inflecting verb
    ++ questionNounObj -- noun object if it's a question
    ++ cl.vComp.subcl -- VV complement, if it's subordinate clause
    ++ cl.miscAdv    ---- NB. Only used if there are several adverbs, or for "waa in" construction.
    } ;              ---- Primary places for adverbs are obj, sii or dhex.


  VFun : Type = Tense -> Anteriority -> Polarity -> Agreement -> BaseVerb
    -> Str ;

  vf : ClType -> VFun = \clt -> case clt of {
    Subord     => vfSubord ;
    WhQuestion => vfQuestion ; -- INF + waayaa 'why did you fail to go'
    _          => vfStatement } ;

  vfStatement : VFun = \t,ant,p,agr,vp ->
    case <t,ant,p> of {
      <Cond,_,Pos> => vp.s ! VInf ++ pastV have_V ;
      <Cond,_,Neg> => condNegV vp ;
      <Pres,Simul> => presV vp ;
      <Past,Simul> => pastV vp ;
      <Pres,Anter> => vp.s ! VInf ++ presCopula ! agrPol ; ---- just guessing
      <Past,Anter> => vp.s ! VInf ++ pastV (cSug "jir")  ;
      <Fut,Simul>  => vp.s ! VInf ++ presV (cSug "doon") ;
      <Fut,Anter>  => vp.s ! VInf ++ pastV (cSug "doon")
      }
    where {
      agrPol : {agr:Agreement ; pol:Polarity} = {agr=agr; pol=p} ;
      pastV : BaseVerb -> Str = \v ->
        case p of { Neg => v.s ! VNegPast Simple ;
                    Pos => v.s ! VPast Simple (agr2vagr agr) } ;

      presV : BaseVerb -> Str = \v -> v.s ! VPres Simple (agr2vagr agr) p ;

      condNegV : BaseVerb -> Str = \v -> case agr of {
          Sg2|Sg3 Fem
           |Pl2 => v.s ! VNegCond SgFem ;
          Pl1 _ => v.s ! VNegCond PlInv ;
          _     => v.s ! VNegCond SgMasc --Sg1|Sg3 Masc|Pl3|Impers
          }
      } ;

  vfQuestion : VFun = \t,ant,p,agr,vp ->
    case <t,ant,p> of {
      <_,_,Neg> => vp.s ! VInf ++ vfStatement t ant Pos agr (useV fail_V) ;
      _ => vfStatement t ant p agr vp
    } ;

  vfSubord : VFun = \t,ant,p,agr,vp ->
    case <t,ant,p> of {
      <Pres,Simul,Pos> => vfStatement Pres ant Neg agr vp ;
      _ => vfStatement t ant p agr vp
      } ; -- TODO other relative forms

  infVP : VerbPhrase -> Str = linVP VInf Statement ;

  STMarker : Type = ClType => Polarity => Str ;

  -- NB. Agreement is used only for negative questions. If we want to change it
  -- in other sentence types, we need to change predVP and mkClause accordingly;
  -- certain VVs put stuff between STM and subject pronoun. Some VVs render now
  -- incorrectly in negative questions.
  mkStm : Agreement -> STM -> STMarker = \agr,stm ->
    \\cltyp,pol =>
      case <cltyp,pol> of {
        <Statement,Pos> => showSTM stm ;
        <Statement,Neg> => "ma" ;
        <Subord,Pos>    => [] ;
        <Subord,Neg>    => "aan" ;
        <WhQuestion,_>  => "ma" ; -- neg. wh-questions are formed with waayaa 'fail to do sth', so they are syntactically positive
        <PolarQuestion,Pos> => "ma" ;
        <PolarQuestion,Neg> => case agr of { -- Negative question in past tense has only one form, need subject pronoun to know what the subject is.
          Sg1 => "miyaanan" ; -- Saeed p. 200
          Sg2 => "miyaanad" ; -- Saeed p. 200
          Sg3 Masc => "miyaanu" ; -- Saeed p. 200
          Sg3 Fem  => "miyaanay" ; -- ???
          Pl1 Excl => "miyaanaannu" ; -- ???
          Pl1 Incl => "miyaanaynu" ; -- ???
          Pl2 => "miyaanaydin" ; -- ???
          Pl3 => "miyaanay" ; -- ???
          Impers => "ma aan" } -- not merged
      } ;

  modSTM : (pos, neg : Str) -> STMarker -> STMarker = \pos,neg,stm ->
    \\cltyp,pol =>
      case pol of {
         Pos => pos ;
         _   => neg
      } ;
--------------------------------------------------------------------------------
-- linrefs

oper
  linCN : CNoun -> Str = \cn -> cn.s ! Indef Sg ++ cn.mod ! Indefinite ! Sg ! Abs ;
  linAdv : Adverb -> Str = \adv ->
     adv.berri
  ++ adv.sii
  ++ (prepTable ! adv.c2).s ! adv.np.a
  ++ adv.dhex
  ++ adv.np.s
  ++ adv.miscAdv ;

  linVP : VForm -> ClType -> VerbPhrase -> Str = \vf,cltyp,vp ->
    let pred = vp.s ! vf ;
        pr = prepCombTable ! vp.obj.a ! vp.c2 ;
        neg = case <cltyp,isNeg vf> of {
                <Subord,True> => "aan" ;
                _             => []
               } ;
     in wordOrder cltyp neg pred pr vp ;

  -- Light version of the word order complexity in mkClause.
  wordOrder : ClType -> (neg,pred,prepcomb : Str) -> VerbPhrase -> Str =
    \cltyp,neg,pred,pr,vp ->
        vp.berri -- AdV
     ++ case cltyp of {
          Subord => [] ;
          _ => vp.obj.s } -- noun object if it's a statement
     ++ neg
     ++ vp.vComp.subjunc -- "waa in" construction
     ++ case cltyp of {
          Subord => vp.obj.s ; -- noun object if it's subordinate clause
          _      => [] }
     ++ vp.aComp ! pagr2agr vp.obj.a  -- AP complement agreeing with object
     ++ pr       -- object if it's a pronoun
     ++ vp.sii   -- restricted set of particles
     ++ vp.dhex  -- restricted set of nouns/adverbials
     ++ vp.obj2   -- "second object"
     ++ vp.vComp.inf -- VV complement, if it's infinitive
     ++ pred         -- the verb inflected
     ++ vp.vComp.subcl ! Sg3 Masc  -- VV complement, if it's subordinate clause
     ++ vp.miscAdv ; ---- NB. Only used if there are several adverbs, or for "waa in" construction.

}
