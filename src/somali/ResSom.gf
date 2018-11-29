resource ResSom = ParamSom ** open Prelude, Predef, ParamSom in {

--------------------------------------------------------------------------------
-- Nouns
oper

  Noun : Type = {s : NForm => Str ; g : Gender} ;
  Noun2 : Type = Noun ** {c2 : Preposition} ;
  Noun3 : Type = Noun2 ** {c3 : Preposition} ;

  CNoun : Type = Noun ** {mod : Number => Case => Str ; hasMod : Bool} ;

  PNoun : Type = {s : Str ; a : Agreement} ;

  mkPNoun : Str -> Agreement -> PNoun = \str,agr -> {s = str ; a = agr} ;

  mkNoun : (x1,_,_,x4 : Str) -> Gender -> Noun = \wiil,wiilka,wiilal,wiilasha,gender ->
    let bisadi = case gender of
                   { Fem  => case wiil of { _ + #c => wiil+"i" ; _ => wiil} ;
                     Masc => wiil } ;
        bisadood =  case gender of
                       { Fem  => case wiilal of { _ + "o" => wiilal+"od" ; _ => wiil} ;
                         Masc => wiil } ;
        defStems : Str -> Vowel => Str = \s -> case s of {
          ilk + "aha" =>
               table { vA => ilk+"ah" ;
                       vE => ilk+"eh" ;
                       vI => ilk+"ih" ;
                       vO => ilk+"oh" ;
                       vU => ilk+"uh"
                       } ;
          _ => table { _ => init s }
          } ;

    in { s = table {
           Indef Sg => wiil ;
           Indef Pl => wiilal ;
           IndefNom => bisadi ;
           Numerative => bisadood ;
           Def Sg vow => defStems wiilka ! vow ;
           Def Pl vow => defStems wiilasha ! vow } ;
         g = gender } ;

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
                 ka = allomorph mKa mas ;
                 ta = allomorph mTa mas ;
                 sha = case ta of {"sha" => ta ; _ => s + ta } in
    mkNoun mas (mas + ka) (mas + "a" + s) (mas + "a" + sha) Masc ;

  -- 4a) Feminine, plural with ó
  nUl ul = let o  = case last ul of { "i" => "yo" ; _ => "o" } ;
               u  = case last ul of { "l" => init ul ; _ => ul } ;
               sha = allomorph mTa ul in
    mkNoun ul (u + sha) (ul + o) (ul + "aha") Fem ;

  -- 4b) Masculine, plural with ó, 2 syllables
  nGuri guri = let o = allomorph mO guri ;
                   ga = allomorph mKa guri ;
                   gury = case last guri of { -- TODO does this generalise? Or just exception?
                                 "i" => init guri + "y" ;
                                 _   => guri } in
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
-- Smart paradigm
-- Substantiv som slutar på –o/–ad är så gott som alltid feminina, t.ex. qaáddo sked, bisád katt.
-- Substantiv som slutar på –e är så gott som alltid maskulina, t.ex. dúbbe hammare, fúre nyckel.
-- För övriga ord säger ordets form dessvärre väldigt lite om ordets genus. Däremot kan betoningens plats i ordet väldigt ofta avslöja ordets genus. Man kan alltså i flesta fall höra vilket genus ett substantiv har.

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

  mkNg : Str -> Gender -> Noun = \n,g -> case n of {
      _ + ("r"|"n"|"l"|"g")
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
    sp : Str } ;

  NounPhrase : Type = BaseNP ** {s : Case => Str} ;

  useN : Noun -> CNoun ** BaseNP = \n -> n **
    { mod = \\_,_ => [] ; hasMod = False ;
      a = Sg3 n.g ; isPron = False ; sp = []
    } ;
--------------------------------------------------------------------------------
-- Pronouns
-- De somaliska possessiva pronomenen, precis som de svenska, har två olika genusformer i singular och en enda form i plural.
--  ägaren då det ägda föremålet är
--  m.sg. f.sg.plural
--  kayga tayda kuwayga
--  kaaga taada kuwaaga
--  kiisa tiisa kuwiisa
--  keeda teeda kuweeda
--
--  kaayaga taayada kuwayaga (1 pl. exkl.)
--  keenna teenna kuweenna (1 pl. inkl.)
--  kiinna tiinna kuwiinna
--  kooda tooda kuwooda
  Pronoun : Type = NounPhrase ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord
  Quant : Type = SS ; ---- TODO

  Determiner : Type = {
    s : Case => Str ;
   sp : Gender => Case => Str ;
    d : NForm
    } ;

  mkDeterminer : (x1,_,x3 : Str) -> NForm -> Determiner = \an,kani,tani,nf ->
    let ani : Str = case an of { _ + #c => an+"i" ;
                                 _      => case nf of { Def _ _ => "u" ;
                                                        _       => [] }
                               } ;
        bind : Str -> Str = \x -> case x of { "" => [] ;  _ => BIND ++ x } ;
    in { s = table { Nom => bind ani ; Abs => bind an } ;
        sp = table { Fem => table { Nom => tani ; Abs => init tani } ;
                     Masc => table { Nom => kani ; Abs => init kani }
                   } ;
         d = nf
       } ;

--------------------------------------------------------------------------------
-- Prepositions

-- Prepositionen u dras obligatoriskt samman med föregående pronomen
-- så att /a/ + /u/ > /oo/.

  Prep : Type = {s : Agreement => Str} ;

  mkPrep : (x1,_,_,_,_,x6 : Str) -> Prep = \ku,ii,kuu,noo,idiin,loo -> {
    s = table {
          Sg1      => ii ;
          Sg2      => kuu ;
          Pl2      => idiin ;
          Pl1 Excl => noo ;
          Pl1 Incl => "i" + noo ;
          Impers   => loo ;
          _        => ku
        }
    } ;

  prepTable : Preposition => Prep = table {
    ku => mkPrep "ku" "igu" "kugu" "nagu" "idinku" "lagu" ;
    ka => mkPrep "ka" "iga" "kaa" "naga" "idinka" "laga" ;
    la => mkPrep "la" "ila" "kula" "nala" "idinla" "lala" ;
    u  => mkPrep "u" "ii" "kuu" "noo" "idiin" "loo" ;
    noPrep => mkPrep [] "i" "ku" "na" "idin" "la"
  } ;

  prepCombTable : Agreement => PrepCombination => Str = table {
    Sg1 => table { ugu => "iigu" ; uga => "iiga" ;
                   ula => "iila" ; kaga => "igaga" ;
                   kula => "igula" ; kala => "igala" ;
                   Single p => (prepTable ! p).s ! Sg1 } ;
    Sg2 => table { ugu => "kuugu" ; uga => "kaaga" ;
                   ula => "kuula" ; kaga => "kaaga" ;
                   kula => "kugula" ; kala => "kaala" ;
                   Single p => (prepTable ! p).s ! Sg2 } ;
    Pl1 Excl =>
           table { ugu => "noogu" ; uga => "nooga" ;
                   ula => "noola" ; kaga => "nagaga" ;
                   kula => "nagula" ; kala => "nagala" ;
                   Single p => (prepTable ! p).s ! Pl1 Excl } ;
    Pl1 Incl =>
           table { ugu => "inoogu" ; uga => "inooga" ;
                   ula => "inoola" ; kaga => "inagaga" ;
                   kula => "inagula" ; kala => "inagala" ;
                   Single p => (prepTable ! p).s ! Pl1 Incl } ;

    Pl2 => table { ugu => "idiinku" ; uga => "idiinka" ;
                   ula => "idiinla" ; kaga => "idinkaga" ;
                   kula => "idinkula" ; kala => "idinkala" ;
                   Single p => (prepTable ! p).s ! Pl2 } ;
    Impers =>
           table { ugu => "loogu" ; uga => "looga" ;
                   ula => "loola" ; kaga => "lagaga" ;
                   kula => "lagula" ; kala => "lagala" ;
                   Single p => (prepTable ! p).s ! Impers } ;
--
    a   => table { ugu => "ugu" ; uga => "uga" ;
                   ula => "ula" ; kaga => "kaga" ;
                   kula => "kula" ; kala => "kala" ;
                   Single p => (prepTable ! p).s ! a }
  } ;

-- TODO: Negationen má `inte' skrivs samman med en föregående preposition.

--------------------------------------------------------------------------------
-- Adjectives

-- Sequences of adjectives follow the rules for restrictive relatives clauses, i.e. are linked by oo 'and' on an indefinite head NounPhrase and by ee 'and' on a definite NounPhrase (8.1).

 -- Komparativ
 -- För att uttrycka motsvarigheten till svenskans komparativ placerar man på somaliska helt enkelt prepositionen ká 'från, av, än' framför adjektivet i fråga. Adjektivet får ingen ändelse.
 -- Shan waa ay ká yar tahay siddéed. Fem är mindre än åtta.
 -- Superlativ
 -- Motsvarigheten till svenskans superlativ bildas med prepositionsklustret ugú som till sin betydelse närmast motsvarar svenskans allra, t.ex.
 -- ugu horrayntii (det att komma) allra först

  Adjective : Type = {s : AForm => Str} ;
  Adjective2 : Type = Adjective ** { c2 : Preposition } ;


  duplA : Str -> Adjective = \yar ->
    let yaryar = duplicate yar
    in mkAdj yar yaryar ;

  mkAdj : (str,pl : Str) -> Adjective = \sg,pl -> {
    s = table {
          AF Sg Abs => sg ;
          AF Pl Abs => pl ;
          AF Sg Nom => sg + "i" ;
          AF Pl Nom => pl + "i" }
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

  AdjPhrase : Type = Adjective ;
--------------------------------------------------------------------------------
-- Verbs

  Verb : Type = {s : VForm => Str} ;
  Verb2 : Type = Verb ** {c2 : Preposition} ;
  Verb3 : Type = Verb2 ** {c3 : Preposition} ;

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

        -- Some predictable sound changes
        t : Str = case arag of { -- kari+seen, noq+deen, (sug|joogsa|qaada)+teen,
               _ + ("i"|"y") => "s" ;     -- t changes into s in front of i/y
               _ + ("x"|"q"|"c") => "d" ; -- t changes into d in front of x/q/c
               _             => "t" } ;   
        ay : Str = case ark of {
               _ + ("i"|"e") => "ey" ;
               _             => "ay" } ;
        n : Str = case arag of {
               _ + #v => "nn" ; -- n duplicates after vowel
               _      => "n" } ;
        an : Str = case qaado of {
               _ + "o" => "an" ; -- Allomorph for imperatives
               _       => "in" } ;
   in { s = table {
          VPres (Sg1|Sg3 Masc|Impers) pol
                        => qaat + if_then_Pol pol "aa" "o" ;
          VPres (Sg2|Sg3 Fem) pol
                        => arag + t + if_then_Pol pol "aa" "o" ;
          VPres (Pl1 _) pol
                        => arag + n + if_then_Pol pol "aa" "o"  ;
          VPres Pl2 pol => arag + t + "aan" ;
          VPres Pl3 pol => qaat + "aan" ;

          VPast (Sg1|Sg3 Masc|Impers)
                        => qaat + ay ;
          VPast (Sg2|Sg3 Fem)
                        => arag + t + ay ; -- t, d or s
          VPast (Pl1 _) => arag + n + ay ;
          VPast Pl2     => arag + t + "een" ; -- t, d or s
          VPast Pl3     => qaat + "een" ;

          VImp Sg Pos   => qaado ;
          VImp Pl Pos   => qaat + "a" ;
          VImp Sg Neg   => arag + an ;
          VImp Pl Neg   => qaat + "ina" ;

          VInf          => arki ;
          VRel          => arki ; -- TODO does this exist?
          VNegPast      => arkin }
      } ;

-------------------------
-- Regular verb paradigms

  cSug, cKari, cYaree, cJoogso, cQaado : Str -> Verb ;

  cSug sug =
    let cabb : Str = case sug of {
          _ + "b" => sug + "b" ; -- TODO: more duplication patterns
          _       => sug }
     in mkVerb sug cabb sug ;

  cKari, cYaree = \kari -> mkVerb kari (kari+"y") kari ;

  cJoogso joogso =
    let joogsa = init joogso + "a" ;
     in mkVerb joogso (joogsa + "d") joogsa ;

  cQaado qaado =
    let qaa = drop 2 qaado
     in mkVerb qaado  -- Imperative sg, with the vowel
              (qaa + "t")    -- Per1 Sg, Per3 Pl and Per3 Sg Masc
              (qaa + "da") ; -- Per2 Pl and others

  -- Constructs verbs like u baahan+ahay
  prefixV : Str -> Verb -> Verb = \s,v -> {
    s = \\vf => s + v.s ! vf
  } ;

------------------
-- Irregular verbs

  copula : Verb = {
    s = table {
          VPres Sg1 pol    => if_then_Pol pol "ahay" "ihi" ;
          VPres Sg2 pol    => if_then_Pol pol "tahay" "ihid" ;
          VPres (Sg3 Masc|Impers) pol => if_then_Pol pol "yahay" "aha" ;
          VPres (Sg3 Fem)  pol => if_then_Pol pol "tahay" "aha" ;
          VPres (Pl1 _) pol => if_then_Pol pol "nahay" "ihin" ;
          VPres Pl2 pol     => if_then_Pol pol "tihiin" "ihidin" ;
          VPres Pl3 pol     => if_then_Pol pol "yihiin" "aha" ;
          VImp Sg pol       => if_then_Pol pol "ahaw" "ahaanin" ;
          VImp Pl pol       => if_then_Pol pol "ahaada" "ahaanina" ;

          VPast (Sg1|Sg3 Masc|Impers)
                          => "ahaa" ;
          VPast (Sg2|Sg3 Fem)
                          => "ahayd" ;
          VPast (Pl1 _)   => "ahayn" ;
          VPast Pl2       => "ahaydeen" ;
          VPast Pl3       => "ahaayeen" ;
          VNegPast        => "ahi" ;
          VRel            => "ah" ;
          VInf            => "ahaan" }
     } ;
   -- I somaliskan används inte något kopulaverb motsvarande svenskans är mellan
   -- två substantivfraser som utgör subjekt respektive predikatsfyllnad.
   -- Observera också att kopulaverbet vara alltid hamnar efter det adjektiv
   -- som utgör predikatsfyllnaden.
  have_V : Verb =
   let hold_V = mkVerb "hayso" "haysat" "haysa" in {
    s = table {
          VPres Sg1        Pos => "leeyahay" ;
          VPres Sg2        Pos => "leedahay" ;
          VPres (Sg3 Fem)  Pos => "leedahay" ;
          VPres (Sg3 Masc|Impers) Pos
                                => "leeyahay" ;
          VPres (Pl1 _)    Pos => "leenahay" ;
          VPres Pl2        Pos => "leedihiin" ;
          VPres Pl3        Pos => "leeyihiin" ;
          VPast x               => "l" + copula.s ! VPast x ;
          VRel                  => "leh" ;
          x                     => hold_V.s ! x }
    } ;
          
-- Till VERBFRASEN ansluter sig
-- · satstypsmarkörer (waa, ma...),
-- · subjekts-pronomenet la man,
-- · objektspronomenen,
-- · prepositionerna och
-- · riktnings-adverben soó (mot en plats/person), sií (bort frånen plats/person), wadá tillsammans (mot en gemensam punkt), kalá iväg, isär (bort från en gemensam punkt).
-- Riktningsadverben har ibland en mycket konkret betydelse, men många gånger är betydelsen mera abstrakt.

-- Till satsmarkörerna, dvs. både fokusmarkörerna och satstypsmarkörerna ansluter sig
-- subjektspronomenen aan, aad, uu, ay, aynu, men inte la (man).

------------------
-- VP
  Adverb : Type = {s,s2 : Str} ;

  Complement : Type = {
    comp : Agreement => {p1,p2 : Str} -- Agreement for AP complements
    } ;

  VerbPhrase : Type = Verb ** Complement ** {
    isPred : Bool ;      -- to choose right sentence type marker
    adv : Adverb ;          -- they're ~complicated~
    c2, c3 : Preposition -- can combine together and with object pronouns
    } ;

  VPSlash : Type = VerbPhrase ; ---- TODO more fields

  useV : Verb -> VerbPhrase = \v -> v ** {
    comp = \\_ => <[],[]> ;
    isPred = False ;
    adv = {s,s2 = []} ;
    c2,c3 = noPrep ;
    } ;

  compl : NounPhrase -> VerbPhrase -> Str = \np,vp ->
    prepCombTable ! np.a ! combine vp.c2 vp.c3 ;

  complV2 : NounPhrase -> Verb2 -> Str = \np,vp ->
      prepCombTable ! np.a ! combine vp.c2 noPrep ;
--------------------------------------------------------------------------------
-- Sentences etc.
  Clause : Type = {s : Tense => Anteriority => Polarity => Str} ;
  RClause,
  ClSlash,
  Sentence : Type = SS ; ---- TODO

  doonaa : Str -> Verb = \inf -> 
    let doon : Verb = cSug "doon" in {s = \\vf => inf ++ doon.s ! vf} ;

  vf : Tense -> Anteriority -> Polarity -> Agreement -> Verb
    -> {fin : Str ; inf : Str} = \t,ant,p,agr,vp ->
     let pastV : Verb -> Str = \v ->
           case p of { Neg => v.s ! VNegPast ;
                       Pos => v.s ! VPast agr } ;
         presV : Verb -> Str = \v -> v.s ! VPres agr p ;
     in case <t,ant> of {
       <Pres,Simul> => {fin = presV vp      ; inf = [] } ;
       <Pres,Anter> => {fin = presV copula  ; inf = vp.s ! VInf } ; ---- just guessing
       <Past,Simul> => {fin = pastV vp      ; inf = [] } ;
       <Past,Anter> => {fin = pastV copula  ; inf = vp.s ! VInf } ; ---- TODO: habitual aspect
       <Fut,Simul>  => {fin = presV (doonaa (vp.s ! VInf)) ; inf = []} ;
       <Fut,Anter>  => {fin = pastV (doonaa (vp.s ! VInf)) ; inf = []} ;
       <_,Simul>  => {fin = presV vp ; inf = []} ; -- TODO conditional
       <_,Anter>  => {fin = pastV vp ; inf = []}   -- TODO conditional
       } ;

  stmarker : Agreement => Polarity => Str = \\a,b =>
    let stm = if_then_Pol b "w" "m"
     in stm + subjpron ! a ;

  stmarkerNoContr : Agreement => Polarity => Str = \\a,b =>
    let stm = if_then_Pol b "waa" "ma"
     in stm ++ subjpron ! a ;

  subjpron : Agreement => Str = table {
    Sg1|Pl1 _ => "aan" ;
    Sg2|Pl2   => "aad" ;
    Sg3 Masc  => "uu" ;
    _         => "ay" } ;

--------------------------------------------------------------------------------
-- linrefs

oper
  linVP : VerbPhrase -> Str = \vp -> vp.s ! VInf ; ----
  linCN : CNoun -> Str = \cn -> cn.s ! IndefNom ;
}
