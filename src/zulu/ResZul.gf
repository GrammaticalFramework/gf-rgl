--# -path=.:../abstract:../common:../../prelude

resource ResZul = open Prelude,Predef,ParamX in {

  param
    ClassGender = C1_2 | C1a_2a | C3_4 | C5_6 | C7_8 | C9_10 | C11_10 | C9_6 | C14 | C15 | C17 ;
    SemanticType = Human | Animate | Misc ;
    NForm = NFull | NReduced | NPoss | NLoc ;
    Agr = First Number | Second Number | Third ClassGender Number ;

    -- SMood = SIndic | SPot | SSubj ; -- | SConsec ;
    -- DMood = Princ | Part ; -- mood dimension that depends on grammatical context: principal and participial
    CType = MainCl | RelCl ;
    Aspect = Null | Prog | Excl ;
    BasicTense = PresTense | FutTense | PastTense | RemFutTense | RemPastTense ;
    -- ImpForm = Imper | Polite ;
    -- Polarity = Pos | Neg ;

    -- NOTE: Although Poulos+Msimang use "verb form" instead of mood,
    -- we use VForm (verb form) to indicate combination of all parameters
    -- regarding the verb, and hence will use use "mood" for convenience.

    -- replacing BasicTense with Tense, just for now
    -- VForm = VFIndic DMood Polarity BasicTense Aspect | VFPot DMood Polarity Aspect | VFSubj Polarity ;
    -- VForm = VFIndic DMood Polarity BasicTense Aspect | VFPot DMood Polarity Aspect | VFSubj Polarity ;
    VForm = VFIndic CType Polarity BasicTense ;
    VPType = CopIdent | CopAssoc | CopDescr | CopEq | VNPCompl | NoComp | VSCompl | AdvComp | CopLoc ; -- VACompl |
    AuxType = PartAux ; -- TODO: add SubjAux, InfAux, ConsecAux etc (p327)
    AType = AdjType | RelType ;

    AForm = AF1 | AF2 | AF3 ; -- two forms for implementing sound changes Poulos+Msimang p143, one for monosyllabic
    SCForm = SC | SCVow | SCNeg | SCNegVow | SCPS | SCPart | SCVowP | SCBe | SCRP ;
    OCForm = OC | OCAE | OCIOU | OCMono | OCThing ;
    RCForm = RelC | RelCA ;

    -- verb root characteristics
    RForm = R_a | R_ile | R_e | R_i | R_anga ;
    RInit = RA | RE | RI | RO | RU | RC ;
    Syl = SylMono | SylMult ;
    Voice = Active | Passive ;

    QuantDef = Article Specificity | Demonstrative Distance ;
    -- Definiteness = Indef | Def ;
    Specificity = Spec | Nonspec ;
    Distance = Dem1 | Dem2 | Dem3 ;

  oper

    prefix_nasal : Str -> Str = \r -> case r of {
      "ph"+x => "mp" + x ;
      "bh"+x => "mb" + x ;
      #nasal_de_asp+"h"+x => "n"+(take 1 r) + x ;
      "hl"+x => "nhl"+x ;
      "h"+x => "nk"+x ;
      "sh"+x => "ntsh"+x ;
      "l"+x => "nd"+x ;
      #nasal_m+x => "m"+r ;
      #nasal_ng+x => "ng"+r ;
      #nasal+x => r ;
      _ => "n"+r
    } ;
    --------------
    -- PRONOUNS --
    --------------
    -- mkFullPron : Str -> Agr -> { s : NForm => Str ; agr : Agr ; empty : Str ; proDrop : Bool } = \s,agr -> {
    --   s = s ;
    --   agr = agr ;
    --   empty = [] ;
    --   proDrop = False
    -- } ;

    mkPron : Agr -> { s : NForm => Str ; agr : Agr ; empty : Str ; proDrop : Bool } = \agr -> {
      s = table {
        NFull => pron_stem!agr +"na" ;
        NReduced => pron_stem!agr ;
        NPoss => poss_pron_stem!agr ;
        NLoc => case agr of {
          First _ | Second Pl => "ki" ++BIND++ pron_stem!agr ;
          _ => "ku" ++BIND++ pron_stem!agr
        }
      } ;
      agr = agr ;
      empty = [] ;
      proDrop = False
    } ;

    full_pron : Str -> Str = \s -> s ++BIND++ "na" ;

    pron_stem : Agr => Str = table {
      First Sg => "mi" ;
      First Pl => "thi" ;
      Second Sg => "we" ;
      Second Pl => "ni" ;
      Third C1_2 Sg => "ye" ;
      Third C1_2 Pl => "bo" ;
      Third C1a_2a Sg => "ye" ;
      Third C1a_2a Pl => "bo" ;
      Third C3_4 Sg  => "wo" ;
      Third C3_4 Pl => "yo" ;
      Third C5_6 Sg => "lo" ;
      Third C5_6 Pl => "wo" ;
      Third C7_8 Sg => "so" ;
      Third C7_8 Pl => "zo" ;
      Third C9_10 Sg => "yo" ;
      Third C9_10 Pl => "zo" ;
      Third C11_10 Sg => "lo" ;
      Third C11_10 Pl => "zo" ;
      Third C9_6 Sg => "yo" ;
      Third C9_6 Pl => "wo" ;
      Third C14 _ => "bo" ;
      Third C15 _ => "kho" ;
      Third C17 _ => "kho"
    } ;

    poss_pron_stem : Agr => Str = table {
      First Sg => "mi" ;
      First Pl => "thu" ;
      Second Sg => "kho" ;
      Second Pl => "nu" ;
      Third C1_2 Sg => "khe" ;
      Third C1_2 Pl => "bo" ;
      Third C1a_2a Sg => "khe" ;
      Third C1a_2a Pl => "bo" ;
      Third C3_4 Sg  => "wo" ;
      Third C3_4 Pl => "yo" ;
      Third C5_6 Sg => "lo" ;
      Third C5_6 Pl => "wo" ;
      Third C7_8 Sg => "so" ;
      Third C7_8 Pl => "zo" ;
      Third C9_10 Sg => "yo" ;
      Third C9_10 Pl => "zo" ;
      Third C11_10 Sg => "lo" ;
      Third C11_10 Pl => "zo" ;
      Third C9_6 Sg => "yo" ;
      Third C9_6 Pl => "wo" ;
      Third C14 _ => "bo" ;
      Third C15 _ => "kho" ;
      Third C17 _ => "kho"
    } ;

    dem_pron : Distance => Agr => Str = table {
      Dem1 => table {
        First Sg => "lo" ;
        First Pl => "laba" ;
        Second Sg => "lo" ;
        Second Pl => "laba" ;
        Third C1_2 Sg => "lo" ;
        Third C1_2 Pl => "laba" ;
        Third C1a_2a Sg => "lo" ;
        Third C1a_2a Pl => "laba" ;
        Third C3_4 Sg => "lo" ;
        Third C3_4 Pl => "le" ;
        Third C5_6 Sg => "leli" ;
        Third C5_6 Pl => "la" ;
        Third C7_8 Sg => "lesi" ;
        Third C7_8 Pl => "lezi" ;
        Third C9_10 Sg => "le" ;
        Third C9_10 Pl => "lezi" ;
        Third C11_10 Sg => "lolu" ;
        Third C11_10 Pl => "lezi" ;
        Third C9_6 Sg => "le" ;
        Third C9_6 Pl => "la" ;
        Third C14 _ => "lobu" ;
        Third C15 _ => "lokhu" ;
        Third C17 _ => "lapha"
      } ;
      Dem2 => table {
        First Sg => "lowo" ;
        First Pl => "labo" ;
        Second Sg => "lowo" ;
        Second Pl => "labo" ;
        Third C1_2 Sg => "lowo" ;
        Third C1_2 Pl => "labo" ;
        Third C1a_2a Sg => "lowo" ;
        Third C1a_2a Pl => "labo" ;
        Third C3_4 Sg => "lowo" ;
        Third C3_4 Pl => "leyo" ;
        Third C5_6 Sg => "lelo" ;
        Third C5_6 Pl => "lawo" ;
        Third C7_8 Sg => "leso" ;
        Third C7_8 Pl => "lezo" ;
        Third C9_10 Sg => "leyo" ;
        Third C9_10 Pl => "lezo" ;
        Third C11_10 Sg => "lolo" ;
        Third C11_10 Pl => "lezo" ;
        Third C9_6 Sg => "leyo" ;
        Third C9_6 Pl => "lawo" ;
        Third C14 _ => "lobo" ;
        Third C15 _ => "lokho" ;
        Third C17 _ => "lapho"
      } ;
      Dem3 => table {
        First Sg => "loya" ;
        First Pl => "labaya" ;
        Second Sg => "loya" ;
        Second Pl => "labaya" ;
        Third C1_2 Sg => "loya" ;
        Third C1_2 Pl => "labaya" ;
        Third C1a_2a Sg => "loya" ;
        Third C1a_2a Pl => "labaya" ;
        Third C3_4 Sg => "loya" ;
        Third C3_4 Pl => "leya" ;
        Third C5_6 Sg => "leliya" ;
        Third C5_6 Pl => "lawaya" ;
        Third C7_8 Sg => "lesiya" ;
        Third C7_8 Pl => "leziya" ;
        Third C9_10 Sg => "leya" ;
        Third C9_10 Pl => "leziya" ;
        Third C11_10 Sg => "loluya" ;
        Third C11_10 Pl => "leziya" ;
        Third C9_6 Sg => "leya" ;
        Third C9_6 Pl => "lawaya" ;
        Third C14 _ => "lobuya" ;
        Third C15 _ => "lokhuya" ;
        Third C17 _ => "laphaya"
      }
    } ;

    -----------
    -- VERBS --
    -----------
    regVerb : Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \root ->
    {
      s = table {
        R_a => root ++BIND++ "a" ;
        R_ile => case root of {
          _+"el" => root ++BIND++ "e" ;
          _+"al" => (tk 2 root) + "el" ++BIND++ "e" ;
          _ => root ++BIND++ "ile"
        } ;
        R_e => case root of {
          _+"al" => (tk 2 root) + "el" ++BIND++ "e" ;
          _ => root ++BIND++ "e"
        } ;
        R_i => root ++BIND++ "i" ;
        R_anga => root ++BIND++ "anga"
      } ;
      r = case root of {
        "a"+_ => RA ;
        "e"+_ => RE ;
        "i"+_ => RI ;
        "o"+_ => RO ;
        "u"+_ => RU ;
        _ => RC
      } ;
      syl = case root of {
        _+#cons+#vowel+#cons+_ => SylMult ;
        _ => SylMono
      } ;
      voice = Active
    } ;

    th_Verb : Str -> Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \th,thi ->
    {
      s = table {
        R_a => thi ;
        R_ile => th ++BIND++ "ile" ;
        R_e => th ++BIND++ "e" ;
        R_i => th ++BIND++ "i" ;
        R_anga => th ++BIND++ "anga"
      } ;
      r = case th of {
        "a"+_ => RA ;
        "e"+_ => RE ;
        "i"+_ => RI ;
        "o"+_ => RO ;
        "u"+_ => RU ;
        _ => RC
      } ;
      syl = case th of {
        _+#cons+#vowel+#cons+_ => SylMult ;
        _ => SylMono
      } ;
      voice = Active
    } ;

    three_Verb : Str -> Str -> Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \root,r_a,r_ile -> {
      s = table {
        R_a => r_a ;
        R_ile => r_ile ;
        R_e => root ++BIND++ "e" ;
        R_i => root ++BIND++ "i" ;
        R_anga => root ++BIND++ "anga"
      } ;
      r = case root of {
        "a"+_ => RA ;
        "e"+_ => RE ;
        "i"+_ => RI ;
        "o"+_ => RO ;
        "u"+_ => RU ;
        _ => RC
      } ;
      syl = case root of {
        _+#cons+#vowel+#cons+_ => SylMult ;
        _ => SylMono
      } ;
      voice = Active
    } ;

    four_Verb : Str -> Str -> Str -> Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \root,r_a,r_ile,r_e -> {
      s = table {
        R_a => r_a ;
        R_ile => r_ile ;
        R_e => r_e ;
        R_i => root ++BIND++ "i" ;
        R_anga => root ++BIND++ "anga"
      } ;
      r = case root of {
        "a"+_ => RA ;
        "e"+_ => RE ;
        "i"+_ => RI ;
        "o"+_ => RO ;
        "u"+_ => RU ;
        _ => RC
      } ;
      syl = case root of {
        _+#cons+#vowel+#cons+_ => SylMult ;
        _ => SylMono
      } ;
      voice = Active
    } ;

    -- irregVerb : Str -> Str -> Str -> Str -> Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \hamba,hambile,hambe,hambi,hambanga -> {
    --   s = table {
    --     R_a => hamba ;
    --     R_ile => hambile ;
    --     R_e => hambe ;
    --     R_i => hambi ;
    --     R_anga => hambanga
    --   } ;
    --   r = case root of {
    --     "a"+_ => RA ;
    --     "e"+_ => RE ;
    --     "i"+_ => RI ;
    --     "o"+_ => RO ;
    --     "u"+_ => RU ;
    --     _ => RC
    --   } ;
    --   syl = case root of {
    --     _+#cons+#vowel+#cons+_ => SylMult ;
    --     _ => SylMono
    --   } ;
    --   voice = Active
    -- } ;

    passiveVerb : Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \root ->
    {
      s = table {
        R_a => root ++BIND++ "a" ;
        R_ile => root ++BIND++ "ile" ;
        R_e => root ++BIND++ "e" ;
        R_i => root ++BIND++ "i" ;
        R_anga => root ++BIND++ "anga"
      } ;
      r = case root of {
        "a"+_ => RA ;
        "e"+_ => RE ;
        "i"+_ => RI ;
        "o"+_ => RO ;
        "u"+_ => RU ;
        _ => RC
      } ;
      syl = case root of {
        _+#cons+#vowel+#cons+_ => SylMult ;
        _ => SylMono
      } ;
      voice = Passive
    } ;

    -- Determine which form of the verb root to use
    -- we're keeping the case statement for when we add back -e and -ile
    rform : VForm -> Bool -> RForm = \vform,longform -> case longform of {
      True => case vform of {
        VFIndic _ Pos PresTense => R_a ;
        VFIndic MainCl Neg PresTense => R_i ;
        VFIndic RelCl Neg PresTense => R_i ;
        VFIndic _ _ FutTense => R_a ;
        VFIndic _ _ RemFutTense => R_a ;
        VFIndic _ Pos PastTense => R_ile ;
        VFIndic _ Neg PastTense => R_anga ;
        VFIndic _ Pos RemPastTense => R_a ;
        VFIndic _ Neg RemPastTense => R_anga
      } ;
      False => case vform of {
        VFIndic _ Pos PresTense => R_a ;
        VFIndic MainCl Neg PresTense => R_i ;
        VFIndic RelCl Neg PresTense => R_i ;
        VFIndic _ _ FutTense => R_a ;
        VFIndic _ _ RemFutTense => R_a ;
        VFIndic _ Pos PastTense => R_e ;
        VFIndic _ Neg PastTense => R_anga ;
        VFIndic _ Pos RemPastTense => R_a ;
        VFIndic _ Neg RemPastTense => R_anga
      }
    } ;

    -- VERB MORPHEMES --

    -- tense prefix
    tensePref : VForm -> RInit -> Syl -> Str = \vform,r,syl ->
      case <r,vform,syl> of {
        <RC,VFIndic _ Pos FutTense,SylMono> => "zoku" ++BIND ;
        <RC,VFIndic _ Pos FutTense,_> => "zo" ++BIND ;
        <_,VFIndic _ Pos FutTense,_> => "zokw" ++BIND ;
        <RC,VFIndic _ Neg FutTense,_> => "zuku" ++BIND ;
        <_,VFIndic _ Neg FutTense,_> => "zukw" ++BIND ;

        <RC,VFIndic _ Pos RemFutTense,SylMono> => "yoku" ++BIND ;
        <RC,VFIndic _ Pos RemFutTense,_> => "yo" ++BIND ;
        <_,VFIndic _ Pos RemFutTense,_> => "yokw" ++BIND ;
        <RC,VFIndic _ Neg RemFutTense,_> => "yuku" ++BIND ;
        <_,VFIndic _ Neg RemFutTense,_> => "yukw" ++BIND ;

        <(RA|RE),VFIndic _ _ RemPastTense> => [] ;
        <_,VFIndic _ Pos RemPastTense> => "a" ++BIND ;
        <_,VFIndic _ _ _,_> => [] --;
        -- VFPot _ _ _ => [] ;
        -- VFSubj _ => []
      } ;

    -- negative prefix
    negPref : VForm -> Str = \vform ->
      case vform of {
        VFIndic _ Neg _ => "a"++BIND ;
        VFIndic _ _ _ => []
      } ;

      -- TODO : sound rules to choose between nge and nga
    -- negPref2 : VForm -> Str = \vform ->
    --   case vform of {
    --     VFIndic Part Neg FutTense Null => "nga" ++BIND ; -- sometimes nge? p274
    --     -- VFIndic Part Neg _ _ => pre { "z" => "nge" ; _ => "nga" } ++BIND ;
    --     VFIndic Part Neg _ _ => "nga" ++BIND ;
    --     VFIndic _ _ _ _ => [] ;
    --     VFPot _ _ _ => [] ;
    --     VFSubj Neg => pre { "z" => "nge" ; _ => "nga" } ++BIND ;
    --     VFSubj Pos => []
    --   } ;

    negPrefNga : VForm -> Str = \vform -> case vform of {
      VFIndic _ Neg _ => "nga" ;
      VFIndic _ Pos _ => []
    } ;

    negPrefNge : VForm -> Str = \vform -> case vform of {
      VFIndic _ Neg _ => "nge" ;
      VFIndic _ Pos _ => []
    } ;

    icompNeg1 : VForm -> Str = \vform -> case vform of {
      VFIndic _ Neg PresTense => "a"++BIND ;
      VFIndic _ _ _ => []
    } ;

    icompNeg2 : VForm -> Str = \vform -> case vform of {
      VFIndic _ Neg _ => "nga"++BIND ;
      VFIndic _ _ _ => []
    } ;

    -- -- progressive prefix
    -- progPref : VForm -> Str = \vform ->
    --   case vform of {
    --     VFIndic _ Pos PastTense Prog => nonExist ; -- progressive past does not occur
    --     VFIndic _ Pos PastTense _ => [] ;
    --     VFIndic _ Pos _ Prog => "sa" ++BIND ;
    --     VFIndic _ Pos _ _ => [] ;
    --     VFIndic _ Neg FutTense Prog => "se" ++BIND ;
    --     VFIndic _ Neg _ Prog => "sa" ++BIND ;
    --     VFIndic _ _ _ Prog => nonExist ;
    --     VFIndic _ _ _ _ => [] ;
    --     VFPot _ _ Prog => "se" ++BIND ;
    --     VFPot _ _ _ => [] ;
    --     VFSubj _ => []
    --
    --   } ;

    -- progressive prefix
    -- progPref : VForm -> Str = \vform ->
    --   case vform of {
    --     VFIndic _ Pos PastTense _ => nonExist ; -- progressive past does not occur
    --     -- VFIndic _ Pos PastTense _ => [] ;
    --     VFIndic _ Pos _ _ => "sa" ++BIND ;
    --     -- VFIndic _ Pos _ _ => [] ;
    --     VFIndic _ Neg FutTense _ => "se" ++BIND ;
    --     VFIndic _ Neg _ _ => "sa" ++BIND ;
    --     VFIndic _ _ _ _ => nonExist ;
    --     -- VFIndic _ _ _ _ => [] ;
    --     VFPot _ _ _ => "se" ++BIND ;
    --     -- VFPot _ _ _ => [] ;
    --     VFSubj _ => nonExist
    --
    --   } ;

    -- exclusive se prefix
    -- exclSePref : VForm -> Str = \vform ->
    --   case vform of {
    --     VFIndic _ Pos _ Excl => "se"++BIND ;
    --     VFIndic _ _ _ _ => [] ;
    --     VFPot _ Pos Excl => "se"++BIND ;
    --     VFPot _ _ _ => [] ;
    --     VFSubj _ => []
    --   } ;
    --
    -- -- exclusive ka prefix
    -- exclKaPref : VForm -> Str = \vform ->
    --   case vform of {
    --     VFIndic _ Neg (PresTense | FutTense) Excl => "ka" ++BIND ;
    --     VFIndic _ _ _ _ => [] ;
    --     VFPot _ _ _ => [] ;
    --     VFSubj _ => []
    --   } ;

    -- potential prefix
    -- potPref : VForm -> Str = \vform ->
    --   case vform of {
    --     VFPot _ Pos _ => "nga" ++BIND ;
    --     VFPot _ Neg _ => "nge" ++BIND ;
    --     VFIndic _ _ _ _ => [] ;
    --     VFSubj _ => []
    --   } ;

    -- VForm = VFIndic DMood Polarity BasicTense Aspect | VFPot DMood Polarity Aspect | VFSubj Polarity ;
    -- aux_be : VForm -> Agr -> Str = \vform,agr ->
    -- let
    --   sc = subjConc vform agr False ;
    --   scvow = subjConc vform agr True ;
    --   short_be = case agr of {
    --     -- Second Pl => sc ++ "bu" ;
    --     -- Third C3_4 Sg => sc ++ "bu" ;
    --     -- Third C3_4 Pl => sc ++ "bi" ;
    --     -- Third C9_6 Sg | Third C9_10 Sg => sc ++ "bi" ;
    --     First _ | Second _ | Third _ _ => subjConcLookup!agr!SCBe
    --   }
    -- in
    -- case vform of {
    --   VFIndic Princ Pos PresTense _ => [] ;
    --   VFIndic Princ Pos PerfTense _ => short_be ++BIND ; -- 2021-01-26, chose to only implement short form
    --   VFIndic Princ Pos FutTense _ => sc ++ "zobe" ;
    --   VFIndic Princ Pos PastTense _ => scvow ++ "abe" ;
    --
    --   VFIndic Princ Neg PresTense _ => [] ;
    --   VFIndic Princ Neg PerfTense _ => short_be ++BIND ;
    --   VFIndic Princ Neg FutTense _ => sc ++ "zobe" ;
    --   VFIndic Princ Neg PastTense _ => scvow ++ "abe" ;
    --
    --   VFIndic Part Pos PresTense _ => [] ;
    --   VFIndic Part Pos PerfTense _ => short_be ++BIND ;
    --   VFIndic Part Pos FutTense _ => sc ++ "zobe" ;
    --   VFIndic Part Pos PastTense _ => scvow ++ "abe" ;
    --
    --   VFIndic Part Neg PresTense _ => [] ;
    --   VFIndic Part Neg PerfTense _ => short_be ++BIND ;
    --   VFIndic Part Neg FutTense _ => sc ++ "zobe" ;
    --   VFIndic Part Neg PastTense _ => scvow ++ "abe" ;
    --
    --   VFPot _ Pos _ => sc ++ "ngaba" ;
    --   VFPot _ Neg _ => sc ++ "ngebe" ;
    --   VFSubj Pos => sc ++ "be" ++BIND ;
    --   VFSubj Neg => sc ++ "ngabi"
    -- } ;

    -------------
    -- ADVERBS --
    -------------
    regAdv : Str -> { s : Str ; asp : Aspect ; reqLocS : Bool } = \adv ->
    {
      s = adv ;
      asp = Null ;
      reqLocS = False
    } ;

    aspAdv : Str -> Aspect -> { s : Str ; asp : Aspect ; reqLocS : Bool } = \adv,asp ->
    {
      s = adv ;
      asp = asp ;
      reqLocS = False
    } ;

    -- together with
    withPref : RInit => Str = table {
      RU => "no" ;
      RI => "ne" ;
      RO => "no" ;
      _  => "na"
    } ;

    -- just like
    eqPref : RInit => Str = table {
      RU => "njengo" ;
      RI => "njenge" ;
      RO => "njengo" ;
      _  => "njenga"
    } ;

    -- as big as
    eqSizePref : RInit => Str = table {
      RU => "ngango" ;
      RI => "ngange" ;
      RO => "ngango" ;
      _  => "nganga"
    } ;

    -- with
    instrPref : RInit => Str = table {
      RU => "ngo" ;
      RI => "nge" ;
      RO => "ngo" ;
      _  => "nga"
    } ;

    --------------------
    -- QUALIFICATIVES --
    --------------------
    regAdj : Str -> { s : AForm => Str ; empty : Str ; t : AType } = \a ->
    {
      s = table {
        AF1 => a ;
        AF2 => prefix_nasal a ;
        AF3 => case a of {
          #cons+#cons*+#vowel => "u"+a ;
          _ => a
        }
      } ;
      -- b = case a of {
      --   ("kh"|"th"|"sh"|"b"|"f"|"hl")+_ => True ;
      --   ("m"|"n")+_ => True ;
      --   _ => False
      -- } ;
      empty = [] ;
      t = AdjType
    } ;

    relAdj : Str -> { s : AForm => Str ; empty : Str ; t : AType } = \a ->
    {
      s = \\_ => a ;
      -- b = case a of {
      --   ("kh"|"th"|"sh"|"b"|"f"|"hl")+_ => True ;
      --   ("m"|"n")+_ => True ;
      --   _ => False
      -- } ;
      empty = [] ;
      t = RelType
    } ;

    rel_yo_2 : Str = BIND++"yo" ;

    relSuf : VForm -> Str = \vform -> case vform of {
      VFIndic _ Pos PresTense => rel_yo_2 ;
      VFIndic _ Pos PastTense => rel_yo_2 ;
      VFIndic _ _ _ => []
    } ;

    -- chooses the form of the root to use for N-prefixes
    aformN : Agr -> AForm = \agr ->
      case agr of {
        Third C1_2 Sg => AF3 ;
        Third C1a_2a Sg => AF3 ;
        Third C3_4 Sg => AF3 ;
        Third C7_8 Pl => AF2 ;
        Third C9_10 Sg => AF2 ;
        Third C9_10 Pl => AF2 ;
        Third C9_6 Sg => AF2 ;
        Third C11_10 Pl => AF2 ;
        _ => AF1
      } ;

    -- TODO: check ; RInit is used to indicate what precedes the adj pref
    adjPrefLookup : Agr => VForm => Str =
      table {
        Third C1_2 Sg => table { VFIndic _ _ _ => "m"++BIND } ;
        Third C1_2 Pl => table { VFIndic _ _ _ => "ba"++BIND } ;
        Third C1a_2a Sg => table { VFIndic _ _ _ => "m"++BIND } ;
        Third C1a_2a Pl => table { VFIndic _ _ _ => "ba"++BIND } ;
        Third C3_4 Sg  => table { VFIndic _ _ _ => "m"++BIND } ;
        Third C3_4 Pl => table { VFIndic _ _ _ => "mi"++BIND } ;
        Third C5_6 Sg => table { VFIndic _ _ _ => "li"++BIND } ;
        Third C5_6 Pl => table { VFIndic _ _ _ => "ma"++BIND } ;
        Third C7_8 Sg => table { VFIndic _ _ _ => "si"++BIND } ;
        Third C7_8 Pl => table { VFIndic _ _ _ => "zi"++BIND } ; -- nasal for 8,9,10 assumed to be fixed to root
        Third C9_10 Sg => table {
          VFIndic MainCl Pos PresTense => "yi" ++BIND ;
          VFIndic _ Pos PresTense => [] ;
          VFIndic RelCl Neg PresTense => [] ;
          VFIndic _ _ FutTense => "yi"++BIND ;
          VFIndic _ _ RemFutTense => "yi"++BIND ;
          VFIndic _ _ PastTense => "yi" ++BIND ;
          VFIndic _ _ RemPastTense => "yi" ++BIND ;
          VFIndic _ _ _ => "i"++BIND
        } ;
        Third C9_10 Pl => table { VFIndic _ _ _ => "zi"++BIND } ;
        Third C11_10 Sg => table { VFIndic _ _ _ => "lu"++BIND } ;
        Third C11_10 Pl => table { VFIndic _ _ _ => "zi"++BIND } ;
        Third C9_6 Sg => table {
          VFIndic MainCl Pos PresTense => "yi" ++BIND ;
          VFIndic _ Pos PresTense => [] ;
          VFIndic RelCl Neg PresTense => [] ;
          VFIndic _ _ FutTense => "yi"++BIND ;
          VFIndic _ _ RemFutTense => "yi"++BIND ;
          VFIndic _ _ PastTense => "yi" ++BIND ;
          VFIndic _ _ RemPastTense => "yi" ++BIND ;
          VFIndic _ _ _ => "i"++BIND
        } ;
        Third C9_6 Pl => table { VFIndic _ _ _ => "ma"++BIND } ;
        Third C14 _ => table { VFIndic _ _ _ => "bu"++BIND } ;
        Third C15 _ => table { VFIndic _ _ _ => "ku"++BIND } ;
        Third C17 _ => table { VFIndic _ _ _ => "ku"++BIND } ;
        First Sg => table { VFIndic _ _ _ => "m"++BIND } ;
        First Pl => table { VFIndic _ _ _ => "ba"++BIND } ;
        Second Sg => table { VFIndic _ _ _ => "m"++BIND } ;
        Second Pl => table { VFIndic _ _ _ => "om"++BIND }
      } ;

      adjPref : Agr -> VForm -> Str = \agr,vform -> case vform of {
        VFIndic RelCl Pos PresTense => case agr of {
          (First Sg | Second Sg | Third C1_2 Sg | Third C1a_2a Sg | Third C3_4 Sg) => "m"++BIND ;
          (First Pl | Second Pl | Third _ _) => []
        } ;
        VFIndic _ _ _ => adjPrefLookup!agr!vform
      } ;

    atwhichPhiPref : Agr => Str =
      table {
        Third C1_2 Sg => "mu" ;
        Third C1_2 Pl => "ba" ;
        Third C1a_2a Sg => "mu" ;
        Third C1a_2a Pl => "ba" ;
        Third C3_4 Sg  => "mu" ;
        Third C3_4 Pl => "mi" ;
        Third C5_6 Sg => "li" ;
        Third C5_6 Pl => "ma" ;
        Third C7_8 Sg => "si" ;
        Third C7_8 Pl => "zi" ;
        Third C9_10 Sg => "yi" ;
        Third C9_10 Pl => "zi" ;
        Third C11_10 Sg => "lu" ;
        Third C11_10 Pl => "zi" ;
        Third C9_6 Sg => "yi" ;
        Third C9_6 Pl => "ma" ;
        Third C14 _ => "bu" ;
        Third C15 _ => "ku" ;
        Third C17 _ => "ku" ;
        First Sg => "mu" ;
        First Pl => "ba" ;
        Second Sg => "mu" ;
        Second Pl => "om"
      } ;

    -----------
    -- NOUNS --
    -----------
    -- worst case
    mkNoun : (noms,nomp,locs,locp : Str) -> ClassGender -> { s : Number => NForm => Str ; c : ClassGender ; empty : Str } =
      \noms,nomp,locs,locp,cg ->
      let
        sg_agr = Third cg Sg ;
        pl_agr = Third cg Pl ;
      in
      {
        s = table {
          Sg => table {
            NFull => noms ;
            NReduced => (drop_init_vowel noms) ;
            NPoss => (drop_init_vowel noms) ;
            NLoc => locs
        } ;
          Pl => table {
            NFull => nomp ;
            NReduced => (drop_init_vowel nomp) ;
            NPoss => (drop_init_vowel nomp) ;
            NLoc => locp
          }
        } ;
        c = cg ;
        empty = []
      } ;

    semiRegNoun : (root,locs,locp : Str) -> ClassGender -> { s : Number => NForm => Str ; c : ClassGender ; empty : Str } =
      \root,locs,locp,cg ->
      let
        noms : Str = nomNoun root Sg cg ;
        nomp : Str = nomNoun root Pl cg ;
      in
      mkNoun noms nomp locs locp cg ;

    mkELocN : (root : Str) -> ClassGender -> { s : Number => NForm => Str ; c : ClassGender ; empty : Str } =
      \root,cg ->
      let
        noms : Str = nomNoun root Sg cg ;
        nomp : Str = nomNoun root Pl cg ;
        locs : Str = onlyLocPrefix root Sg cg ;
        locp : Str = onlyLocPrefix root Pl cg ;
      in
      mkNoun noms nomp locs locp cg ;

    regNoun : Str -> ClassGender -> { s : Number => NForm => Str ; c : ClassGender ; empty : Str } =
      \root,cg ->
      let
        noms : Str = nomNoun root Sg cg ;
        nomp : Str = nomNoun root Pl cg ;
        locs : Str = locNoun root Sg cg ;
        locp : Str = locNoun root Pl cg ;
        empty = []
      in
      mkNoun noms nomp locs locp cg ;

    kwaProperName : Str -> ClassGender -> { s : Number => NForm => Str ; c : ClassGender ; empty : Str } =
    \root,cg ->
    let
      noms : Str = nomNoun root Sg cg ;
      nomp : Str = nomNoun root Sg cg ;
      locs : Str = "Kwa"+root ;
      locp : Str = "Kwa"+root ;
    in
      mkNoun noms nomp locs locp cg ;

    initNP : Bool -> Agr -> RInit = \ispron,agr -> case ispron of {
      True => RC ;
      False => nominit!agr
    } ;

    nominit : Agr => RInit =
    table {
      Third C1_2 Sg => RU ;
      Third C1_2 Pl => RA ;
      Third C1a_2a Sg => RU ;
      Third C1a_2a Pl => RO ;
      Third C3_4 Sg  => RU ;
      Third C3_4 Pl => RI ;
      Third C5_6 Sg => RI ;
      Third C5_6 Pl => RA ;
      Third C7_8 Sg => RI ;
      Third C7_8 Pl => RI ;
      Third C9_10 Sg => RI ;
      Third C9_10 Pl => RI ;
      Third C11_10 Sg => RU ;
      Third C11_10 Pl => RI ;
      Third C9_6 Sg => RI ;
      Third C9_6 Pl => RA ;
      Third C14 _ => RU ;
      Third C15 _ => RU ;
      Third C17 _ => RU ;
      (First _ | Second _ )  => RC
    } ;

    locinit : Agr => RInit =
    table {
      Third C1_2 Sg => RC ;
      Third C1_2 Pl => RC ;
      Third C1a_2a Sg => RC ;
      Third C1a_2a Pl => RC ;
      Third _ _  => RE ;
      (First _ | Second _ )  => RC
    } ;

    onlyLocPrefix : Str -> Number -> ClassGender -> Str = \root,n,cg ->
    case <cg,n> of
    {
      <C1_2,Sg> => case root of {
        _+#cons+#vowel+#cons+_+#vowel+_ => "kum"+root ;
        _ => "kumu"+root
      } ; -- umu for single syllables, um for the rest
      <C1_2,Pl> => "kuba"+root ; -- abe for tribes or guilds
      <C1a_2a,Sg> => "ku"+root ;
      <C1a_2a,Pl> => "ko"+root ;
      <C3_4,Sg> => case root of {
        ("m"|"n")+_ => "e"+root ;
        _ => "em"+root
      } ;
      <C3_4,Pl> => "emi"+root ;
      <C5_6,Sg> => "e"+root ; -- ili long form (not used?)
      <C5_6,Pl> => case root of {
        "i"+_ => "eme"+root ;
        _ => "ema"+root
      } ; -- ame for roots starting with i
      <C7_8,Sg> => case root of {
        #vowel+_ => "es"+root ;
        _ => "esi"+root
      } ; -- is for roots starting with vowel
      <C7_8,Pl> => case root of {
        #vowel+_ => "ez"+root ;
        _ => "ezi"+root  -- iz for roots starting with vowel
      } ;
      <C9_10,Sg> => "e"+(prefix_nasal root) ; -- em for labial, en for alveolar (TODO: does this correctly split options?)
      <C9_10,Pl> => "ezi"+(prefix_nasal root) ; -- izim for labial, izin for alveolar (TODO: does this correctly split options?)
      <C11_10,Sg> => "o"+root ;
      <C11_10,Pl> => "ezi"+(prefix_nasal root) ; -- izim for labial, izin for alveolar, izi(n|m)k for roots starting with kh
      <C9_6,Sg> => "e"+(prefix_nasal root) ; -- em for labial, en for alveolar (TODO: does this correctly split options?)
      <C9_6,Pl> => case root of {
        "i"+_ => "eme"+root ;
        _ => "ema"+root
      } ; -- ame for roots starting with i
      <C14,_> => "ebu"+root ;
      <C15,_> => case root of {
        ("a"|"e")+_ => "ekw"+root ;
        (#cons|"y")+_ => "eku"+root ;
        _ => "ek"+root
        } ; -- ukw for roots starting with a/e, uk for roots starting with o
      <C17,_> => "eku"+root  -- sometimes ukw
    } ;

    -- Src: Doke, Linda Hall
    addLocSuffix : Str -> Str = \root ->
      case root of
      {
        _+"mbo" => (tk 3 root) + "njeni" ;
        _+"mbu" => (tk 3 root) + "njini" ;
        _+"pho" => (tk 3 root) + "sheni" ;
        _+"bho" => (tk 3 root) + "jeni" ;
        _+"phu" => (tk 3 root) + "shini" ;
        _+"bhu" => (tk 3 root) + "jini" ;
        _+"bo" => (tk 2 root) + "tsheni" ;
        _+"bu" => (tk 2 root) + "tshini" ;
        _+"mo" => (tk 2 root) + "nyeni" ;
        _+"mu" => (tk 2 root) + "nyini" ;
        _+("a"|"e") => (init root)+"eni" ;
        _+"i" => (init root)+"ini" ;
        _+"o" => (init root)+"weni" ;
        _+"u" => (init root)+"wini" ;
        _ => (init root)+"ini"
      } ;

    drop_init_vowel : Str -> Str = \s ->
    case s of {
      ("a"|"e"|"i"|"o"|"u")+_ => (drop 1 s) ;
      _ => s
    } ;

    vowel : pattern Str = #("a"|"e"|"i"|"o"|"u") ;
    cons : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"y"|"z") ;
    labial_cons : pattern Str = #("p"|"b"|"f"|"v"|"w") ;
    alveolar_cons : pattern Str = #("s"|"d"|"t"|"z") ;
    nasal_de_asp : pattern Str = #("t"|"k"|"x"|"c"|"q") ;
    nasal_m : pattern Str = #("v"|"f"|"b") ;
    nasal_ng : pattern Str = #("x"|"c"|"q") ;
    nasal : pattern Str = #("n"|"m") ;

    nomNoun : Str -> Number -> ClassGender -> Str = \root,n,cg ->
      case <cg,n> of
      {
        <C1_2,Sg> => case root of {
          _+#cons+#vowel+#cons+_+#vowel+_ => "um"+root ;
          _ => "umu"+root
        } ; -- umu for single syllables, um for the rest
        <C1_2,Pl> => "aba"+root ; -- abe for tribes or guilds
        <C1a_2a,Sg> => "u"+root ;
        <C1a_2a,Pl> => "o"+root ;
        <C3_4,Sg> => case root of {
          ("m"|"n")+_+#vowel+#cons+_+#vowel+_ => "u"+root ;
          _+(#cons|"y")+#vowel+#cons+_+#vowel+_ => "um"+root ;
          "o"+_ => "um"+root ;
          _ => "umu"+root
        } ; -- umu for single syllables, um for the rest
        <C3_4,Pl> => "imi"+root ;
        <C5_6,Sg> => "i"+root ; -- ili long form (not used?)
        <C5_6,Pl> => case root of {
          "i"+_ => "ame"+root ;
          _ => "ama"+root
        } ; -- ame for roots starting with i
        <C7_8,Sg> => case root of {
          #vowel+_ => "is"+root ;
          _ => "isi"+root
        } ; -- is for roots starting with vowel
        <C7_8,Pl> => case root of {
          #vowel+_ => "iz"+root ;
          _ => "izi" + root
        } ;
        <C9_10,Sg> => "i" + prefix_nasal root ;
        <C9_10,Pl> => "izi" + prefix_nasal root ;
        <C11_10,Sg> => "u"+root ;
        <C11_10,Pl> => "izi" + prefix_nasal root ;
        <C9_6,Sg> => "i" + prefix_nasal root ;
        <C9_6,Pl> => case root of {
          "i"+_ => "ame"+root ;
          _ => "ama"+root
        } ; -- ame for roots starting with i
        <C14,_> => "ubu"+root ;
        <C15,_> => case root of {
          ("a"|"e")+_ => "ukw"+root ;
          (#cons|"y")+_ => "uku"+root ;
          _ => "uk"+root
        } ; -- ukw for roots starting with a/e, uk for roots starting with o
        <C17,_> => "uku"+root  -- sometimes ukw
      } ;

    locNoun : Str -> Number -> ClassGender -> Str = \root,n,cg ->
        case <cg,n> of
        {
          <C1_2,Sg> => case root of {
            _+#cons+#vowel+#cons+_+#vowel+_ => "kum"+root ;
            _ => "kumu"+root
          } ; -- umu for single syllables, um for the rest
          <C1_2,Pl> => "kuba"+root ; -- abe for tribes or guilds
          <C1a_2a,Sg> => "ku"+root ;
          <C1a_2a,Pl> => "ko"+root ;
          <C3_4,Sg> => case root of {
            ("m"|"n")+_ => "e"+(addLocSuffix root) ;
            _ => "em"+(addLocSuffix root)
          } ;
          <C3_4,Pl> => "emi"+(addLocSuffix root) ;
          <C5_6,Sg> => "e"+(addLocSuffix root) ; -- ili long form (not used?)
          <C5_6,Pl> => case root of {
            "i"+_ => "eme"+(addLocSuffix root) ;
            _ => "ema"+(addLocSuffix root)
          } ; -- ame for roots starting with i
          <C7_8,Sg> => case root of {
            #vowel+_ => "es"+(addLocSuffix root) ;
            _ => "esi"+(addLocSuffix root)
          } ; -- is for roots starting with vowel
          <C7_8,Pl> => case root of {
            #vowel+_ => "ez"+(addLocSuffix root) ;
            _ => "ezi"+(addLocSuffix root)  -- iz for roots starting with vowel
          } ;
          <C9_10,Sg> => "e"+(addLocSuffix (prefix_nasal root)) ; -- em for labial, en for alveolar (TODO: does this correctly split options?)
          <C9_10,Pl> => "ezi"+(addLocSuffix (prefix_nasal root)) ; -- izim for labial, izin for alveolar (TODO: does this correctly split options?)
          <C11_10,Sg> => "o"+(addLocSuffix root) ;
          <C11_10,Pl> => "ezi"+(addLocSuffix (prefix_nasal root)) ; -- izim for labial, izin for alveolar, izi(n|m)k for roots starting with kh
          <C9_6,Sg> => "e"+(addLocSuffix (prefix_nasal root)) ; -- em for labial, en for alveolar (TODO: does this correctly split options?)
          <C9_6,Pl> => case root of {
            "i"+_ => "eme"+(addLocSuffix root) ;
            _ => "ema"+(addLocSuffix root)
          } ; -- ame for roots starting with i
          <C14,_> => "ebu"+(addLocSuffix root) ;
          <C15,_> => case root of {
            ("a"|"e")+_ => "ekw"+(addLocSuffix root) ;
            (#cons|"y")+_ => "eku"+root ;
            _ => "ek"+(addLocSuffix root)
            } ; -- ukw for roots starting with a/e, uk for roots starting with o
          <C17,_> => "eku"+(addLocSuffix root)  -- sometimes ukw
        } ;

      locS : Agr => Str = table {
        Third C1_2 _ => [] ;
        Third C1a_2a _ => [] ;
        Third _ _  => "s"++BIND ;
        (First _ | Second _ )  => []
      } ;

      -- loc_n_cop_pref : VForm -> Agr -> Str = \vform,agr -> case vform of {
      --   VFIndic _ Neg PresTense => kho_cop vform agr ;
      --   VFIndic _ _ _ => id_pre_cop_pref vform agr
      -- } ;

      loc_n_cop_base : {
        empty : Str ;
        s : NForm => Str ;
        agr : Agr ;
        i : RInit ;
        proDrop : Bool ;
        isPron : Bool ;
        } -> VForm -> Str = \np,vform -> case vform of {
        VFIndic _ Neg PresTense => np.s!NLoc ;
        VFIndic _ _ _ => locS!np.agr ++ np.s!NLoc
      } ;

      lin_NP : {
        empty : Str ;
        s : NForm => Str ;
        mod : Str ;
        dem : Str ;
        predet_pre : Str ;
        predet_post : Str ;
        agr : Agr ;
        proDrop : Bool ;
        isPron : Bool ;
        -- reqLocS : Bool ;
        qdef : QuantDef
      } -> Str = \np ->
      np.predet_pre ++
      case <np.qdef,np.isPron> of {
        <Article d,_> => np.s ! NFull ++ np.mod ;
        <Demonstrative d,False> => np.dem ++ np.s ! NReduced ++ np.mod ;
        <Demonstrative d,True> => np.dem ++ np.s ! NFull ++ np.mod
      }
      ++ np.predet_post ;

      loc_NP : {
        empty : Str ;
        s : NForm => Str ;
        mod : Str ;
        dem : Str ;
        predet_pre : Str ;
        predet_post : Str ;
        agr : Agr ;
        proDrop : Bool ;
        isPron : Bool ;
        -- reqLocS : Bool ;
        qdef : QuantDef
      } -> Str = \np -> np.s!NLoc ++ np.dem ++ np.mod ++ np.predet_pre ++ np.predet_post ;

      poss_NP : {
        empty : Str ;
        s : NForm => Str ;
        -- mod : Str ;
        -- dem : Str ;
        -- predet_pre : Str ;
        -- predet_post : Str ;
        agr : Agr ;
        proDrop : Bool ;
        isPron : Bool
        -- reqLocS : Bool ;
        -- qdef : QuantDef
      } -> Str = \np -> np.s!NPoss ;

      pref_lin_NP : {
        empty : Str ;
        s : NForm => Str ;
        mod : Str ;
        dem : Str ;
        predet_pre : Str ;
        predet_post : Str ;
        agr : Agr ;
        proDrop : Bool ;
        isPron : Bool ;
        -- reqLocS : Bool ;
        qdef : QuantDef
      } -> Str = \np -> np.s ! NReduced ++ np.dem ++ np.mod
      ++ np.predet_pre ++ np.predet_post ;

    ----------------
    -- CONGRUENCE --
    ----------------

    -- SUBJECT AGREEMENT MORPHEME --

    -- NOTE : the empty SCVow for class 6 might produce a dangling BIND token, causing bugs
    -- TODO: SC following vowel
    subjConcLookup : Agr => SCForm => Str =
      table {
        -- agr                     default        before vowel     after neg pref    sit/part         potential/subjunct/indirect relative
        First Sg =>         table {SC => "ngi" ;  SCVow => "ng"++BIND ;  SCNeg => "ngi" ; SCNegVow => "ng" ; SCPart => "ngi" ; SCPS => "ngi" ; SCVowP => "ngi" ; SCBe => "bengi" ; SCRP => "ngangi" } ;
        Second Sg =>        table {SC => "u" ;    SCVow => "w"++BIND ;   SCNeg => "wu" ;  SCNegVow => "w" ;  SCPart => "u" ;   SCPS => "u" ;   SCVowP => "wu" ;  SCBe => "ubu" ;   SCRP => "wawu" } ;
        First Pl =>         table {SC => "si" ;   SCVow => "s"++BIND ;   SCNeg => "si" ;  SCNegVow => "s" ; SCPart => "si" ;  SCPS => "si" ;  SCVowP => "si" ;  SCBe => "besi" ;   SCRP => "sasi" } ;
        Second Pl =>        table {SC => "ni" ;   SCVow => "n"++BIND ;   SCNeg => "ni" ;  SCNegVow => "n" ; SCPart => "ni" ;  SCPS => "ni" ;  SCVowP => "ni" ;  SCBe => "beni" ;   SCRP => "nani" } ;
        Third C1_2 Sg =>    table {SC => "u" ;    SCVow => "w"++BIND ;   SCNeg => "ka" ;  SCNegVow => "k" ; SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wu" ;  SCBe => "ube" ;    SCRP => "waye" } ;
        Third C1_2 Pl =>    table {SC => "ba" ;   SCVow => "b"++BIND ;   SCNeg => "ba" ;  SCNegVow => "b" ; SCPart => "be" ;  SCPS => "ba" ;  SCVowP => "ba" ;  SCBe => "babe" ;   SCRP => "babe" } ;
        Third C1a_2a Sg =>  table {SC => "u" ;    SCVow => "w"++BIND ;   SCNeg => "ka" ;  SCNegVow => "k" ; SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wu" ;  SCBe => "ube" ;    SCRP => "waye" } ;
        Third C1a_2a Pl =>  table {SC => "ba" ;   SCVow => "b"++BIND ;   SCNeg => "ba" ;  SCNegVow => "b" ; SCPart => "be" ;  SCPS => "ba" ;  SCVowP => "ba" ;  SCBe => "babe" ;   SCRP => "babe" } ;
        Third C3_4 Sg =>    table {SC => "u" ;    SCVow => "w"++BIND ;   SCNeg => "wu" ;  SCNegVow => "w" ; SCPart => "u" ;   SCPS => "u" ;   SCVowP => "wu" ;  SCBe => "ubu" ;    SCRP => "wawu" } ;
        Third C3_4 Pl =>    table {SC => "i" ;    SCVow => "y"++BIND ;   SCNeg => "yi" ;  SCNegVow => "y" ; SCPart => "i" ;   SCPS => "i" ;   SCVowP => "yi" ;  SCBe => "ibi" ;    SCRP => "yayi" } ;
        Third C5_6 Sg =>    table {SC => "li" ;   SCVow => "l"++BIND ;   SCNeg => "li" ;  SCNegVow => "l" ; SCPart => "li" ;  SCPS => "li" ;  SCVowP => "li" ;  SCBe => "beli" ;   SCRP => "lali" } ;
        Third C5_6 Pl =>    table {SC => "a" ;    SCVow => [] ;          SCNeg => "wa" ;  SCNegVow => "w" ; SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wa" ;  SCBe => "abe" ;    SCRP => "aye" } ;
        Third C7_8 Sg =>    table {SC => "si" ;   SCVow => "s"++BIND ;   SCNeg => "si" ;  SCNegVow => "s" ; SCPart => "si" ;  SCPS => "si" ;  SCVowP => "si" ;  SCBe => "besi" ;   SCRP => "sasi" } ;
        Third C7_8 Pl =>    table {SC => "zi" ;   SCVow => "z"++BIND ;   SCNeg => "zi" ;  SCNegVow => "z" ; SCPart => "zi" ;  SCPS => "zi" ;  SCVowP => "zi" ;  SCBe => "bezi" ;   SCRP => "zazi" } ;
        Third C9_10 Sg =>   table {SC => "i" ;    SCVow => "y"++BIND ;   SCNeg => "yi" ;  SCNegVow => "y" ; SCPart => "yi" ;  SCPS => "i" ;   SCVowP => "yi" ;  SCBe => "ibi" ;    SCRP => "yayi" } ;
        -- Third C9_10 Sg =>   table {SC => "i" ;    SCVow => "i"++BIND ;   SCNeg => "yi" ;  SCPart => "yi" ;  SCPS => "i" ;   SCVowP => "yi" ;  SCBe => "ibi" } ;
        Third C9_10 Pl =>   table {SC => "zi" ;   SCVow => "z"++BIND ;   SCNeg => "zi" ;  SCNegVow => "z" ; SCPart => "zi" ;  SCPS => "zi" ;  SCVowP => "zi" ;  SCBe => "bezi" ;   SCRP => "zazi" } ;
        Third C11_10 Sg =>  table {SC => "lu" ;   SCVow => "lw"++BIND ;  SCNeg => "lu" ;  SCNegVow => "l" ; SCPart => "lu" ;  SCPS => "lu" ;  SCVowP => "lu" ;  SCBe => "belu" ;   SCRP => "lwalu" } ;
        Third C11_10 Pl =>  table {SC => "zi" ;   SCVow => "z"++BIND ;   SCNeg => "zi" ;  SCNegVow => "z" ; SCPart => "zi" ;  SCPS => "zi" ;  SCVowP => "zi" ;  SCBe => "bezi" ;   SCRP => "zazi" } ;
        Third C9_6 Sg =>    table {SC => "i" ;    SCVow => "y"++BIND ;   SCNeg => "yi" ;  SCNegVow => "y" ; SCPart => "yi" ;  SCPS => "i" ;   SCVowP => "yi" ;  SCBe => "ibi" ;    SCRP => "yayi" } ;
        Third C9_6 Pl =>    table {SC => "a" ;    SCVow => [] ;          SCNeg => "wa" ;  SCNegVow => "w" ; SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wa" ;  SCBe => "abe" ;    SCRP => "aye" } ;
        Third C14 _ =>      table {SC => "bu" ;   SCVow => "b"++BIND ;   SCNeg => "bu" ;  SCNegVow => "b" ; SCPart => "bu" ;  SCPS => "bu" ;  SCVowP => "bu" ;  SCBe => "bebu" ;   SCRP => "kwaku" } ;
        Third C15 _ =>      table {SC => "ku" ;   SCVow => "kw"++BIND ;  SCNeg => "ku" ;  SCNegVow => "k" ; SCPart => "ku" ;  SCPS => "ku" ;  SCVowP => "ku" ;  SCBe => "beku" ;   SCRP => "kwaku" } ;
        Third C17 _ =>      table {SC => "ku" ;   SCVow => "kw"++BIND ;  SCNeg => "ku" ;  SCNegVow => "k" ; SCPart => "ku" ;  SCPS => "ku" ;  SCVowP => "ku" ;  SCBe => "beku" ;   SCRP => "kwaku" }
      } ;

    -- scvow_bind : Agr -> Str = \agr -> case agr of {
    --   (Third C5_6 Pl|Third C9_6 Pl) => [] ;
    --   _ => subjConcLookup ! agr ! SCVow ++BIND
    -- } ;

    subjConc : VForm -> Agr -> Bool -> Str = \vform,agr,prevow ->
      case <prevow,vform> of {
          <False,VFIndic _ Neg _> => subjConcLookup ! agr ! SCNeg ++BIND ;
          <True,VFIndic _ Neg _> => subjConcLookup ! agr ! SCNegVow ++BIND ;
          <True,VFIndic _ _ _> => subjConcLookup ! agr ! SCVow ;
          <_,VFIndic _ _ RemPastTense> => subjConcLookup ! agr ! SCVow ;
          <_,VFIndic _ _ _>   => subjConcLookup ! agr ! SC ++BIND
      } ;

    -- -be aux: reference time in relation to coding time
    -- relSubjConc : BasicTense -> Agr -> Str = \tense,agr ->
    -- relSubjConc : BasicTense -> Agr -> Str = \tense,agr ->
    --   case tense of {
    --     PastTense => (subjConcLookup ! agr ! SCVow) ++ "a" ++BIND++ case agr of {
    --       Second Sg | Third C3_4 Sg => "w" ++BIND ;
    --       Third C1_2 Sg | Third C1a_2a Sg | Third C3_4 Pl | Third C5_6 Pl | Third C9_10 Sg => "y" ++BIND ;
    --       First _ | Second _ | Third _ _ => []
    --     } ;
    --     PerfTense | PresTense => subjConcLookup ! agr ! SC ++BIND++ "be" ; -- NOTE: present tense doesn't make much sense here, so default to perf
    --     FutTense => subjConcLookup ! agr ! SC ++BIND++ "zobe"
    --   } ;

    impPref : Polarity -> Str = \pol -> case pol of {
      Pos => [] ;
      Neg => "u" ++BIND++ "nga" ++BIND
    } ;

    icomp_pref : VForm -> Agr -> Str = \vform,agr ->
    let
      neg1 = icompNeg1 vform ;
      neg2 = icompNeg2 vform ;
    in
    case vform of {
      VFIndic _ Pos PresTense => subjConcLookup ! agr ! SC ++BIND ;
      VFIndic _ Pos PastTense => [] ; -- "be"++BIND++ subjConcLookup ! agr ! SCBe ++BIND ;
      VFIndic _ Pos RemPastTense => subjConcLookup ! agr ! SC ++BIND++ "be" ++ subjConcLookup ! agr ! SCBe ++BIND ;
      VFIndic _ Pos FutTense => subjConcLookup ! agr ! SC ++BIND ++ "zobe" ++ subjConcLookup ! agr ! SC ++BIND ;
      VFIndic _ Pos RemFutTense => subjConcLookup ! agr ! SC ++BIND ++ "yobe" ++ subjConcLookup ! agr ! SC ++BIND ;

      -- might have to add an SCNegBe to the table
      VFIndic _ Neg PresTense => neg1 ++ subjConcLookup ! agr ! SCNeg ++BIND ++ neg2 ;
      VFIndic _ Neg PastTense => "nga" ; -- neg1 ++ "be"++BIND++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2 ;
      VFIndic _ Neg RemPastTense => neg1 ++ subjConcLookup ! agr ! SC ++BIND++ "be" ++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2 ;
      VFIndic _ Neg FutTense => neg1 ++ subjConcLookup ! agr ! SCNeg ++BIND ++ "zobe" ++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2 ;
      VFIndic _ Neg RemFutTense => neg1 ++ subjConcLookup ! agr ! SCNeg ++BIND ++ "yobe" ++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2
    } ;

    -- OBJECT AGREEMENT MORPHEME --

    objConcLookup : Agr => OCForm => Str =
      table {
        First Sg =>         table {OC => "ngi" ;  OCAE => "ng" ;  OCIOU => "ng" ; OCMono => "ngi" ; OCThing => "ngi" } ;
        Second Sg =>        table {OC => "ku" ;   OCAE => "k" ;   OCIOU => "k" ;  OCMono => "ku" ;  OCThing => "ku" } ;
        First Pl =>         table {OC => "si" ;   OCAE => "s" ;   OCIOU => "s" ;  OCMono => "si" ;  OCThing => "si" } ;
        Second Pl =>        table {OC => "ni" ;   OCAE => "n" ;   OCIOU => "n" ;  OCMono => "ni" ;  OCThing => "ni" } ;
        Third C1_2 Sg =>    table {OC => "m" ;    OCAE => "m" ;   OCIOU => "m" ;  OCMono => "mu" ;  OCThing => "wu" } ;
        Third C1_2 Pl =>    table {OC => "ba" ;   OCAE => "b" ;   OCIOU => "b" ;  OCMono => "ba" ;  OCThing => "ba" } ;
        Third C1a_2a Sg =>  table {OC => "m" ;    OCAE => "m" ;   OCIOU => "m" ;  OCMono => "mu" ;  OCThing => "wu" } ;
        Third C1a_2a Pl =>  table {OC => "ba" ;   OCAE => "b" ;   OCIOU => "b" ;  OCMono => "ba" ;  OCThing => "ba" } ;
        Third C3_4 Sg =>    table {OC => "wu" ;   OCAE => "w" ;   OCIOU => "w" ;  OCMono => "wu" ;  OCThing => "wu" } ;
        Third C3_4 Pl =>    table {OC => "yi" ;   OCAE => "y" ;   OCIOU => "y" ;  OCMono => "yi" ;  OCThing => "yi" } ;
        Third C5_6 Sg =>    table {OC => "li" ;   OCAE => "l" ;   OCIOU => "l" ;  OCMono => "li" ;  OCThing => "li" } ;
        Third C5_6 Pl =>    table {OC => "wa" ;   OCAE => "w" ;   OCIOU => "w" ;  OCMono => "wa" ;  OCThing => "wa" } ;
        Third C7_8 Sg =>    table {OC => "si" ;   OCAE => "s" ;   OCIOU => "s" ;  OCMono => "si" ;  OCThing => "si" } ;
        Third C7_8 Pl =>    table {OC => "zi" ;   OCAE => "z" ;   OCIOU => "z" ;  OCMono => "zi" ;  OCThing => "zi" } ;
        Third C9_10 Sg =>   table {OC => "yi" ;   OCAE => "y" ;   OCIOU => "y" ;  OCMono => "yi" ;  OCThing => "yi" } ;
        Third C9_10 Pl =>   table {OC => "zi" ;   OCAE => "z" ;   OCIOU => "z" ;  OCMono => "zi" ;  OCThing => "zi" } ;
        Third C11_10 Sg =>  table {OC => "lu" ;   OCAE => "lw" ;  OCIOU => "l" ;  OCMono => "lu" ;  OCThing => "lu" } ;
        Third C11_10 Pl =>  table {OC => "zi" ;   OCAE => "z" ;   OCIOU => "z" ;  OCMono => "zi" ;  OCThing => "zi" } ;
        Third C9_6 Sg =>    table {OC => "yi" ;   OCAE => "y" ;   OCIOU => "y" ;  OCMono => "yi" ;  OCThing => "yi" } ;
        Third C9_6 Pl =>    table {OC => "wa" ;   OCAE => "w" ;   OCIOU => "w" ;  OCMono => "wa" ;  OCThing => "wa" } ;
        Third C14 _ =>      table {OC => "bu" ;   OCAE => "bw" ;  OCIOU => "b" ;  OCMono => "bu" ;  OCThing => "bu" } ;
        Third C15 _ =>      table {OC => "ku" ;   OCAE => "kw" ;  OCIOU => "k" ;  OCMono => "ku" ;  OCThing => "ku" } ;
        Third C17 _ =>      table {OC => "ku" ;   OCAE => "kw" ;  OCIOU => "k" ;  OCMono => "ku" ;  OCThing => "ku" }
      } ;

    -- ignoring the thing thing for now, must probably add something like gender to nouns...
    objConc : Agr -> RInit -> Syl -> Str = \agr,rinit,syl ->
      case rinit of {
        (RA|RE) => objConcLookup ! agr ! OCAE ++BIND ;
        (RI|RO|RU) => objConcLookup ! agr ! OCIOU ++BIND ;
        RC => case syl of {
          SylMono => objConcLookup ! agr ! OCMono ++BIND ;
          _ => objConcLookup ! agr ! OC ++BIND
        }
      } ;

    -- ADJECTIVE ANTECEDENT AGREEMENT MORPHEME --

    relAdjPrefLookup : Agr => Str = --table {
      -- Pos =>
      table {
        Third C1_2 Sg => "m" ;
        Third C1_2 Pl => "" ;
        Third C1a_2a Sg => "m" ;
        Third C1a_2a Pl => "" ;
        Third C3_4 Sg  => "m" ;
        Third C3_4 Pl => "mi" ;
        Third C5_6 Sg => "" ;
        Third C5_6 Pl => "ma" ;
        Third C7_8 Sg => "" ;
        Third C7_8 Pl => "" ;
        Third C9_10 Sg => "" ;
        Third C9_10 Pl => "" ;
        Third C11_10 Sg => "" ;
        Third C11_10 Pl => "" ;
        Third C9_6 Sg => "" ;
        Third C9_6 Pl => "ma" ;
        Third C14 _ => "bu" ;
        Third C15 _ => "ku" ;
        Third C17 _ => "ku" ;
        (First _ | Second _ )  => "m"
      -- } ;
      -- Neg => table {
      --   Third C1_2 Sg => "ongem" ;
      --   Third C1_2 Pl => "angeba" ;
      --   Third C1a_2a Sg => "ongem" ;
      --   Third C1a_2a Pl => "angeba" ;
      --   Third C3_4 Sg  => "ongem" ;
      --   Third C3_4 Pl => "engemi" ;
      --   Third C5_6 Sg => "engeli" ;
      --   Third C5_6 Pl => "angema" ;
      --   Third C7_8 Sg => "engesi" ;
      --   Third C7_8 Pl => "engezi" ;
      --   Third C9_10 Sg => "enge" ;
      --   Third C9_10 Pl => "engezi" ;
      --   Third C11_10 Sg => "ongelu" ;
      --   Third C11_10 Pl => "engezi" ;
      --   Third C9_6 Sg => "enge" ;
      --   Third C9_6 Pl => "angema" ;
      --   Third C14 _ => "ongebu" ;
      --   Third C15 _ => "ongeku" ;
      --   Third C17 _ => "ongeku" ;
      --   (First _ | Second _ )  => "ongem"
      -- }
    } ;

    -- RELATIVE ANTECEDENT AGREEMENT MORPHEME --

    relConc : VForm -> Agr -> RInit -> Str = \p,a,r -> case p of {
    --  VFIndic CType Polarity BasicTense
      VFIndic RelCl Pos PresTense => relConcLookup!a!r ;
      VFIndic RelCl Pos RemPastTense => relConcLookup!a!RA ;
      VFIndic RelCl Pos PastTense => relConcLookup!a!r ;
      VFIndic RelCl Pos _ => relConcLookup!a!RC ;
      VFIndic RelCl Neg PresTense => case r of {
        RC => relConcLookup!a!RC ++ "nga" ++BIND ;
        _ => relConcLookup!a!RC ++ "ng" ++BIND
      } ;
      VFIndic RelCl Neg PastTense => case r of {
        RC => relConcLookup!a!RC ++ "nga" ++BIND ;
        _ => relConcLookup!a!RC ++ "ng" ++BIND
      } ;
      VFIndic RelCl Neg RemPastTense => case r of {
        RC => relConcLookup!a!RC ++ "nga" ++BIND ;
        _ => relConcLookup!a!RC ++ "ng" ++BIND
      } ;
      VFIndic RelCl Neg FutTense => relConcLookup!a!RC ++ "nga" ++BIND ;
      VFIndic RelCl Neg RemFutTense => relConcLookup!a!RC ++ "nga" ++BIND ;
      VFIndic _ _ _ => []
    } ;

    relConcCop : VForm -> Agr -> RInit ->Str = \vform,a,r -> case vform of {
      VFIndic _ _ PresTense => relConcLookup!a!RC ;
      VFIndic _ _ FutTense => relConcLookup!a!RC ;
      VFIndic _ _ RemFutTense => relConcLookup!a!RC ;
      VFIndic _ _ PastTense => relCopConcBeLookup!a ;
      VFIndic _ _ RemPastTense => case a of {
        Third C5_6 Pl => [] ; -- relConcLookup!a!RA ; -- a + aye = aye
        (First _ | Second _ | Third _ _ ) => shortRelConc!a ++BIND  --++ subjConcLookup!a!SCRP
      }
    } ;

    relConcLookup : Agr => RInit => Str =
      table {
        Third C1_2 Sg => table { RO => [] ; (RA|RE) => "ow"++BIND ; _ => "o"++BIND } ;
        Third C1_2 Pl => table { RC => "aba"++BIND ; _ => "ab"++BIND } ;
        Third C1a_2a Sg => table { RO => [] ; (RA|RE) => "ow"++BIND ; _ => "o"++BIND } ;
        Third C1a_2a Pl => table { RC => "aba"++BIND ; _ => "ab"++BIND } ;
        Third C3_4 Sg  => table { RO => [] ; (RA|RE) => "ow"++BIND ; _ => "o"++BIND } ;
        Third C3_4 Pl => table { RE => [] ; (RA|RO) => "ey" ++BIND ; _ => "e"++BIND } ;
        Third C5_6 Sg => table { RC => "eli"++BIND ; _ => "el"++BIND } ;
        Third C5_6 Pl => table { RC => "a"++BIND ; _ => [] } ;
        Third C7_8 Sg => table { RC => "esi"++BIND ; _ => "es"++BIND } ;
        Third C7_8 Pl => table { RC => "ezi"++BIND ; _ => "ez"++BIND } ;
        Third C9_10 Sg => table { RE => [] ; (RA|RO) => "ey" ++BIND ; _ => "e"++BIND } ;
        Third C9_10 Pl => table { RC => "ezi"++BIND ; _ => "ez"++BIND } ;
        Third C11_10 Sg => table { RC => "olu"++BIND ; (RA|RE) => "olw" ; _ => "ol"++BIND } ;
        Third C11_10 Pl => table { RC => "ezi"++BIND ; _ => "ez"++BIND } ;
        Third C9_6 Sg => table { RE => [] ; (RA|RO) => "ey" ; _ => "e"++BIND } ;
        Third C9_6 Pl => table { RC => "a"++BIND ; _ => [] } ;
        Third C14 _ => table { RC => "obu"++BIND ; _ => "ob"++BIND } ;
        Third C15 _ => table { RC => "oku"++BIND ; (RA|RE) => "okw" ; _ => "ok"++BIND } ;
        Third C17 _ => table { RC => "oku"++BIND ; (RA|RE) => "okw" ; _ => "ok"++BIND } ;
        First Sg => table { RC => "engi"++BIND ; _ => "eng"++BIND } ;
        First Pl => table { RC => "esi"++BIND ; _ => "es"++BIND } ;
        Second Sg  => table { RE => "ow"++BIND ; _ => "o"++BIND } ;
        Second Pl => table { RC => "eni"++BIND ; _ => "en"++BIND }
    } ;

    relCopConcBeLookup : Agr => Str =
      table {
        Third C1_2 Sg => "obe"++BIND ;
        Third C1_2 Pl => "ababe"++BIND ;
        Third C1a_2a Sg => "obe"++BIND ;
        Third C1a_2a Pl => "ababe"++BIND ;
        Third C3_4 Sg  => "obu"++BIND ;
        Third C3_4 Pl => "ebi"++BIND ;
        Third C5_6 Sg => "ebeli"++BIND ;
        Third C5_6 Pl => "abe"++BIND ;
        Third C7_8 Sg => "ebesi"++BIND ;
        Third C7_8 Pl => "ebezi"++BIND ;
        Third C9_10 Sg => "ebi"++BIND ;
        Third C9_10 Pl => "ebezi"++BIND ;
        Third C11_10 Sg => "obelu"++BIND ;
        Third C11_10 Pl => "ebezi"++BIND ;
        Third C9_6 Sg => "ebi"++BIND ;
        Third C9_6 Pl => "abe"++BIND ;
        Third C14 _ => "obebu"++BIND ;
        Third C15 _ => "obeku"++BIND ;
        Third C17 _ => "obeku"++BIND ;
        First Sg => "ebengi"++BIND ;
        First Pl => "ebesi"++BIND ;
        Second Sg  => "obu"++BIND ;
        Second Pl => "ebeni"++BIND
    } ;

    -- relCopConcBeLookup : Agr => RInit => Str =
    --   table {
    --     Third C1_2 Sg => table { _ => "obe"++BIND } ;
    --     Third C1_2 Pl => table { _ => "ababe"++BIND } ;
    --     Third C1a_2a Sg => table { _ => "obe"++BIND } ;
    --     Third C1a_2a Pl => table { _ => "ababe"++BIND } ;
    --     Third C3_4 Sg  => table { _ => "obe"++BIND } ;
    --     Third C3_4 Pl => table { _ => "ebi"++BIND } ;
    --     Third C5_6 Sg => table { _ => "beli"++BIND } ;
    --     Third C5_6 Pl => table { _ => "abe"++BIND } ;
    --     Third C7_8 Sg => table { _ => "ebesi"++BIND } ;
    --     Third C7_8 Pl => table { _ => "ebezi"++BIND } ;
    --     Third C9_10 Sg => table { _ => "ebi"++BIND } ;
    --     Third C9_10 Pl => table { _ => "ebezi"++BIND } ;
    --     Third C11_10 Sg => table { _ => "obelu"++BIND } ;
    --     Third C11_10 Pl => table { _ => "ebezi"++BIND } ;
    --     Third C9_6 Sg => table { _ => "ebi"++BIND } ;
    --     Third C9_6 Pl => table { _ => "abe"++BIND } ;
    --     Third C14 _ => table { _ => "obebu"++BIND } ;
    --     Third C15 _ => table { _ => "obeku"++BIND } ;
    --     Third C17 _ => table { _ => "obeku"++BIND } ;
    --     First Sg => table { _ => "ebengi"++BIND } ;
    --     First Pl => table { _ => "ebesi"++BIND } ;
    --     Second Sg  => table { _ => "obe"++BIND } ;
    --     Second Pl => table { _ => "ebeni"++BIND }
    -- } ;

    -- ENUMERATIVE ANTECEDENT AGREEMENT MORPHEME --

    enumConc : Polarity -> Agr -> Str = \pol,agr -> case pol of {
      Pos => enumConcLookup!agr ;
      Neg => "nge" ++BIND++ enumConcLookup!agr
    } ;

    enumConcLookup : Agr => Str =
      table {
        Third C1_2 Sg => "mu" ;
        Third C1_2 Pl => "ba" ;
        Third C1a_2a Sg => "mu" ;
        Third C1a_2a Pl => "ba" ;
        Third C3_4 Sg  => "mu" ;
        Third C3_4 Pl => "mi" ;
        Third C5_6 Sg => "li" ;
        Third C5_6 Pl => "ma" ;
        Third C7_8 Sg => "si" ;
        Third C7_8 Pl => "zi" ;
        Third C9_10 Sg => "yi" ;
        Third C9_10 Pl => "zi" ;
        Third C11_10 Sg => "lu" ;
        Third C11_10 Pl => "zi" ;
        Third C9_6 Sg => "yi" ;
        Third C9_6 Pl => "ma" ;
        Third C14 _ => "bu" ;
        Third C15 _ => "ku" ;
        Third C17 _ => "ku" ;
        (First _ | Second _ )  => "mu"
      } ;

    shortRelConc : Agr => Str =
      table {
        Third C1_2 Sg => "o" ;
        Third C1_2 Pl => "a" ;
        Third C1a_2a Sg => "o" ;
        Third C1a_2a Pl => "a" ;
        Third C3_4 Sg  => "o" ;
        Third C3_4 Pl => "e" ;
        Third C5_6 Sg => "e" ;
        Third C5_6 Pl => "a" ;
        Third C7_8 Sg => "e" ;
        Third C7_8 Pl => "e" ;
        Third C9_10 Sg => "e" ;
        Third C9_10 Pl => "e" ;
        Third C11_10 Sg => "o" ;
        Third C11_10 Pl => "e" ;
        Third C9_6 Sg => "e" ;
        Third C9_6 Pl => "a" ;
        Third C14 _ => "o" ;
        Third C15 _ => "o" ;
        Third C17 _ => "o" ;
        First Sg => "e" ;
        First Pl => "e" ;
        Second Sg  => "o" ;
        Second Pl => "e"
      } ;

    -- POSSESSIVE ANTECEDENT AGREEMENT MORPHEME --

    poss_concord_agr : Agr => RInit => Str =
      table {
        First Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
        First Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" } ;
        Second Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
        Second Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" } ;
        Third C1_2 Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
        Third C1_2 Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" } ;
        Third C1a_2a Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
        Third C1a_2a Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" } ;
        Third C3_4 Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
        Third C3_4 Pl => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
        Third C5_6 Sg => table {(RA|RC) => "la" ; (RE|RI) => "le" ; (RO|RU) => "lo" } ;
        Third C5_6 Pl => table {(RA|RC) => "a" ; (RE|RI) => "e" ; (RO|RU) => "o" } ;
        Third C7_8 Sg => table {(RA|RC) => "sa" ; (RE|RI) => "se" ; (RO|RU) => "so" } ;
        Third C7_8 Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" } ;
        Third C9_10 Sg => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
        Third C9_10 Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" } ;
        Third C11_10 Sg => table {(RA|RC) => "lwa" ; (RE|RI) => "lwe" ; (RO|RU) => "lo" } ;
        Third C11_10 Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" } ;
        Third C9_6 Sg => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
        Third C9_6 Pl => table {(RA|RC) => "a" ; (RE|RI) => "e" ; (RO|RU) => "o" } ;
        Third C14 _ => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" } ;
        Third C15 _ => table {(RA|RC) => "kwa" ; (RE|RI) => "kwe" ; (RO|RU) => "ko" } ;
        Third C17 _ => table {(RA|RC) => "kwa" ; (RE|RI) => "kwe" ; (RO|RU) => "ko" }
      } ;

    poss_concord : ClassGender => Number => RInit => Str =
      table {
        C1_2 => table {
          Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
          Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" }
        } ;
        C1a_2a => table {
          Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
          Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" }
        } ;
        C3_4 => table {
          Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
          Pl => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" }
        } ;
        C5_6 => table {
          Sg => table {(RA|RC) => "la" ; (RE|RI) => "le" ; (RO|RU) => "lo" } ;
          Pl => table {(RA|RC) => "a" ; (RE|RI) => "e" ; (RO|RU) => "o" }
        } ;
        C7_8 => table {
          Sg => table {(RA|RC) => "sa" ; (RE|RI) => "se" ; (RO|RU) => "so" } ;
          Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" }
        } ;
        C9_10 => table {
          Sg => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
          Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" }
        } ;
        C11_10 => table {
          Sg => table {(RA|RC) => "lwa" ; (RE|RI) => "lwe" ; (RO|RU) => "lo" } ;
          Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" }
        } ;
        C9_6 => table {
          Sg => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
          Pl => table {(RA|RC) => "a" ; (RE|RI) => "e" ; (RO|RU) => "o" }
        } ;
        C14 => table {
          _ => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" }
        } ;
        C15 => table {
          _ => table {(RA|RC) => "kwa" ; (RE|RI) => "kwe" ; (RO|RU) => "ko" }
        } ;
        C17 => table {
          _ => table {(RA|RC) => "kwa" ; (RE|RI) => "kwe" ; (RO|RU) => "ko" }
        }
      } ;

    poss_concord_c1a : ClassGender => Number => RInit => Str =
      table {
        C1_2 => table {
          Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
          Pl => table {(RA|RC) => "baka" ; (RE|RI) => "bake" ; (RO|RU) => "bako" }
        } ;
        C1a_2a => table {
          Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
          Pl => table {(RA|RC) => "baka" ; (RE|RI) => "bake" ; (RO|RU) => "bako" }
        } ;
        C3_4 => table {
          Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
          Pl => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" }
        } ;
        C5_6 => table {
          Sg => table {(RA|RC) => "lika" ; (RE|RI) => "like" ; (RO|RU) => "liko" } ;
          Pl => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" }
        } ;
        C7_8 => table {
          Sg => table {(RA|RC) => "sika" ; (RE|RI) => "sike" ; (RO|RU) => "siko" } ;
          Pl => table {(RA|RC) => "zika" ; (RE|RI) => "zike" ; (RO|RU) => "ziko" }
        } ;
        C9_10 => table {
          Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
          Pl => table {(RA|RC) => "zika" ; (RE|RI) => "zike" ; (RO|RU) => "ziko" }
        } ;
        C11_10 => table {
          Sg => table {(RA|RC) => "luka" ; (RE|RI) => "luke" ; (RO|RU) => "luko" } ;
          Pl => table {(RA|RC) => "zika" ; (RE|RI) => "zike" ; (RO|RU) => "ziko" }
        } ;
        C9_6 => table {
          Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
          Pl => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" }
        } ;
        C14 => table {
          _ => table {(RA|RC) => "buka" ; (RE|RI) => "buke" ; (RO|RU) => "buko" }
        } ;
        C15 => table {
          _ => table {(RA|RC) => "kuka" ; (RE|RI) => "kuke" ; (RO|RU) => "kuko" }
        } ;
        C17 => table {
          _ => table {(RA|RC) => "kuka" ; (RE|RI) => "kuke" ; (RO|RU) => "kuko" }
        }
      } ;

    -- QUANTITATIVE AGREEMENT MORPHEME --
    -- (demonstatives)

    exclQuantConc : Agr => Str = table {
      Third C1_2 Sg => "ye" ;
      Third C1_2 Pl => "bo" ;
      Third C1a_2a Sg => "ye" ;
      Third C1a_2a Pl => "bo" ;
      Third C3_4 Sg  => "wo" ;
      Third C3_4 Pl => "yo" ;
      Third C5_6 Sg => "lo" ;
      Third C5_6 Pl => "o" ;
      Third C7_8 Sg => "so" ;
      Third C7_8 Pl => "zo" ;
      Third C9_10 Sg => "yo" ;
      Third C9_10 Pl => "zo" ;
      Third C11_10 Sg => "lo" ;
      Third C11_10 Pl => "zo" ;
      Third C9_6 Sg => "yo" ;
      Third C9_6 Pl => "o" ;
      Third C14 _ => "bo" ;
      Third C15 _ => "ko" ;
      Third C17 _ => "ko" ;
      First Sg => "nge" ;
      First Pl => "so" ;
      Second Sg  => "we" ;
      Second Pl => "no"
    } ;

    -----------------
    -- COPULATIVES --
    -----------------

    pre_cop_pref : VForm -> Agr -> Str = \vform,agr ->
    let
      sc = subjConc vform agr False ;
      scvow = subjConc vform agr True
    in
      case vform of {
        VFIndic _ Pos PresTense => sc ;
        VFIndic _ Neg PresTense => "a" ++BIND++ subjConcLookup!agr!SCNeg ++BIND ;
        VFIndic _ _ FutTense => sc ++ (tensePref vform RC SylMult) ++ "ba" ;
        VFIndic _ _ RemFutTense => sc ++ (tensePref vform RC SylMult) ++ "ba" ;
        VFIndic _ _ _ => []
    } ;

    ap_cop_pref : VForm -> Agr -> AType -> Str = \vform,agr,atype ->
    let
      sc = subjConc vform agr False ;
      scvow = subjConc vform agr True
    in
    -- TODO:
    -- for positive, present: SC only inserted with class 9
      case vform of {
        VFIndic MainCl Pos PresTense => case <agr,atype> of {
          <(Third _ _ | First _ | Second _),AdjType> => [] ;
          <(Third C9_10 Sg | Third C9_6 Sg),_> => sc  ; -- i++i = i
          <(Third _ _ | First _ | Second _),_> => sc
        } ;
        VFIndic MainCl Neg PresTense => case <agr,atype> of {
          <(Third C9_10 Sg | Third C9_6 Sg),AdjType> => "a" ++BIND++ "y" ++BIND ;
          <(Third C9_10 Sg | Third C9_6 Sg),_> => "a" ++BIND++ subjConcLookup!agr!SCNeg ++BIND ;
          <(Third _ _ | First _ | Second _),_> => "a" ++BIND++ subjConcLookup!agr!SCNeg ++BIND
        } ;
        VFIndic RelCl Pos PresTense => [] ;
        VFIndic RelCl Neg PresTense => "nge" ++BIND ;

        VFIndic MainCl Pos (FutTense|RemFutTense) => case agr of {
          -- Third C9_10 Sg | Third C9_6 Sg => sc ++ (tensePref vform RC SylMult) ++ "b" ; -- ++BIND ;
          Third _ _ | First _ | Second _ => sc ++ (tensePref vform RC SylMult) ++ "ba" -- ++BIND
        } ;
        VFIndic RelCl Pos (FutTense|RemFutTense) => case agr of {
          -- Third C9_10 Sg | Third C9_6 Sg => (tensePref vform RC SylMult) ++ "b" ; -- ++BIND ;
          Third _ _ | First _ | Second _ => (tensePref vform RC SylMult) ++ "ba" -- ++BIND
        } ;
        VFIndic MainCl Neg (FutTense|RemFutTense) => case agr of {
          -- Third C9_10 Sg | Third C9_6 Sg => "a" ++BIND++ sc ++ (tensePref vform RC SylMult) ++ "b" ;
          Third _ _ | First _ | Second _ => "a" ++BIND++ sc ++ (tensePref vform RC SylMult) ++ "ba"
        } ;
        VFIndic RelCl Neg (FutTense|RemFutTense) => "nge" ++BIND++ (tensePref vform RC SylMult) ++ "ba" ;

        VFIndic MainCl Pos PastTense => subjConcLookup!agr!SCBe ++BIND ;
        VFIndic MainCl Neg PastTense => subjConcLookup!agr!SCBe ++BIND++ "nge" ++BIND ;
        VFIndic RelCl Pos PastTense  => [] ;
        VFIndic RelCl Neg PastTense  => "nge" ++BIND ;

        VFIndic _ Pos RemPastTense => subjConcLookup!agr!SCRP ++BIND ;
        VFIndic _ Neg RemPastTense => subjConcLookup!agr!SCRP ++BIND++ "nge" ++BIND
    } ;

      id_pre_cop_pref : VForm -> Agr -> Str = \vform,agr -> let
        sc = subjConc vform agr False
      in case vform of {
        VFIndic MainCl Pos PresTense => sc ;
        VFIndic MainCl Neg PresTense => "a" ++BIND++ sc ; -- "aku" ++BIND ;
        VFIndic RelCl Pos PresTense => [] ;
        VFIndic RelCl Neg PresTense => "nge" ++BIND ;

        VFIndic MainCl Pos FutTense => sc ++ "zoba" ;
        VFIndic MainCl Neg FutTense => "a" ++BIND++ sc ++ "zukuba" ;
        VFIndic RelCl Pos FutTense => "zoba" ;
        VFIndic RelCl Neg FutTense => "nge" ++BIND++ "zukuba" ;

        VFIndic MainCl Pos RemFutTense => sc ++ "yoba" ;
        VFIndic MainCl Neg RemFutTense => "a" ++BIND++ sc ++ "yukuba" ;
        VFIndic RelCl Pos RemFutTense => "yoba" ;
        VFIndic RelCl Neg RemFutTense => "nge" ++BIND++ "yukuba" ;

        VFIndic MainCl Pos PastTense => subjConcLookup!agr!SCBe ++BIND ;
        VFIndic MainCl Neg PastTense => subjConcLookup!agr!SCBe ++ BIND ++ "nge" ++BIND ;
        VFIndic RelCl Pos PastTense => [] ;
        VFIndic RelCl Neg PastTense => "nge" ++BIND ;

        VFIndic _ Pos RemPastTense => subjConcLookup!agr!SCRP ++BIND ;
        VFIndic _ Neg RemPastTense => subjConcLookup!agr!SCRP ++BIND++ "nge" ++BIND
      } ;

      assoc_pre_cop_pref : VForm -> Agr -> Str = \vform,agr -> let
        sc = subjConc vform agr False
      in case vform of {
        VFIndic MainCl Pos PresTense => sc ;
        VFIndic MainCl Neg PresTense => "a" ++BIND++ sc ;
        VFIndic RelCl Pos PresTense => [] ;
        VFIndic RelCl Neg PresTense => "nge"++BIND ;

        VFIndic MainCl Pos FutTense => sc ++ "zoba" ;
        VFIndic MainCl Neg FutTense => "a" ++BIND++ sc ++ "zukuba" ;
        VFIndic RelCl Pos FutTense => "zoba" ;
        VFIndic RelCl Neg FutTense => "nge" ++BIND++ "zukuba" ;

        VFIndic MainCl Pos RemFutTense => sc ++ "yoba" ;
        VFIndic MainCl Neg RemFutTense => "a" ++BIND++ sc ++ "yukuba" ;
        VFIndic RelCl Pos RemFutTense => "yoba" ;
        VFIndic RelCl Neg RemFutTense => "nge" ++BIND++ "yukuba" ;

        VFIndic MainCl Pos PastTense => subjConcLookup!agr!SCBe ++BIND ;
        VFIndic MainCl Neg PastTense => subjConcLookup!agr!SCBe ++BIND++ "nge" ++BIND ;
        VFIndic RelCl Pos PastTense => [] ;
        VFIndic RelCl Neg PastTense => "nge" ++BIND ;

        VFIndic _ Pos RemPastTense => subjConcLookup!agr!SCRP ++BIND ;
        VFIndic _ Neg RemPastTense => subjConcLookup!agr!SCRP ++BIND++ "nge" ++BIND
      } ;

      -- REF: Poulos & Msimang p355
      -- id_cop_pref has the following forms
      -- ngu:
      --     - absolute pronoun of 2nd person sg
      --     - class 1
      --     - all other a-, o-, u- commencing absolute pronouns and nouns except class 11
      --
      -- y:
      --     - i- commencing absolute pronouns and nouns
      --
      -- ngu:
      --     - everything else?
      id_cop_pref : Agr -> Str = \agr -> case agr of {
        Third C1_2 Sg => "ng"++BIND ;
        Third C1_2 Pl => "ng"++BIND ;
        Third C1a_2a Sg => "ng"++BIND ;
        Third C1a_2a Pl => "ng"++BIND ;
        Third C3_4 Sg  => "ng"++BIND ;
        Third C3_4 Pl => "y"++BIND ;
        Third C5_6 Sg => "y"++BIND ;
        Third C5_6 Pl => "ng"++BIND ;
        Third C7_8 Sg => "y"++BIND ;
        Third C7_8 Pl => "y"++BIND ;
        Third C9_10 Sg => "y"++BIND ;
        Third C9_10 Pl => "y"++BIND ;
        Third C11_10 Sg => "w"++BIND ;
        Third C11_10 Pl => "y"++BIND ;
        Third C9_6 Sg => "y"++BIND ;
        Third C9_6 Pl => "ng"++BIND ;
        Third C14 _ => "ng"++BIND ;
        Third C15 _ => "ng"++BIND ;
        Third C17 _ => "ng"++BIND ;
        First Sg => "y"++BIND ;
        First Pl => "y"++BIND ;
        Second Sg  => "ng"++BIND ;
        Second Pl => "y"++BIND
      } ;

      assoc_cop_pref : Polarity -> Agr -> Str = \pol,agr -> case pol of {
        Neg => "na"++BIND ;
        Pos => case agr of {
          Third C1_2 Sg => "no" ;
          Third C1_2 Pl => "na" ;
          Third C1a_2a Sg => "no" ;
          Third C1a_2a Pl => "na" ;
          Third C3_4 Sg  => "no" ;
          Third C3_4 Pl => "ne" ;
          Third C5_6 Sg => "ne" ;
          Third C5_6 Pl => "na" ;
          Third C7_8 Sg => "ne" ;
          Third C7_8 Pl => "ne" ;
          Third C9_10 Sg => "ne" ;
          Third C9_10 Pl => "ne" ;
          Third C11_10 Sg => "no" ;
          Third C11_10 Pl => "ne" ;
          Third C9_6 Sg => "ne" ;
          Third C9_6 Pl => "na" ;
          Third C14 _ => "no" ;
          Third C15 _ => "no" ;
          Third C17 _ => "no" ;
          First Sg => "na" ;
          First Pl => "na" ;
          Second Sg  => "na" ;
          Second Pl => "na"
        } ++BIND
      } ;

      kho_cop : VForm -> Agr -> Str = \vform,agr -> case vform of {
        VFIndic MainCl Neg PresTense => neg_kho_cop_pref agr ++ "kho";
        VFIndic RelCl Neg PresTense => (relConcCop vform agr RC) ++ (ap_cop_pref (VFIndic RelCl Neg PresTense) agr RelType) ++BIND++ "kho" ;
        VFIndic RelCl p t => (relConcCop vform agr RC) ++ (ap_cop_pref (VFIndic RelCl p t) agr RelType) ++ "khona" ;
        VFIndic MainCl p t => (ap_cop_pref (VFIndic MainCl p t) agr RelType) ++ "khona"
      } ;

      neg_kho_cop_pref : Agr -> Str = \agr ->
        "a" ++BIND++
         case agr of {
          Third C1_2 Sg => "ke" ;
          Third C1_2 Pl => "be" ;
          Third C1a_2a Sg => "ke" ;
          Third C1a_2a Pl => "be" ;
          -- Third C3_4 Sg  => "no" ;
          -- Third C3_4 Pl => "ne" ;
          -- Third C5_6 Sg => "ne" ;
          Third C5_6 Pl => "we" ;
          -- Third C7_8 Sg => "ne" ;
          -- Third C7_8 Pl => "ne" ;
          -- Third C9_10 Sg => "ne" ;
          -- Third C9_10 Pl => "ne" ;
          -- Third C11_10 Sg => "no" ;
          -- Third C11_10 Pl => "ne" ;
          -- Third C9_6 Sg => "ne" ;
          -- Third C9_6 Pl => "na" ;
          -- Third C14 _ => "no" ;
          -- Third C15 _ => "no" ;
          -- Third C17 _ => "no" ;
          -- First Sg => "na" ;
          -- First Pl => "na" ;
          -- Second Sg  => "na" ;
          -- Second Pl => "na"
          (First _ | Second _ | Third _ _ ) => subjConcLookup!agr!SCNeg
        } ++BIND ;


    ----------------------------------------
    -- OTHER
    ----------------------------------------

    link_conj : Str -> Str -> Str -> Bool -> Str = \conj,s_full,s_novow,fix -> case fix of {
      True => conj ++BIND ++ s_novow ;
      False => conj ++ s_full
    } ;
}
