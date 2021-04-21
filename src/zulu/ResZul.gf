--# -path=.:../abstract:../common:../../prelude

resource ResZul = open Prelude,Predef,ParamX in {

  param
    -- Number = Sg | Pl ;
    ClassGender = C1_2 | C1a_2a | C3_4 | C5_6 | C7_8 | C9_10 | C11_10 | C9_6 | C14 | C15 | C17 ;
    -- ClassPrefixForm = Full | NoInitVowel ;
    NForm = Full | Reduced ;
    Agr = First Number | Second Number | Third ClassGender Number ;

    -- SMood = SIndic | SPot | SSubj ; -- | SConsec ;
    DMood = Princ | Part ; -- mood dimension that depends on grammatical context: principal and participial
    Aspect = Null | Prog | Excl ;
    -- NOTE: removing the
    -- Tense = Absolute BasicTense | Relative BasicTense BasicTense ;
    -- NOTE: PerfTense maps to recent past, PastTense to remote past. Remote future not yet included.
    -- BasicTense = PerfTense | PastTense | PresTense | FutTense ;
    BasicTense = PerfTense | PastTense | PresTense | FutTense ;
    ZTense = Absolute BasicTense | Relative BasicTense BasicTense ;
    -- ImpForm = Imper | Polite ;
    -- Polarity = Pos | Neg ;

    -- NOTE: Although Poulos+Msimang use "verb form" instead of mood,
    -- we use VForm (verb form) to indicate combination of all parameters
    -- regarding the verb, and hence will use use "mood" for convenience.

    -- replacing BasicTense with Tense, just for now
    -- VForm = VFIndic DMood Polarity BasicTense Aspect | VFPot DMood Polarity Aspect | VFSubj Polarity ;
    VForm = VFIndic DMood Polarity BasicTense Aspect | VFPot DMood Polarity Aspect | VFSubj Polarity ;
    VPType = CopIdent | CopAssoc | CopDescr | CopEq | VNPCompl | VACompl | NoComp | VSCompl | AdvComp ;
    AuxType = PartAux ; -- TODO: add SubjAux, InfAux, ConsecAux etc (p327)

    AForm = AF1 | AF2 ; -- two forms for implementing sound changes Poulos+Msimang p143
    SCForm = SC | SCVow | SCNeg | SCHort | SCPS | SCPart | SCVowP | SCBe ;
    OCForm = OC | OCAE | OCIOU | OCMono | OCThing ;
    RCForm = RelC | RelCA ;
    --PCForm = (RA|RC) | EI | OU ;

    -- verb root characteristics
    RForm = R_a | R_ile | R_e | R_i | R_anga ;
    RInit = RA | RE | RI | RO | RU | RC ;
    Syl = SylMono | SylMult ;
    Voice = Active | Passive ;

    -- nominal and pronominal commencing sounds
    --NomInit = NA | NI | NO | NU | NC ;

  oper
    mkPron : Str -> Agr -> { s : Str ; agr : Agr ; empty : Str ; proDrop : Bool } = \s,agr -> {
      s = s ;
      agr = agr ;
      empty = [] ;
      proDrop = False
    } ;

  ---------------------
  -- VERB STRUCTURES --
  ---------------------
    regVerb : Str -> { s : RForm => Str ; r : RInit ; syl : Syl ; voice : Voice } = \root ->
    {
      s = table {
        R_a => root + "a" ;
        R_ile => case root of {
          _+"el" => root + "e" ;
          _ => root + "ile"
        } ;
        R_e => root + "e" ;
        R_i => root + "i" ;
        R_anga => root + "anga"
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
        R_ile => th + "ile" ;
        R_e => th + "e" ;
        R_i => th + "i" ;
        R_anga => th + "anga"
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
        R_e => root + "e" ;
        R_i => root + "i" ;
        R_anga => root + "anga"
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
        R_i => root + "i" ;
        R_anga => root + "anga"
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
        R_a => root + "a" ;
        R_ile => root + "ile" ;
        R_e => root + "e" ;
        R_i => root + "i" ;
        R_anga => root + "anga"
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

    -- NOTE : the empty SCVow for class 6 might produce a dangling BIND token, causing bugs
    -- TODO: SC following vowel
    subjConcLookup : Agr => SCForm => Str =
      table {
        -- agr                     default        before vowel     after neg pref   after hort        sit/part         potential/subjunct
        First Sg =>         table {SC => "ngi" ;  SCVow => "ng" ;  SCNeg => "ngi" ; SCHort => "ngi" ; SCPart => "ngi" ; SCPS => "ngi" ; SCVowP => "ngi" ; SCBe => "bengi" } ;
        Second Sg =>        table {SC => "u" ;    SCVow => "w" ;   SCNeg => "wu" ;  SCHort => "u" ;   SCPart => "u" ;   SCPS => "u" ;   SCVowP => "wu" ; SCBe => "ubu" } ;
        First Pl =>         table {SC => "si" ;   SCVow => "s" ;   SCNeg => "si" ;  SCHort => "si" ;  SCPart => "si" ;  SCPS => "si" ;  SCVowP => "si" ; SCBe => "besi" } ;
        Second Pl =>        table {SC => "ni" ;   SCVow => "n" ;   SCNeg => "ni" ;  SCHort => "ni" ;  SCPart => "ni" ;  SCPS => "ni" ;  SCVowP => "ni" ; SCBe => "beni" } ;
        Third C1_2 Sg =>    table {SC => "u" ;    SCVow => "w" ;   SCNeg => "ka" ;  SCHort => "ka" ;  SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wu" ; SCBe => "ube" } ;
        Third C1_2 Pl =>    table {SC => "ba" ;   SCVow => "b" ;   SCNeg => "ba" ;  SCHort => "ba" ;  SCPart => "be" ;  SCPS => "ba" ;  SCVowP => "ba" ; SCBe => "bebe" } ;
        Third C1a_2a Sg =>  table {SC => "u" ;    SCVow => "w" ;   SCNeg => "ka" ;  SCHort => "ka" ;  SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wu" ; SCBe => "ube" } ;
        Third C1a_2a Pl =>  table {SC => "ba" ;   SCVow => "b" ;   SCNeg => "ba" ;  SCHort => "ba" ;  SCPart => "be" ;  SCPS => "ba" ;  SCVowP => "ba" ; SCBe => "bebe" } ;
        Third C3_4 Sg =>    table {SC => "u" ;    SCVow => "w" ;   SCNeg => "wu" ;  SCHort => "wu" ;  SCPart => "u" ;   SCPS => "u" ;   SCVowP => "wu" ; SCBe => "ubu" } ;
        Third C3_4 Pl =>    table {SC => "i" ;    SCVow => "y" ;   SCNeg => "yi" ;  SCHort => "yi" ;  SCPart => "i" ;   SCPS => "i" ;   SCVowP => "yi" ; SCBe => "ibi" } ;
        Third C5_6 Sg =>    table {SC => "li" ;   SCVow => "l" ;   SCNeg => "li" ;  SCHort => "li" ;  SCPart => "li" ;  SCPS => "li" ;  SCVowP => "li" ; SCBe => "beli" } ;
        Third C5_6 Pl =>    table {SC => "a" ;    SCVow => [] ;    SCNeg => "wa" ;  SCHort => "wa" ;  SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wa" ; SCBe => "abe" } ;
        Third C7_8 Sg =>    table {SC => "si" ;   SCVow => "s" ;   SCNeg => "si" ;  SCHort => "si" ;  SCPart => "si" ;  SCPS => "si" ;  SCVowP => "si" ; SCBe => "besi" } ;
        Third C7_8 Pl =>    table {SC => "zi" ;   SCVow => "z" ;   SCNeg => "zi" ;  SCHort => "zi" ;  SCPart => "zi" ;  SCPS => "zi" ;  SCVowP => "zi" ; SCBe => "bezi" } ;
        Third C9_10 Sg =>   table {SC => "i" ;    SCVow => "y" ;   SCNeg => "yi" ;  SCHort => "yi" ;  SCPart => "yi" ;  SCPS => "i" ;   SCVowP => "yi" ; SCBe => "ibi" } ;
        Third C9_10 Pl =>   table {SC => "zi" ;   SCVow => "z" ;   SCNeg => "zi" ;  SCHort => "zi" ;  SCPart => "zi" ;  SCPS => "zi" ;  SCVowP => "zi" ; SCBe => "bezi" } ;
        Third C11_10 Sg =>  table {SC => "lu" ;   SCVow => "lw" ;  SCNeg => "lu" ;  SCHort => "lu" ;  SCPart => "lu" ;  SCPS => "lu" ;  SCVowP => "lu" ; SCBe => "belu" } ;
        Third C11_10 Pl =>  table {SC => "zi" ;   SCVow => "z" ;   SCNeg => "zi" ;  SCHort => "zi" ;  SCPart => "zi" ;  SCPS => "zi" ;  SCVowP => "zi" ; SCBe => "bezi" } ;
        Third C9_6 Sg =>   table {SC => "i" ;    SCVow => "y" ;   SCNeg => "yi" ;  SCHort => "yi" ;  SCPart => "yi" ;  SCPS => "i" ;   SCVowP => "yi" ; SCBe => "ibi" } ;
        Third C9_6 Pl =>   table {SC => "a" ;    SCVow => [] ;    SCNeg => "wa" ;  SCHort => "wa" ;  SCPart => "e" ;   SCPS => "a" ;   SCVowP => "wa" ; SCBe => "abe" } ;
        Third C14 _ =>      table {SC => "bu" ;   SCVow => "b" ;  SCNeg => "bu" ;  SCHort => "bu" ;  SCPart => "bu" ;  SCPS => "bu" ;   SCVowP => "bu" ; SCBe => "bebu" } ;
        Third C15 _ =>      table {SC => "ku" ;   SCVow => "kw" ;  SCNeg => "ku" ;  SCHort => "ku" ;  SCPart => "ku" ;  SCPS => "ku" ;  SCVowP => "ku" ; SCBe => "beku" } ;
        Third C17 _ =>      table {SC => "ku" ;   SCVow => "kw" ;  SCNeg => "ku" ;  SCHort => "ku" ;  SCPart => "ku" ;  SCPS => "ku" ;  SCVowP => "ku" ; SCBe => "beku" }
      } ;

    --
    icomp_pref : VForm -> Agr -> Str = \vform,agr ->
    let
      neg1 = icompNeg1 vform ;
      neg2 = icompNeg2 vform ;
    in
    case vform of {
      VFIndic _ Pos PresTense _ => subjConcLookup ! agr ! SC ++BIND ;
      VFIndic _ Pos PastTense _ => subjConcLookup ! agr ! SC ++BIND++ "be" ++ subjConcLookup ! agr ! SCBe ++BIND ;
      VFIndic _ Pos PerfTense _ => [] ; -- "be"++BIND++ subjConcLookup ! agr ! SCBe ++BIND ;
      VFIndic _ Pos FutTense _ => subjConcLookup ! agr ! SC ++BIND ++ "zobe" ++ subjConcLookup ! agr ! SC ++BIND;

      -- might have to add an SCNegBe to the table
      VFIndic _ Neg PresTense _ => neg1 ++ subjConcLookup ! agr ! SCNeg ++BIND ++ neg2 ;
      VFIndic _ Neg PastTense _ => neg1 ++ subjConcLookup ! agr ! SC ++BIND++ "be" ++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2 ;
      VFIndic _ Neg PerfTense _ => "nga" ; -- neg1 ++ "be"++BIND++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2 ;
      VFIndic _ Neg FutTense _ => neg1 ++ subjConcLookup ! agr ! SCNeg ++BIND ++ "zobe" ++ subjConcLookup ! agr ! SCBe ++BIND ++ neg2 ;

      VFPot _ Pos _ => subjConcLookup ! agr ! SC ++BIND++"ngaba" ++ subjConcLookup ! agr ! SCPS ++BIND ;
      VFPot _ Neg _ => subjConcLookup ! agr ! SC ++BIND++"ngebe" ++ subjConcLookup ! agr ! SCPS ++BIND ++ neg2 ;
      VFSubj Neg => subjConcLookup ! agr ! SCNeg ++BIND ;
      VFSubj Pos => subjConcLookup ! agr ! SC ++BIND
    } ;

    subjConc : VForm -> Agr -> Bool -> Str = \vform,agr,prevow ->
      case prevow of {
        True => subjConcLookup ! agr ! SCVow ++BIND ;
        False => case vform of {
          VFIndic Princ Neg _ _ => subjConcLookup ! agr ! SCNeg ++BIND ;
          VFIndic _ Pos PastTense _ => subjConcLookup ! agr ! SCVow ++BIND ;
          VFIndic Princ _ _ _    => subjConcLookup ! agr ! SC ++BIND ;
          VFIndic Part _ _ _ => subjConcLookup ! agr ! SCPart ++BIND ;
          VFPot _ _ _ => subjConcLookup ! agr ! SCPS ++BIND ;
          VFSubj Neg => subjConcLookup ! agr ! SCNeg ++BIND ;
          VFSubj Pos => subjConcLookup ! agr ! SC ++BIND
        }
      } ;

    -- -be aux: reference time in relation to coding time
    -- relSubjConc : BasicTense -> Agr -> Str = \tense,agr ->
    relSubjConc : BasicTense -> Agr -> Str = \tense,agr ->
      case tense of {
        PastTense => subjConcLookup ! agr ! SCVow ++BIND++ "a" ++BIND++ case agr of {
          Second Sg | Third C3_4 Sg => "w" ++BIND ;
          Third C1_2 Sg | Third C1a_2a Sg | Third C3_4 Pl | Third C5_6 Pl | Third C9_10 Sg => "y" ++BIND ;
          First _ | Second _ | Third _ _ => []
        } ;
        PerfTense | PresTense => subjConcLookup ! agr ! SC ++BIND++ "be" ; -- NOTE: present tense doesn't make much sense here, so default to perf
        FutTense => subjConcLookup ! agr ! SC ++BIND++ "zobe"
      } ;

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
        Third C9_6 Sg =>   table {OC => "yi" ;   OCAE => "y" ;   OCIOU => "y" ;  OCMono => "yi" ;  OCThing => "yi" } ;
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

    rform : VForm -> Bool -> RForm = \vform,longform -> case longform of {
      True => case vform of {
        VFIndic _ Pos PresTense _ => R_a ; -- VTerm
        VFIndic _ Pos FutTense _ => R_a ; -- VTerm
        VFIndic Princ Pos PerfTense _ => R_ile ; -- VPLF
        VFIndic Part Pos PerfTense _ => R_e ; -- VPLF
        VFIndic _ Pos PastTense _ => R_a ; -- VTerm
        VFIndic _ Neg PresTense _ => R_i ; -- VTerm
        VFIndic _ Neg FutTense _ => R_a ; -- VTerm
        -- VFIndic Princ Neg PerfTense Null => perfsuff ; -- VPLF
        -- VFIndic Part Neg PerfTense Null => "" ; -- VPLF
        VFIndic _ Neg PerfTense _ => R_anga ; -- VNegP
        VFIndic _ Neg PastTense _ => R_anga ; -- VNegP
        VFPot _ Pos _ => R_a ;
        VFPot _ Neg _ => R_e ;
        VFSubj Pos => R_e ;
        VFSubj Neg => R_i
      } ;
      False => case vform of {
        VFIndic _ Pos PresTense _ => R_a ; -- VTerm
        VFIndic _ Pos FutTense _ => R_a ; -- VTerm
        VFIndic _ Pos PerfTense _ => R_e ; -- VPSF
        VFIndic _ Pos PastTense _ => R_a ; -- VTerm
        VFIndic _ Neg PresTense _ => R_i ; -- VTerm
        VFIndic _ Neg FutTense _ => R_a ; -- VTerm
        VFIndic _ Neg PerfTense _ => R_anga ; -- VNegP
        VFIndic _ Neg PastTense _ => R_anga ; -- VNegP
        VFPot _ Pos _ => R_a ;
        VFPot _ Neg _ => R_e ;
        VFSubj Pos => R_e ;
        VFSubj Neg => R_i
      }
    } ;

    -- verb terminative suffixes
      -- gives the long form perf suffix if applicable (whether based on object/adverb or inchoative root)
    vtermSuff : VForm -> Bool -> Str -> Str -> Str = \vform,longform,perfsuff,suff ->
      case longform of {
        True => case vform of {
                  VFIndic _ Pos PresTense _ => suff ; -- VTerm
                  VFIndic _ Pos FutTense _ => suff ; -- VTerm
                  VFIndic Princ Pos PerfTense _ => perfsuff ; -- VPLF
                  VFIndic Part Pos PerfTense _ => "e" ; -- VPLF
                  VFIndic _ Pos PastTense _ => suff ; -- VTerm
                  VFIndic _ Neg PresTense _ => "i" ; -- VTerm
                  VFIndic _ Neg FutTense _ => suff ; -- VTerm
                  -- VFIndic Princ Neg PerfTense Null => perfsuff ; -- VPLF
                  -- VFIndic Part Neg PerfTense Null => "" ; -- VPLF
                  VFIndic _ Neg PerfTense _ => "anga" ; -- VNegP
                  VFIndic _ Neg PastTense _ => "anga" ; -- VNegP
                  VFPot _ Pos _ => suff ;
                  VFPot _ Neg _ => "e" ;
                  VFSubj Pos => "e" ;
                  VFSubj Neg => "i"
                } ;
        False => case vform of {
                  VFIndic _ Pos PresTense _ => suff ; -- VTerm
                  VFIndic _ Pos FutTense _ => suff ; -- VTerm
                  VFIndic _ Pos PerfTense _ => "e" ; -- VPSF
                  VFIndic _ Pos PastTense _ => suff ; -- VTerm
                  VFIndic _ Neg PresTense _ => "i" ; -- VTerm
                  VFIndic _ Neg FutTense _ => suff ; -- VTerm
                  VFIndic _ Neg PerfTense _ => "anga" ; -- VNegP
                  VFIndic _ Neg PastTense _ => "anga" ; -- VNegP
                  VFPot _ Pos _ => suff ;
                  VFPot _ Neg _ => "e" ;
                  VFSubj Pos => "e" ;
                  VFSubj Neg => "i"
                }
      } ;

    -- tense prefixes
    tensePref : VForm -> Str = \vform ->
      case vform of {
        VFIndic _ Pos FutTense _ => "zo" ++BIND ;
        VFIndic _ Neg FutTense _ => "zu" ++BIND ;
        VFIndic _ Pos PastTense _ => "a" ++BIND ;
        VFIndic _ _ _ _ => [] ;
        VFPot _ _ _ => [] ;
        VFSubj _ => []
      } ;

    -- negative prefixes
    negPref : VForm -> Agr -> Str = \vform,agr ->
      case vform of {
        -- VFIndic Princ Neg PastTense _ => case agr of {
        --   Third (C1_2 | C1a_2a) Sg => "a"++BIND ;
        --   Third _ _ => "ka"++BIND ;
        --   First _ => "ka"++BIND ;
        --   Second _ => "ka"++BIND
        -- } ;
        VFIndic Princ Neg _ _ => "a"++BIND ;
        VFIndic _ _ _ _ => [] ;
        VFPot _ _ _ => [] ;
        VFSubj _ => []
      } ;

      -- TODO : sound rules to choose between nge and nga
      negPref2 : VForm -> Str = \vform ->
        case vform of {
          VFIndic Part Neg FutTense Null => "nga" ++BIND ; -- sometimes nge? p274
          -- VFIndic Part Neg _ _ => pre { "z" => "nge" ; _ => "nga" } ++BIND ;
          VFIndic Part Neg _ _ => "nga" ++BIND ;
          VFIndic _ _ _ _ => [] ;
          VFPot _ _ _ => [] ;
          VFSubj Neg => pre { "z" => "nge" ; _ => "nga" } ++BIND ;
          VFSubj Pos => []
        } ;

      negPrefNga : VForm -> Str = \vform -> case vform of {
        VFIndic _ Neg _ _ => "nga" ;
        VFIndic _ Pos _ _ => [] ;
        VFPot _ Neg _ => "nga" ;
        VFPot _ Pos _ => [] ;
        VFSubj Neg => "nga" ;
        VFSubj Pos => []
      } ;

      negPrefNge : VForm -> Str = \vform -> case vform of {
        VFIndic _ Neg _ _ => "nge" ;
        VFIndic _ Pos _ _ => [] ;
        VFPot _ Neg _ => "nge" ;
        VFPot _ Pos _ => [] ;
        VFSubj Neg => "nge" ;
        VFSubj Pos => []
      } ;

    icompNeg1 : VForm -> Str = \vform -> case vform of {
      VFIndic _ Neg PresTense _ => "a"++BIND ;
      VFIndic _ _ _ _ => [] ;
      VFPot _ _ _ => [] ;
      VFSubj _ => []
    } ;

    icompNeg2 : VForm -> Str = \vform -> case vform of {
      VFIndic _ Neg _ _ => "nga"++BIND ;
      VFIndic _ _ _ _ => [] ;
      VFPot _ Neg _ => "nga"++BIND ;
      VFPot _ _ _ => [] ;
      VFSubj Neg => "nga"++BIND ;
      VFSubj Pos => []
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
    progPref : VForm -> Str = \vform ->
      case vform of {
        VFIndic _ Pos PastTense _ => nonExist ; -- progressive past does not occur
        -- VFIndic _ Pos PastTense _ => [] ;
        VFIndic _ Pos _ _ => "sa" ++BIND ;
        -- VFIndic _ Pos _ _ => [] ;
        VFIndic _ Neg FutTense _ => "se" ++BIND ;
        VFIndic _ Neg _ _ => "sa" ++BIND ;
        VFIndic _ _ _ _ => nonExist ;
        -- VFIndic _ _ _ _ => [] ;
        VFPot _ _ _ => "se" ++BIND ;
        -- VFPot _ _ _ => [] ;
        VFSubj _ => nonExist

      } ;

    -- exclusive se prefix
    exclSePref : VForm -> Str = \vform ->
      case vform of {
        VFIndic _ Pos _ Excl => "se"++BIND ;
        VFIndic _ _ _ _ => [] ;
        VFPot _ Pos Excl => "se"++BIND ;
        VFPot _ _ _ => [] ;
        VFSubj _ => []
      } ;

    -- exclusive ka prefix
    exclKaPref : VForm -> Str = \vform ->
      case vform of {
        VFIndic _ Neg (PresTense | FutTense) Excl => "ka" ++BIND ;
        VFIndic _ _ _ _ => [] ;
        VFPot _ _ _ => [] ;
        VFSubj _ => []
      } ;

    -- potential prefixes
    potPref : VForm -> Str = \vform ->
      case vform of {
        VFPot _ Pos _ => "nga" ++BIND ;
        VFPot _ Neg _ => "nge" ++BIND ;
        VFIndic _ _ _ _ => [] ;
        VFSubj _ => []
      } ;

    -- VForm = VFIndic DMood Polarity BasicTense Aspect | VFPot DMood Polarity Aspect | VFSubj Polarity ;
    aux_be : VForm -> Agr -> Str = \vform,agr ->
    let
      sc = subjConc vform agr False ;
      scvow = subjConc vform agr True ;
      short_be = case agr of {
        -- Second Pl => sc ++ "bu" ;
        -- Third C3_4 Sg => sc ++ "bu" ;
        -- Third C3_4 Pl => sc ++ "bi" ;
        -- Third C9_6 Sg | Third C9_10 Sg => sc ++ "bi" ;
        First _ | Second _ | Third _ _ => subjConcLookup!agr!SCBe
      }
    in
    case vform of {
      VFIndic Princ Pos PresTense _ => [] ;
      VFIndic Princ Pos PerfTense _ => short_be ++BIND ; -- 2021-01-26, chose to only implement short form
      VFIndic Princ Pos FutTense _ => sc ++ "zobe" ;
      VFIndic Princ Pos PastTense _ => scvow ++ "abe" ;

      VFIndic Princ Neg PresTense _ => [] ;
      VFIndic Princ Neg PerfTense _ => short_be ++BIND ;
      VFIndic Princ Neg FutTense _ => sc ++ "zobe" ;
      VFIndic Princ Neg PastTense _ => scvow ++ "abe" ;

      VFIndic Part Pos PresTense _ => [] ;
      VFIndic Part Pos PerfTense _ => short_be ++BIND ;
      VFIndic Part Pos FutTense _ => sc ++ "zobe" ;
      VFIndic Part Pos PastTense _ => scvow ++ "abe" ;

      VFIndic Part Neg PresTense _ => [] ;
      VFIndic Part Neg PerfTense _ => short_be ++BIND ;
      VFIndic Part Neg FutTense _ => sc ++ "zobe" ;
      VFIndic Part Neg PastTense _ => scvow ++ "abe" ;

      VFPot _ Pos _ => sc ++ "ngaba" ;
      VFPot _ Neg _ => sc ++ "ngebe" ;
      VFSubj Pos => sc ++ "be" ++BIND ;
      VFSubj Neg => sc ++ "ngabi"
    } ;

  --------------------
  -- ADV STRUCTURES --
  --------------------
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

  -- associative copulative
  advPref : RInit => Str = table {
    RU => "no" ;
    RI => "ne" ;
    RO => "no" ;
    _  => "na"
  } ;

  eqPref : RInit => Str = table {
    RU => "ngango" ;
    RI => "ngange" ;
    RO => "ngango" ;
    _  => "nganga"
  } ;

  --------------------
  -- ADJ STRUCTURES --
  --------------------
  regAdj : Str -> { s : AForm => Str ; b : Bool ; empty : Str } = \a ->
  {
    s = table {
      AF1 => a ;
      AF2 => case a of {
        "kh"+x => "nk"+x ;
        "th"+x => "nt"+x ;
        "sh"+x => "ntsh"+x ;
        --"b"+#vowel => a ; -- TODO: check: monosyllabic, like bi?
        "b"+x => "mb"+x ;
        "f"+x => "mf"+x ;
        "hl"+x => "nhl"+x ;
        _ => a
      }
    } ;
    b = case a of {
      ("kh"|"th"|"sh"|"b"|"f"|"hl")+_ => True ;
      ("m"|"n")+_ => True ;
      _ => False
    } ;
    empty = []
  } ;

  relConc : Agr => RCForm => Str =
    table {
      Third C1_2 Sg => table { RelC => "o" ; RelCA => "ow" } ;
      Third C1_2 Pl => table { _ => "aba" } ;
      Third C1a_2a Sg => table { RelC => "o" ; RelCA => "ow" } ;
      Third C1a_2a Pl => table { _ => "aba" } ;
      Third C3_4 Sg  => table { RelC => "o" ; RelCA => "ow" } ;
      Third C3_4 Pl => table { RelC => "e" ; RelCA => "ey" } ;
      Third C5_6 Sg => table { RelC => "eli" ; RelCA => "el" } ;
      Third C5_6 Pl => table { _ => "a" } ;
      Third C7_8 Sg => table { RelC => "esi" ; RelCA => "es" } ;
      Third C7_8 Pl => table { RelC => "ezi" ; RelCA => "ez" } ;
      Third C9_10 Sg => table { RelC => "e" ; RelCA => "ey" } ;
      Third C9_10 Pl => table { RelC => "ezi" ; RelCA => "ez" } ;
      Third C11_10 Sg => table { RelC => "olu" ; RelCA => "olw" } ;
      Third C11_10 Pl => table { RelC => "ezi" ; RelCA => "ez" } ;
      Third C9_6 Sg => table { RelC => "e" ; RelCA => "ey" } ;
      Third C9_6 Pl => table { _ => "a" } ;
      Third C14 _ => table { RelC => "obu" ; RelCA => "ob" } ;
      Third C15 _ => table { RelC => "oku" ; RelCA => "okw" } ;
      Third C17 _ => table { RelC => "oku" ; RelCA => "okw" } ;
      First Sg => table { RelC => "engi" ; RelCA => "eng" } ;
      First Pl => table { RelC =>  "esi" ; RelCA => "es" } ;
      Second Sg  => table { RelC => "o" ; RelCA => "ow" } ;
      Second Pl => table { RelC => "eni" ; RelCA => "en" }
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

    -- relSuf : VForm -> Bool -> Str = \longform ->
    -- case longform of {
    --   True => pre { #vowel => [] ; #cons => [] ; "y" => [] ; _ => "yo" } ;
    --   False => []
    -- } ;

    rel_yo : Str = pre {
      #vowel => [] ;
      #cons => [] ;
      _ => BIND++"yo"
    } ;

    relSuf : VForm -> Str = \vform -> case vform of {
      VFIndic _ Pos PresTense _ => rel_yo ;
      VFIndic _ Pos FutTense _ => [] ;
      VFIndic Princ Pos PerfTense _ => rel_yo ;
      VFIndic Part Pos PerfTense _ => [] ;
      VFIndic _ Pos PastTense _ => rel_yo ;

      VFIndic _ Neg FutTense _ => [] ;
      VFIndic _ Neg PerfTense _ => [] ; -- TODO : make dependent on boolean; p157
      VFIndic _ Neg PastTense _ => [] ;
      VFIndic _ Neg PresTense _ => rel_yo ;

      VFPot _ Pos _ => rel_yo ;
      VFPot _ Neg _ => rel_yo ;

      VFSubj _ => []
    } ;

  -- chooses the form of the root to use for N-prefixes
  aformN : Agr -> AForm = \agr ->
    case agr of {
      Third C7_8 Pl => AF2 ;
      Third C9_10 Sg => AF2 ;
      Third C9_10 Pl => AF2 ;
      Third C9_6 Sg => AF2 ;
      Third C11_10 Pl => AF2 ;
      _ => AF1
    } ;

  adjConcLookup : Agr => AForm => Str =
    table {
      Third C1_2 Sg => \\_ => "om" ;
      Third C1_2 Pl => \\_ => "aba" ;
      Third C1a_2a Sg => \\_ => "omu" ;
      Third C1a_2a Pl => \\_ => "aba" ;
      Third C3_4 Sg  => \\_ => "omu" ;
      Third C3_4 Pl => \\_ => "emi" ;
      Third C5_6 Sg => \\_ => "eli" ;
      Third C5_6 Pl => \\_ => "ama" ;
      Third C7_8 Sg => \\_ => "esi" ;
      Third C7_8 Pl => table { AF1 => "ezin" ; AF2 => "ezi" } ;
      Third C9_10 Sg => table { AF1 => "en" ; AF2 => "e" } ;
      Third C9_10 Pl => table { AF1 => "ezin" ; AF2 => "ezi" } ;
      Third C11_10 Sg => \\_ => "olu" ;
      Third C11_10 Pl => table { AF1 => "ezin" ; AF2 => "ezi" } ;
      Third C9_6 Sg => table { AF1 => "en" ; AF2 => "e" } ;
      Third C9_6 Pl => \\_ => "ama" ;
      Third C14 _ => \\_ => "obu" ;
      Third C15 _ => \\_ => "oku" ;
      Third C17 _ => \\_ => "oku" ;
      (First _ | Second _ )  => \\_ => "om"
    } ;

    adjPrefLookup : Agr => AForm => Str =
      table {
        Third C1_2 Sg => \\_ => "mu" ;
        Third C1_2 Pl => \\_ => "ba" ;
        Third C1a_2a Sg => \\_ => "mu" ;
        Third C1a_2a Pl => \\_ => "ba" ;
        Third C3_4 Sg  => \\_ => "mu" ;
        Third C3_4 Pl => \\_ => "mi" ;
        Third C5_6 Sg => \\_ => "li" ;
        Third C5_6 Pl => \\_ => "ma" ;
        Third C7_8 Sg => \\_ => "si" ;
        Third C7_8 Pl => table { AF1 => "zin" ; AF2 => "zi" } ;
        Third C9_10 Sg => table { AF1 => "in" ; AF2 => "i" } ;
        Third C9_10 Pl => table { AF1 => "zin" ; AF2 => "zi" } ;
        Third C11_10 Sg => \\_ => "lu" ;
        Third C11_10 Pl => table { AF1 => "zin" ; AF2 => "zi" } ;
        Third C9_6 Sg => table { AF1 => "in" ; AF2 => "i" } ;
        Third C9_6 Pl => \\_ => "ma" ;
        Third C14 _ => \\_ => "bu" ;
        Third C15 _ => \\_ => "ku" ;
        Third C17 _ => \\_ => "ku" ;
        First Sg => \\_ => "mu" ;
        First Pl => \\_ => "ba" ;
        Second Sg => \\_ => "mu" ;
        Second Pl => \\_ => "om"
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

    quantConc : Agr => Str = table {
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
  ---------------------
  -- NOUN STRUCTURES --
  ---------------------
    -- worst case
    mkNoun : (noms,nomp,locs,locp : Str) -> ClassGender -> { s : Number => NForm => Str ; loc : Number => Str ; c : ClassGender ; empty : Str } =
      \noms,nomp,locs,locp,cg ->
      {
        s = table { Sg => table { Full => noms ; Reduced => (drop_init_vowel noms) } ;
                      Pl => table { Full => nomp ; Reduced => (drop_init_vowel nomp) } } ;
        loc = table { Sg => locs ;
                      Pl => locp } ;
        c = cg ;
        empty = []
      } ;

    semiRegNoun : (root,locs,locp : Str) -> ClassGender -> { s : Number => NForm => Str ; loc : Number => Str ; c : ClassGender ; empty : Str } =
      \root,locs,locp,cg ->
      let
        noms : Str = nomNoun root Sg cg ;
        nomp : Str = nomNoun root Pl cg ;
      in
      mkNoun noms nomp locs locp cg ;

    regNoun : Str -> ClassGender -> { s : Number => NForm => Str ; loc : Number => Str ; c : ClassGender ; empty : Str } =
      \root,cg ->
      let
        noms : Str = nomNoun root Sg cg ;
        nomp : Str = nomNoun root Pl cg ;
        locs : Str = locNoun root Sg cg ;
        locp : Str = locNoun root Pl cg ;
        empty = []
      in
      mkNoun noms nomp locs locp cg ;

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

    -- Src: Doke
    addLocSuffix : Str -> Str = \root ->
      case root of
      {
        _+"bo" => (tk 2 root) + "tsheni" ;
        _+"pho" => (tk 3 root) + "sheni" ;
        _+"bho" => (tk 3 root) + "jeni" ;
        _+"bu" => (tk 2 root) + "tshini" ;
        _+"phu" => (tk 3 root) + "shini" ;
        _+"bhu" => (tk 3 root) + "jini" ;
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
    bilabial_cons : pattern Str = #("m"|"bh"|"ph"|"b"|"p") ;
    alveolar_cons : pattern Str = #("s"|"d"|"t"|"z") ;

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
                                    _ => "izi"+root
                                  } ; -- iz for roots starting with vowel
        <C9_10,Sg> => case root of {
                                    ("m"|"n")+_ => "i"+root ;
                                    #labial_cons+_ => "im"+root ;
                                    _ => "in"+root
                                  } ; -- im for labial, in for alveolar (TODO: does this correctly split options?)
        <C9_10,Pl> => case root of {
                                    ("m"|"n")+_ => "izi"+root ;
                                    #labial_cons+_ => "izim"+root ;
                                    _ => "izin"+root
                                  } ; -- izim for labial, izin for alveolar (TODO: does this correctly split options?)
        <C11_10,Sg> => "u"+root ;
        <C11_10,Pl> => case root of {
                                    "kh"+_ => "izink" + (drop 2 root) ;
                                    "ph"+_ => "izimp" + (drop 2 root) ;
                                    "th"+_ => "izint" + (drop 2 root) ;
                                    "sh"+_ => "izintsh" + (drop 2 root) ;
                                    #labial_cons+_ => "izim"+root ;
                                    ("m"|"n")+_ => "izi"+root ;

                                    _ => "izin"+root
                                  } ; -- izim for labial, izin for alveolar, izi(n|m)k for roots starting with kh
        <C9_6,Sg> => case root of {
                                    ("m"|"n")+_ => "i"+root ;
                                    #labial_cons+_ => "im"+root ;
                                    _ => "in"+root
                                  } ; -- im for labial, in for alveolar (TODO: does this correctly split options?)
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
                                      #labial_cons+_ => "em"+(addLocSuffix root) ;
                                      "gw"+_ => "em"+(addLocSuffix root) ;
                                      _ => "en"+(addLocSuffix root)
                                    } ; -- -- em for labial, en for alveolar (TODO: does this correctly split options?)
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
                                      _ => "ezi"+(addLocSuffix root)
                                    } ; -- iz for roots starting with vowel
          <C9_10,Sg> => case root of {
                                      ("m"|"n")+_ => "e"+(addLocSuffix root) ;
                                      #labial_cons+_ => "em"+(addLocSuffix root) ;
                                      _ => "en"+(addLocSuffix root)
                                    } ; -- em for labial, en for alveolar (TODO: does this correctly split options?)
          <C9_10,Pl> => case root of {
                                      ("m"|"n")+_ => "ezi"+(addLocSuffix root) ;
                                      #labial_cons+_ => "ezim"+(addLocSuffix root) ;
                                      _ => "ezin"+(addLocSuffix root)
                                    } ; -- izim for labial, izin for alveolar (TODO: does this correctly split options?)
          <C11_10,Sg> => "o"+(addLocSuffix root) ;
          <C11_10,Pl> => case root of {
                                      "kh"+_ => "ezink" + (drop 2 (addLocSuffix root)) ;
                                      "ph"+_ => "ezimp" + (drop 2 (addLocSuffix root)) ;
                                      "th"+_ => "ezint" + (drop 2 (addLocSuffix root)) ;
                                      "sh"+_ => "ezintsh" + (drop 2 (addLocSuffix root)) ;
                                      #labial_cons+_ => "ezim"+(addLocSuffix root) ;
                                      ("m"|"n")+_ => "ezi"+(addLocSuffix root) ;
                                      _ => "ezin"+(addLocSuffix root)
                                    } ; -- izim for labial, izin for alveolar, izi(n|m)k for roots starting with kh
          <C9_6,Sg> => case root of {
                                      ("m"|"n")+_ => "e"+(addLocSuffix root) ;
                                      #labial_cons+_ => "em"+(addLocSuffix root) ;
                                      _ => "en"+(addLocSuffix root)
                                    } ; -- em for labial, en for alveolar (TODO: does this correctly split options?)
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

    poss_concord : ClassGender => Number => RInit => Str =
      table {
              C1_2 => table {Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
                             Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" }
                            };
              C1a_2a => table {Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
                               Pl => table {(RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" }
                              };
              C3_4 => table {Sg => table {(RA|RC) => "wa" ; (RE|RI) => "we" ; (RO|RU) => "wo" } ;
                             Pl => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" }
                            };
              C5_6 => table {Sg => table {(RA|RC) => "la" ; (RE|RI) => "le" ; (RO|RU) => "lo" } ;
                             Pl => table {(RA|RC) => "a" ; (RE|RI) => "e" ; (RO|RU) => "o" }
                            };
              C7_8 => table {Sg => table {(RA|RC) => "sa" ; (RE|RI) => "se" ; (RO|RU) => "so" } ;
                             Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" }
                            };
              C9_10 => table {Sg => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
                              Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" }
                            };
              C11_10 => table {Sg => table {(RA|RC) => "lwa" ; (RE|RI) => "lwe" ; (RO|RU) => "lo" } ;
                             Pl => table {(RA|RC) => "za" ; (RE|RI) => "ze" ; (RO|RU) => "zo" }
                            };
              C9_6 => table {Sg => table {(RA|RC) => "ya" ; (RE|RI) => "ye" ; (RO|RU) => "yo" } ;
                              Pl => table {(RA|RC) => "a" ; (RE|RI) => "e" ; (RO|RU) => "o" }
                            };
              C14 => table { _ => table { (RA|RC) => "ba" ; (RE|RI) => "be" ; (RO|RU) => "bo" }
                            };
              C15 => table { _ => table { (RA|RC) => "kwa" ; (RE|RI) => "kwe" ; (RO|RU) => "ko" }
                            };
              C17 => table { _ => table { (RA|RC) => "kwa" ; (RE|RI) => "kwe" ; (RO|RU) => "ko" }
                            }
            } ;

    poss_concord_c1a : ClassGender => Number => RInit => Str =
      table {
              C1_2 => table {Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
                              Pl => table {(RA|RC) => "baka" ; (RE|RI) => "bake" ; (RO|RU) => "bako" }
                            };
              C1a_2a => table {Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
                               Pl => table {(RA|RC) => "baka" ; (RE|RI) => "bake" ; (RO|RU) => "bako" }
                              };
              C3_4 => table {Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
                             Pl => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" }
                            };
              C5_6 => table {Sg => table {(RA|RC) => "lika" ; (RE|RI) => "like" ; (RO|RU) => "liko" } ;
                             Pl => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" }
                            };
              C7_8 => table {Sg => table {(RA|RC) => "sika" ; (RE|RI) => "sike" ; (RO|RU) => "siko" } ;
                             Pl => table {(RA|RC) => "zika" ; (RE|RI) => "zike" ; (RO|RU) => "ziko" }
                            };
              C9_10 => table {Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
                              Pl => table {(RA|RC) => "zika" ; (RE|RI) => "zike" ; (RO|RU) => "ziko" }
                            };
              C11_10 => table {Sg => table {(RA|RC) => "luka" ; (RE|RI) => "luke" ; (RO|RU) => "luko" } ;
                               Pl => table {(RA|RC) => "zika" ; (RE|RI) => "zike" ; (RO|RU) => "ziko" }
                          };
              C9_6 => table {Sg => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" } ;
                              Pl => table {(RA|RC) => "ka" ; (RE|RI) => "ke" ; (RO|RU) => "ko" }
                            };
              C14 => table { _ => table { (RA|RC) => "buka" ; (RE|RI) => "buke" ; (RO|RU) => "buko" }
                            };
              C15 => table { _ => table { (RA|RC) => "kuka" ; (RE|RI) => "kuke" ; (RO|RU) => "kuko" }
                            };
              C17 => table { _ => table { (RA|RC) => "kuka" ; (RE|RI) => "kuke" ; (RO|RU) => "kuko" }
                            }
      } ;

-- REF: Poulos & Msimang p355
-- cop_pref has the following forms
-- ngu:
--     - absolute pronoun of 2nd person sg
--     - class 1
--     - all other a-, o-, u- commencing absolute pronouns and nouns except class 11
--
-- y:
--     - i- commencing absolute pronouns and nouns
--
-- yi:
--     - everything else?

    cop_pref : Agr -> Str = \agr ->
    let
      i = nominit!agr ;
    in
      case i of {
        (RA | RO | RU) => case agr of {
          Third C11_10 Sg => "w" ; -- pp356,358
          (First _ | Second _ ) => subjConcLookup!agr!SC ++BIND++ "ng" ;
          Third _ _ => "ng" -- variants { "ng" ; "w" }  -- u is deleted before vowel ; variant see p358
        } ;
        RI => case agr of {
          Second Sg => "u" ;
          Third C1_2 Sg => "ng" ; -- variants { "ng" ; "w" } ; -- umu/um already has u ; variant see p358
          (First _ | Second _) => subjConcLookup!agr!SC ++BIND++ "y" ;
          Third _ _ => "y"
        } ;
        _ => case agr of {
          (First _ | Second _) => subjConcLookup!agr!SC ++BIND++ "yi" ; -- variants { "yi" ; "i" } ;
          Third _ _ => "yi"
        }
      } ;

    ----------------------------------------
    -- OTHER
    ----------------------------------------

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

    link_conj : Str -> Str -> Str -> Bool -> Str = \conj,s_full,s_novow,fix -> case fix of {
      True => conj ++BIND ++ s_novow ;
      False => conj ++ s_full
    } ;

    pre_cop_pref : VForm -> Agr -> Str = \vform,agr ->
    let
      sc = subjConc vform agr False ;
      scvow = subjConc vform agr True
    in
      case vform of {
        VFIndic _ Pos PresTense _ => sc ;
        VFIndic _ Pos FutTense _ => sc ; --sc ++ "zobe" ;
        VFIndic _ Pos PerfTense _ => [] ; -- ; scvow ++ "abe" ;
        VFIndic _ Pos PastTense _ => subjConcLookup!agr!SC ; -- long form past: kwabe ku-

        VFIndic _ Neg FutTense _ => sc ++ "nge" ++BIND ;
        VFIndic _ Neg PerfTense _ => "nge" ++BIND ;
        VFIndic _ Neg PastTense _ => subjConcLookup!agr!SC ++ "nge" ++BIND ;
        VFIndic Princ Neg PresTense _ => case agr of {
          (First _ | Second _ ) => "ka" ++BIND++ sc ;
          Third _ _ => "akusi" ++BIND
        } ;
        VFIndic Part Neg PresTense _ => sc ++ "nge" ++BIND ;
        VFPot _ Pos _ => sc ; -- sc ++ "ngaba" ;
        VFPot _ Neg _ => sc ; --sc ++ "ngebe" ;
        VFSubj Pos => sc ; --sc ++ "be" ++BIND ;
        VFSubj Neg => sc -- sc ++ "ngabi"
      } ;

    instrument_pref : Agr -> Str = \agr ->
    let
      i = nominit!agr ;
    in
      case i of {
        (RA | RE | RC) => "nga" ++BIND ;
        RI => "nge" ++BIND ;
        (RO | RU) => "ngo" ++BIND
      } ;
}
