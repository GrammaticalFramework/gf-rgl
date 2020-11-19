interface DiffScand = open CommonScand, Prelude in {
  flags coding=utf8 ;

--1 Differences between Scandinavian languages

-- Norway has three genders, Danish and Swedish have two.

  oper
    NGender : PType ;
 
    ngen2gen : NGender -> Gender ;

    neutrum, utrum : NGender ;

---    gennum : Gender -> Number -> GenNum ;

-- This is the form of the noun in "det stora berget"/"det store berg".

    detDef : Species ;

-- Danish and Norwegian verbs, but not Swedish verbs, 
-- have two possible compound-tense auxiliaries ("have" or "være").

    Verb : Type ;

    hasAuxBe : Verb -> Bool ;

-- The rest of the parameters are function words used in the syntax modules.

    conjThat : Str ;
    conjThan : Str ;
    compMore : Str ;
    conjAnd  : Str ;
    infMark  : Str ;

    subjIf : Str ;

    artIndef : NGender => Str ;
    detIndefPl : Str ;

    verbHave : Verb ;
    verbBe   : Verb ;

    verbBecome : Verb ;

    auxFut : Str  ; -- ska
    auxFutKommer : Str ; --  "kommer" 
    auxFutPart : Str ; --  "til" 
    auxCond : Str ;

    negation : Polarity => Str ;

-- For determiners; mostly two-valued even in Norwegian.

    genderForms : (x1,x2 : Str) -> NGender => Str ;

-- The forms of a relative pronoun ("som", "vars", "i vilken").

    relPron : Gender => Number => RCase => Str ;

-- Pronoun "sådan" used in $Relative.RelCl$.

    pronSuch : GenNum => Str ;

    reflPron : Agr -> Str ;

    hur_IAdv : {s : Str} ;

    av_Prep : Str ;

    possPron : Number -> Person -> Number -> Gender -> Str ;  -- mina bilar: Sg, P1, Utr, Pl
}

