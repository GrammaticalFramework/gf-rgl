abstract ExtraLatAbs =
  Extra, ExtraLexiconLatAbs, Conjunction
  ** {
    cat CS ;
  fun
    useS : S -> CS ;
    -- do not drop pronouns
    UsePronNonDrop : Pron -> NP ;

    -- add adjective before the noun
    AdjCNPre : AP -> CN -> CN ;

    -- handle conjunction with suffix
    ConjNPque : Conj -> ListNP -> NP ;

    -- Alternative form for female noun phrase
    everybodyFem_NP : NP ;
    somebodyFem_NP : NP ;
    nobodyFem_NP : NP ;
    
    
    -- Some empty prepositions to enforce cases
    Nom_Prep : Prep ;
    Gen_Prep : Prep ;
    Acc_Prep : Prep ;
    Dat_Prep : Prep ;
    Abl_Prep : Prep ;

    -- Preposition with alternate case
    inAbl_Prep : Prep ;
    onAbl_Prep : Prep ;
}
