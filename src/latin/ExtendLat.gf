concrete ExtendLat of Extend = CatLat ** ExtendFunctor-[VPS,ComplDirectVQ,ComplDirectVS,CompIQuant,EmptyRelSlash,ExistsNP,ExistCN,ExistMassCN,ExistPluralCN,GenModNP,PredAPVP,PredIAdvVP,SlashBareV2S,StrandQuestSlash,StrandRelSlash] with (Grammar=GrammarLat) ** open MissingLat in {
  lincat
    VPS = Comp ;
  lin
    -- ComplDirectVS : VS -> Utt -> VP ;      -- say: "today"
    ComplDirectVS vs utt = AdvVP (UseV <lin V vs : V>) (lin Adv {s = \\_ => ":" ++ quoted utt.s}) ; -- DEFAULT complement added as Adv in quotes
    -- ComplDirectVQ : VQ -> Utt -> VP ;      -- ask: "when"
    ComplDirectVQ vq utt = AdvVP (UseV <lin V vq : V>) (lin Adv {s = \\_ => ":" ++ quoted utt.s}) ; -- DEFAULT complement added as Adv in quotes
} ;