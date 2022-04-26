-- The only place where literal Thai strings are defined 
-- (except for Lexicon and Structural).
-- Convert this into StringsThai by 
-- gf
-- > rf -file=thai/src/StringsTha.gf | ps -env=quotes -to_thai | wf -file=thai/StringsTha.gf
-- สุคสันต์วันเกิด!

resource StringsTha = {

flags coding = utf8 ;

oper

aphai_s = "Op3ay" ; -- excuse2
baan_s = "bT2a:n" ; -- house
biar_s = "ebi:OrK" ; -- beer
ca_s = "ca." ; -- Modal
cet_s = "ecSd" ; -- seven
chan_s = "c1an" ; -- I
chay_s = "a%c2T1" ; -- yes
cheut_s = "ec2v:T2t" ; -- shirt2
chuay_s = "c2T1wy" ; -- help1
di_s = "di" ; -- I (fem)1
dii_s = "di:" ; -- hello2
duay_s = "dT2wy" ; -- help2
dvm_s = "dvm" ; -- drink
et_s = "eOSd" ; -- one'
haa_s = "hT2a:" ; -- five
hay_s = "a%hT2" ; -- give
hoog_s = "hT2Og" ; -- room
hok_s = "hk" ; -- six
jai_s = "a%j" ; -- understand2
kaaw_s = "eka:" ; -- nine
kew_s = "e'kT2w" ; -- glass (drink Classif)
khaw_s = "ek1a:" ; -- he
khon_s = "k2n" ; -- people Classif
khoo_s = "k1O" ; -- please
khoog_s = "k1Og" ; -- Possessive
khoop_s = "k1Ob" ; -- thank
khow_s = "ek1T2w" ; -- understand1
khun_s = "k2un'" ; -- you
koon_s = "kT1On" ; -- bye2
laa_s = "la:" ; -- bye1
lag_s = "hlag" ; -- houses Classif
lap_s = "hlab" ; -- sleep2 
lem_s = "elT1m" ; -- books Classif
mak_s = "ma:k" ; -- very
may_s = "a&mT1" ; -- not
m'ay_s = "a&hm" ; -- Question
mvvn_s = "hmv:T1n" ; -- ten thousand
nag_s = "hna.g" ; -- book1
nai_s = "a&hn" ; -- where2
nam_s = "na+" ; -- water
nan_s = "naT2n" ; -- that
nii_s = "ni:T2" ; -- this
nit_s = "nid" ; -- little1
noon_s = "nOn" ; -- sleep1
noi_s = "hnT1Oy" ; -- little2
nvg_s = "hnvg" ; -- one
pay_s = "a&p" ; -- go
peet_s = "e'pd" ; -- eight
pen_s = "epSn" ; -- be, can-know
phan_s = "p2an" ; -- thousand
phom_s = "p1m" ; -- I (masc)
puu_s = "p1u:T2" ; -- woman1
rai_s = "a&r" ; -- how-much2
rak_s = "rak" ; -- love
raw_s = "era:" ; -- we
rooy_s = "rT2Oy" ; -- hundred
saam_s = "sa:m" ; -- three
sawat_s = "swas" ; -- hello1
seen_s = "e'sn" ; -- hundred thousand
seua_s = "esv:T2O" ; -- shirt1
si_s = "s'i" ; -- Imperative
sii_s = "si:T1" ; -- four
sip_s = "sib" ; -- ten
soog_s = "sOg" ; -- two
svv_s = "sv:O" ; -- book2
thii_s = "t5i:T1" ; -- Ord
thoot_s = "o:t5r'" ; -- sorry2
thao_s = "et5T1a:" ; -- how-much1
tog_s = "tT2Og" ; -- must
waa_s = "wT1a:" ; -- that Conj
way_s = "a&hw" ; -- can-potent
yaa_s = "OyT1a:" ; -- Neg Imper
yaak_s = "Oya:k" ; -- want
yay_s = "a%hy'" ; -- big
yig_s = "hy'ig" ; -- woman2
yii_s = "yi:T1" ; -- two'
yin_s = "yin" ; -- you're-welcome1
yuu_s = "yu:" ; -- where0(?)

}
