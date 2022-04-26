compile = runghc +RTS -K200M -RTS Compile 

all: src compdemo linkdemo

src:
	runghc MkMini.hs

compdemo:
	$(compile) Afr Ara Bul Cat Chi Dan Dut Eng Est Eus Fin Fre Ger Gre Hin Ice Ita Jpn Lav Mlt Mon Nep Nor Nno Pes Pnb Pol Por Ron Rus Snd Spa Swe Tha Urd

linkdemo:
	$(compile) -link Afr Ara Bul Cat Chi Dan Dut Eng Est Eus Fin Fre Ger Gre Hin Ice Ita Jpn Lav Mlt Mon Nep Nor Nno Pes Pnb Pol Por Ron Rus Snd Spa Swe Tha Urd
