
pause on

use "$rep_files/migchoicedta", clear

// CLOGIT
gen mfxP1 = .
gen mfxR1 = .
gen se_mfxR1 = .
gen se_mfxP1 = .
local i = 1
forvalues j=0.05(0.05)2.45 {
	if `i'<=49 {
		clogit mig c.(rainShock priceShock)##c.(land) i.year,cl(district) group(hhcode)
		margins, dydx(rainShock priceShock) at(land=`j') noestimcheck predict(pu0) post
		replace mfxP1 = _b[priceShock] in `i'
		replace se_mfxP1 = _se[priceShock] in `i'
		replace mfxR1 = _b[rainShock] in `i'
		replace se_mfxR1 = _se[rainShock] in `i'
	}
	local i = `i' + 1
}
cap drop Ha
gen Ha = _n * .05

cap drop statCode* 
cap drop s1 s2
gen s1 = "replace PshrMLin = PshrMLin + " + string(mfxP1) 
cap gen Ha2 = Ha[_n+1]
gen s2 = "* ( ( 0.1/" + string(Ha) + ")^Rlambda - (0.1/" + string(Ha2) + ")^Rlambda )"
gen statCodeP_linear = s1 + s2
replace statCodeP_linear = "replace PshrMLin = PshrMLin + " + string(mfxP1[_n-1]) + "* (0.1/2.5)^Rlambda " in 50

replace s1 = "replace RshrMLin = RshrMLin + " + string(mfxR1) 
replace s2 = "* ( ( 0.1/" + string(Ha) + ")^Rlambda - (0.1/" + string(Ha2) + ")^Rlambda )"
gen statCodeR_linear = s1 + s2
drop s1 s2
replace statCodeR_linear = "replace RshrMLin = RshrMLin + " + string(mfxR1[_n-1]) + "* (0.1/2.5)^Rlambda " in 50

gen mfxP = .
gen mfxR = .
gen se_mfxR = .
gen se_mfxP = .
local i = 1
forvalues j=0.05(0.05)2.45 {
	if `i'<=49 {
		clogit mig c.(rainShock priceShock)##c.(land land2) i.year,cl(district) group(hhcode)
		margins, dydx(rainShock priceShock) at(land=`j' land2=`=`j'^2') noestimcheck predict(pu0) post
		replace mfxP = _b[priceShock] in `i'
		replace se_mfxP = _se[priceShock] in `i'
		replace mfxR = _b[rainShock] in `i'
		replace se_mfxR = _se[rainShock] in `i'
	}
	local i = `i' + 1
}
cap drop Ha
gen Ha = _n * .05

gen s1 = "replace PshrMQuad = PshrMQuad + " + string(mfxP) 
cap gen Ha2 = Ha[_n+1]
gen s2 = "* ( ( 0.1/" + string(Ha) + ")^Rlambda - (0.1/" + string(Ha2) + ")^Rlambda )"
gen statCodeP_quadratic = s1 + s2
replace statCodeP_quadratic = "replace PshrMQuad = PshrMQuad + " + string(mfxP1[_n-1]) + "* (0.1/2.5)^Rlambda " in 50

replace s1 = "replace RshrMQuad = RshrMQuad + " + string(mfxR) 
replace s2 = "* ( ( 0.1/" + string(Ha) + ")^Rlambda - (0.1/" + string(Ha2) + ")^Rlambda )"
gen statCodeR_quadratic = s1 + s2
replace statCodeR_quadratic = "replace RshrMQuad = RshrMQuad + " + string(mfxR1[_n-1]) + "* (0.1/2.5)^Rlambda " in 50

keep statCode*

keep in 1/50
drop in 1

pause: EXTRACT THE SCRIPT FOR GENERATING THE CODE BELOW, THEN TYPE end

use "$rep_files/mainvillageregs", clear
			
	*keep villages with area planted \lambda		
	global IFEXT if _samplePlant==1
	keep $IFEXT
	
	cap drop RshrMLin
	gen RshrMLin = 0
	replace RshrMLin = RshrMLin + .2331469* ( ( 0.1/.1)^Rlambda - (0.1/.15)^Rlambda )
	replace RshrMLin = RshrMLin + .2301083* ( ( 0.1/.15)^Rlambda - (0.1/.2)^Rlambda )
	replace RshrMLin = RshrMLin + .2270725* ( ( 0.1/.2)^Rlambda - (0.1/.25)^Rlambda )
	replace RshrMLin = RshrMLin + .2240395* ( ( 0.1/.25)^Rlambda - (0.1/.3)^Rlambda )
	replace RshrMLin = RshrMLin + .2210094* ( ( 0.1/.3)^Rlambda - (0.1/.35)^Rlambda )
	replace RshrMLin = RshrMLin + .2179823* ( ( 0.1/.35)^Rlambda - (0.1/.4)^Rlambda )
	replace RshrMLin = RshrMLin + .2149583* ( ( 0.1/.4)^Rlambda - (0.1/.45)^Rlambda )
	replace RshrMLin = RshrMLin + .2119373* ( ( 0.1/.45)^Rlambda - (0.1/.5)^Rlambda )
	replace RshrMLin = RshrMLin + .2089194* ( ( 0.1/.5)^Rlambda - (0.1/.55)^Rlambda )
	replace RshrMLin = RshrMLin + .2059047* ( ( 0.1/.55)^Rlambda - (0.1/.6)^Rlambda )
	replace RshrMLin = RshrMLin + .2028933* ( ( 0.1/.6)^Rlambda - (0.1/.65)^Rlambda )
	replace RshrMLin = RshrMLin + .1998852* ( ( 0.1/.65)^Rlambda - (0.1/.7)^Rlambda )
	replace RshrMLin = RshrMLin + .1968805* ( ( 0.1/.7)^Rlambda - (0.1/.75)^Rlambda )
	replace RshrMLin = RshrMLin + .1938791* ( ( 0.1/.75)^Rlambda - (0.1/.8)^Rlambda )
	replace RshrMLin = RshrMLin + .1908813* ( ( 0.1/.8)^Rlambda - (0.1/.85)^Rlambda )
	replace RshrMLin = RshrMLin + .187887* ( ( 0.1/.85)^Rlambda - (0.1/.9)^Rlambda )
	replace RshrMLin = RshrMLin + .1848963* ( ( 0.1/.9)^Rlambda - (0.1/.95)^Rlambda )
	replace RshrMLin = RshrMLin + .1819092* ( ( 0.1/.95)^Rlambda - (0.1/1)^Rlambda )
	replace RshrMLin = RshrMLin + .1789258* ( ( 0.1/1)^Rlambda - (0.1/1.05)^Rlambda )
	replace RshrMLin = RshrMLin + .1759461* ( ( 0.1/1.05)^Rlambda - (0.1/1.1)^Rlambda )
	replace RshrMLin = RshrMLin + .1729703* ( ( 0.1/1.1)^Rlambda - (0.1/1.15)^Rlambda )
	replace RshrMLin = RshrMLin + .1699983* ( ( 0.1/1.15)^Rlambda - (0.1/1.2)^Rlambda )
	replace RshrMLin = RshrMLin + .1670302* ( ( 0.1/1.2)^Rlambda - (0.1/1.25)^Rlambda )
	replace RshrMLin = RshrMLin + .164066* ( ( 0.1/1.25)^Rlambda - (0.1/1.3)^Rlambda )
	replace RshrMLin = RshrMLin + .1611058* ( ( 0.1/1.3)^Rlambda - (0.1/1.35)^Rlambda )
	replace RshrMLin = RshrMLin + .1581497* ( ( 0.1/1.35)^Rlambda - (0.1/1.4)^Rlambda )
	replace RshrMLin = RshrMLin + .1551977* ( ( 0.1/1.4)^Rlambda - (0.1/1.45)^Rlambda )
	replace RshrMLin = RshrMLin + .1522499* ( ( 0.1/1.45)^Rlambda - (0.1/1.5)^Rlambda )
	replace RshrMLin = RshrMLin + .1493062* ( ( 0.1/1.5)^Rlambda - (0.1/1.55)^Rlambda )
	replace RshrMLin = RshrMLin + .1463668* ( ( 0.1/1.55)^Rlambda - (0.1/1.6)^Rlambda )
	replace RshrMLin = RshrMLin + .1434316* ( ( 0.1/1.6)^Rlambda - (0.1/1.65)^Rlambda )
	replace RshrMLin = RshrMLin + .1405009* ( ( 0.1/1.65)^Rlambda - (0.1/1.7)^Rlambda )
	replace RshrMLin = RshrMLin + .1375744* ( ( 0.1/1.7)^Rlambda - (0.1/1.75)^Rlambda )
	replace RshrMLin = RshrMLin + .1346525* ( ( 0.1/1.75)^Rlambda - (0.1/1.8)^Rlambda )
	replace RshrMLin = RshrMLin + .1317349* ( ( 0.1/1.8)^Rlambda - (0.1/1.85)^Rlambda )
	replace RshrMLin = RshrMLin + .1288219* ( ( 0.1/1.85)^Rlambda - (0.1/1.9)^Rlambda )
	replace RshrMLin = RshrMLin + .1259135* ( ( 0.1/1.9)^Rlambda - (0.1/1.95)^Rlambda )
	replace RshrMLin = RshrMLin + .1230096* ( ( 0.1/1.95)^Rlambda - (0.1/2)^Rlambda )
	replace RshrMLin = RshrMLin + .1201104* ( ( 0.1/2)^Rlambda - (0.1/2.05)^Rlambda )
	replace RshrMLin = RshrMLin + .1172159* ( ( 0.1/2.05)^Rlambda - (0.1/2.1)^Rlambda )
	replace RshrMLin = RshrMLin + .1143261* ( ( 0.1/2.1)^Rlambda - (0.1/2.15)^Rlambda )
	replace RshrMLin = RshrMLin + .111441* ( ( 0.1/2.15)^Rlambda - (0.1/2.2)^Rlambda )
	replace RshrMLin = RshrMLin + .1085608* ( ( 0.1/2.2)^Rlambda - (0.1/2.25)^Rlambda )
	replace RshrMLin = RshrMLin + .1056854* ( ( 0.1/2.25)^Rlambda - (0.1/2.3)^Rlambda )
	replace RshrMLin = RshrMLin + .1028149* ( ( 0.1/2.3)^Rlambda - (0.1/2.35)^Rlambda )
	replace RshrMLin = RshrMLin + .0999493* ( ( 0.1/2.35)^Rlambda - (0.1/2.4)^Rlambda )
	replace RshrMLin = RshrMLin + .0970886* ( ( 0.1/2.4)^Rlambda - (0.1/2.45)^Rlambda )
	replace RshrMLin = RshrMLin + .094233* ( ( 0.1/2.45)^Rlambda - (0.1/2.5)^Rlambda )
	replace RshrMLin = RshrMLin + .094233 * (0.1/2.5)^Rlambda 

	gen RshrMQuad = 0
	replace RshrMQuad = RshrMQuad + .2883063* ( ( 0.1/.1)^Rlambda - (0.1/.15)^Rlambda )
	replace RshrMQuad = RshrMQuad + .2507674* ( ( 0.1/.15)^Rlambda - (0.1/.2)^Rlambda )
	replace RshrMQuad = RshrMQuad + .2150029* ( ( 0.1/.2)^Rlambda - (0.1/.25)^Rlambda )
	replace RshrMQuad = RshrMQuad + .1810449* ( ( 0.1/.25)^Rlambda - (0.1/.3)^Rlambda )
	replace RshrMQuad = RshrMQuad + .1489163* ( ( 0.1/.3)^Rlambda - (0.1/.35)^Rlambda )
	replace RshrMQuad = RshrMQuad + .1186309* ( ( 0.1/.35)^Rlambda - (0.1/.4)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0901949* ( ( 0.1/.4)^Rlambda - (0.1/.45)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0636077* ( ( 0.1/.45)^Rlambda - (0.1/.5)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0388626* ( ( 0.1/.5)^Rlambda - (0.1/.55)^Rlambda )
	replace RshrMQuad = RshrMQuad + .015948* ( ( 0.1/.55)^Rlambda - (0.1/.6)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0051515* ( ( 0.1/.6)^Rlambda - (0.1/.65)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0244545* ( ( 0.1/.65)^Rlambda - (0.1/.7)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0419818* ( ( 0.1/.7)^Rlambda - (0.1/.75)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0577554* ( ( 0.1/.75)^Rlambda - (0.1/.8)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0717983* ( ( 0.1/.8)^Rlambda - (0.1/.85)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0841334* ( ( 0.1/.85)^Rlambda - (0.1/.9)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0947835* ( ( 0.1/.9)^Rlambda - (0.1/.95)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1037705* ( ( 0.1/.95)^Rlambda - (0.1/1)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1111151* ( ( 0.1/1)^Rlambda - (0.1/1.05)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1168366* ( ( 0.1/1.05)^Rlambda - (0.1/1.1)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1209527* ( ( 0.1/1.1)^Rlambda - (0.1/1.15)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1234791* ( ( 0.1/1.15)^Rlambda - (0.1/1.2)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1244299* ( ( 0.1/1.2)^Rlambda - (0.1/1.25)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1238168* ( ( 0.1/1.25)^Rlambda - (0.1/1.3)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1216496* ( ( 0.1/1.3)^Rlambda - (0.1/1.35)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1179363* ( ( 0.1/1.35)^Rlambda - (0.1/1.4)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1126824* ( ( 0.1/1.4)^Rlambda - (0.1/1.45)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.1058921* ( ( 0.1/1.45)^Rlambda - (0.1/1.5)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0975673* ( ( 0.1/1.5)^Rlambda - (0.1/1.55)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0877089* ( ( 0.1/1.55)^Rlambda - (0.1/1.6)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.076316* ( ( 0.1/1.6)^Rlambda - (0.1/1.65)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0633868* ( ( 0.1/1.65)^Rlambda - (0.1/1.7)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0489189* ( ( 0.1/1.7)^Rlambda - (0.1/1.75)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.0329092* ( ( 0.1/1.75)^Rlambda - (0.1/1.8)^Rlambda )
	replace RshrMQuad = RshrMQuad + -.015355* ( ( 0.1/1.8)^Rlambda - (0.1/1.85)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0037459* ( ( 0.1/1.85)^Rlambda - (0.1/1.9)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0243948* ( ( 0.1/1.9)^Rlambda - (0.1/1.95)^Rlambda )
	replace RshrMQuad = RshrMQuad + .046591* ( ( 0.1/1.95)^Rlambda - (0.1/2)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0703315* ( ( 0.1/2)^Rlambda - (0.1/2.05)^Rlambda )
	replace RshrMQuad = RshrMQuad + .0956103* ( ( 0.1/2.05)^Rlambda - (0.1/2.1)^Rlambda )
	replace RshrMQuad = RshrMQuad + .1224175* ( ( 0.1/2.1)^Rlambda - (0.1/2.15)^Rlambda )
	replace RshrMQuad = RshrMQuad + .1507388* ( ( 0.1/2.15)^Rlambda - (0.1/2.2)^Rlambda )
	replace RshrMQuad = RshrMQuad + .1805552* ( ( 0.1/2.2)^Rlambda - (0.1/2.25)^Rlambda )
	replace RshrMQuad = RshrMQuad + .2118413* ( ( 0.1/2.25)^Rlambda - (0.1/2.3)^Rlambda )
	replace RshrMQuad = RshrMQuad + .2445661* ( ( 0.1/2.3)^Rlambda - (0.1/2.35)^Rlambda )
	replace RshrMQuad = RshrMQuad + .278692* ( ( 0.1/2.35)^Rlambda - (0.1/2.4)^Rlambda )
	replace RshrMQuad = RshrMQuad + .3141749* ( ( 0.1/2.4)^Rlambda - (0.1/2.45)^Rlambda )
	replace RshrMQuad = RshrMQuad + .3509644* ( ( 0.1/2.45)^Rlambda - (0.1/2.5)^Rlambda )
	replace RshrMQuad = RshrMQuad + .3509644 * (0.1/2.5)^Rlambda 

	cap drop PshrMLin
	gen PshrMLin = 0	
	replace PshrMLin = PshrMLin + .7958234* ( ( 0.1/.1)^Rlambda - (0.1/.15)^Rlambda )
	replace PshrMLin = PshrMLin + .7889974* ( ( 0.1/.15)^Rlambda - (0.1/.2)^Rlambda )
	replace PshrMLin = PshrMLin + .7821758* ( ( 0.1/.2)^Rlambda - (0.1/.25)^Rlambda )
	replace PshrMLin = PshrMLin + .7753587* ( ( 0.1/.25)^Rlambda - (0.1/.3)^Rlambda )
	replace PshrMLin = PshrMLin + .7685463* ( ( 0.1/.3)^Rlambda - (0.1/.35)^Rlambda )
	replace PshrMLin = PshrMLin + .7617387* ( ( 0.1/.35)^Rlambda - (0.1/.4)^Rlambda )
	replace PshrMLin = PshrMLin + .7549361* ( ( 0.1/.4)^Rlambda - (0.1/.45)^Rlambda )
	replace PshrMLin = PshrMLin + .7481385* ( ( 0.1/.45)^Rlambda - (0.1/.5)^Rlambda )
	replace PshrMLin = PshrMLin + .7413462* ( ( 0.1/.5)^Rlambda - (0.1/.55)^Rlambda )
	replace PshrMLin = PshrMLin + .7345594* ( ( 0.1/.55)^Rlambda - (0.1/.6)^Rlambda )
	replace PshrMLin = PshrMLin + .727778* ( ( 0.1/.6)^Rlambda - (0.1/.65)^Rlambda )
	replace PshrMLin = PshrMLin + .7210022* ( ( 0.1/.65)^Rlambda - (0.1/.7)^Rlambda )
	replace PshrMLin = PshrMLin + .7142322* ( ( 0.1/.7)^Rlambda - (0.1/.75)^Rlambda )
	replace PshrMLin = PshrMLin + .7074682* ( ( 0.1/.75)^Rlambda - (0.1/.8)^Rlambda )
	replace PshrMLin = PshrMLin + .7007102* ( ( 0.1/.8)^Rlambda - (0.1/.85)^Rlambda )
	replace PshrMLin = PshrMLin + .6939583* ( ( 0.1/.85)^Rlambda - (0.1/.9)^Rlambda )
	replace PshrMLin = PshrMLin + .6872129* ( ( 0.1/.9)^Rlambda - (0.1/.95)^Rlambda )
	replace PshrMLin = PshrMLin + .6804738* ( ( 0.1/.95)^Rlambda - (0.1/1)^Rlambda )
	replace PshrMLin = PshrMLin + .6737414* ( ( 0.1/1)^Rlambda - (0.1/1.05)^Rlambda )
	replace PshrMLin = PshrMLin + .6670156* ( ( 0.1/1.05)^Rlambda - (0.1/1.1)^Rlambda )
	replace PshrMLin = PshrMLin + .6602967* ( ( 0.1/1.1)^Rlambda - (0.1/1.15)^Rlambda )
	replace PshrMLin = PshrMLin + .6535848* ( ( 0.1/1.15)^Rlambda - (0.1/1.2)^Rlambda )
	replace PshrMLin = PshrMLin + .64688* ( ( 0.1/1.2)^Rlambda - (0.1/1.25)^Rlambda )
	replace PshrMLin = PshrMLin + .6401823* ( ( 0.1/1.25)^Rlambda - (0.1/1.3)^Rlambda )
	replace PshrMLin = PshrMLin + .6334921* ( ( 0.1/1.3)^Rlambda - (0.1/1.35)^Rlambda )
	replace PshrMLin = PshrMLin + .6268092* ( ( 0.1/1.35)^Rlambda - (0.1/1.4)^Rlambda )
	replace PshrMLin = PshrMLin + .620134* ( ( 0.1/1.4)^Rlambda - (0.1/1.45)^Rlambda )
	replace PshrMLin = PshrMLin + .6134665* ( ( 0.1/1.45)^Rlambda - (0.1/1.5)^Rlambda )
	replace PshrMLin = PshrMLin + .6068068* ( ( 0.1/1.5)^Rlambda - (0.1/1.55)^Rlambda )
	replace PshrMLin = PshrMLin + .6001551* ( ( 0.1/1.55)^Rlambda - (0.1/1.6)^Rlambda )
	replace PshrMLin = PshrMLin + .5935113* ( ( 0.1/1.6)^Rlambda - (0.1/1.65)^Rlambda )
	replace PshrMLin = PshrMLin + .5868759* ( ( 0.1/1.65)^Rlambda - (0.1/1.7)^Rlambda )
	replace PshrMLin = PshrMLin + .5802487* ( ( 0.1/1.7)^Rlambda - (0.1/1.75)^Rlambda )
	replace PshrMLin = PshrMLin + .5736298* ( ( 0.1/1.75)^Rlambda - (0.1/1.8)^Rlambda )
	replace PshrMLin = PshrMLin + .5670195* ( ( 0.1/1.8)^Rlambda - (0.1/1.85)^Rlambda )
	replace PshrMLin = PshrMLin + .5604178* ( ( 0.1/1.85)^Rlambda - (0.1/1.9)^Rlambda )
	replace PshrMLin = PshrMLin + .553825* ( ( 0.1/1.9)^Rlambda - (0.1/1.95)^Rlambda )
	replace PshrMLin = PshrMLin + .5472409* ( ( 0.1/1.95)^Rlambda - (0.1/2)^Rlambda )
	replace PshrMLin = PshrMLin + .5406657* ( ( 0.1/2)^Rlambda - (0.1/2.05)^Rlambda )
	replace PshrMLin = PshrMLin + .5340996* ( ( 0.1/2.05)^Rlambda - (0.1/2.1)^Rlambda )
	replace PshrMLin = PshrMLin + .5275428* ( ( 0.1/2.1)^Rlambda - (0.1/2.15)^Rlambda )
	replace PshrMLin = PshrMLin + .5209951* ( ( 0.1/2.15)^Rlambda - (0.1/2.2)^Rlambda )
	replace PshrMLin = PshrMLin + .5144568* ( ( 0.1/2.2)^Rlambda - (0.1/2.25)^Rlambda )
	replace PshrMLin = PshrMLin + .507928* ( ( 0.1/2.25)^Rlambda - (0.1/2.3)^Rlambda )
	replace PshrMLin = PshrMLin + .5014088* ( ( 0.1/2.3)^Rlambda - (0.1/2.35)^Rlambda )
	replace PshrMLin = PshrMLin + .4948992* ( ( 0.1/2.35)^Rlambda - (0.1/2.4)^Rlambda )
	replace PshrMLin = PshrMLin + .4883994* ( ( 0.1/2.4)^Rlambda - (0.1/2.45)^Rlambda )
	replace PshrMLin = PshrMLin + .4819095* ( ( 0.1/2.45)^Rlambda - (0.1/2.5)^Rlambda )	
	replace PshrMLin = PshrMLin + .4819095 * (0.1/2.5)^Rlambda 
		
	cap drop PshrMQuad
	gen PshrMQuad = 0
	replace PshrMQuad = PshrMQuad + .7700235* ( ( 0.1/.1)^Rlambda - (0.1/.15)^Rlambda )
	replace PshrMQuad = PshrMQuad + .7370471* ( ( 0.1/.15)^Rlambda - (0.1/.2)^Rlambda )
	replace PshrMQuad = PshrMQuad + .7048191* ( ( 0.1/.2)^Rlambda - (0.1/.25)^Rlambda )
	replace PshrMQuad = PshrMQuad + .6733959* ( ( 0.1/.25)^Rlambda - (0.1/.3)^Rlambda )
	replace PshrMQuad = PshrMQuad + .6428266* ( ( 0.1/.3)^Rlambda - (0.1/.35)^Rlambda )
	replace PshrMQuad = PshrMQuad + .6131531* ( ( 0.1/.35)^Rlambda - (0.1/.4)^Rlambda )
	replace PshrMQuad = PshrMQuad + .5844102* ( ( 0.1/.4)^Rlambda - (0.1/.45)^Rlambda )
	replace PshrMQuad = PshrMQuad + .556626* ( ( 0.1/.45)^Rlambda - (0.1/.5)^Rlambda )
	replace PshrMQuad = PshrMQuad + .5298219* ( ( 0.1/.5)^Rlambda - (0.1/.55)^Rlambda )
	replace PshrMQuad = PshrMQuad + .5040138* ( ( 0.1/.55)^Rlambda - (0.1/.6)^Rlambda )
	replace PshrMQuad = PshrMQuad + .4792119* ( ( 0.1/.6)^Rlambda - (0.1/.65)^Rlambda )
	replace PshrMQuad = PshrMQuad + .455422* ( ( 0.1/.65)^Rlambda - (0.1/.7)^Rlambda )
	replace PshrMQuad = PshrMQuad + .4326455* ( ( 0.1/.7)^Rlambda - (0.1/.75)^Rlambda )
	replace PshrMQuad = PshrMQuad + .4108807* ( ( 0.1/.75)^Rlambda - (0.1/.8)^Rlambda )
	replace PshrMQuad = PshrMQuad + .3901228* ( ( 0.1/.8)^Rlambda - (0.1/.85)^Rlambda )
	replace PshrMQuad = PshrMQuad + .3703646* ( ( 0.1/.85)^Rlambda - (0.1/.9)^Rlambda )
	replace PshrMQuad = PshrMQuad + .3515972* ( ( 0.1/.9)^Rlambda - (0.1/.95)^Rlambda )
	replace PshrMQuad = PshrMQuad + .3338099* ( ( 0.1/.95)^Rlambda - (0.1/1)^Rlambda )
	replace PshrMQuad = PshrMQuad + .3169915* ( ( 0.1/1)^Rlambda - (0.1/1.05)^Rlambda )
	replace PshrMQuad = PshrMQuad + .3011297* ( ( 0.1/1.05)^Rlambda - (0.1/1.1)^Rlambda )
	replace PshrMQuad = PshrMQuad + .286212* ( ( 0.1/1.1)^Rlambda - (0.1/1.15)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2722258* ( ( 0.1/1.15)^Rlambda - (0.1/1.2)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2591584* ( ( 0.1/1.2)^Rlambda - (0.1/1.25)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2469976* ( ( 0.1/1.25)^Rlambda - (0.1/1.3)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2357316* ( ( 0.1/1.3)^Rlambda - (0.1/1.35)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2253488* ( ( 0.1/1.35)^Rlambda - (0.1/1.4)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2158387* ( ( 0.1/1.4)^Rlambda - (0.1/1.45)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2071911* ( ( 0.1/1.45)^Rlambda - (0.1/1.5)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1993966* ( ( 0.1/1.5)^Rlambda - (0.1/1.55)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1924467* ( ( 0.1/1.55)^Rlambda - (0.1/1.6)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1863331* ( ( 0.1/1.6)^Rlambda - (0.1/1.65)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1810488* ( ( 0.1/1.65)^Rlambda - (0.1/1.7)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1765869* ( ( 0.1/1.7)^Rlambda - (0.1/1.75)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1729411* ( ( 0.1/1.75)^Rlambda - (0.1/1.8)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1701058* ( ( 0.1/1.8)^Rlambda - (0.1/1.85)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1680751* ( ( 0.1/1.85)^Rlambda - (0.1/1.9)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1668439* ( ( 0.1/1.9)^Rlambda - (0.1/1.95)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1664064* ( ( 0.1/1.95)^Rlambda - (0.1/2)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1667572* ( ( 0.1/2)^Rlambda - (0.1/2.05)^Rlambda )
	replace PshrMQuad = PshrMQuad + .16789* ( ( 0.1/2.05)^Rlambda - (0.1/2.1)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1697981* ( ( 0.1/2.1)^Rlambda - (0.1/2.15)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1724736* ( ( 0.1/2.15)^Rlambda - (0.1/2.2)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1759082* ( ( 0.1/2.2)^Rlambda - (0.1/2.25)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1800917* ( ( 0.1/2.25)^Rlambda - (0.1/2.3)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1850128* ( ( 0.1/2.3)^Rlambda - (0.1/2.35)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1906587* ( ( 0.1/2.35)^Rlambda - (0.1/2.4)^Rlambda )
	replace PshrMQuad = PshrMQuad + .1970146* ( ( 0.1/2.4)^Rlambda - (0.1/2.45)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2040646* ( ( 0.1/2.45)^Rlambda - (0.1/2.5)^Rlambda )
	replace PshrMQuad = PshrMQuad + .2040646 * (0.1/2.5)^Rlambda 
 
	global rainS5L rain_cumdev345 rain5lambda
	global rainS8L rain_cumdev678 rain8lambda
	global rainDL rain_cumdev_diff rainDlambda	
	
	global p5rL P_rice_y2m1_y5m3 P445lambda
	global p8rL P_rice_y5m4_y8m3 P448lambda
	global pDrL delta_RP_diff P44Dlambda		
	global X $R $C $C2 $S $pDrL $rainDL
	
	cap gen RlambdaSQ = Rlambda^2
	
DNV $X
	_pctile Rlambda if e(sample)==1,n(100)
	global Nj 
	forvalues j=1/99 {
		global Nj $Nj `=r(r`j')'
	}
	
	global p5rL c.P_rice_y2m1_y5m3##c.Rlambda
	global p8rL c.P_rice_y5m4_y8m3##c.Rlambda  
	global pDrL c.delta_RP_diff##c.Rlambda   
	global rainS5L c.rain_cumdev345##c.Rlambda
	global rainS8L c.rain_cumdev678##c.Rlambda
	global rainDL c.rain_cumdev_diff##c.Rlambda

	global X5 $R $p5rL $rainS5L $C $C2 $S 
	global X8 $R $p8rL $rainS8L $C $C2 $S 
	global X $R $C $C2 $S $pDrL $rainDL	
	
DNV $X	
	margins, dydx(rain_cumdev_diff) at(Rlambda=($Nj)) post
	mat b = e(b)
	mat B = b'
	cap drop B1
	svmat B

DNV $X	
	_pctile Rlambda if e(sample)==1,n(100)
	cap drop vRshrM
	gen vRshrM = 0
	forvalues j=1/98 {
		replace vRshrM = (B1[`j']+B1[`=`j'+1'])/2 if inrange(Rlambda,r(r`j'),r(r`=`j'+1'))
	}
	replace vRshrM = B1[1] if Rlambda<r(r1)
	replace vRshrM = B1[99] if Rlambda>r(r99)

DNV $X	
	margins, dydx(delta_RP_diff) at(Rlambda=($Nj)) post
	mat b = e(b)
	mat B = b'
	cap drop B1
	svmat B
	
	_pctile Rlambda if e(sample)==1,n(100)
	cap drop vPshrM
	gen vPshrM = 0
	forvalues j=1/98 {
		replace vPshrM = (B1[`j']+B1[`=`j'+1'])/2 if inrange(Rlambda,r(r`j'),r(r`=`j'+1'))
	}
	replace vPshrM = B1[1] if Rlambda<r(r1)
	replace vPshrM = B1[99] if Rlambda>r(r99)
	
*****TABLE 7
cap log c
log using "$output/table7.txt", replace	text
		
	tabstat PshrM* vPshrM if d_ln_emig_shr<., stat(mean sd p25 p50 p75) col(stat)	
	corr PshrM* vPshrM if d_ln_emig_shr<., 
	tabstat RshrM* vRshrM if d_ln_emig_shr<., stat(mean sd p25 p50 p75) col(stat)	
	corr RshrM* vRshrM if d_ln_emig_shr<.

log c
	
	
	
	
