
use "$rep_files/migchoicedta", clear
 
gen haCat = _n
gen mfxP = .
gen mfxR = .
gen se_mfxR = .
gen se_mfxP = .
forvalues j=1/4 {
	clogit mig c.(rainShock priceShock)##i.(landCat) i.year, cl(district) group(hhcode)
	margins if e(sample), dydx(rainShock priceShock) at(landCat==`j') noestimcheck predict(pu0) post
	replace mfxP = _b[priceShock] in `j'
	replace se_mfxP = _se[priceShock] in `j'
	replace mfxR = _b[rainShock] in `j'
	replace se_mfxR = _se[rainShock] in `j'
}

g uP = mfxP + 1.7 * se_mfxP
g lP = mfxP - 1.7 * se_mfxP
g uR = mfxR + 1.7 * se_mfxR
g lR = mfxR - 1.7 * se_mfxR

g haLab = "0-0.28" in 1
replace haLab = "0.3-0.64" in 2
replace haLab = "0.69-1.5" in 3
replace haLab = "1.6-5" in 4

encode haLab, g(haLab_)

twoway (rcap uR lR haLab_, lcolor(gs0) lwidth(medthick) msize(large)) (scatter mfxR haLab_, mcolor(gs0)), ///
	ylabel(,angle(360)) plotregion(color(white)) xtitle("landholding size") title("rainfall shock elasticity") ///
	xlabel(,valuelabel) yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph export "$output/figure3rain.pdf", replace
	
twoway (rcap uP lP haLab_, lcolor(gs0) lwidth(medthick) msize(large)) (scatter mfxP haLab_, mcolor(gs0)), ///
	ylabel(,angle(360)) plotregion(color(white)) xtitle("landholding size") title("price shock elasticity") ///
	xlabel(,valuelabel) yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph export "$output/figure3price.pdf", replace
