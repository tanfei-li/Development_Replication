
use "$rep_files/figure1dta", clear

qui su mig02to05 if lnLAND>=. [aw=hhwt]
twoway (lpolyci mig02to05 lnLAND if lnLAND<2.53 [aw=hhwt],deg(1) bw(.4) lwidth(thick) xscale(range(-5.5)) yaxis(1) ytitle("share of households with international migrants") ///
	xtitle("log land-holdings (Ha) under household control") lcolor(gs0) ylabel(,angle(360) axis(1))  ///
	ciplot(rconnected) msymbol(i) alpattern(dash solid dash))  ///
	, xlabel(-5 -3 -1 1) ylabel(,angle(360)) text( `=r(mean)+0.001' -5.6 "landless", placement(ne)) ///
	legend(label(1 "95% confidence interval on local linear probability") ///
	 label(2 "Prob(household has any migrants | log land-holdings)") ///
	 size(small) row(2) ring(0) pos(6) color(gs0))
*EDIT BY HAND TO ADD ARROW
*graph export "$output/figure1.pdf", replace
