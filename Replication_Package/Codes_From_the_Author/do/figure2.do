
use "$rep_files/figure2dta", clear

xtline riceprice_index,overlay legend(off) xtitle("") ytitle("Rice Price Index (2002m1=100) | (2004m1=100)") ///
	ylabel(,angle(360)) xline(527,lpattern(dash) lcolor(red) lwidth(medthick)) xlabel(504 516 528 540 552 564 576) scheme(s1color) ///
	text(180 528 "Import Ban", placement(ne) size(large))
graph export "$output/figure2.pdf",replace
