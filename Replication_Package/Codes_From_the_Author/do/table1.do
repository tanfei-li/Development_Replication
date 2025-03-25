
use "$rep_files/table1dta", clear	
		
foreach v in 5 8 {

	preserve

	g total_tki_`v'g0 = total_tki_`v' 
	replace total_tki_`v'g0 = . if total_tki_`v'==0
	g emig_shr_`v'g0 = emig_shr_`v' 
	replace emig_shr_`v'g0 = . if emig_shr_`v'==0		
			
	keep pop_`v' total_tki_`v' emig_shr_`v' any_tki_`v' total_tki_`v'g0 emig_shr_`v'g0
	order pop_`v' total_tki_`v' emig_shr_`v' any_tki_`v' total_tki_`v'g0 emig_shr_`v'g0

	outreg2 using "$output/table1_200`v'.xls", replace excel sum(detail) eqkeep(mean p50 sd max)
	
	restore

}

preserve

g d_emig_shrg0 = d_emig_shr
replace d_emig_shrg0 = . if emig_shr_8==0 | emig_shr_5==0
g d_tkig0 = d_tki
replace d_tkig0 = . if any_tki_5==0 | any_tki_8==0

keep d_*
order d_tki d_emig_shr d_tkig0 d_emig_shrg0 d_ln_emig_shr

outreg2 using "$output/table1_Delta.xls", replace excel sum(detail) eqkeep(mean p50 sd max)

restore

collapse (sum) pop* total_tki*, by(urban)
set obs 4
foreach v of varlist _all {
	replace `v' = `v'[1]+`v'[2] in 3
	replace `v' = `v'[1] / `v'[3] in 4
}

g var5 = pop_5[4] in 1
g var8 = pop_8[4] in 1
replace var5 = total_tki_5[4] in 2
replace var8 = total_tki_8[4] in 2
replace var5 = total_tki_5[3] in 3
replace var8 = total_tki_8[3] in 3

keep in 1/3
keep var*

export excel using "$output/table1_national"
