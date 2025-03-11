
use "$rep_files/table8figures4and5dta", clear

#delimit ;

global bHat = 1/1.913;

foreach t in 01 02 03 04 05 06 07 08 09 1 {;
	local tau = 0.`t';
	if ("`t'"=="1") local tau = 1;
	local tauScale = `tau';
						
	gen C_vj2_tau`t' = 
				(
				`tauScale' * wageAvg * 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				)
					/
				(`tauScale' * 1 + 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				);
			
	local tau = 1;		
	local tauScale = 0.`t';
	if ("`t'"=="1") local tauScale = 1;
	
	gen TAU1_C_vj2_tau`t' = 
				(
				`tauScale' * wageAvg * 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				)
					/
				(`tauScale' * 1 + 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				);	
	};					


forvalues t=10/99 {;

	local tau = 0.`t';
	if (`t'==1) local tau = 1;
	local tauScale = `tau';
						
	gen C_vj2_tau`t' = 
				(
				`tauScale' * wageAvg * 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				)
					/
				(`tauScale' * 1 + 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				);
			
	local tau = 1;		
	local tauScale = 0.`t';
	if (`t'==1) local tauScale = 1;
	
	gen TAU1_C_vj2_tau`t' = 
				(
				`tauScale' * wageAvg * 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				)
					/
				(`tauScale' * 1 + 
				(
				(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)*rain5L^(Rlambda/$bHat) - rain8L^(Rlambda/$bHat))
					/
				((alphaYbi*rainAvg*`tau')^(Rlambda/$bHat)*(exp(d_ln_emig_shr-(1/$bHat)*P44Dlambda)-1))
				)^($bHat/Rlambda)
				);				

};				

egen C_vj = rowmean(TAU*);
gen C_vj_tau100 = TAU1_C_vj2_tau1;

replace C_vj = C_vj_tau100;

#d cr

preserve
gen kabb = substr(string(kab_kode_i),3,2)
tostring prop kab,replace
replace kab = kabb
compress kab
collapse (mean) C_vj [pw=exp(ln_pop_5)],by(prop kab)
tempfile costKab
save `costKab'
restore

gen CshrExp = C_vj_tau100/annualExp


***FIGURE 4

format C_vj %9.0f

**a
#delimit ;
spmap C_vj if prop<80 using "$rep_files/2000_alldesa_shp", id(_ID) clmethod(quantile) clnumber(6)
	osize(none none none none none none none) 
	ocolor(none none none none none none none) 
	ndfcolor(white) ndocolor(gs15)
	ndlabel(".") ndsize(vthin)
	fcolor(Reds)
	legend(on symysize(6) symxsize(6) textwidth(12) colgap(2) forcesize bmargin(5) size(medium));
#d cr
*EDIT TO ADD ISLAND NAMES
graph save "$output/figure4a", replace
*graph export "$output/figure4a.pdf", replace
*graph export "$output/figure4a.png", replace

**b

preserve

use "$rep_files/district_dbf",clear
tostring KODE_KAB,replace
gen prop=substr(KODE_K,1,2)
replace prop = "21" if prop=="20"
replace prop = "94" if prop=="95"
gen kab = substr(KODE_K,3,2)
replace kab = "01" if prop=="21" & kab=="10"
replace kab = "02" if prop=="21" & kab=="11"
replace kab = "03" if prop=="21" & kab=="12"
replace kab = "04" if prop=="21" & kab=="13"
replace kab = "71" if prop=="21" & kab=="74"

ren id _ID

merge 1:1 prop kab using `costKab'
drop if _m==2
drop _m

replace prop = "76" if prop=="73" & inrange(real(kab),19,23)
replace kab = "01" if kab=="19" & prop=="76"
replace kab = "02" if kab=="20" & prop=="76"
replace kab = "03" if kab=="21" & prop=="76"
replace kab = "04" if kab=="22" & prop=="76"
replace kab = "05" if kab=="23" & prop=="76"
replace kab= "22" if kab=="24" & prop=="73"

destring prop kab, replace
merge 1:1 prop kab using "$rep_files/exp6KAB"
drop if _m==2
drop _m

gen CshrExp = C_vj/annualExp


format CshrE %9.2f

spmap CshrE if prop<80 using "$rep_files/district_shp", id(_ID) clmethod(quantile) clnumber(6) osize(vvthin vvthin vvthin vvthin vvthin vvthin) ///
		ocolor(gs10 gs10 gs10 gs10 gs10 gs10 gs10) fcolor(YlOrRd) ndocolor(gs10) ndsize(vthin) ///
		legend(symysize(6) symxsize(6) size(medium)) ndlabel(".") ndfcolor(white)

*EDIT TO ADD ISLAND NAMES
graph save "$output/figure4b",replace	
*graph export "$output/figure4b.pdf", replace
*graph export "$output/figure4b.png", replace

restore
