
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

***TABLE 8
	preserve
	gen nn = 1
	collapse (mean) avgC = CshrExp (sd) sdC = CshrExp (min) minC = CshrExp (p50) p50C = CshrExp (max) maxC = CshrExp, by(nn)
	drop nn
	tempfile a
	save `a'
	restore

	preserve
	gen nn = 1
	collapse (mean) avgC = C_vj (sd) sdC = C_vj (min) minC = C_vj (p50) p50C = C_vj (max) maxC = C_vj, by(nn)
	drop nn
	append using `a'
	export excel using "$output/table8", replace firstrow(variable)
	restore
