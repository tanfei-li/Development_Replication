
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

***FIGURE 5

*dropping missing obs
drop if droppostmap==1

cap drop _C_vj
gen _C_vj = C_vj_tau100

cap drop *paretoShr* *constrained*
gen paretoShr5 = .
gen paretoShr8 = .
gen constrained5 = .
gen constrained8 = .
gen paretoShr5noCC = .
gen paretoShr8noCC = .
gen _paretoShr5 = .
gen _paretoShr8 = .
gen _constrained5 = .
gen _constrained8 = .
gen _paretoShr5noCC = .
gen _paretoShr8noCC = .

forvalues t=10/99 {

	replace _C_vj = C_vj2_tau`t'

	cap drop P_5 P_8
	gen P_5 = price5/.55/9500
	gen P_8 = price8/.55/9400

	cap drop R_L5 R_U5 
	gen R_L5 = (.`t'*_C_vj/(P_5*rain5L))^(1/$bHat) + 1
	gen R_U5 = ((wageAvg-C_vj)/(alphaYbi*P_5*rainAvg))^(1/$bHat) + 1

	cap drop R_L8 R_U8
	gen R_L8 = (.`t'*_C_vj/(P_8*rain8L))^(1/$bHat) + 1
	gen R_U8 = ((wageAvg-C_vj)/(alphaYbi*P_8*rainAvg))^(1/$bHat) + 1

	replace paretoShr5 = ( (1/R_L5)^Rlambda - (1/R_U5)^Rlambda )
	qui su paretoShr5
	replace _paretoShr5 = r(mean) in `t'
	replace paretoShr8 = ( (1/R_L8)^Rlambda - (1/R_U8)^Rlambda )
	qui su paretoShr8
	replace _paretoShr8 = r(mean) in `t'

	replace constrained5 = 1 - (1/R_L5)^Rlambda
	qui su constrained5
	replace _constrained5 = r(mean) in `t'
	replace constrained8 = 1 - (1/R_L8)^Rlambda
	qui su constrained8
	replace _constrained8 = r(mean) in `t'

	replace paretoShr5noCC = (1/R_U5)^Rlambda 
	qui su paretoShr5noCC
	qui replace _paretoShr5noCC = r(mean) in `t'
	replace paretoShr8noCC = (1/R_U8)^Rlambda 
	qui su paretoShr8noCC
	qui replace _paretoShr8noCC = r(mean) in `t'
	
}


forvalues t=1/9 {

	replace _C_vj = C_vj2_tau0`t'

	cap drop P_5 P_8
	gen P_5 = price5/.55/9500
	gen P_8 = price8/.55/9400

	cap drop R_L5 R_U5 
	gen R_L5 = (.0`t'*_C_vj/(P_5*rain5L))^(1/$bHat) + 1
	gen R_U5 = ((wageAvg-C_vj)/(alphaYbi*P_5*rainAvg))^(1/$bHat) + 1

	cap drop R_L8 R_U8
	gen R_L8 = (.0`t'*_C_vj/(P_8*rain8L))^(1/$bHat) + 1
	gen R_U8 = ((wageAvg-C_vj)/(alphaYbi*P_8*rainAvg))^(1/$bHat) + 1

	replace paretoShr5 = ( (1/R_L5)^Rlambda - (1/R_U5)^Rlambda )
	qui su paretoShr5
	replace _paretoShr5 = r(mean) in `t'
	replace paretoShr8 = ( (1/R_L8)^Rlambda - (1/R_U8)^Rlambda )
	qui su paretoShr8
	replace _paretoShr8 = r(mean) in `t'

	replace constrained5 = 1 - (1/R_L5)^Rlambda
	qui su constrained5
	replace _constrained5 = r(mean) in `t'
	replace constrained8 = 1 - (1/R_L8)^Rlambda
	qui su constrained8
	replace _constrained8 = r(mean) in `t'

	replace paretoShr5noCC = (1/R_U5)^Rlambda 
	qui su paretoShr5noCC
	qui replace _paretoShr5noCC = r(mean) in `t'
	replace paretoShr8noCC = (1/R_U8)^Rlambda 
	qui su paretoShr8noCC
	qui replace _paretoShr8noCC = r(mean) in `t'
	
}

cap gen tau = .
forvalues t=1/9 {
	replace tau = 0.0`t' if _n==`t'
}

forvalues t=10/99 {
	replace tau = 0.`t' if _n==`t'
}

twoway (lpoly _constrained5 tau,deg(1) bw(0.01) lcolor(gs8) lwidth(thick)) ///
		(lpoly _constrained8 tau,deg(1) bw(0.01) lcolor(gs0) lwidth(thick)), ///
		legend(label(1 "2005") label(2 "2008") ring(0) row(2) pos(6) size(medlarge)) ///
		xtitle("upfront cost share {&tau}{&isin}(0,1]",size(large)) ylabel(,angle(360)) ///
		title("fraction of liquidity constrained households",size(large)) ytick(0(0.2)1.0, grid)
graph export "$output/figure5.pdf",replace
		
