
clear all 
set more off 
cap log c

/*-----------------------------------------------*/
/* Set up working directory for all the do files */
/*-----------------------------------------------*/

global userdir "C:\Users\121685\Desktop\Development_Replication"
global rep_files "$userdir/AEJApplied_20150548_replication/dta"
global rep_code "$userdir/AEJApplied_20150548_replication/do"
global output "$userdir/AEJApplied_20150548_replication/tabfig"

adopath + "$userdir/AER_20150548_replication/ado"

/*----------------------------------------------*/
/* Preliminaries for (two step) estimation      */
/*----------------------------------------------*/

*estimation programs

cap program drop POIRIER
program define POIRIER
	syntax [varlist(fv)] [, EForm(string) Level(real 95) ]
	cap drop kappa*
	cap drop xb*
		
	noi cap  biprobit (Mt8 = $X8 $Z8 $FE) (Mt5 = $X5 $Z5 $FE) $IFEXT 

	predict xb1 if e(sample), xb1 
	predict xb2 if e(sample), xb2
	gen kappa8 = (normalden(xb1)*normal((xb2-(e(rho))*xb1)/((1-(e(rho))^2)^(1/2))))/binormal(xb1,xb2,(e(rho))) 
	gen kappa5 = (normalden(xb2)*normal((xb1-(e(rho))*xb2)/((1-(e(rho))^2)^(1/2))))/binormal(xb1,xb2,(e(rho)))
	global kap kappa8 kappa5 
	
	qui reg $M $X $kap $FE _J* $IF, cluster($CLU)
	
end


***************************************************************************
************************************DNV************************************
***************************************************************************
cap program drop DNV
program define DNV
	syntax [varlist(fv)] [, EForm(string) Level(real 95) ]
	cap drop kappa*
	cap drop samp*
	
	noi cap sureg (Mt8 $X8 $Z8 $FE) (Mt5 $X5 $Z5 $FE) $IFEXT
	predict kappa8 if e(sample), equation(Mt8)
	predict kappa5 if e(sample), equation(Mt5)
	
	gen kappa5X8 = (kappa5)*(kappa8)
	global kap kappa8 kappa5 kappa5X8
	forvalues j=2/$kpowD {
		gen kappa8_`j' = (kappa8)^`j'
		gen kappa5_`j' = (kappa5)^`j'
		gen kappa5`j'X8 = ((kappa5)^`j')*(kappa8)
		gen kappa5X8`j' = (kappa5)*((kappa8)^`j')
		gen kappa5X8_`j' = ((kappa5)^`j')*((kappa8)^`j')
		global kap $kap kappa8_`j' kappa5_`j' kappa5`j'X8 kappa5X8`j' kappa5X8_`j'
	}
	
	qui reg $M $X $kap $FE _J* $IF, cluster($CLU) 

end


***************************************************************************
********************************bootstrap-tse******************************
***************************************************************************
cap program drop BOOTtse
program define BOOTtse
	qui {
		preserve
			clear
			gen iter = .
			save "$output/BOOT_DATA_$BFILE",replace
		if $N==2 {
			drop iter
			gen iter = ""
			save "$TABOUT", replace 
		}
		restore
	}

	forvalues n=1/$REPS {
		if (`n'==1) dis "-->bootstrap iteration: `n'"
		if (`n'!=1) dis "..`n'"
		qui {
			preserve
			keep if _samplePlant==1
			keep $Z5 $Z8 $X5 $X8 $X $XD $priceD $price5 $price8 d_ln_emig_shr Mt5 Mt8 prop *district* _J* _sample*
			bsample, cluster($CLU)
			$EST $X
			gen iter = `n'
			foreach b in $BETA {
				gen beta_`b' = _b[`b']
				gen se_`b' = _se[`b']
			}
			keep beta* se* iter 
			keep in 1
			append using "$output/BOOT_DATA_$BFILE"
			save "$output/BOOT_DATA_$BFILE",replace
			restore
		}
	}

	preserve
	// get original Wald Stat
	dis "-->estimation on original sample"
	qui $EST $X 
	
	// calculate percentile t confidence intervals
	qui use "$output/BOOT_DATA_$BFILE",clear
	foreach v in $BETA {
		qui {
			*original beta
			gen BETA_`v' = _b[`v']
			*original SE
			gen oSE_`v' = _se[`v']
			*original Wald stat
			local W_`v' = _b[`v']/_se[`v']
			gen Wb_`v' = (beta_`v' - _b[`v'])/se_`v'
			
			*boot t crit val
			gen sig_`v' = ""
			_pctile Wb_`v', p(5,95)
			if !inrange(`W_`v'',r(r1),r(r2)) {
				replace sig_`v' = "\$^{*}\$"
			}
			_pctile Wb_`v', p(2.5,97.5)
			if !inrange(`W_`v'',r(r1),r(r2)) {
				replace sig_`v' = "\$^{**}\$"
			}
			_pctile Wb_`v', p(0.5,99.5)
			if !inrange(`W_`v'',r(r1),r(r2)) {
				replace sig_`v' = "\$^{***}\$"
			}					
		}
	}

	// get columns in order for table
	qui {
		format BETA* *SE* %9.3f
		tostring BETA* *SE*,replace usedisplayformat force
		foreach v of newlist $BETA {
			*boot-t: original SE
			gen SE_`v' = "(" + oSE_`v' + ")" + sig_`v'
		}
		keep BETA* SE* iter
		keep if iter==1
		reshape long BETA SE, i(iter) j(covariate) string
		expand 2
		bys covariate: replace BETA = SE if _n==2
		bys covariate: gen row = _n
		keep covariate BETA row 
		ren BETA column$N
		if $N==2 {
			merge using "$TABOUT"
		}
		else{
			merge 1:1 covariate row using "$TABOUT"
		}
		drop _m 
		cap drop iter
		save "$TABOUT", replace
		restore
	}
	
end


***************************************************************************
************************************MKTAB**********************************
***************************************************************************
cap program drop MKTAB
program define MKTAB	
	preserve
	use "$TABOUT", clear
	drop row
	order column*, sequential
	order covariate
	export excel using "$TABOUT", firstrow(variables) replace
	restore	
end
	




******************************************************************

	
*outreg
	global outE excel
	global sym "symbol($^{***}$, $^{**}$, $^{*}$)"	
	
*output bootstrap results
	cap mkdir "$output/logs"
	global LOG "$output"

*DNV specs
	global kpowD 3 			/* polynomial order  on selection terms */
	global EST1 SULPM		/* first stage estimator: SU-LPM */
	
*bootstrap specification
	global REPS 1000
	set seed 10101	

*two-step vars
	global R Rlambda shareHH_aboveRminRany paddy_farmland_shr_5 
	global C ln_dist_kec ln_ddist_kabcap ln_ddist_emigctr nonlandmotor_travel urban
	global desaC ln_dist_emigctr_desa nonlandmotor_travel urban
	global S postprim 
	global C2 arabShr chineseShr muslim_shr
	
	global rainS5 rain_cumdev345
	global rainS8 rain_cumdev678
	global rainD rain_cumdev_diff
	global rainS5L rain_cumdev345 rain5lambda
	global rainS8L rain_cumdev678 rain8lambda
	global rainDL rain_cumdev_diff rainDlambda
	global rainS5F rain_cumdev345 rain5lambda shareHHaboveXrain5 
	global rainS8F rain_cumdev678 rain8lambda shareHHaboveXrain8
	global rainDF rain_cumdev_diff rainDlambda shareHHaboveXrainD
	
	global p5r P_rice_y2m1_y5m3
	global p8r P_rice_y5m4_y8m3 
	global pDr delta_RP_diff
	global p5rL P_rice_y2m1_y5m3 P445lambda
	global p8rL P_rice_y5m4_y8m3 P448lambda
	global pDrL delta_RP_diff P44Dlambda
	global p5rF P_rice_y2m1_y5m3 P445lambda shareHHaboveXpr5
	global p8rF P_rice_y5m4_y8m3 P448lambda shareHHaboveXpr8
	global pDrF delta_RP_diff P44Dlambda shareHHaboveXprD		
	
*dep var
	global M d_ln_emig_shr	

*instruments
	global Z5 lnVinSD lnAinSD lnN5inSD lnpop5 
	global Z8 lnVinSD lnAinSD lnN8inSD lnpop8 		
	
	global X5 $R $p5r $rainS5 $C $C2 $S 
	global X8 $R $p8r $rainS8 $C $C2 $S 
	global X $R $C $C2 $S $pDr $rainD 	

*clustering and FE
	global j district
	global FE i.prop 
	global CLU district		
	
/*-----------------------------------------------*/
/* Figure do files                               */
/*-----------------------------------------------*/

do "$rep_code/figure1.do"		//output: figure1.pdf -- need to edit to add arrow// 
do "$rep_code/figure2.do" 		//output: figure2.pdf//  
do "$rep_code/figure3.do" 		//output: figure3rain.pdf, figure3price.pdf//
do "$rep_code/figure4.do" 		//output: figure4a.pdf, figure4b.pdf -- need to edit to add island names// 
do "$rep_code/figure5.do" 		//output: figure5.pdf//

/*-----------------------------------------------*/
/* Table do files                                */
/*-----------------------------------------------*/

do "$rep_code/table1.do" 		//output: table1_2005.xls, table1_2008.xls, table1_Delta.xls, table1_national.xlsx//
do "$rep_code/table2.do" 		//output: table2.tex//
do "$rep_code/table3.do"  		//output: table3reducedform.xls -- need to edit to stack SEs// 
do "$rep_code/table4.do"  		//output: table4extensive.xls// 
do "$rep_code/table5.do"  		//output: col 1: table5intensive.xml/.txt, cols 2-9: table5intensive.xls// 
do "$rep_code/table6.do"  		//output: table6oppcost.xls// 
do "$rep_code/table7.do"  		//output: table7.txt// 
do "$rep_code/table8.do"  		//output: table8.xls// 
	



clear all 
set more off 
cap log c

/*-----------------------------------------------*/
/* Set up working directory for all the do files */
/*-----------------------------------------------*/

global userdir "C:\Users\121685\Desktop\Development_Replication"
global rep_files "$userdir/AEJApplied_20150548_replication/dta"
global rep_code "$userdir/AEJApplied_20150548_replication/do"
global output "$userdir/AEJApplied_20150548_replication/tabfig"

adopath + "$userdir/AER_20150548_replication/ado"

/*----------------------------------------------*/
/* Preliminaries for (two step) estimation      */
/*----------------------------------------------*/

*estimation programs

cap program drop POIRIER
program define POIRIER
	syntax [varlist(fv)] [, EForm(string) Level(real 95) ] //define the expected inputs. [varlist(fv)] means the program can take an optional list of variables, including factor variables (fv). EForm(string): Allows an optional string input for exponential form (e.g., odds ratios). Level(real 95): Specifies the confidence level (default 95%)
	cap drop kappa*
	cap drop xb*
		
	noi cap  biprobit (Mt8 = $X8 $Z8 $FE) (Mt5 = $X5 $Z5 $FE) $IFEXT ////biprobit (Mt8 = ...) (Mt5 = ...) estimates a bivariate probit model, meaning two binary dependent variables (Mt8 and Mt5) are modeled jointly.
////noi (short for noisily) ensures any error message is displayed.
///cap (capture) prevents Stata from stopping execution if an error occurs.
	predict xb1 if e(sample), xb1 //generate linear predictions 
	predict xb2 if e(sample), xb2
	gen kappa8 = (normalden(xb1)*normal((xb2-(e(rho))*xb1)/((1-(e(rho))^2)^(1/2))))/binormal(xb1,xb2,(e(rho))) 
	gen kappa5 = (normalden(xb2)*normal((xb1-(e(rho))*xb2)/((1-(e(rho))^2)^(1/2))))/binormal(xb1,xb2,(e(rho)))
	global kap kappa8 kappa5 //calculating the inverse mill ratios 
	
	qui reg $M $X $kap $FE _J* $IF, cluster($CLU)
	
end


***************************************************************************
************************************DNV************************************
***************************************************************************
cap program drop DNV
program define DNV
	syntax [varlist(fv)] [, EForm(string) Level(real 95) ]
	cap drop kappa*
	cap drop samp*
	
	noi cap sureg (Mt8 $X8 $Z8 $FE) (Mt5 $X5 $Z5 $FE) $IFEXT
	predict kappa8 if e(sample), equation(Mt8)
	predict kappa5 if e(sample), equation(Mt5)
	
	gen kappa5X8 = (kappa5)*(kappa8)
	global kap kappa8 kappa5 kappa5X8
	forvalues j=2/$kpowD {
		gen kappa8_`j' = (kappa8)^`j'
		gen kappa5_`j' = (kappa5)^`j'
		gen kappa5`j'X8 = ((kappa5)^`j')*(kappa8)
		gen kappa5X8`j' = (kappa5)*((kappa8)^`j')
		gen kappa5X8_`j' = ((kappa5)^`j')*((kappa8)^`j')
		global kap $kap kappa8_`j' kappa5_`j' kappa5`j'X8 kappa5X8`j' kappa5X8_`j'
	}
	
	qui reg $M $X $kap $FE _J* $IF, cluster($CLU) 

end


***************************************************************************
********************************bootstrap-tse******************************
***************************************************************************
cap program drop BOOTtse
program define BOOTtse
	qui {
		preserve
			clear
			gen iter = .
			save "$output/BOOT_DATA_$BFILE",replace
		if $N==2 {
			drop iter
			gen iter = ""
			save "$TABOUT", replace 
		}
		restore
	}

	forvalues n=1/$REPS {
		if (`n'==1) dis "-->bootstrap iteration: `n'"
		if (`n'!=1) dis "..`n'"
		qui {
			preserve
			keep if _samplePlant==1
			keep $Z5 $Z8 $X5 $X8 $X $XD $priceD $price5 $price8 d_ln_emig_shr Mt5 Mt8 prop *district* _J* _sample*
			bsample, cluster($CLU)
			$EST $X
			gen iter = `n'
			foreach b in $BETA {
				gen beta_`b' = _b[`b']
				gen se_`b' = _se[`b']
			}
			keep beta* se* iter 
			keep in 1
			append using "$output/BOOT_DATA_$BFILE"
			save "$output/BOOT_DATA_$BFILE",replace
			restore
		}
	}

	preserve
	// get original Wald Stat
	dis "-->estimation on original sample"
	qui $EST $X 
	
	// calculate percentile t confidence intervals
	qui use "$output/BOOT_DATA_$BFILE",clear
	foreach v in $BETA {
		qui {
			*original beta
			gen BETA_`v' = _b[`v']
			*original SE
			gen oSE_`v' = _se[`v']
			*original Wald stat
			local W_`v' = _b[`v']/_se[`v']
			gen Wb_`v' = (beta_`v' - _b[`v'])/se_`v'
			
			*boot t crit val
			gen sig_`v' = ""
			_pctile Wb_`v', p(5,95)
			if !inrange(`W_`v'',r(r1),r(r2)) {
				replace sig_`v' = "\$^{*}\$"
			}
			_pctile Wb_`v', p(2.5,97.5)
			if !inrange(`W_`v'',r(r1),r(r2)) {
				replace sig_`v' = "\$^{**}\$"
			}
			_pctile Wb_`v', p(0.5,99.5)
			if !inrange(`W_`v'',r(r1),r(r2)) {
				replace sig_`v' = "\$^{***}\$"
			}					
		}
	}

	// get columns in order for table
	qui {
		format BETA* *SE* %9.3f
		tostring BETA* *SE*,replace usedisplayformat force
		foreach v of newlist $BETA {
			*boot-t: original SE
			gen SE_`v' = "(" + oSE_`v' + ")" + sig_`v'
		}
		keep BETA* SE* iter
		keep if iter==1
		reshape long BETA SE, i(iter) j(covariate) string
		expand 2
		bys covariate: replace BETA = SE if _n==2
		bys covariate: gen row = _n
		keep covariate BETA row 
		ren BETA column$N
		if $N==2 {
			merge using "$TABOUT"
		}
		else{
			merge 1:1 covariate row using "$TABOUT"
		}
		drop _m 
		cap drop iter
		save "$TABOUT", replace
		restore
	}
	
end


***************************************************************************
************************************MKTAB**********************************
***************************************************************************
cap program drop MKTAB
program define MKTAB	
	preserve
	use "$TABOUT", clear
	drop row
	order column*, sequential
	order covariate
	export excel using "$TABOUT", firstrow(variables) replace
	restore	
end
	




******************************************************************

	
*outreg
	global outE excel
	global sym "symbol($^{***}$, $^{**}$, $^{*}$)"	
	
*output bootstrap results
	cap mkdir "$output/logs"
	global LOG "$output"

*DNV specs
	global kpowD 3 			/* polynomial order  on selection terms */
	global EST1 SULPM		/* first stage estimator: SU-LPM */
	
*bootstrap specification
	global REPS 1000
	set seed 10101	

*two-step vars
	global R Rlambda shareHH_aboveRminRany paddy_farmland_shr_5 
	global C ln_dist_kec ln_ddist_kabcap ln_ddist_emigctr nonlandmotor_travel urban
	global desaC ln_dist_emigctr_desa nonlandmotor_travel urban
	global S postprim 
	global C2 arabShr chineseShr muslim_shr
	
	global rainS5 rain_cumdev345
	global rainS8 rain_cumdev678
	global rainD rain_cumdev_diff
	global rainS5L rain_cumdev345 rain5lambda
	global rainS8L rain_cumdev678 rain8lambda
	global rainDL rain_cumdev_diff rainDlambda
	global rainS5F rain_cumdev345 rain5lambda shareHHaboveXrain5 
	global rainS8F rain_cumdev678 rain8lambda shareHHaboveXrain8
	global rainDF rain_cumdev_diff rainDlambda shareHHaboveXrainD
	
	global p5r P_rice_y2m1_y5m3
	global p8r P_rice_y5m4_y8m3 
	global pDr delta_RP_diff
	global p5rL P_rice_y2m1_y5m3 P445lambda
	global p8rL P_rice_y5m4_y8m3 P448lambda
	global pDrL delta_RP_diff P44Dlambda
	global p5rF P_rice_y2m1_y5m3 P445lambda shareHHaboveXpr5
	global p8rF P_rice_y5m4_y8m3 P448lambda shareHHaboveXpr8
	global pDrF delta_RP_diff P44Dlambda shareHHaboveXprD		
	
*dep var
	global M d_ln_emig_shr	

*instruments
	global Z5 lnVinSD lnAinSD lnN5inSD lnpop5 
	global Z8 lnVinSD lnAinSD lnN8inSD lnpop8 		
	
	global X5 $R $p5r $rainS5 $C $C2 $S 
	global X8 $R $p8r $rainS8 $C $C2 $S 
	global X $R $C $C2 $S $pDr $rainD 	

*clustering and FE
	global j district
	global FE i.prop 
	global CLU district		
	
	

****Table 5*****************************************************

use "$rep_files/mainvillageregs", clear	
			
	*keep villages with area planted \lambda		
	global IFEXT if _samplePlant==1
	keep $IFEXT
	
	global Z5 lnVinSD lnAinSD lnN5inSD lnpop5 
	global Z8 lnVinSD lnAinSD lnN8inSD lnpop8  
	
	global TABOUT "$output/table5intensive"
	global out outreg2 using "$TABOUT", $outE asterisk(se) label dec(3) $sym
	global BFILE table5intensive
	
	*COLUMN 1
	global X $R $C $C2 $S $pDr $rainD 		
	reg d_ln_emig_shr $X _J* $FE, cl($CLU)
	$out replace keep($rainD $pDr)	
	
	*--PANEL A--*
	global EST DNV
	
	*COLUMN 2
		global X5 $R $p5r $rainS5 $C $C2 $S 
		global X8 $R $p8r $rainS8 $C $C2 $S 
		global X $R $C $C2 $S $pDr $rainD 
		
		global BETA $pDr $rainD
		global N 2
		BOOTtse	
				
	*COLUMN 3
		global X5 $R $p5r $rainS5L $C $C2 $S 
		global X8 $R $p8r $rainS8L $C $C2 $S 
		global X $R $C $C2 $S $pDr $rainDL
		
		global BETA $pDr $rainDL
		global N 3
		BOOTtse		
	
	*COLUMN 4
		global X5 $R $p5rL $rainS5L $C $C2 $S 
		global X8 $R $p8rL $rainS8L $C $C2 $S 
		global X $R $C $C2 $S $pDrL $rainDL
		
		global BETA $pDrL $rainDL
		global N 4
		BOOTtse		

	*COLUMN 5
		global X5 $R $C $C2 $S $p5rF $rainS5F
		global X8 $R $C $C2 $S $p8rF $rainS8F
		global X $R $C $C2 $S $pDrF $rainDF	
			
		global BETA $rainDF $pDrF
		global N 5
		BOOTtse			
	
	*--PANEL B--*
	global EST POIRIER
	
	*COLUMN 6
		global X5 $R $p5r $rainS5 $C $C2 $S 
		global X8 $R $p8r $rainS8 $C $C2 $S 
		global X $R $C $C2 $S $pDr $rainD 
		
		global BETA $pDr $rainD
		global N 6
		BOOTtse	
			
	
	*COLUMN 7
		global X5 $R $p5r $rainS5L $C $C2 $S 
		global X8 $R $p8r $rainS8L $C $C2 $S 
		global X $R $C $C2 $S $pDr $rainDL
		
		global BETA $pDr $rainDL
		global N 7
		BOOTtse		
	
	*COLUMN 8
		global X5 $R $p5rL $rainS5L $C $C2 $S 
		global X8 $R $p8rL $rainS8L $C $C2 $S 
		global X $R $C $C2 $S $pDrL $rainDL
		
		global BETA $pDrL $rainDL		
		global N 8
		BOOTtse		

	*COLUMN 9
		global X5 $R $C $C2 $S $p5rF $rainS5F
		global X8 $R $C $C2 $S $p8rF $rainS8F
		global X $R $C $C2 $S $pDrF $rainDF	
			
		global BETA $rainDF $pDrF
		global N 9
		BOOTtse			
		
*--->MAKE TABLE
	MKTAB	

