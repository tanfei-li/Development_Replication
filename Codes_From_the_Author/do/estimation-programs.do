
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
	




