
use "$rep_files/table3dta", clear
	
	xtset vid

	global TABOUT "$output/table3reducedform.xls"
	global out outreg2 using "$TABOUT", $outE asterisk(se) label keep(rain price rainLambda priceLambda) dec(6) $sym
	
	*COLUMN 1
	xtreg emig_shr t2 rain price, fe cl(vid)
	$out replace
	xtreg emig_shr t2 rain price, fe cl(district)	
	$out append
	
	*COLUMN 2
	xtreg emig_shr t2 rain price rainLambda priceLambda, fe cl(vid)	
	$out append
	xtreg emig_shr t2 rain price rainLambda priceLambda, fe cl(district)	
	$out append
	
	*COLUMN 3
	**originally two_side emig_shr t2* rain price vid 
	xtreg emig_shr t2* rain price, fe cluster(vid)	
	$out append	
	bootstrap _b, reps($REPS) cluster(district) seed(101) : xtreg emig_shr t2* rain price, fe cluster(vid)	
	$out append	
	
	*COLUMN 4
	**originally two_side emig_shr t2* rain price rainLambda priceLambda vid
	xtreg emig_shr t2* rain price rainLambda priceLambda, fe cluster(vid)
	$out append	
	bootstrap _b, reps($REPS) cluster(district) seed(101) : xtreg emig_shr t2* rain price rainLambda priceLambda, fe cluster(vid)
	$out append			
