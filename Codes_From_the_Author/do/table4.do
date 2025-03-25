
use "$rep_files/mainvillageregs", clear	
			
	*keep villages with area planted \lambda		
	global IFEXT if _samplePlant==1
	keep $IFEXT
	
	global TABOUT "$output/table4extensive.xls"
	global out outreg2 using "$TABOUT", $outE asterisk(se) label dec(3) $sym	
	
	*COLUMN 1
	global Z5 lnVinSD lnAinSD lnN5inSD lnplant_sawahMinP1 lnplant_sawahMaxP1
	global Z8 lnVinSD lnAinSD lnN8inSD lnplant_sawahMinP1 lnplant_sawahMaxP1
		
	global ZX5 $C $C2 $S $p5r $rainS5 shareHH_aboveRminRany paddy_farmland_shr_5 $Z5
	global ZX8 $C $C2 $S $p8r $rainS8 shareHH_aboveRminRany paddy_farmland_shr_5 $Z8	
	
	bootstrap, reps(500) cluster(district): sureg (Mt8 $ZX8 $FE) (Mt5 $ZX5 $FE)
	$out replace keep(lnplant_sawahMaxP1 lnplant_sawahMinP1 lnN8inSD lnN5inSD lnAinSD ln_dist_keccap ln_ddist_emigctr $p8r $p5r $rainS5 $rainS8)
		
	*COLUMN 2
	global Z5 lnVinSD lnAinSD lnN5inSD lnpop5 
	global Z8 lnVinSD lnAinSD lnN8inSD lnpop8  
		
	global ZX5 $C $C2 $S $p5r $rainS5 Rlambda shareHH_aboveRminRany paddy_farmland_shr_5 $Z5
	global ZX8 $C $C2 $S $p8r $rainS8 Rlambda shareHH_aboveRminRany paddy_farmland_shr_5 $Z8
			
	bootstrap, reps(500) cluster(district): sureg (Mt8 $ZX8 $FE) (Mt5 $ZX5 $FE)
	$out append keep(Rlambda lnpop8 lnpop5 lnN8inSD lnN5inSD lnAinSD ln_dist_keccap ln_ddist_emigctr $p8r $p5r $rainS5 $rainS8)
	
	*COLUMN 3
	global Z5 lnVinSD lnAinSD lnN5inSD lnplant_sawahMinP1 lnplant_sawahMaxP1
	global Z8 lnVinSD lnAinSD lnN8inSD lnplant_sawahMinP1 lnplant_sawahMaxP1
		
	global ZX5 $C $C2 $S $p5r $rainS5 shareHH_aboveRminRany paddy_farmland_shr_5 $Z5
	global ZX8 $C $C2 $S $p8r $rainS8 shareHH_aboveRminRany paddy_farmland_shr_5 $Z8
		
	biprobit (Mt8 = $ZX8 $FE) (Mt5 = $ZX5 $FE), cl($CLU)
	$out append keep(lnplant_sawahMaxP1 lnplant_sawahMinP1 lnN8inSD lnN5inSD lnAinSD ln_dist_keccap ln_ddist_emigctr $p8r $p5r $rainS5 $rainS8)	
	
	*COLUMN 4
	global Z5 lnVinSD lnAinSD lnN5inSD lnpop5 
	global Z8 lnVinSD lnAinSD lnN8inSD lnpop8  
		
	global ZX5 $C $C2 $S $p5r $rainS5 Rlambda shareHH_aboveRminRany paddy_farmland_shr_5 $Z5
	global ZX8 $C $C2 $S $p8r $rainS8 Rlambda shareHH_aboveRminRany paddy_farmland_shr_5 $Z8
		
	biprobit (Mt8 = $ZX8 $FE) (Mt5 = $ZX5 $FE), cl($CLU)
	$out append keep(Rlambda lnpop8 lnpop5 lnN8inSD lnN5inSD lnAinSD ln_dist_keccap ln_ddist_emigctr $p8r $p5r $rainS5 $rainS8)
