
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
