
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
