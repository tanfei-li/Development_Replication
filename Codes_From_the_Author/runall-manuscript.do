
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
	




