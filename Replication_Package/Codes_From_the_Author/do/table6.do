
use "$rep_files/mainvillageregs", clear	
			
	*keep villages with area planted \lambda		
	global IFEXT if _samplePlant==1
	keep $IFEXT

	gen P345Xpptkis = pptkis * P_rice_y2m1_y5m3
	gen P678Xpptkis = pptkis * P_rice_y5m4_y8m3
	gen PDXpptkis = pptkis * delta_RP_diff
	
	gen rain345Xpptkis = pptkis * rain_cumdev345
	gen rain678Xpptkis = pptkis * rain_cumdev678
	gen rainDXpptkis = pptkis * rain_cumdev_diff	
	
	forvalues j=0/1 {
	
		foreach x in pptkis {
			
			gen `x'`j' = `x'==`j' if `x'<.
			
			gen rain345X`x'_q`j' = rain_cumdev345 * (`x'==`j') if `x'<.
			gen rain678X`x'_q`j' = rain_cumdev678 * (`x'==`j') if `x'<.
			gen rainDX`x'_q`j' = rain_cumdev_diff * (`x'==`j') if `x'<.
			gen P345X`x'_q`j' = P_rice_y2m1_y5m3 * (`x'==`j') if `x'<.
			gen P678X`x'_q`j' = P_rice_y5m4_y8m3 * (`x'==`j') if `x'<.
			gen PDX`x'_q`j' = delta_RP_diff * (`x'==`j') if `x'<.	
						
		}
	}		
	
	lab var PDXpptkis_q0 "\$\Delta\$ price shock \$\times\$ no recruiter presence"
	lab var PDXpptkis_q1 "\$\Delta\$ price shock \$\times\$ recruiter presence"
	lab var rainDXpptkis_q0 "\$\Delta\$ rainfall shock \$\times\$ no recruiter presence"
	lab var rainDXpptkis_q1 "\$\Delta\$ rainfall shock \$\times\$ recruiter presence"
	
	forvalues j=1/4 {
	
		foreach x in gdpQt {
			
			gen `x'`j' = `x'==`j' if `x'<.
			
			gen rain345X`x'_q`j' = rain_cumdev345 * (`x'==`j') if `x'<.
			gen rain678X`x'_q`j' = rain_cumdev678 * (`x'==`j') if `x'<.
			gen rainDX`x'_q`j' = rain_cumdev_diff * (`x'==`j') if `x'<.
			gen P345X`x'_q`j' = P_rice_y2m1_y5m3 * (`x'==`j') if `x'<.
			gen P678X`x'_q`j' = P_rice_y5m4_y8m3 * (`x'==`j') if `x'<.
			gen PDX`x'_q`j' = delta_RP_diff * (`x'==`j') if `x'<.
			
		}
	}	
	
	lab var PDXgdpQt_q1 "\$\Delta\$ price shock \$\times\$ agricultural GDP, quartile\$=\$1"
	lab var PDXgdpQt_q2 "\$\Delta\$ price shock \$\times\$ agricultural GDP, quartile\$=\$2" 
	lab var PDXgdpQt_q3 "\$\Delta\$ price shock \$\times\$ agricultural GDP, quartile\$=\$3" 
	lab var PDXgdpQt_q4 "\$\Delta\$ price shock \$\times\$ agricultural GDP, quartile\$=\$4" 
	lab var rainDXgdpQt_q1 "\$\Delta\$ rainfall shock \$\times\$ agricultural GDP, quartile\$=\$1"
	lab var rainDXgdpQt_q2 "\$\Delta\$ rainfall shock \$\times\$ agricultural GDP, quartile\$=\$2"
	lab var rainDXgdpQt_q3 "\$\Delta\$ rainfall shock \$\times\$ agricultural GDP, quartile\$=\$3"
	lab var rainDXgdpQt_q4 "\$\Delta\$ rainfall shock \$\times\$ agricultural GDP, quartile\$=\$4"

	global Z5 lnVinSD lnAinSD lnN5inSD lnpop5 
	global Z8 lnVinSD lnAinSD lnN8inSD lnpop8  
	
	global TABOUT "$output/table6oppcost"
	global out outreg2 using "$TABOUT", $outE asterisk(se) label dec(3) $sym
	global BFILE table6oppcost
	
	*COLUMN 1
	local x pptkis

		global X5 `x' $R $C $C2 $S P345X`x'_q0 P345X`x'_q1 rain345X`x'_q0 rain345X`x'_q1 
		global X8 `x' $R $C $C2 $S P678X`x'_q0 P678X`x'_q1 rain678X`x'_q0 rain678X`x'_q1 
		global X `x' $R $C $C2 $S PDX`x'_q0 PDX`x'_q1 rainDX`x'_q0 rainDX`x'_q1
		
		global BETA PDX`x'_q0 PDX`x'_q1 rainDX`x'_q0 rainDX`x'_q1
		global N 2
		BOOTtse		

		global X5 `x' $R $C $C2 $S P_rice_y2m1_y5m3 rain_cumdev345 P345X`x' rain345X`x' 
		global X8 `x' $R $C $C2 $S P_rice_y5m4_y8m3 P678X`x' rain_cumdev678 rain678X`x' 
		global X `x' $R $C $C2 $S delta_RP_diff PDX`x' rain_cumdev_diff rainDX`x'
	
		*NOTE: this result is just for interpreting the significance of the difference between coefficients per the daggers in the table
		global BETA delta_RP_diff PDX`x' rain_cumdev_diff rainDX`x'
		global N 3
		BOOTtse	
		
	*COLUMN 2
	local x gdpQt
	
		global X5 `x'2 `x'3 `x'4 $R $C $C2 $S P345X`x'_q1 P345X`x'_q2 P345X`x'_q3 P345X`x'_q4 rain345X`x'_q1 rain345X`x'_q2 rain345X`x'_q3 rain345X`x'_q4 
		global X8 `x'2 `x'3 `x'4 $R $C $C2 $S P678X`x'_q1 P678X`x'_q2 P678X`x'_q3 P678X`x'_q4 rain678X`x'_q1 rain678X`x'_q2 rain678X`x'_q3 rain678X`x'_q4  
		global X `x'2 `x'3 `x'4 $R $C $C2 $S PDX`x'_q1 PDX`x'_q2 PDX`x'_q3 PDX`x'_q4 rainDX`x'_q1 rainDX`x'_q2 rainDX`x'_q3 rainDX`x'_q4
		
		global BETA PDX`x'_q1 PDX`x'_q2 PDX`x'_q3 PDX`x'_q4 rainDX`x'_q1 rainDX`x'_q2 rainDX`x'_q3 rainDX`x'_q4
		global N 4
		BOOTtse
		
*--->MAKE TABLE
	MKTAB	
		
		
		
