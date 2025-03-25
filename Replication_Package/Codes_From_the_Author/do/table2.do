
use "$rep_files/migchoicedta", clear

eststo clear

clogit mig rainShock i.year, cl(district) group(hhcode)
eststo : margins, dydx(rainShock) noestimcheck predict(pu0) post

clogit mig rainShock priceShock i.year, cl(district) group(hhcode)
eststo : margins, dydx(rainShock priceShock) noestimcheck predict(pu0) post

eststo : clogit mig rainShock rainShockXland priceShock priceShockXland i.year, cl(district) group(hhcode)

eststo : clogit mig rainShock rainShockXland rainShockXland2 priceShock priceShockXland priceShockXland2 i.year, cl(district) group(hhcode)

#d ;
estout using "$output/table2.tex", replace 
	keep(rainShock rainShockXland rainShockXland2 priceShock priceShockXland priceShockXland2, relax)
	order(rainShock rainShockXland rainShockXland2 priceShock priceShockXland priceShockXland2, relax)
	label 
	starlevels(* 0.1 ** 0.05 *** 0.01)
	stats(N, labels("Observations") fmt(%9.0fc))
	cells(b(fmt(3)) se(par star fmt(3))) dropped(".")
	 varwidth(15) 
	mlabels(, none) collabels(, none)
	prefoot("\midrule")
	substitute(_ \_) style(tex);
#d cr

