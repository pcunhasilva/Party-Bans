set more off

* Set Directory
cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/Analysis Files"

* Load data
use dataRegressions, clear

* Convert oil variable
gen oilnew=oilHM*100000000
gen oilnew2= oilnew/popHM


tsset cow_code year

* label variables
label var lag_attack "Terrorist Attacks(lag t-1)"
label var lag_partybanUni "Party Ban (lag t-1)"
label var unitary "Unitary"
label var PRsystem "PR System"
label var dd_cga "Democracy"
label var lnoilcap "Oil Reserves per capita (Log)"
label var NewEthFrac "Ethnic Fragmentation"
label var lag_party_banVD "Party Ban (lag $t-1$) (V-Dem)"
label var lag_partybanIEP "Party Ban (lag $t-1$) (IEP)"


* Models FE
* Combined partybans
set more off
eststo m1: qui xtnbreg n_attacks lag_attack  lag_partybanUni,  fe
eststo m2: qui xtnbreg n_attacks lag_attack  lag_partybanUni  dd_cga unitary PRsystem, fe
eststo m3: qui xtnbreg n_attacks lag_attack  lag_partybanUni  dd_cga unitary PRsystem lnoil NewEthFrac, fe

cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Research Paper/Results"
esttab m1 m2 m3 using table1a.tex, ///
    mtitles("Model 1" "Model 2" "Model 3") ///
   label star(* 0.10 ** 0.05 *** 0.01) nonumbers se noconstant aic ///
   title(Pooled Time Series Negative Binomial - DV: Number of Terrorist Attacks table\label{tabR1a}) ///
   replace

* Model with partybans IEP
set more off
eststo m1: qui xtnbreg n_attacks lag_attack lag_partybanIEP,  fe
eststo m2: qui xtnbreg n_attacks lag_attack lag_partybanIEP dd_cga unitary PRsystem,  fe
eststo m3: qui xtnbreg n_attacks lag_attack lag_partybanIEP dd_cga unitary PRsystem lnoilcap  NewEthFrac,  fe
esttab m1 m2 m3 using tableAppendix1.tex, ///
    mtitles("Model 1" "Model 2" "Model 3") ///
   label  star(* 0.10 ** 0.05 *** 0.01) nonumbers se noconstant  ///
   title(Pooled Time Series Negative Binomial - DV: Number of Terrorist Attacks (IEP) table\label{tabA1}) ///
   replace
   
 * Model with partybans V-DEM
set more off
eststo m1: qui xtnbreg n_attacks lag_attack lag_party_banVD,  fe
eststo m2: qui xtnbreg n_attacks lag_attack lag_party_banVD dd_cga unitary PRsystem,  fe
eststo m3: qui xtnbreg n_attacks lag_attack lag_party_banVD dd_cga unitary PRsystem lnoilcap  NewEthFrac,  fe
esttab m1 m2 m3 using tableAppendix2.tex, ///
    mtitles("Model 1" "Model 2" "Model 3") ///
   label  star(* 0.10 ** 0.05 *** 0.01) nonumbers se noconstant  ///
   title(Pooled Time Series Negative Binomial - DV: Number of Terrorist Attacks (V-Dem) table\label{tabA2}) ///
   replace
   
* Models RE for Combined partybans 
eststo m1: qui xtnbreg n_attacks lag_attack  lag_partybanUni dd_cga unitary PRsystem lnoilcap NewEthFrac, re
eststo m2: qui xtnbreg n_attacks lag_attack  lag_partybanIEP dd_cga unitary PRsystem lnoilcap NewEthFrac, re
eststo m3: qui xtnbreg n_attacks lag_attack  lag_party_banVD dd_cga unitary PRsystem lnoilcap NewEthFrac, re   
 esttab m1 m2 m3 using tableAppendix3.tex, ///
    mtitles("Model 1" "Model 2" "Model 3") ///
   label  star(* 0.10 ** 0.05 *** 0.01) nonumbers se noconstant  ///
   title(Pooled Time Series Negative Binomial - DV: Number of Terrorist Attacks (Random Effects) table\label{tabA3}) ///
   replace  
   
* Models By Political Regime
eststo m1:  xtnbreg n_attacks lag_attack lag_partybanUni unitary PRsystem lnoilcap  NewEthFrac if dd_cga==1,  fe
eststo m2:  xtnbreg n_attacks lag_attack lag_partybanUni unitary PRsystem lnoilcap  NewEthFrac if dd_cga==0,  fe
eststo m3:  xtnbreg n_attacks lag_attack lag_partybanUni unitary PRsystem lnoilcap  NewEthFrac if dd_cga==1,  re
eststo m4:  xtnbreg n_attacks lag_attack lag_partybanUni unitary PRsystem lnoilcap  NewEthFrac if dd_cga==0,  re
esttab m1 m2 m3 m4 using tableAppendix4.tex, ///
    mtitles("Democracies" "Dictatorships" "Democracies" "Dictatorships") ///
   label  star(* 0.10 ** 0.05 *** 0.01) nonumbers se noconstant  ///
   title(Pooled Time Series Negative Binomial - DV: Number of Terrorist Attacks (By Regime Type) table\label{tabA3}) ///
   replace  


   
********************************************
***** Generate Plots   

cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Research Paper/Results"
set more off
xtnbreg n_attacks lag_attack  i.lag_partybanUni i.dd i.unitary i.PRsystem lnoil NewEthFrac , fe


* Linear Prediction when number of previous attack changes.
margins lag_partybanUni, at(lag_attack=(0(1)20) ///
						 unitary = 1 PRsystem = 1 ///
						 lnoil = -15 ///
						 dd = 1 ///
						 NewEthFrac = .4376)
marginsplot, ///
		 graphregion(fcolor(white)) ///
		 legend(order(1 "No Party Ban" 2 "Party Ban")) ///
		 title("") saving(plot1.png, replace)
		 
* Linear Prediction when ethnic changes.
margins lag_partybanUni, at(NewEthFrac=(0(0.05)1) ///
						 unitary = 1 PRsystem = 1 ///
						 lnoil = -15 ///
						 dd = 1 ///
						 lag_attack = 12)
marginsplot, ///
		 graphregion(fcolor(white)) ///
		 legend(order(1 "No Party Ban" 2 "Party Ban")) ///
		 title("") saving(plot2.png, replace)

* Appendix
* Linear Prediction for Democracies
set more off
xtnbreg n_attacks lag_attack  i.lag_partybanUni i.unitary i.PRsystem lnoil NewEthFrac if dd==1, fe

* Linear Prediction when number of previous attack changes.
margins lag_partybanUni, at(lag_attack=(0(1)20) ///
						 unitary = 1 PRsystem = 1 ///
						 lnoil = -15 ///
						 NewEthFrac = .4376)
marginsplot, ///
		 graphregion(fcolor(white)) ///
		 legend(order(1 "No Party Ban" 2 "Party Ban")) ///
		 title("") saving(plotA1Democracies.png, replace)
		 
* Linear Prediction when ethnic changes.
margins lag_partybanUni, at(NewEthFrac=(0(0.05)1) ///
						 unitary = 1 PRsystem = 1 ///
						 lnoil = -15 ///
						 lag_attack = 12)
marginsplot, ///
		 graphregion(fcolor(white)) ///
		 legend(order(1 "No Party Ban" 2 "Party Ban")) ///
		 title("") saving(plotA2Democracies.png, replace)
		 		 
* Linear Prediction for Autocracies
set more off
xtnbreg n_attacks lag_attack  i.lag_partybanUni i.unitary i.PRsystem lnoilc NewEthFrac if dd==0, fe

* Linear Prediction when number of previous attack changes.
margins lag_partybanUni, at(lag_attack=(0(1)20) ///
						 unitary = 1 PRsystem = 1 ///
						 lnoil = -15 ///
						 NewEthFrac = .4376)
marginsplot, ///
		 graphregion(fcolor(white)) ///
		 legend(order(1 "No Party Ban" 2 "Party Ban")) ///
		 title("") saving(plotA3Autocracies.png, replace)
		 
* Linear Prediction when ethnic changes.
margins lag_partybanUni, at(NewEthFrac=(0(0.05)1) ///
						 unitary = 1 PRsystem = 1 ///
						 lnoil = -15 ///
						 lag_attack = 12)
marginsplot, ///
		 graphregion(fcolor(white)) ///
		 legend(order(1 "No Party Ban" 2 "Party Ban")) ///
		 title("") saving(plotA4Autocracies.png, replace)
		 
		 
***********************
gen interaction =  polity2*lag_partybanUni

xtnbreg n_attacks lag_attack  interaction lag_partybanUni  polity2 unitary PRsystem lnoil NewEthFrac, fe

forvalues i = -10(1)10{
	 display `i'
	 di _b[interaction] * `i' +  _b[lag_partybanUni]
	 }
		 	 

