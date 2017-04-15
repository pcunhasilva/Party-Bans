set more off

* Set Directory
cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/Analysis Files"

* Load data
import delimited "dataFinal.csv", colrange(2) clear
 
* Convert strings to numeric and deal with NA's
foreach i of varlist oilhm civviol civtot ethviol ethfrac fd_n_attacks lnoilcap polity2 lnpop{
	 destring `i', replace force 
	} 
	
* Finding duplicates
duplicates tag cow_code year, gen(isdup)
drop if isdup==1


* Difference of Means
ttest d_civtot, by(partybaniep) 
ttest d_civtot, by(party_banvd) 
ttest fd_n_attacks, by(partybaniep) 
ttest fd_n_attacks, by(party_banvd) 


* Set TSCS 
tsset cow_code year

* Unit Root Tests
xtfisher d_civtot, lags(2) 
xtfisher fd_n_attacks, lags(2) 
xtfisher n_attacks, lags(2)
xtfisher lnoilcap, lags(2) // Has unit root.
xtfisher ethfrac, lags(2) // Has unit root.

* Testing serial correlation
xtserial civtot banall polity2 prsystem lnoilcap lnpop, output
xtserial n_attacks banall polity2 prsystem lnoilcap lnpop, output
xtserial civtot party_banvd polity2 prsystem lnoilcap lnpop, output
xtserial n_attacks party_banvd polity2 prsystem lnoilcap lnpop, output



***************************************************************************
**************************** First Difference *****************************
***************************************************************************

* Mark no missing data
mark nomiss 
markout nomiss n_attacks fd_n_attacks partybaniep party_banvd polity2 unitary prsystem lnoilcap lnpop ethfrac


*******************************
****** Painel Correct SE ******
*******************************

* Models with fd_n_attacks (No Controls):
xtpcse fd_n_attacks L.partybaniep if nomiss==1,  pairwise correlation(ar1) 
xtpcse fd_n_attacks L.party_banvd if nomiss==1,  pairwise correlation(ar1) 

* Models with fd_n_attacks (With Political Controls):
xtpcse fd_n_attacks L.partybaniep polity2 unitary prsystem if nomiss==1,  pairwise correlation(ar1)
xtpcse fd_n_attacks L.party_banvd polity2 unitary prsystem if nomiss==1,  pairwise correlation(ar1)

* Models with fd_n_attacks (With Political and Economic Controls):
xtpcse fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap if nomiss==1,  pairwise correlation(ar1)
xtpcse fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap if nomiss==1,  pairwise correlation(ar1)

* Models with fd_n_attacks (With Political, Economic, and Demographic Controls):
xtpcse fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop if nomiss==1,  pairwise correlation(ar1)
xtpcse fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop if nomiss==1,  pairwise correlation(ar1)

xtpcse fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac if nomiss==1,  pairwise correlation(ar1)
xtpcse fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac if nomiss==1 ,  pairwise correlation(ar1)


*********************************
****** Fixed-effects AR(1) ******
*********************************

* Models with fd_n_attacks (No Controls):
xtregar fd_n_attacks L.partybaniep if nomiss==1, fe
xtregar fd_n_attacks L.party_banvd if nomiss==1, fe 

* Models with fd_n_attacks (With Political Controls):
xtregar fd_n_attacks L.partybaniep polity2 unitary prsystem if nomiss==1, fe
xtregar fd_n_attacks L.party_banvd polity2 unitary prsystem if nomiss==1, fe

* Models with fd_n_attacks (With Political and Economic Controls):
xtregar fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap if nomiss==1, fe
xtregar fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap if nomiss==1, fe

* Models with fd_n_attacks (With Political, Economic, and Demographic Controls):
xtregar fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop if nomiss==1,  fe
xtregar fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop if nomiss==1, fe

xtregar fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac if nomiss==1,  fe
xtregar fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac if nomiss==1 ,  fe


*****************************************************************
**************************** Level ******************************
*****************************************************************

*******************************
****** Binomial Negative ******
*******************************

* Models with n_attacks (No Controls):
xtnbreg n_attacks L.n_attacks L.partybaniep if nomiss==1,  fe
xtnbreg n_attacks L.n_attacks L.party_banvd if nomiss==1,  fe

* Models with n_attacks (With Political Controls):
xtnbreg n_attacks L.n_attacks L.partybaniep polity2 unitary prsystem if nomiss==1, fe
xtnbreg n_attacks L.n_attacks L.party_banvd polity2 unitary prsystem if nomiss==1, fe

* Models with n_attacks (With Political and Economic Controls):
xtnbreg n_attacks L.n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap if nomiss==1,  fe
xtnbreg n_attacks L.n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap if nomiss==1,  fe

* Models with n_attacks (With Political, Economic, and Demographic Controls):
xtnbreg n_attacks L.n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop if nomiss==1,  fe
xtnbreg n_attacks L.n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop if nomiss==1,  fe

xtnbreg n_attacks L.n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac if nomiss==1,  fe
xtnbreg n_attacks L.n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac if nomiss==1 ,  fe



*****************************************************************
**************************** Dummy ******************************
*****************************************************************

* Mark no missing data
mark nomiss2 
markout nomiss2 d_civtot d_civtot partybaniep party_banvd polity2 unitary prsystem lnoilcap lnpop ethfrac

* Models with d_civtot (With Political, Economic, and Demographic Controls):
xtlogit d_civtot L.d_civtot L.partybaniep polity2 unitary prsystem lnoilcap lnpop ethfrac if nomiss2==1,  fe
xtlogit d_civtot L.d_civtot L.party_banvd polity2 unitary prsystem lnoilcap lnpop ethfrac if nomiss2==1 ,  fe



****************** Cross-Section Models************************
*1989 to 2000
preserve
collapse (mean) n_attacks civtot  polity2 ln_oil ethfrac ln_pop (first) banall lelecsystem if year>1989 & year<2000, by(cabrnew)

reg n_attacks banall i.lelecsystem polity2 ethfrac ln_oil ln_pop, robust
reg civtot banall i.lelecsystem polity2 ethfrac ln_oil ln_pop, robust

nbreg n_attacks banall i.lelecsystem polity2 ethfrac ln_oil ln_pop
nbreg civtot banall i.lelecsystem polity2 ethfrac ln_oil ln_pop
restore
*1979 to 1988
preserve
collapse (mean) n_attacks civtot  polity2 ln_oil ethfrac ln_pop (first) banall lelecsystem if year>1978 & year<1989, by(cabrnew)

reg n_attacks banall i.lelecsystem polity2 ethfrac ln_oil ln_pop, robust
reg civtot banall i.lelecsystem polity2 ethfrac ln_oil ln_pop, robust

nbreg n_attacks banall i.lelecsystem polity2 ethfrac ln_oil ln_pop
nbreg civtot banall i.lelecsystem polity2 ethfrac ln_oil ln_pop
restore
