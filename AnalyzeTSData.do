set more off

* Set Directory
cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/Analysis Files"

* Load data
import delimited "dataFinal.csv", colrange(2) clear
 
* Convert strings to numeric and deal with NA's
foreach i of varlist  year cow_code country_id partybaniep party_banvd oilhm pophm polity2 ccode ccodealp code_wb banethnic banrelig bansys banall govstruct elecsystem unitary prsystem country cname ethfrac country_abb civviol civtot ethviol n_attacks {
	 destring `i', replace force 
	} 
	
* Finding duplicates
duplicates tag cow_code year, gen(isdup)
drop if isdup==1

* Generate ln(Oil capita)
gen lnoilcap = ln(oilhm/pophm + 1)

* Generate ln(pop)
gen lnpop = ln(pop)

* Interpolate Ethnic Frag
by cow_code, sort: ipolate ethfrac year, gen(ethfracNew) 


* Difference of Means
ttest civtot, by(partybaniep) 
ttest civtot, by(party_banvd) 
ttest n_attacks, by(partybaniep) 
ttest n_attacks, by(party_banvd) 


* Set TSCS 
tsset cow_code year

* Unit Root Tests
xtfisher civtot, lags(2) 
xtfisher n_attacks, lags(2) 
xtfisher polity2, lags(2)
xtfisher n_attacks, lags(2)
xtfisher lnoilcap, lags(2) // Has unit root.
xtfisher ethfrac, lags(2) // Has unit root.

* Testing serial correlation
xtserial civtot banall polity2 prsystem lnoilcap lnpop, output
xtserial n_attacks banall polity2 prsystem lnoilcap lnpop, output
xtserial civtot party_banvd polity2 prsystem lnoilcap lnpop, output
xtserial n_attacks party_banvd polity2 prsystem lnoilcap lnpop, output

**************************************
****** Negative Binomial Models ******
**************************************

* Models with civtot (No Controls):
xtnbreg civtot L.partybaniep, fe //Increases violence
xtnbreg civtot L.party_banvd, fe //Increases violence

* Models with civtot (With Political Controls):
xtnbreg civtot L.partybaniep polity2 unitary prsystem, fe
xtnbreg civtot L.party_banvd polity2 unitary prsystem, fe

* Models with civtot (With Political and Economic Controls):
xtnbreg civtot L.partybaniep polity2 unitary prsystem D.lnoilcap, fe
xtnbreg civtot L.party_banvd polity2 unitary prsystem D.lnoilcap, fe

* Models with civtot (With Political, Economic, and Demographic Controls):
xtnbreg civtot L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop, fe
xtnbreg civtot L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop, fe

xtnbreg civtot L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac , fe
xtnbreg civtot L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac , fe


* Models with civtot (No Controls):
xtnbreg n_attacks L.partybaniep, fe //Increases violence
xtnbreg n_attacks L.party_banvd, fe //Increases violence

* Models with civtot (With Political Controls):
xtnbreg n_attacks L.partybaniep polity2 unitary prsystem, fe
xtnbreg n_attacks L.party_banvd polity2 unitary prsystem, fe

* Models with civtot (With Political and Economic Controls):
xtnbreg n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap, fe
xtnbreg n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap, fe

* Models with civtot (With Political, Economic, and Demographic Controls):
xtnbreg n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop, fe
xtnbreg n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop, fe

xtnbreg n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac , fe
xtnbreg n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac , fe



*******************************
****** Painel Correct SE ******
*******************************

* Models with civtot (No Controls):
xtpcse civtot L.partybaniep,  pairwise correlation(ar1) 
xtpcse civtot L.party_banvd,  pairwise correlation(ar1) 

* Models with civtot (With Political Controls):
xtpcse civtot L.partybaniep polity2 unitary prsystem,  pairwise correlation(ar1)
xtpcse civtot L.party_banvd polity2 unitary prsystem,  pairwise correlation(ar1)

* Models with civtot (With Political and Economic Controls):
xtpcse civtot L.partybaniep polity2 unitary prsystem D.lnoilcap,  pairwise correlation(ar1)
xtpcse civtot L.party_banvd polity2 unitary prsystem D.lnoilcap,  pairwise correlation(ar1)

* Models with civtot (With Political, Economic, and Demographic Controls):
xtpcse civtot L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop,  pairwise correlation(ar1)
xtpcse civtot L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop,  pairwise correlation(ar1)

xtpcse civtot L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac ,  pairwise correlation(ar1)
xtpcse civtot L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac ,  pairwise correlation(ar1)


* Models with civtot (No Controls):
xtpcse n_attacks L.partybaniep,  pairwise correlation(ar1)
xtpcse n_attacks L.party_banvd,  pairwise correlation(ar1)

* Models with civtot (With Political Controls):
xtpcse n_attacks L.partybaniep polity2 unitary prsystem,  pairwise correlation(ar1)
xtpcse n_attacks L.party_banvd polity2 unitary prsystem,  pairwise correlation(ar1)

* Models with civtot (With Political and Economic Controls):
xtpcse n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap,  pairwise correlation(ar1)
xtpcse n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap,  pairwise correlation(ar1)

* Models with civtot (With Political, Economic, and Demographic Controls):
xtpcse n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop,  pairwise correlation(ar1)
xtpcse n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop,  pairwise correlation(ar1)

xtpcse n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac ,  pairwise correlation(ar1)
xtpcse n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.ethfrac ,  pairwise correlation(ar1)


*******************************************

* OLS
xtreg civtot banall polity2 i.lelecsystem ethfrac ln_oil ln_pop, fe
* PCSE
xtpcse civtot banall polity2 i.lelecsystem, pairwise correlation(ar1)
xtpcse civtot banall polity2 i.lelecsystem ethfrac, pairwise correlation(ar1)
xtpcse civtot banall polity2 i.lelecsystem ln_oil ln_pop, pairwise correlation(ar1)
xtpcse civtot banall polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, pairwise correlation(ar1) 
xtpcse civtot banall polity2 i.lelecsystem  ln_oil ln_pop, pairwise correlation(ar1) 
* xtregar
xtregar civtot banall polity2 i.lelecsystem, fe
xtregar civtot banall polity2 i.lelecsystem ethfrac, fe
xtregar civtot banall polity2 i.lelecsystem ln_oil ln_pop, fe
xtregar civtot banall polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, fe
xtregar civtot banall polity2 i.lelecsystem ethfrac ln_oil ln_pop, re

* Models with n_attacks:
xtnbreg n_attacks banall polity2 i.lelecsystem, fe
xtnbreg n_attacks banall polity2 i.lelecsystem ethfrac, fe
xtnbreg n_attacks banall polity2 i.lelecsystem ln_oil ln_pop, fe
xtnbreg n_attacks banall polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, fe
* OLS
xtreg n_attacks banall polity2 i.lelecsystem, fe
xtreg n_attacks banall polity2 i.lelecsystem ethfrac, fe
xtreg n_attacks banall polity2 i.lelecsystem ln_oil ln_pop, fe
xtreg n_attacks banall polity2 i.lelecsystem ethfrac ln_oil ln_pop, fe
* PCSE
xtpcse n_attacks banall polity2 i.lelecsystem, pairwise correlation(ar1)
xtpcse n_attacks banall polity2 i.lelecsystem ethfrac, pairwise correlation(ar1)
xtpcse n_attacks banall polity2 i.lelecsystem ln_oil ln_pop, pairwise correlation(ar1)
xtpcse n_attacks banall polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, pairwise correlation(ar1) 
xtpcse n_attacks banall polity2 i.lelecsystem  ln_oil ln_pop, pairwise correlation(ar1) 
* xtregar
xtregar n_attacks L.banall L.n_attacks interaction polity2 i.lelecsystem, fe
xtregar n_attacks L.banall L.n_attacks interaction polity2 i.lelecsystem ethfrac, fe
xtregar n_attacks L.banall L.n_attacks interaction polity2 i.lelecsystem ln_oil ln_pop, fe
xtregar n_attacks L.banall L.n_attacks interaction polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, fe
xtregar n_attacks banall L.n_attacks interaction polity2 i.lelecsystem ethfrac ln_oil ln_pop, re


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
