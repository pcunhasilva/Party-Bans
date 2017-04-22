set more off

* Set Directory
cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/Analysis Files"

* Load data
use datafinal, clear
 
* Convert strings to numeric and deal with NA's
foreach i of varlist  unitary {
	 destring `i', replace force 
	} 
	
* Finding duplicates
duplicates tag cow_code year, gen(isdup)
drop if isdup==1

* Difference of Means
ttest d_civtot, by(partybaniep) 
ttest d_civtot, by(party_banvd) 
ttest d_civtot, by(partybanuni) 
ttest fd_n_attacks, by(partybaniep) 
ttest fd_n_attacks, by(party_banvd) 
ttest fd_n_attacks, by(partybanuni)
ttest n_attacks, by(partybaniep) 
ttest n_attacks, by(party_banvd)
ttest n_attacks, by(partybanuni) 

* Set TSCS 
tsset cow_code year

* Unit Root Tests
xtfisher d_civtot, lags(2) 
xtfisher fd_n_attacks, lags(2) 
xtfisher n_attacks, lags(2)
xtfisher lnoilcap, lags(2) // Has unit root.
xtfisher ethfrac, lags(2) // Has unit root.

* Testing serial correlation
xtserial civtot partybaniep polity2 prsystem lnoilcap lnpop, output
xtserial n_attacks partybaniep polity2 prsystem lnoilcap lnpop, output
xtserial civtot party_banvd polity2 prsystem lnoilcap lnpop, output
xtserial n_attacks party_banvd polity2 prsystem lnoilcap lnpop, output

xtserial fd_n_attacks partybaniep prsystem  lnoilcap lnpop


***************************************************************************
**************************** First Difference *****************************
***************************************************************************

* Mark no missing data
mark nomiss 
markout nomiss n_attacks dd_cga fd_n_attacks partybanuni partybaniep party_banvd polity2 unitary prsystem lnoilcap lnpop newethfrac


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

xtpcse fd_n_attacks L.partybaniep polity2 unitary prsystem D.lnoilcap lnpop D.newethfrac if nomiss==1,  pairwise correlation(ar1)
xtpcse fd_n_attacks L.party_banvd polity2 unitary prsystem D.lnoilcap lnpop D.newethfrac if nomiss==1 ,  pairwise correlation(ar1)


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


xtregar n_attacks L.n_attacks L.partybanuni L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1 ,  fe
xtpcse n_attacks L.n_attacks L.partybanuni L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1 ,   pairwise correlation(ar1)
xtreg n_attacks L.n_attacks L.partybanuni L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1 

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

xtnbreg n_attacks L.n_attacks L.partybaniep L.polity2 L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1,  fe
xtnbreg n_attacks L.n_attacks L.party_banvd L.polity2 L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1 ,  fe
* Combined partybans
xtnbreg n_attacks L.n_attacks L.partybanuni L.polity2 L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1 ,  fe

* Models with n_attacks (With Political, Economic, and Demographic Controls, and DD):
xtnbreg n_attacks L.n_attacks L.partybaniep L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1,  fe
xtnbreg n_attacks L.n_attacks L.party_banvd L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss==1 ,  fe


*****************************************************************
**************************** Dummy ******************************
*****************************************************************

* Mark no missing data
mark nomiss2 
markout nomiss2 d_civtot d_civtot dd_cga partybaniep party_banvd polity2 unitary prsystem lnoilcap lnpop newethfrac

* Models with d_civtot (With Political, Economic, and Demographic Controls):
xtlogit d_civtot L.d_civtot L.partybaniep L.polity2 L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1,  fe
xtlogit d_civtot L.d_civtot L.party_banvd L.polity2 L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1 ,  fe

* Combined partybans
xtlogit d_civtot L.d_civtot L.partybanuni L.polity2 L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1 ,  fe

* With DD
xtlogit d_civtot L.d_civtot L.partybaniep L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1,  fe
xtlogit d_civtot L.d_civtot L.party_banvd L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1 ,  fe

* Combined partybans
xtlogit d_civtot L.d_civtot L.partybanuni L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1 ,  fe


**** Order Logit
xtologit civtot L.civtot L.partybaniep L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1
xtologit civtot L.civtot L.party_banvd L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1

* Combined partybans
xtologit civtot L.civtot L.partybanuni L.dd_cga L.unitary L.prsystem L.lnoilcap L.lnpop L.newethfrac if nomiss2==1



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


*******************************************

mark nomiss 
markout nomiss n_attacks partybanuni partybanuni partybaniep party_banvd dd_cga unitary prsystem lnoilcap lnpop newethfrac


tsset cow_code year

gen oilcap = oilHM/popHM
gen lnoilcap = ln(oilcap+0.0000001)

gen lag_attack = L.n_attacks
gen lag_partybanUni = L.partybanUni
gen lag_partybanIEP = L.partybanIEP
gen lag_party_banVD = L.party_banVD

destring party_banvd, replace force

by cow_code: gen cum_attacks=sum(n_attacks)

* Combined partybans FE
xtnbreg n_attacks lag_attack  lag_partybanUni dd_cga unitary PRsystem lnoilcap NewEthFrac, irr  fe

xtnbreg n_attacks lag_attack  L.party_banvd dd_cga unitary prsystem lnoilcap lnpop newethfrac ,  irr fe

xtnbreg n_attacks lag_attack  L.partybaniep dd_cga unitary prsystem lnoilcap lnpop newethfrac ,  irr fe
estimates store femodel
* Combined partybans RE
xtnbreg n_attacks lag_attack  lag_partybanUni dd_cga unitary PRsystem lnoilcap NewEthFrac, irr  re

xtnbreg n_attacks L.n_attacks  L.party_banvd dd_cga unitary prsystem lnoilcap lnpop newethfrac , irr re

xtnbreg n_attacks L.n_attacks partybaniep L.partybaniep dd_cga unitary prsystem lnoilcap lnpop newethfrac , irr re


hausman femodel .

xtregar n_attacks L.n_attacks partybanuni L.partybanuni dd_cga unitary prsystem lnoilcap lnpop newethfrac,  fe

reg n_attacks partybanuni

gen dcum_attacks = D.cum_attacks

xtserial n_attacks lag_attack   lag_partybanUni dd_cga unitary PRsystem lnoilcap NewEthFrac
