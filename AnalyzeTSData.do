set more off

* Set Directory
cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/Analysis Files"

* Load data
import delimited "dataFinal.csv", colrange(2) clear

* Generate country code
encode cabr, gen(cabrnew)
 
* Convert strings to numeric and deal with NA's
foreach i of varlist polity2 civviol civtot ethviol oil_gas_valuepop_2000 pop_maddison n_attacks ethfrac banethnic banrelig bansys banall lelecsystem{
	destring `i', replace force 
	} 
	
* Finding duplicates
duplicates tag cabrnew year, gen(isdup)
edit if isdup
drop if isdup==1
	
* Set TSCS 
tsset cabrnew year

* Unit Root Tests
xtfisher civtot, lags(2) 
xtfisher n_attacks, lags(2) 
xtfisher polity2, lags(2)
xtfisher n_attacks, lags(2)
xtfisher ethfrac, lags(2) // Has unit root

* Gen ln variables
gen ln_oil = ln(oil_gas_valuepop_2000 + 1)
gen ln_pop = ln(pop_maddison + 1)

* Models with civtot:
xtnbreg civtot banall polity2 i.lelecsystem, fe
xtnbreg civtot banall polity2 i.lelecsystem ethfrac, fe
xtnbreg civtot banall polity2 i.lelecsystem ln_oil ln_pop, fe
xtnbreg civtot banall polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, fe
xtnbreg civtot banall polity2 i.lelecsystem ethfrac ln_oil ln_pop, re
* OLS
xtreg civtot banall polity2 i.lelecsystem ethfrac ln_oil ln_pop, fe

* Models with n_attacks:
xtnbreg n_attacks banall polity2 i.lelecsystem, fe
xtnbreg n_attacks banall polity2 i.lelecsystem ethfrac, fe
xtnbreg n_attacks banall polity2 i.lelecsystem ln_oil ln_pop, fe
xtnbreg n_attacks banall polity2 i.lelecsystem D.ethfrac ln_oil ln_pop, fe
* OLS
xtreg n_attacks banall polity2 i.lelecsystem ethfrac ln_oil ln_pop, fe
