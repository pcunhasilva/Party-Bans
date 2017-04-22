set more off

* Set Directory
cd "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/Analysis Files"

* Load data
use dataRegressions, clear

tsset cow_code year


* Models


* Combined partybans FE
xtnbreg n_attacks lag_attack  lag_partybanUni dd_cga unitary PRsystem lnoilcap NewEthFrac,  fe

xtnbreg n_attacks lag_attack lag_partybanIEP dd_cga unitary PRsystem lnoilcap  NewEthFrac ,  fe

xtnbreg n_attacks lag_attack  lag_party_banVD dd_cga unitary PRsystem lnoilcap  NewEthFrac ,  fe

 lag_party_banVD
* Combined partybans RE
xtnbreg n_attacks lag_attack  lag_partybanUni dd_cga unitary PRsystem lnoilcap NewEthFrac,   re

xtnbreg n_attacks L.n_attacks  lag_partybanIEP dd_cga unitary PRsystem lnoilcap  NewEthFrac ,  re

xtnbreg n_attacks L.n_attacks lag_party_banVD  dd_cga unitary PRsystem lnoilcap  NewEthFrac ,  re
