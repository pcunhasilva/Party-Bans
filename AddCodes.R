# Extract polityIV code
polIVcode <- ddply(.data = polityIVFinal, .variables = c("country"), 
      .fun = summarize, acode = first(scode), ccode = first(ccode))

# Add codes to terrorism data
tpcFinal <- merge(tpcFinal, polIVcode, by = "country", all.x = TRUE)
tpcFinal$acode[tpcFinal$country=="Andorra"] <- "AND"
tpcFinal$acode[tpcFinal$country=="Antigua and Barbuda"] <- "ATG"
tpcFinal$acode[tpcFinal$country=="Bahamas"] <- "BHS"
tpcFinal$acode[tpcFinal$country=="Barbados"] <- "BRB"
tpcFinal$acode[tpcFinal$country=="Belize"] <- "BLZ"
tpcFinal$acode[tpcFinal$country=="Bosnia-Herzegovina"] <- "BIH"
tpcFinal$acode[tpcFinal$country=="Brunei"] <- "BRN"
tpcFinal$acode[tpcFinal$country=="Corsica"] <- NA
tpcFinal$acode[tpcFinal$country=="Democratic Republic of the Congo"] <- "CON"
tpcFinal$acode[tpcFinal$country=="Dominica"] <- "DMA"
tpcFinal$acode[tpcFinal$country=="East Germany (GDR)"] <- "GDR"
tpcFinal$acode[tpcFinal$country=="Falkland Islands"] <- "FLK"
tpcFinal$acode[tpcFinal$country=="French Guiana"] <- "GUF"
tpcFinal$acode[tpcFinal$country=="French Polynesia"] <- "PYF"
tpcFinal$acode[tpcFinal$country=="Gibraltar"] <- "GIB"
tpcFinal$acode[tpcFinal$country=="Great Britain"] <- "UKG"
tpcFinal$acode[tpcFinal$country=="Grenada"] <- "GRD"
tpcFinal$acode[tpcFinal$country=="Guadeloupe"] <- "GLP"
tpcFinal$acode[tpcFinal$country=="Hong Kong"] <- "HKG"
tpcFinal$acode[tpcFinal$country=="Iceland"] <- "ISL"
tpcFinal$acode[tpcFinal$country=="Macau"] <- "CHN"
tpcFinal$acode[tpcFinal$country=="Maldives"] <- "MDV"
tpcFinal$acode[tpcFinal$country=="Martinique"] <- "MTQ"
tpcFinal$acode[tpcFinal$country=="Myanmar"] <- "MMR"
tpcFinal$acode[tpcFinal$country=="New Caledonia"] <- "NCL"
tpcFinal$acode[tpcFinal$country=="New Hebrides"] <- NA
tpcFinal$acode[tpcFinal$country=="North Korea"] <- "PRK"
tpcFinal$acode[tpcFinal$country=="North Yemen"] <- "YAR"
tpcFinal$acode[tpcFinal$country=="Northern Ireland"] <- "UKG"
tpcFinal$acode[tpcFinal$country=="People's Republic of the Congo"] <- "CON"
tpcFinal$acode[tpcFinal$country=="Republic of the Congo"] <- "ZAI"
tpcFinal$acode[tpcFinal$country=="Rhodesia"] <- NA
tpcFinal$acode[tpcFinal$country=="Serbia-Montenegro"] <- "SER"
tpcFinal$acode[tpcFinal$country=="South Korea"] <- "ROK"
tpcFinal$acode[tpcFinal$country=="South Vietnam"] <- "RVN"
tpcFinal$acode[tpcFinal$country=="South Yemen"] <- "YPR"
tpcFinal$acode[tpcFinal$country=="Soviet Union"] <- "USR"
tpcFinal$acode[tpcFinal$country=="St. Lucia"] <- NA
tpcFinal$acode[tpcFinal$country=="Vanuatu"] <- NA
tpcFinal$acode[tpcFinal$country=="West Germany (FRG)"] <- "GFR"
tpcFinal$acode[tpcFinal$country=="Zaire"] <- "ZAI"

# Rename code variable of terrorism data
names(tpcFinal)[5] <- "acode"

# Remove countries with acode = NA from the data:
tpcFinal <- tpcFinal[!is.na(tpcFinal),]

# Recounting the number of terrorist attacks
tpcFinal <- ddply(.data = tpcFinal, .variables = c("year", "acode"), 
             .fun = summarize, n_attacks = sum(n_attacks))
