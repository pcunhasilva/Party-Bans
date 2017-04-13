# Extract polityIV code
QoGcode <- ddply(.data = smalldata_qgov, .variables = c("country_name"), 
      .fun = summarize, cow_code = first(cow_code), 
      countryname = first(country_name))

# Add codes to terrorism data
small_gtd <- merge(small_gtd, QoGcode, by.x = "country", by.y = "country_name", all.x = TRUE)
small_gtd$cow_code[small_gtd$country=="Cyprus"] <- 196
small_gtd$cow_code[small_gtd$country=="Bosnia-Herzegovina"] <- 70
small_gtd$cow_code[small_gtd$country=="East Germany (GDR)"] <- 265
small_gtd$cow_code[small_gtd$country=="Ethiopia"] <- 530
small_gtd$cow_code[small_gtd$country=="Falkland Islands"] <- NA
small_gtd$cow_code[small_gtd$country=="France"] <- 220
small_gtd$cow_code[small_gtd$country=="Guadeloupe"] <- NA
small_gtd$cow_code[small_gtd$country=="International"] <- NA
small_gtd$cow_code[small_gtd$country=="Ivory Coast"] <- 437
small_gtd$cow_code[small_gtd$country=="Kosovo"] <- NA
small_gtd$cow_code[small_gtd$country=="Malaysia"] <- 820
small_gtd$cow_code[small_gtd$country=="Martinique"] <- NA
small_gtd$cow_code[small_gtd$country=="New Caledonia"] <- NA
small_gtd$cow_code[small_gtd$country=="North Korea"] <- 731
small_gtd$cow_code[small_gtd$country=="Pakistan"] <- 770
small_gtd$cow_code[small_gtd$country=="Rhodesia"] <- NA
small_gtd$cow_code[small_gtd$country=="South Korea"] <- 732
small_gtd$cow_code[small_gtd$country=="South Yemen"] <- 680
small_gtd$cow_code[small_gtd$country=="Soviet Union"] <- NA
small_gtd$cow_code[small_gtd$country=="St. Kitts and Nevis"] <- 60 
small_gtd$cow_code[small_gtd$country=="Sudan"] <- 625
small_gtd$cow_code[small_gtd$country=="West Bank and Gaza Strip"] <- NA
small_gtd$cow_code[small_gtd$country=="West Germany (FRG)"] <- 260
small_gtd$cow_code[small_gtd$country=="Western Sahara"] <- NA


# Recounting the number of terrorist attacks
small_gtd <- ddply(.data = small_gtd, .variables = c("year", "cow_code"), 
                        .fun = summarize, n_attacks = sum(n_attacks))
