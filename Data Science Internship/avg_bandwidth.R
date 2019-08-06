f <- form_dataframe("/Users/aaronkruchten/Desktop/bd4d85723b4f141d96cde95c6aed3941f25ec1624ff9ecfe46bf8d0d6156e436","HCDataOctetsOut")
View(f)
avg_RTT_per_min = 15*60
f$HCDataOctetsOut <- as.numeric(as.character(f$HCDataOctetsOut))
f <- compute_diff(f,2)
f$ThroughputPerRoundTrip = f$HCDataOctetsOutDiff/avg_RTT_per_min
