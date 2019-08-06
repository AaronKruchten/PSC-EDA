big_ass_random_sample <- read.csv("/Users/aaronkruchten/Desktop/Most Data flows br33/2019-07-25-15-52 Chronograf Data.csv")

flow_names <- names(sort(table(big_ass_random_sample$HCDataOctetsOut.flow),decreasing = TRUE)[1:10000])

find_most_data_transfers <- function(lst,db){
  total_vector = c()
  nrow_vector = c()
  for(i in 1:length(lst)){
    good_format_name = paste("'",lst[i],"'",sep = "")
    new_frame = query_by_flow(good_format_name,"DataOctetsOut",db)
    total = sum(as.numeric(new_frame$DataOctetsOut))
    total_vector[i] = total
    nrow_vector[i] = nrow(new_frame)
  }
  new_frame = data.frame(flow_names = lst,amount_of_data = total_vector,amount_of_rows = nrow_vector)
  return(new_frame)
}

data_transfers <- find_most_data_transfers(flow_names,"ALL_PSC_br034.dmz.bridges.psc.edu")
View(data_transfers)
setwd("/Users/aaronkruchten/Desktop/Most Data flows two")
write.csv(data_transfers,"/Users/aaronkruchten/Desktop/Most Data flows two/Most_data_flows_two.csv")

most_data_flow_og <- read.csv("/Users/aaronkruchten/Desktop/Most Data flows two/Most_data_flows_two.csv")


data_transfer_subset <- subset.data.frame(most_data_flow_og,most_data_flow_og$amount_of_rows > 100)
data_transfer_subset$flow_names = as.character(data_transfer_subset$flow_names)

measurements = "HCDataOctetsOut,OctetsRetrans,HCDataOctetsIn,CurMSS,PipeSize,CurRTO,CongSignals,CurCwnd,CurSsthresh,Timeouts,CurRwinSent,MaxRwinSent,ZeroRwinSent,CurRwinRcvd,MaxRwinRcvd,ZeroRwinRcvd,MaxCaCwnd"
library(influxdbr)
for(i in 1:1933){
  directory = "/Users/aaronkruchten/Desktop/PCA flows/"
  flow_name = data_transfer_subset$flow_names[i]
  directory = paste(directory,flow_name,sep = "")
  if(!file.exists(directory)){
    dir.create(directory)
    query_all_single_flow_arbitrary_measures(flow_name,"ALL_PSC_br034.dmz.bridges.psc.edu",measurements = measurements,username = "reader",password = "listen",save_location = directory)
  } else {
    query_all_single_flow_arbitrary_measures(flow_name,"ALL_PSC_br034.dmz.bridges.psc.edu",measurements = "MaxCaCwnd",username = "reader",password = "listen",save_location = directory)
  }
}

pca_files <- list.files("/Users/aaronkruchten/Desktop/PCA flows")
true_false = matrix(rep(NA,length(measurement_vector[[1]])*2049),nrow = 2049,ncol = length(measurement_vector[[1]]))
for(i in 1:length(measurement_vector[[1]])){
  print(i)
  for(j in 1:length(pca_files)){
    directory = paste("/Users/aaronkruchten/Desktop/PCA flows/",pca_files[j],sep = "")
    flow_name = paste("'",pca_files[j],"'",sep="")
    measurement_name = paste(flow_name,"_",measurement_vector[[1]][i],".csv",sep="")
    if(!(measurement_name %in% list.files(directory))){
      true_false[j,i] = TRUE
    } else {
      true_false[j,i] = FALSE
    }
  } 
}



