#in this file we will compute the mean probability of packet loss between globus flows and gssishd flows
globus_most_data_flow <- read.csv("/Users/aaronkruchten/Desktop/Most Data flows/most_data_flows globus.csv")
gssishd_most_data_flow <- read.csv("/Users/aaronkruchten/Desktop/Most Data flows/gssishd_flows.csv")
sshd_most_data_flow <- read.csv("/Users/aaronkruchten/Desktop/Most Data flows/sshd_most_data_flows.csv")

sorted_globus <- sort(globus_most_data_flow$amount_of_data, decreasing = TRUE)
sorted_gssishd <- sort(gssishd_most_data_flow$amount_of_data, decreasing = TRUE)
sorted_sshd <- sort(sshd_most_data_flow$amount_of_data,decreasing = TRUE)

database = "ALL_PSC_br034.dmz.bridges.psc.edu"
measures = "HCDataOctetsOut,OctetsRetrans,HCDataOctetsIn,ElapsedSecs,CurMSS,PipeSize,MaxPipeSize,
CurRTO,Congsignals,CurCwnd,CurSsthresh,Timeouts,CurRwinSent,MaxRwinSent,ZeroRwinSent,CurRwinRcvd,MaxRwinRcvd,ZeroRwinRcvd"


for(i in 1:400){
  flow_index_globus = which(globus_most_data_flow$amount_of_data == sorted_globus[i])
  flow_index_gssishd = which(gssishd_most_data_flow$amount_of_data == sorted_gssishd[i])
  flow_index_sshd = which(sshd_most_data_flow$amount_of_data == sorted_sshd[i])
  globus_flow = globus_most_data_flow$flow_names[flow_index_globus]
  gssishd_flow = gssishd_most_data_flow$flow_names[flow_index_gssishd]
  sshd_flow = sshd_most_data_flow$flow_names[flow_index_sshd]
  globus_save_location = paste("/Users/aaronkruchten/Desktop/globus flows/",globus_flow,sep="")
  gssishd_save_location = paste("/Users/aaronkruchten/Desktop/gsisshd flow/",gssishd_flow,sep ="")
  sshd_save_location = paste("/Users/aaronkruchten/Desktop/sshd flow/",sshd_flow, sep = "")
  if(!file.exists(globus_save_location[1])){
    dir.create(globus_save_location[1])
    query_all_single_flow(globus_flow[1],database,save_location = globus_save_location,username = "reader",password = "listen")
  }
  if(!file.exists(gssishd_save_location[1])){
    dir.create(gssishd_save_location[1])
    query_all_single_flow(gssishd_flow[1],database,save_location = gssishd_save_location[1], username = "reader", password = "listen")
  }
  if(!file.exists(sshd_save_location[1])){
    dir.create(sshd_save_location[1])
    query_all_single_flow(sshd_flow[1],database,save_location = sshd_save_location[1],username = "reader", password = "listen")
  }
}

mean_p_vector_gsisshd = c()
mean_p_vector_globus = c()
mean_p_vector_sshd = c()

gsisshd_files <- list.files(path = "/Users/aaronkruchten/Desktop/gsisshd flow")
globus_files <- list.files(path = "/Users/aaronkruchten/Desktop/globus flows")
sshd_files <- list.files(path = "/Users/aaronkruchten/Desktop/sshd flow")

number_of_files = min(length(gsisshd_files),length(globus_files),length(sshd_files))

library(IDPmisc)

for(j in 1:number_of_files){
  globus_directory = "/Users/aaronkruchten/Desktop/globus flows/"
  gssishd_directory = "/Users/aaronkruchten/Desktop/gsisshd flow/"
  sshd_directory = "/Users/aaronkruchten/Desktop/sshd flow/"
  globus_file_directory = paste(globus_directory,globus_files[j],sep = "")
  gsisshd_file_directory= paste(gssishd_directory,gsisshd_files[j],sep = "")
  sshd_file_directory = paste(sshd_directory,sshd_files[j], sep = "")
  mean_p_vector_globus[j] = compute_mean_p(globus_file_directory)
  mean_p_vector_gsisshd[j] = compute_mean_p(gsisshd_file_directory)
  mean_p_vector_sshd[j] = compute_mean_p(sshd_file_directory)
}

compute_mean_p <- function(directory){
  measures = "CurMSS,SmoothedRTT,HCDataOctetsOut,HCDataOctetsIn"
  print(directory)
  frame <- form_dataframe(directory,measurements = measures)
  imputed_frame <- impute_frame(frame,10)
  imputed_frame$bandwidth = imputed_frame$HCDataOctetsOut + imputed_frame$HCDataOctetsIn
  p = 1/ (sqrt(2/3) * imputed_frame$bandwidth * (imputed_frame$SmoothedRTT / imputed_frame$CurMSS) )^2
  good_indices = p <= 1
  p = p[good_indices]
  p_clean = NaRV.omit(p)
  return(mean(p_clean))
}

mean(na.omit(mean_p_vector_globus))
mean(na.omit(mean_p_vector_gsisshd))
mean(na.omit(mean_p_vector_sshd))



indices_globus = mean_p_vector_globus <= 1
clean_mean_p_vector_globus = na.omit(mean_p_vector_globus[indices_globus])

indices_gsisshd = mean_p_vector_gsisshd <= 1
clean_mean_p_vector_gsisshd = na.omit(mean_p_vector_gsisshd[indices_gsisshd])

indices_sshd = mean_p_vector_sshd <= 1
clean_mean_p_vector_sshd = na.omit(mean_p_vector_sshd[indices_sshd])

mean_one <- mean(na.omit(clean_mean_p_vector_globus))
mean_two <- mean(na.omit(clean_mean_p_vector_gsisshd))
mean_three <- mean(na.omit(clean_mean_p_vector_sshd))
mean(mean_one,mean_two,mean_three)


#we have found that the packet loss for gsisshd appears to be slightly larger than packet loss for globus
#histogram inspection initially shows that both distributions appear to have a similar distribution
#taking a log transform of both gsisshd appear to be close to log normal
#however log tranform of globus appears to be bimodal
#using a ks test it seems unlikely that they follow the same distribution. 

#if we wanted to formally figure out if there is a differene between these two means we may be able to use bootstrapping
#This may be worth looking into in the future. 








