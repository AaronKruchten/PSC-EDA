View(most_data_flow)
sorted_data <- sort(most_data_flow$amount_of_data, decreasing = TRUE)
for(i in 1:)

  
flow_commands <- read.csv("/Users/aaronkruchten/Desktop/flow_commands/2019-06-19-11-37 Chronograf Data.csv")
flows_gsishd <- subset.data.frame(flow_commands, command.value == "gsisshd")
csv_name = "gsisshd flows br34.csv"
filename = paste("/Users/aaronkruchten/Desktop/flow_commands/",csv_name,sep ="")
write.csv(flows_gsishd,file = filename)

return_globus_flows_with_most_data <- function(x){
  
  all_flows_br34_commands <- read.csv("/Users/aaronkruchten/Desktop/globus flows br34/flow commands br34.csv")
  
  all_flows_br34_globus_command = subset.data.frame(all_flows_br34_commands,all_flows_br34_commands$command.value == "gsisshd")
  
  all_flows_br34_globus_command_lst = as.character(all_flows_br34_globus_command$command.flow)
  
  random_sample_of_flows <- read.csv("/Users/aaronkruchten/Desktop/globus flows br34/HC data octets out.csv")
  
  common_occuring_flows = names(sort(table(random_sample_of_flows$HCDataOctetsOut.sample),decreasing = TRUE))
  interesting_flow_vector = search_for_flows(all_flows_br34_globus_command_lst,common_occuring_flows)
  interesting_flows = all_flows_br34_globus_command_lst[interesting_flow_vector][1:x]
  
  
  most_data_transfers = find_most_data_transfers(interesting_flows,"ALL_PSC_br034.dmz.bridges.psc.edu")
  new_frame = data.frame(flow_names = interesting_flows)
  final_frame = cbind(new_frame,most_data_transfers)
  return(final_frame)
}

gssishd_frame_flows <- return_globus_flows_with_most_data(250)
filename = paste("/Users/aaronkruchten/Desktop/Most Data flows/","gssishd_flows.csv",sep= "")
write.csv(gssishd_frame_flows,filename)

which.max(gssishd_frame_flows$amount_of_data)


