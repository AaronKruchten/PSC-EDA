library(influxdbr)
library(testit)


#flow_name should be the name of the flow in single quotes and double quotes
#GOOD Example: "'flow_name'" 
#BAD Example: "flow_name"
#BAD Example: 'flow_name'

#measurements shoud be a character array/string of comma separated measurements with no space
#GOOD Example: "EarlyRetrans,CountRTT,ECN"

#database is a string of the database name we are considering

#it looks like from the data flows are often measured about once a minute. The way this function is written it will break if we have 
#two measurements in the same minute. For that reason we remove one of the measurements that occur twice in one minute. 
#I believe these occurr very rarely and we are not losing much data by doing this. We could solve this problem in a couple other ways 
#but I believe this would lead to either a much less time efficient function or a resulting data frame that is much larger than necessary.


#function returns a time series data frame for the given measurments. Removes all rows with Just NAs
query_by_flow <- function(flow_name,measurements,database){
  #create and call query
  begin_query = "Select value FROM "
  middle_query = " WHERE flow = "
  query_string = paste(begin_query,measurements,middle_query,flow_name,sep="")
  connection <- influx_connection(scheme = "http",host = "influx.blearndata.net",port = 8086,user = "reader",pass = "listen")
  new_query = influx_query(connection,db = database,query = query_string,return_xts = FALSE)
  
  #initialize data frame
  data_frame = new_query[[1]]
  data_frame$statement_id = c()
  data_frame$series_tags = c()
  data_frame$series_partial = c()
  
  
  #create time series skipping by minute for oldest and most recent time observed in our data
  min_time = min(data_frame$time)
  max_time = max(data_frame$time)
  time_values = format(seq(min_time,max_time+60,by = "min"),'%Y-%m-%d %H:%M')
  
  
  #remove duplicates
  data_frame$time = format(data_frame$time,'%Y-%m-%d %H:%M')
  data_frame = unique(data_frame)
  
  #parse measurments and create new matrix
  measurement_vector <- strsplit(x = measurements,split = ",")[[1]]
  number_of_time_measuresments = length(time_values)
  number_of_measurements = length(measurement_vector)
  new_matrix = matrix(nrow = length(time_values),ncol = length(measurement_vector) + 1)
  new_matrix[,1] = time_values
  data_frame_index = 1
  
  #fill in the new matrix. Puts in NA if we don't have a measurement for that minute
  for(i in 2:(number_of_measurements +1)){
    for(j in 1:number_of_time_measuresments){
      #this assert will often fail if there are typos in the metrics inputted
      if(data_frame_index <= length(data_frame$time) && data_frame$time[data_frame_index] == time_values[j]){
        new_matrix[j,i] = data_frame$value[data_frame_index] 
        data_frame_index = data_frame_index + 1
      } else {
        new_matrix[j,i] = NA
      } 
    }
  }
  #convert matrix to data frame and name columns
  names = append(c("Time"),sort(measurement_vector, decreasing = FALSE))
  final_frame = as.data.frame(new_matrix)
  colnames(final_frame) = names
  return(remove_na_rows(final_frame))
}

#helper function to remove rows that only have NAs
remove_na_rows <- function(data_frame){
  drop_vector = c()
  drop_vector_index = 1
  for(i in 1:nrow(data_frame)){
    if(sum(is.na(data_frame[i,2:ncol(data_frame)])) == (ncol(data_frame) -1)){
      drop_vector[drop_vector_index] = i
      drop_vector_index = drop_vector_index + 1
    }
  }
  if(length(drop_vector) == 0){
    return(data_frame)
  } else {
    data_frame = data_frame[-drop_vector,]
    return(data_frame)
  }
}


#queries all measurements for a given flow and saves them into separate csv files into a location defined by the user
#same conventions as in query_by_flow
#save_location should be a string of a file location
query_all_single_flow <- function(flow_name,database,save_location){
  setwd(save_location)
  begin_query = "Select value FROM "
  middle_query = " WHERE flow = "
  query_vector <- c("AbruptTimeouts","ActiveOpen","CERcvd","CongAvoid","CongSignals","CountRTT","CurAppRQueue","CurAppWQueue","CurCwnd","CurMSS","CurRTO","CurReasmQueue","CurRwinRcvd","CurRwinSent","CurSsthresh","CurTimeoutCount","DSACKDups","DataOctetsIn","DataOctetsOut","DataSegsIn","DataSegsOut","DupAckEpisodes","DupAcksIn","DupAcksOut","ECESent","ECN","ECNsignals","EarlyRetrans","EarlyRetransDelay","ElapsedMicroSecs","ElapsedSecs","EndTime","FastRetran","HCDataOctetsIn","HCDataOctetsOut","HCSumRTT","HCThruOctetsAcked","HCThruOctetsReceived","InRecovery","IpTosIn","IpTosOut","IpTtl","LimCwnd","LimMSS","MSSRcvd","MSSSent","MaxAppRQueue","MaxAppWQueue","MaxCaCwnd","MaxMSS","MaxPipeSize","MaxRTO","MaxRTT","MaxReasmQueue","MaxRwinRcvd","MaxRwinSent","MaxSsCwnd","MaxSsthresh","MinMSS","MinRTO","MinRTT","MinSsthresh","Nagle","NonRecovDA","NonRecovDAEpisodes","OctetsRetrans","OtherReductions","OtherReductionsCM","PipeSize","PostCongCountRTT","PostCongSumRTT","PreCongSumCwnd","PreCongSumRTT","Priority","RTTVar","RcvNxt","RcvRTT","RecInitial","RetranThresh","SACKBlocksRcvd","SACKsRcvd","SampleRTT","SegsIn","SegsOut","SegsRetrans","SendStall","SlowStart","SmoothedRTT","SndInitial","SndLimTimeCwnd","SndLimTimePace","SndLimTimeSnd","SndLimTimeStartUp","SndLimTimeTSODefer","SndLimTransCwnd","SndLimTransPace","SndLimTransRwin","SndLimTransSnd","SndLimTransStartUp","SndLimTransTSODefer","SndMax","SndNxt","SndUna","SoftErrorReason","SoftErrors","SpuriousFrDetected","SpuriousRtoDetected","StartTime","StartTimeStamp","State","SubsequentTimeouts","SumOctetsReordered","SumRTT","ThruOctetsAcked","ThruOctetsReceived","TimeStamps","Timeouts","WillSendSACK","WillUseSACK","WinScaleRcvd","WinScaleRcvd","WinScaleSent","ZeroRwinRcvd","ZeroRwinSent","analyzed","command","dest_ip","dest_port","path","src_ip","src_port")
  connection <- influx_connection(scheme = "http",host = "influx.blearndata.net",port = 8086,user = "reader",pass = "listen")
  for(i in 1:length(query_vector)){
    measurement = query_vector[i]
    query_string = paste(begin_query,measurement,middle_query,flow_name,sep="")
    new_query = influx_query(connection,db = database,query = query_string,return_xts = FALSE)
    new_frame = new_query[[1]]
    new_frame$statement_id = c()
    new_frame$series_names = c()
    new_frame$series_tags = c()
    new_frame$series_partial = c()
    file_name = paste(flow_name,"_",query_vector[i],".csv",sep="")
    write.csv(new_frame,file_name)
  }
}

#returns a list of the amount of octets transferred by different flows. find most with which.max(output of function)
#lst should be a list of the flow names 
#db is a string of the database name
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
  new_frame = data.frame(amount_of_data = total_vector,amount_of_rows = nrow_vector)
  return(total_vector)
}

#query a flow and the measurements you want and write it to a csv to a chosen directory
query_interesting_flows <- function(flow_name,measurements,directory,database){
  setwd(directory)
  flow_file <- query_by_flow(flow_name,measurements,database)
  name = paste(flow_name,"important_measurements_br34",".csv",sep = "_")
  write.csv(flow_file,name)
}

#finds flows that are in both arrays, returns a vector of indices of the first array
search_for_flows <- function(array_one,array_two){
  index_vector = c()
  index = 1
  for(i in 1:length(array_one)){
    if(array_one[i] %in% array_two){
      index_vector[index] = i
      index = index + 1
    }
  }
  return(index_vector)
}


#takes a list of flow names (can be sorted or unsorted) and saves them to a file location defined by the user. 
write_interesting_flows <-function(sorted_names,upper_bound,location,db,measures){
  for(i in 1:length(sorted_names[1:upper_bound])){
    flow_name_string = paste("'",sorted_names[i],"'",sep ="")
    print(flow_name_string)
    query_interesting_flows(flow_name = flow_name_string,measurements = measures,location,db)
  }
}

#df = data frame
#k is number of neighbors
#imputes our time series data frame in a reasonable way
#note that there are many R packages for imputation but none would behave in a reasonable way for our data
impute_frame <- function(df,k){
  imputed_frame = data.frame(df)
  for(i in 1:ncol(df)){
    current_col = df[,i]
    for(j in 1:length(current_col)){
      if(is.na(current_col[j])){
        #Impute function helper
        new_value = impute_vector(current_col,j,k)
        current_col[j] = new_value
      }
    }
    imputed_frame[,i] = current_col
  }
  return(imputed_frame)
}

#imputes a single vector using a nearest neighbors like implementation. helper for impute frame function
impute_vector <- function(vector,index,k){
  #take k nearest neighbors
  #case one where the vector length is less than k
  
  if(length(vector) - sum(is.na(vector)) <= k){
    total = sum(na.omit(vector))
    divisor = length(vector) - sum(is.na(vector))
    new_value = total %/% divisor
    return(new_value)
  }
  else{
    lower_seq = seq(from = index  - 1,to = index - k %/% 2, by = -1)
    upper_seq = seq(from = index +1, to = index + k %/% 2, by = 1)
    max_value = max(upper_seq) + 1
    min_value = min(lower_seq) - 1
    for(i in 1:length(lower_seq)){
      if(lower_seq[i] < 1){
        lower_seq[i] = max_value
        max_value = max_value + 1
      }
    }
    for(i in 1:length(upper_seq)){
      if(upper_seq[i] > length(vector)){
        upper_seq[i] = min_value
        min_value = min_value - 1
      }
    }
    #heuristic use mean if we have a large number of levels in our selection. Mode otherwise
    full_seq = append(upper_seq,lower_seq)
    if(length(table(vector[full_seq])) > k %/% 4){
      number_of_nas = sum(is.na(vector[full_seq]))
      new_value = sum(na.omit(vector[full_seq]))/(k - number_of_nas)
    } else {
      new_value = Mode(vector[full_seq])[1]
    }
    return(new_value)
  }
}

#computes the number of levels for each column in a data frame. 
number_of_levels <- function(df){
  number_of_levels_vector = c()
  for(i in 1:ncol(df)){
    number_of_levels_vector[i] = length(table(df[,i]))
  }
  return(number_of_levels_vector)
}

#transforms frame into soemthing that can hopefully be used with pcalg/corGraph
#df is a dataframe 
#k is k nearest neighbors for imputation by column
transform_frame_pcalg <- function(df,k){
  new_df = impute_frame(df,k)
  number_of_levels_vector = number_of_levels(new_df)
  remove_vector = c()
  index = 1
  for(i in 1:length(number_of_levels_vector)){
    if(number_of_levels_vector[i] < 2){
      remove_vector[index] = i
      index = index + 1
    }
  }
  new_frame = new_df[,-remove_vector]
  print(number_of_levels(new_frame))
  return(new_frame)
}

#transform data frame into a frame that can be used by tetradrunner to form a causal graph
transform_frame_tetradrunner <- function(df,k){
  if(length(df$Timeouts) > 0){
    if(sum(is.na(df$Timeouts)) == nrow(df)){
      print("here")
      df$Timeouts = rep(0,nrow(df))
    }
  }
  copy_frame = remove_na_cols(df)
  copy_frame$Time = c()
  for(i in 1:ncol(copy_frame)){
    copy_frame[,i] = as.numeric(as.character(copy_frame[,i]))
  }
  if(sum(is.na(copy_frame)) == 0){
    return(copy_frame)
  } else {
    copy_frame = impute_frame(copy_frame,k)
    return(copy_frame)
  }
}

#remove any column in the data set that is all NA's
remove_na_cols <- function(df){
  copy_frame = data.frame(df)
  for(i in 1:ncol(copy_frame)){
    if(sum(is.na(copy_frame[,i])) == nrow(copy_frame)){
      copy_frame[,i] = c()
    }
  }
  return(copy_frame)
}

#plots histograms of each column
plot_frame <- function(df){
  for(i in 1:ncol(df)){
    hist(df[,i])
  }
}


#convert adjaceny matrix of type amat to a standard matrix type. 
#standard matrix type can then be turned into a type of graphNEL
convert_adjaceny_to_matrix <- function(adj_matrix){
  nrows = dim(adj_matrix)[1]
  ncols = dim(adj_matrix)[2]
  new_mat = matrix(nrow = nrows,ncol = ncols)
  for(i in 1:nrows){
    new_mat[i,] = adj_matrix[i,]
  }
  return(new_mat)
}

#https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#returns the mode of a vector. return list if there is more than one
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

return_globus_flows_with_most_data <- function(){
  
  all_flows_br34_commands <- read.csv("/Users/aaronkruchten/Desktop/globus flows br34/flow commands br34.csv")
  
  all_flows_br34_globus_command = subset.data.frame(all_flows_br34_commands,all_flows_br34_commands$command.value == "globus-gridftp-")
  
  all_flows_br34_globus_command_lst = as.character(all_flows_br34_globus_command$command.flow)
  
  random_sample_of_flows <- read.csv("/Users/aaronkruchten/Desktop/globus flows br34/random sample of flows with dataoctets out.csv")
  sort(table(random_sample_of_flows$DataOctetsOut.sample),decreasing = TRUE)
  
  common_occuring_flows = names(sort(table(random_sample_of_flows$DataOctetsOut.sample),decreasing = TRUE))[1:1000]
  
  interesting_flow_vector = search_for_flows(all_flows_br34_globus_command_lst,common_occuring_flows)
  interesting_flows = all_flows_br34_globus_command_lst[interesting_flow_vector]
  
  #currently set to return 100 flows that return the most data
  top_hundred_interesting_flows <- interesting_flows[1:100]
  
  most_data_transfers = find_most_data_transfers(top_hundred_interesting_flows,"ALL_PSC_br034.dmz.bridges.psc.edu")
  return(most_data_transfers)
}

