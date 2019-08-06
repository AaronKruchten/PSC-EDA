library(influxdbr)
library(testit)
library(sets)

#flow_name should be the name of the flow as a string

#measurements shoud be a character array/string of comma separated measurements with no space
#GOOD Example: "EarlyRetrans,CountRTT,ECN"

#database is a string of the database name we are considering

#function returns a time series data frame for the given measurments. Removes all rows with Just NAs. Rounds time to nearest minute and
#fills in NA if we have missing data for a measurement for a given time. 
query_by_flow <- function(flow_name,measurements,database){
  #create and call query
  begin_query = "Select value FROM "
  middle_query = " WHERE flow = "
  flow_name = paste("'",flow_name,"'",sep="")
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
  print(typeof(min_time))
  print(typeof(max_time))
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
      assert(j <= length(time_values))
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


#returns a list of the amount of octets transferred by different flows. find most with which.max(output of function)
#lst should be a list of the flow names 
#db is a string of the database name
find_most_data_transfers <- function(lst,db){
  total_vector = c()
  nrow_vector = c()
  for(i in 1:length(lst)){
    new_frame = query_by_flow(lst[i],"HCDataOctetsOut",db)
    total = sum(as.numeric(as.character(new_frame$HCDataOctetsOut)))
    n_rows = nrow(new_frame)
    total_vector[i] = total
    nrow_vector[i] = n_rows
  }
  new_frame = data.frame(amount_of_data = total_vector,number_of_rows = nrow_vector)
  return(new_frame)
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
impute_frame_query_by_flow <- function(df,k){
  imputed_frame = data.frame(df)
  for(i in 1:ncol(df)){
    current_col = df[,i]
    for(j in 1:length(current_col)){
      if(is.na(current_col[j])){
        #Impute function helper
        new_value = impute_vector_query_by_flow(current_col,j,k)
        current_col[j] = new_value
      }
    }
    imputed_frame[,i] = current_col
  }
  return(imputed_frame)
}

#imputes a single vector using a nearest neighbors like implementation. helper for impute frame function
impute_vector_query_by_flow <- function(vector,index,k){
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
  lst = number_of_levels_vector
  names(lst) = colnames(df)
  return(lst)
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
  assert(number_of_levels(new_frame) >= 2)
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

#returns a data frame with x rows that contains the flow name and the amount of data transferred. 
#because of the large size of our data set this function does not guarantee it returns the flow with the most data transferred however
#it returns flows that are likely to have sent out a lot of data
return_globus_flows_with_most_data <- function(x){
  
  all_flows_br34_commands <- read.csv("/Users/aaronkruchten/Desktop/globus flows br34/flow commands br34.csv")
  
  all_flows_br34_globus_command = subset.data.frame(all_flows_br34_commands,all_flows_br34_commands$command.value == "sshd")
  
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

#convert a data frame to numeric
convert_to_numeric <- function(df){
  time = df$Time
  df$Time = c()
  for(i in 1:ncol(df)){
    df[,i] = as.numeric(as.character(df[,i]))
  }
  df$Time = time
  return(df)
}

#given a data frame that contains DataOctetsOut, DataSegsOut, OctetsRetrans,SegsRetrans or any subset of these variables
#computes the difference between these if applicable and returns a new frame
#also OctetsRetrans and SegsRetrans are both cumulative. We transform them so that they are not.
no_retransmitted_data <- function(df){
  new_df = data.frame(df)
  if(length(new_df$HCDataOctetsOut) > 0 & length(new_df$OctetsRetrans) > 0){
    new_df$CleanOctetsOut = new_df$HCDataOctetsOut - new_df$OctetsRetrans
  }
  if(length(new_df$DataSegsOut) > 0 & length(new_df$SegsRetrans) > 0){
    new_df$CleanSegsOut = new_df$DataSegsOut - new_df$SegsRetrans
  }
  remove_rows_vector = c()
  remove_rows_index = 1
  #for(i in 1:nrow(new_df)){
   # if((!is.na(new_df$CleanSegsOut[i]) & new_df$CleanSegsOut[i] < 0) | (!is.na(new_df$CleanOctetsOut[i]) & new_df$CleanOctetsOut[i] <0)){
    #  remove_rows_vector[remove_rows_index] = i
     # remove_rows_index = remove_rows_index + 1
  #  }
  #  if(length(remove_rows_vector) >= 1 ){
   #   new_df = new_df[-remove_rows_vector,]
    #}
  #}
  return(new_df)
}

explore_data_transfers <- function(flow_name,db){
  data_tranfer_measurements = "DataOctetsOut,DataSegsOut,OctetsRetrans,SegsRetrans"
  data_transfer_frame = congestion_frame = query_by_flow(flow_name,data_tranfer_measurements,database = db)
  better_data_transfer_frame = no_retransmitted_data(data_transfer_frame)
  plot(better_data_transfer_frame$Time,better_data_transfer_frame$DataOctetsOut,xlab = "Time",main = "Data Octets Out")
  plot(better_data_transfer_frame$Time,better_data_transfer_frame$DataSegsOut,xlab = "Time",main = "Data Segs Out")
  plot(better_data_transfer_frame$Time,better_data_transfer_frame$CleanOctetsOut,xlab = "Time",main = "Data Octets Out That Are Not Retransmitted Data")
  plot(better_data_transfer_frame$Time,better_data_transfer_frame$CleanSegsOut,xlab = "Time",main = "Data Segments Out That Are Not Retransmitted Data")
}
