library(influxdbr)
library(testit)
library(sets)

#directory is a string of the file directory where we want to read data
#measurements are the flow measurements we want in a string separated by commas
#example: "CurCwnd,CurRTO,FastRetran,Nagle,RetranThresh,SlowStart"
#directory should be filled with files created by query_all_single_flow

#EXAMPLE
#file_directory = "/Users/aaronkruchten/Desktop/test query"
#interesting_measurements = "CurCwnd,CurRTO,FastRetran,Nagle,RetranThresh,SlowStart,CongAvoid,DataOctetsOut,DataSegsOut,OctetsRetrans,SegsRetrans"
#result = form_dataframe(directory = file_directory,measurements = interesting_measurements)
#View(result)
form_dataframe <- function(directory,measurements){
  #search through directory and load in frames we want into a list
  files <- list.files(path = directory)
  measurement_vector <- strsplit(x = measurements,split = ",")[[1]]
  data_frame_lst = list()
  data_frame_lst_index = 1
  min_time_vector = c()
  max_time_vector = c()
  for(i in 1:length(files)){
    curr_measurement = get_measurement_name(files[i])
    if(curr_measurement %in% measurement_vector){
      file_directory = paste(directory,"/",files[i],sep = "")
      new_frame <- read.csv(file_directory)
      new_frame = unique(new_frame)
      data_frame_lst[[data_frame_lst_index]] = new_frame
      min_time_vector[data_frame_lst_index]  = format(min(as.POSIXct(new_frame$time)),'%Y-%m-%d %H:%M')
      max_time_vector[data_frame_lst_index] = format(max(as.POSIXct(new_frame$time)),'%Y-%m-%d %H:%M')
      data_frame_lst_index = data_frame_lst_index + 1
    }
  }
  
  #create new data frame that spans the entire time of our data
  one_minute = 60
  min_time = min(min_time_vector)
  max_time = max(max_time_vector)
  min_time = as.POSIXct(min_time)
  max_time = as.POSIXct(max_time) + one_minute
  time_values = format(seq(min_time,max_time,by = "min"),'%Y-%m-%d %H:%M')
  new_matrix = matrix(nrow = length(time_values),ncol = length(measurement_vector) + 1)
  new_matrix[,1] = time_values
  
  #fill in new matrix with data
  for(i in 2:ncol(new_matrix)){
    assert(i - 1 <= length(data_frame_lst))
    curr_frame = data_frame_lst[[i - 1]]
    data_frame_index = 1
    for(j in 1:length(time_values)){
      assert(data_frame_index <= nrow(curr_frame) + 1)
      assert(i <= length(time_values))
      assert(j <= nrow(new_matrix))
      assert(i <= ncol(new_matrix))
      if(data_frame_index <= nrow(curr_frame) & curr_frame$time[data_frame_index] == time_values[j]){
        new_matrix[j,i] = curr_frame[data_frame_index,3]
        data_frame_index = data_frame_index + 1
      }
      else{
        new_matrix[j,i] = NA
      }
    }
    data_frame_index = 1
  }
  #convert matrix to a dataframe and remove rows with all na
  names = append(c("time"),measurement_vector)
  final_frame = as.data.frame(new_matrix)
  colnames(final_frame) = names
  return(remove_na_rows(final_frame))
}


#takes in a file name of the type outputted by query_all_single_flow and finds get the measurement name from that string. 
#called in form_dataframe
get_measurement_name <- function(file_name){
  character_array = strsplit(file_name,split ="")[[1]]
  end_of_measurement = length(character_array) - 4
  beginning_of_measurement = NA
  for(i in 1:length(character_array)){
    if(character_array[i] == "_"){
      beginning_of_measurement = i + 1
    }
  }
  new_string = ""
  for(i in 1:length(character_array[beginning_of_measurement:end_of_measurement])){
    new_string = paste(new_string,character_array[beginning_of_measurement + i - 1],sep="")
  }
  return(new_string)
}


#helper function to remove rows that only have NAs
#called in form_dataframe
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
#all inputs are strings
#example: query_all_single_flow("e7a458186bf729ee4f2009a36c8b2dc42c329cd5f6ada1e902d80f601c4c2131",
#"ALL_PSC_br034.dmz.bridges.psc.edu","/Users/aaronkruchten/Desktop/test query",username = "database_username", password = "database_password")
query_all_single_flow <- function(flow_name,database,save_location,username,password){
  setwd(save_location)
  flow_name = paste("'",flow_name,"'",sep ="")
  begin_query = "Select value FROM "
  middle_query = " WHERE flow = "
  query_vector <- c("AbruptTimeouts","ActiveOpen","CERcvd","CongAvoid","CongSignals","CountRTT","CurAppRQueue","CurAppWQueue","CurCwnd","CurMSS",
                    "CurRTO","CurReasmQueue","CurRwinRcvd","CurRwinSent","CurSsthresh","CurTimeoutCount","DSACKDups","DataOctetsIn","DataOctetsOut",
                    "DataSegsIn","DataSegsOut","DupAckEpisodes","DupAcksIn","DupAcksOut","ECESent","ECN","ECNsignals","EarlyRetrans",
                    "EarlyRetransDelay","ElapsedMicroSecs","ElapsedSecs","EndTime","FastRetran","HCDataOctetsIn","HCDataOctetsOut","HCSumRTT",
                    "HCThruOctetsAcked","HCThruOctetsReceived","InRecovery","IpTosIn","IpTosOut","IpTtl","LimCwnd","LimMSS","MSSRcvd","MSSSent",
                    "MaxAppRQueue","MaxAppWQueue","MaxCaCwnd","MaxMSS","MaxPipeSize","MaxRTO","MaxRTT","MaxReasmQueue","MaxRwinRcvd","MaxRwinSent",
                    "MaxSsCwnd","MaxSsthresh","MinMSS","MinRTO","MinRTT","MinSsthresh","Nagle","NonRecovDA","NonRecovDAEpisodes","OctetsRetrans",
                    "OtherReductions","OtherReductionsCM","PipeSize","PostCongCountRTT","PostCongSumRTT","PreCongSumCwnd","PreCongSumRTT","Priority",
                    "RTTVar","RcvNxt","RcvRTT","RecInitial","RetranThresh","SACKBlocksRcvd","SACKsRcvd","SampleRTT","SegsIn","SegsOut","SegsRetrans",
                    "SendStall","SlowStart","SmoothedRTT","SndInitial","SndLimTimeCwnd","SndLimTimePace","SndLimTimeSnd","SndLimTimeStartUp",
                    "SndLimTimeTSODefer","SndLimTransCwnd","SndLimTransPace","SndLimTransRwin","SndLimTransSnd","SndLimTransStartUp","SndLimTransTSODefer",
                    "SndMax","SndNxt","SndUna","SoftErrorReason","SoftErrors","SpuriousFrDetected","SpuriousRtoDetected","StartTime","StartTimeStamp",
                    "State","SubsequentTimeouts","SumOctetsReordered","SumRTT","ThruOctetsAcked","ThruOctetsReceived","TimeStamps","Timeouts",
                    "WillSendSACK","WillUseSACK","WinScaleRcvd","WinScaleRcvd","WinScaleSent","ZeroRwinRcvd","ZeroRwinSent","analyzed","command",
                    "dest_ip","dest_port","path","src_ip","src_port")
  connection <- influx_connection(scheme = "http",host = "influx.blearndata.net",port = 8086,user = username,pass = password)
  for(i in 1:length(query_vector)){
    measurement = query_vector[i]
    query_string = paste(begin_query,measurement,middle_query,flow_name,sep="")
    new_query = influx_query(connection,db = database,query = query_string,return_xts = FALSE)
    new_frame = new_query[[1]]
    new_frame$statement_id = c()
    new_frame$series_names = c()
    new_frame$series_tags = c()
    new_frame$series_partial = c()
    new_frame$time = format(new_frame$time,'%Y-%m-%d %H:%M')
    file_name = paste(flow_name,"_",query_vector[i],".csv",sep="")
    write.csv(new_frame,file_name)
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

#https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#returns the mode of a vector. return list if there is more than one
#called in impute frame
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}