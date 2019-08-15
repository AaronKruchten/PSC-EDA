source("/Users/aaronkruchten/Desktop/Data Science Internship/PSC interface github copy.R")
#code for the model described in the following paper
#http://www.ece.ubc.ca/~vincentw/C/BWLcGlobecom10.pdf


#adjusting the matrix so we can solve systems of equations for pi as decribed in the paper
adjust_matrix <- function(matrix){
  for(i in 1:nrow(matrix)){
     matrix[i,i] = matrix[i,i] -1
   }
  number_of_rows = nrow(matrix)
  last_row <- rep(1,nrow(matrix))
  matrix = t(matrix)
  matrix = rbind(matrix,last_row)
  return(matrix)
}

#D(x,y) function described in the paper
compute_diff <- function(frame,col_number){
  col_names = colnames(frame)
  difference = c(as.numeric(as.character(frame[,col_number][1])),diff(as.numeric(as.character(frame[,col_number]))))
  new_name = paste(col_names[col_number],"Diff",sep = "")
  new_col_number = ncol(frame) + 1
  new_names = c(col_names,new_name)
  frame[,new_col_number] = difference
  colnames(frame) = new_names
  return(frame)
}


#gets the correct measurments and creates the frame needed for the steady state model
create_frame <- function(flow_location){
  steady_state_frame <- form_dataframe(flow_location,measurements = "CongSignals,CurCwnd,MaxCaCwnd,SegsRetrans,SmoothedRTT")
  #first we partition range of congestion window size into N equal intervals
  steady_state_frame_numeric <- steady_state_frame
  steady_state_frame_numeric$time = c()
  steady_state_frame_numeric <- convert_to_numeric(steady_state_frame_numeric)

  W = max(steady_state_frame_numeric$CurCwnd[1])
  #we (somewhat) arbitrarily decide on 67 intervals
  #I should probably change this if we want it to work for more vectors but won't worry about it right now
  number_of_intervals = 67
  interval_size = W/number_of_intervals
  steady_state_frame_numeric$interval = trunc(steady_state_frame_numeric$CurCwnd/interval_size)
  # steady_state_frame_numeric$mapped_value = trunc(((steady_state_frame_numeric$interval/number_of_intervals*W) + (steady_state_frame_numeric$interval + 1)/number_of_intervals*W)/2)
  tmp_mat <- compute_diff(steady_state_frame_numeric,4)
  steady_state_frame_numeric$AverageAmountOfTimeBetweenPacketLoss <- tmp_mat$SegsRetransDiff/60
  steady_state_frame_numeric$RoundTripsPerMinute <- 60/(steady_state_frame_numeric$SmoothedRTT/1000)
  return(steady_state_frame_numeric)
}


#random packet loss if assumed to have a poisson distribution with mean lambda
#looking at the data this assumption does not appear to describe the model very well
#however a second assumption was that the time between packet loss is exponential with rate lambda
#this function estimates lambda from the data
compute_lambda <- function(frame){
  col_number_segs_retrans = 4
  frame <- compute_diff(frame,col_number_segs_retrans)
  one_minute = 60
  frame$averageAmountOfTimeBetweenPacketLoss <- frame$SegsRetransDiff/one_minute
  lambda = nrow(frame)/(sum(frame$averageAmount))
  return(lambda)
}


#computes cuberoot as one would expect. R returns NaN if negative for some reason
cuberoot <- function(x){
  if(x >= 0){
    return(x^(1/3))
  } else {
    return(-1*abs(x)^(1/3))
  }
}

#compute the x^~_k as described in the paper
#also returns 
mapped_value <- function(frame,cong_signals_index,number_of_partitions){
  RTT_mean_in_secs = mean(frame$SmoothedRTT)/1000
  RTT_per_sec = 1/RTT_mean_in_secs
  #bits per second
  W = frame$MaxCaCwnd[1]*8*RTT_per_sec
  N = number_of_partitions
  vector_length = tail(frame$CongSignals,n= 1)
  x_tilde_vector = rep(0,vector_length)
  partition_vector = rep(0,vector_length)
  index = 1
  frame <- compute_diff(frame,cong_signals_index)
  for(i in 1:length(frame$CongSignalsDiff)){
    sub_len = frame$CongSignalsDiff[i]
    interval = frame$interval[i]
    #this will probably break in the future so BEWARE
    computed_mapped_value = trunc((W*(interval)/N + W*(interval+1)/N)/2)
    if(sub_len > 0){
      for(j in 1:sub_len){
          partition_vector[index]  = frame$interval[i]
          x_tilde_vector[index] = computed_mapped_value
          index = index + 1
        }
      }
  }
  new_frame <- data.frame(x_tilde = x_tilde_vector,partition = partition_vector)
  return(new_frame)
}


 #w(x, τ) as described in the paper linked above
window_size_function <- function(x,r,beta,alpha){
  value = alpha*(r - cuberoot((1-beta)*x/alpha))^3 + x
  return(value)
}

#D(x,y) as described in the paper linked above
time_duration_function <- function(x,y,beta,alpha){
  first_part = cuberoot((y-x)/alpha)
  second_part = cuberoot((1-beta)*x/alpha)
  return(first_part + second_part)
}

#compute the P_i_j according to a system of equations as described in (15),(16), and (17)
compute_P_matrix <- function(frame,beta,alpha,number_of_partitions){
  RTT_mean_in_secs = mean(frame$SmoothedRTT)/1000
  RTT_per_sec = 1/RTT_mean_in_secs
  #bits per second
  W = frame$MaxCaCwnd[1]*8*RTT_per_sec
  vector_length = number_of_partitions
  data_vector = rep(0,vector_length*vector_length)
  P_matrix = matrix(data_vector,nrow = vector_length,ncol = vector_length)
  for(i in 1:vector_length){
    for(j in 1:(vector_length-1)) {
      P_matrix[i,j] = compute_P(i,j,beta,alpha,frame,number_of_partitions)
    }
  }
  row_sums = rowSums(P_matrix)
  last_col = 1 - row_sums
  P_matrix[,vector_length] = last_col
  print(rowSums(P_matrix))
  return(P_matrix)
}

#corresponds to (14) in the paper
compute_P <- function(i,j,beta,alpha,frame,number_of_partitions){
  RTT_mean_in_secs = mean(frame$SmoothedRTT)/1000
  RTT_per_sec = 1/RTT_mean_in_secs
  #bits per second
  W = frame$MaxCaCwnd[1]*8*RTT_per_sec
  N = number_of_partitions
  lambda = compute_lambda(frame)
  if(j < beta*(i - 0.5)){
    return(0)
  } else {
    a_i = (i - 0.5)*W/N
    second_input_min = (j-1)*W/N
    tau_i_j_min = max(time_duration_function(a_i,second_input_min,beta = beta,alpha = alpha),0)
    second_input_max = j*W/N
    tau_i_j_max = time_duration_function(a_i,second_input_max,beta = beta,alpha = alpha)
    P_i_j = exp(-lambda *(tau_i_j_min)) - exp(-lambda * tau_i_j_max)
    return(P_i_j)
  }
}

#compute s_i_j = a_i *tau_ik + a/4(tau_i_j - L)^4-L^4) with L = cuberoot((1-beta)a_i)/a )
# corresponds to (19) in the paper
compute_s <- function(i,j,frame,beta,alpha,number_of_partitions){
  RTT_mean_in_secs = mean(frame$SmoothedRTT)/1000
  RTT_per_sec = 1/RTT_mean_in_secs
  #bits per second
  W = frame$MaxCaCwnd[1]*8*RTT_per_sec
  N = number_of_partitions
  a_i = (i - 0.5)*W/N
  second_input = (j-0.5) * W/N
  tau_i_j = max(time_duration_function(a_i,second_input,beta = beta,alpha = alpha),0)
  tau_i_j_rounded = tau_i_j
  L = cuberoot( ((1-beta)*a_i)/alpha)
  first_addend = a_i*tau_i_j_rounded
  second_addend = (alpha/4) * ((tau_i_j_rounded - L)^4  - L^4)
  #print(first_addend + second_addend)
  return(first_addend + second_addend)
}

#corresponds to 21 in the paper
compute_average_throughput <- function(frame,beta,alpha,number_of_partitions){
  RTT_mean_in_secs = mean(frame$SmoothedRTT)/1000
  RTT_per_sec = 1/RTT_mean_in_secs
  #bits per second
  W = frame$MaxCaCwnd[1]*8*RTT_per_sec
  print(W)
  P_matrix = compute_P_matrix(frame,beta,alpha,number_of_partitions)
  View(P_matrix)
  P_matrix_adjusted = adjust_matrix(P_matrix)
  b = c(rep(0,nrow(P_matrix)),1)
  # View(P_matrix)
  # View(b)
  pi_vector = qr.solve(P_matrix_adjusted,b)
  pi_vector_two <-(P_matrix^25)[1,]
  View(pi_vector)
  numerator_sum = 0
  denominator_sum = 0
  curr_numerator_vector = c()
  curr_denominator_vector = c()
  index = 1
  s_i_j_sum = 0
  for(i in 1:nrow(P_matrix)){
    curr_pi = pi_vector[i] 
    smaller_numerator_sum = 0
    smaller_denominator_sum  = 0
    for(j in 1:nrow(P_matrix)){
      curr_s_i_j = compute_s(i,j,frame,beta = beta,alpha = alpha,number_of_partitions)
      s_i_j_sum = s_i_j_sum + curr_s_i_j
      a_i = (i - 0.5)*W/number_of_partitions
      second_input = (j-0.5) * W/number_of_partitions
      curr_tau_i_j = max(time_duration_function(a_i,second_input,beta = beta,alpha = alpha),0)
      curr_p_i_j = P_matrix[i,j]
      curr_numerator = curr_s_i_j*curr_p_i_j*curr_pi
      curr_denominator = curr_tau_i_j*curr_p_i_j*curr_pi
      smaller_numerator_sum = smaller_numerator_sum + curr_numerator
      smaller_denominator_sum= smaller_denominator_sum + curr_denominator
    }
    numerator_sum = numerator_sum + smaller_numerator_sum
    denominator_sum = denominator_sum + smaller_denominator_sum
    curr_numerator_vector[index] = smaller_numerator_sum
    curr_denominator_vector[index] = smaller_denominator_sum
    index = index + 1
  }
  View(curr_numerator_vector)
  View(curr_denominator_vector)
  print("num sum")
  print(numerator_sum)
  print("den sum")
  print(denominator_sum)
  final_answer = numerator_sum/denominator_sum * (1/W)
  print("s Sum")
  print(s_i_j_sum)
  return(final_answer)
}

#example of how to run
# test_frame <- create_frame("/Users/aaronkruchten/Desktop/bd4d85723b4f141d96cde95c6aed3941f25ec1624ff9ecfe46bf8d0d6156e436")
# beta = 1/2
# alpha = 1
# number_of_partitions = 67
# avg_throughput <- compute_average_throughput(test_frame,beta,alpha,number_of_partitions)
