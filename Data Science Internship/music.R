library("audio")

piano_frequency <- function(n){
  return((2^(1/12))^(n-49)*440)
}

piano_keys = c(1:88)
frequencies <- piano_frequency(piano_keys)
durations <- rep(1/12,88)

piano_frame <- data.frame(freq = frequencies,duration = durations)

tempo <- 120
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}


all_keys <- c()
for(i in 1:nrow(piano_frame)){
  curr_vec <- make_sine(piano_frame$freq[i],piano_frame$duration[i])
  all_keys <- c(all_keys,curr_vec)
  }
  
play(all_keys)

first_fourier_seires <- function(x){
  first_addend = 1.0000001*sin(x/25)
  second_addend = 4*sin(3*x)/3*pi
  third_addend = 4*sin(5*x)/5*pi
  fourth_addend = 4*sin(7*x)/7*pi
  return(first_addend)
}

play(first_fourier_seires(1:100000))

