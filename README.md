# PSC-EDA
Code for exploratory data analysis on tcp flows internship at the Pittsburgh Supercomputing Center

This repository contains various R code that was written during the summer of 2019 for the exploratory data analysis on TCP 
metrics internships where I analyzed a large data set of over 65 gigabytes and 370 thousand flows.

The folder titled Data science internship contains various files related to the analyses that I did. All of the code and files
served a purpose at some time, however many of the analyses we did did not show much promise. Much of this code is not very clean
and not readable and is mostly stored here for later reference by the author. 

The folder steady state model was an attempt to apply the model in the paper: 
http://www.ece.ubc.ca/~vincentw/C/BWLcGlobecom10.pdf.

The model did not apply to the data very well. This is likely because its assumption that packet loss follows an exponential 
distribution does not appear to be fully correct. 

The file Causal discover R can be used to form a DAG like the one titled DAG.png in the repository. This can be done by following
the example in the bottom of the file. Running the function on a random sample of about 2000 of the flows will likely form a
DAG similar to the one title DAG.png

The file title PSC interface github copy.R contains several function for easily accessing the data. 
----------------------------------------------------------------------------------------------------------------------
function name: query_all_single_flow	

Input: flow_name, database, save_location, database username, database_password

Output: A folder created at the save_location given by the user with the same name as the given flow name containing separate csv files for each measurement indexed by time. 

Further Info: This function creates a useful data directory that form_dataframe can be called on,
form_dataframe	Input: directory, measurements

----------------------------------------------------------------------------------------------------------------------

function name: form_dataframe	

Input: directory, measurements

Output: An R data frame of the given measurements indexed by time. 

Further Info: Some measurements are not recorded at all times and so when this occurs the function will fill in NA for these values. 
The directory given has to be a directory created by query_all_single_flow

----------------------------------------------------------------------------------------------------------------------

function name: query_by_flow	

Input: flow_name, measurments, database, database username, database password

Output: An R data frame of the given measurements indexed by time.

Further Info: This function behaves the same as form_dataframe, however it accesses data directly from the influx database.
his is more convenient if you want analyze a flow one time and be done with it. If you are planning on running many different analyses with 
different measurments on a specific flow, then form_dataframe and query_all_single_flow should be used because it will be much faster. 

----------------------------------------------------------------------------------------------------------------------

function name: impute_frame	

Input: dataframe, k number of neighbors

Output: An imputed R data frame

Further Info: This function imputes(infers NA values and fills them in) a dataframe outputted by form_dataframe. It should work for an arbitrary k but k=10 seems to work the best. Additionally, if a column in a dataframe is entirely NA values this function will do nothing, and those columns should likely be removed. 

This function should also work with frames outputted by query_by_flow, but I have run into issues before. 

