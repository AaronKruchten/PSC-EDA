flow_name = "bd4d85723b4f141d96cde95c6aed3941f25ec1624ff9ecfe46bf8d0d6156e436"
database = "ALL_PSC_br034.dmz.bridges.psc.edu"
save_location = "/Users/aaronkruchten/Desktop/test query"
query_all_single_flow(flow_name = flow_name,database = database, save_location = save_location,username = "reader", password = "listen")
measurements = "CurCwnd,CurRTO,FastRetran,Nagle,RetranThresh,SlowStart,CongAvoid,DataOctetsOut,DataSegsOut,OctetsRetrans,SegsRetrans"
final_frame = form_dataframe(directory = save_location,measurements = measurements)
View(final_frame)