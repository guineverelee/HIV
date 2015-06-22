#Goal:  To find out time-to-:  VSupp401 VReb401 etc (days and event0/censor1)
#Keyboard shortcuts
#Run-all F5
#Run-line Ctrl+R
#CSV date MUST be in yyyy-mm-dd or yyyy/mm/dd (otherwise error)
#format: A character string. If not specified, it will try '"%Y-%m-%d"' then '"%Y/%m/%d"' on the first non-'NA' element, and give an error if neither works.
#csv must be sorted already by PATID and coldate


#Step1:  Settings:  Input filename
filename_VL_CD4_csv <- "database_file.csv"
#print(filename_VL_CD4_csv)

#Step2:  Load main VL CD4 database
VLCD4_df <- read.csv(filename_VL_CD4_csv,colClasses=c(rep("character",2),rep("Date",2),"character",rep("numeric",4)))

#Step3:  Subset to Post-ARV, make unique PATID vector
VLCD4_df2_PostARV <- subset(VLCD4_df,VLCD4_df$TIMING=="Post-ARV")
#Note, row names were NOT reset
unique_id_list <- unique(VLCD4_df2_PostARV$PSEUDO)
#print(unique_id_list[1:10])
#length(unique_id_list)

row_header <- c("PATID","VSupp401Date","VSupp401event","VReb401Date","VReb401event","CensoredDate")
output_table <- rbind(row_header)

#Step4:  Loop and subset each patient...etc
for (n in seq_along(unique_id_list)){
	#print(n)
	#default event is 0=censored; 1=happened
	VSupp_event <- 0
	VReb_event <- 0
	#print(unique_id_list[n])
	#Step4b:  subset to each patient 
	#VLCD4_df3_uniqPATID is ONE patient All Post-ARV NO "NA"
	VLCD4_df2b_uniqPATID <- subset(VLCD4_df2_PostARV, VLCD4_df2_PostARV$PSEUDO==unique_id_list[n])
	VLCD4_df3_uniqPATID <- subset(VLCD4_df2b_uniqPATID, VLCD4_df2b_uniqPATID$VLOAD!="NA")
	#print(head(VLCD4_df3_uniqPATID))
	#Step4c:  Make the VL column into a vector
	VL_vec1 <- VLCD4_df3_uniqPATID$VLOAD
	#print(VL_vec1)
	#print(length(VL_vec1)) #89, 48
	###########VSUPP-START#################
	#Step4d:  Index the first of two consecutive VL <401  #take worst case scenario
	if (length(VL_vec1)>=2){
		for (i in 1:(length(VL_vec1)-1)){
			#print(i)1-88 and 1-47
			if (VL_vec1[i]<401 & VL_vec1[i+1]<401){
				#print(VL_vec1[i])
				#print(i)
				#break NOTE:  break only break for loops, stop() stops everything
				#Step4e:  index is i already, next retrieve the full row and index that for original database
				df1index_VSupp <- row.names(VLCD4_df3_uniqPATID[i,]) #get 6, 97
				df3vecindex_VSupp <- i
				VSupp_event <- 1
				#Step4f:  Retrieve coldate from df1 row index
				VSupp_date <- VLCD4_df[df1index_VSupp,]$COLDATE
				#print(VSupp_date) spot checked; correct
				#print(VSupp_event)
				#print(df3vecindex_VSupp)
				################VREB-START######################
				#Step4g:  Index the first of two consecutive >=401
					if (length(VL_vec1[df3vecindex_VSupp:(length(VL_vec1))])>=2){
						for (j in df3vecindex_VSupp:(length(VL_vec1)-1)){
							#print(VL_vec1[j])
							#break
							if (VL_vec1[j]>=401 & VL_vec1[j+1]>=401){
								#Step4h:  index is j already, next retrieve the full row and index that for original database
								df1index_VReb <- row.names(VLCD4_df3_uniqPATID[j,]) #get 12,115
								#print(df1index_VReb)
								df3vecindex_VReb <- j
								VReb_event <- 1
								#Step4i:  Retrieve VReb coldate
								VReb_date <- VLCD4_df[df1index_VReb,]$COLDATE
								#print(VReb_date) got 1999-11-16 and 2003-06-27 correct
								#print(VReb_event)
								break	
							}else{
								VReb_event <- 0
								VReb_date <- "NA"
							}
						}
					}else{
						VReb_event <- 0
						VReb_date <- "NA"
					}
				break
			}else{
				VSupp_event <- 0
				VSupp_date <- "NA"
				VReb_event <- 0
				VReb_date <- "NA"		
			}
		}
	}else{
		VSupp_event <- 0
		VSupp_date <- "NA"
		VReb_event <- 0
		VReb_date <- "NA"
	}
	#List of output
	#1. unique_id_list[n]
	#2. VSupp_date
	#3. VSupp_event
	#4.  VReb_date
	#5.  VReb_event
	#####################CENSORDATE-START##############################
	if (length(VLCD4_df3_uniqPATID$COLDATE)==0){
		Censor_date <- "NA"
	}else{
		Censor_date <- VLCD4_df3_uniqPATID$COLDATE[length(VLCD4_df3_uniqPATID$COLDATE)]
	}
	#print(censor_date)
	#List of output
	#6.  Censor_date
	print(unique_id_list[n])
	#print(class(VSupp_date))
	#print(VSupp_event)
	#print(VReb_date)
	#print(VReb_event)
	#print(Censor_date)
	row_current <- c(unique_id_list[n],as.character(VSupp_date),VSupp_event,as.character(VReb_date),VReb_event,as.character(Censor_date))
	#print(row_current)
	output_table <- rbind(output_table,row_current)
}

#print(output_table) #correct
write.table(output_table,"output_VSupp_VReb401.csv",sep=",")

