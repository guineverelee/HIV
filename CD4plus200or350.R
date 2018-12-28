#Goal:  To find out time-to-:  (i) CD4 +200 or 350
#Keyboard shortcuts
#Run-all F5
#Run-line Ctrl+R
#CSV date MUST be in yyyy-mm-dd or yyyy/mm/dd (otherwise error)
#format: A character string. If not specified, it will try '"%Y-%m-%d"' then '"%Y/%m/%d"' on the first non-'NA' element, and give an error if neither works.
#csv must be sorted already by PATID and coldate


#Step1:  Settings:  Input filename
filename_VL_CD4_csv <- "20140219 HOMER MBA AMU longitudinal outcome V3 ver06 manualedit MBA NA.csv"
#print(filename_VL_CD4_csv)

#Step2:  Load main VL CD4 database
VLCD4_df <- read.csv(filename_VL_CD4_csv,colClasses=c(rep("character",2),rep("Date",2),"character",rep("numeric",4)))
#str(VLCD4_df)

#Step3:  Subset to Post-ARV, make unique PATID vector
VLCD4_df2_PostARV <- subset(VLCD4_df,VLCD4_df$TIMING=="Post-ARV")
#Note, row names were NOT reset
unique_id_list <- unique(VLCD4_df2_PostARV$PSEUDO)
#print(unique_id_list[1:10])
#unique_id_list <- c("0100-5634","0196-2711")
#length(unique_id_list)

row_header <- c("PATID","CD4_plus200_date","CD_plus200_event","CD4_350_date","CD4_350_event","Censored_date","CD4_Interval_min","CD4_Interval_max","CD4_Interval_mean","CD4_Interval_SD","CD4Followup_counts")
output_table <- rbind(row_header)

#Step4:  Loop and subset each patient...etc
for (n in seq_along(unique_id_list)){
	#print(n)
	#default event is 0=censored; 1=happened
	CD4_plus200_event <- 0
	CD4_350_event <- 0
	#print(unique_id_list[n])
	#Step4b:  subset to each patient 
	#VLCD4_df3_uniqPATID is ONE patient All Post-ARV NO "NA"
	VLCD4_df2b_uniqPATID <- subset(VLCD4_df2_PostARV, VLCD4_df2_PostARV$PSEUDO==unique_id_list[n])
	VLCD4_df2c_uniqPATID <- subset(VLCD4_df2b_uniqPATID, VLCD4_df2b_uniqPATID$CD4!="NA")
	VLCD4_df3_uniqPATID <- subset(VLCD4_df2c_uniqPATID, VLCD4_df2c_uniqPATID$CD4Baseline!="NA")
	#print(head(VLCD4_df3_uniqPATID))
	#Step4c:  Make the CD4 column into a vector
	CD4_vec1 <- VLCD4_df3_uniqPATID$CD4
	CD4Baseline_vec2 <- VLCD4_df3_uniqPATID$CD4Baseline
	#print(CD4_vec1)
	#print(CD4Baseline_vec2)
	###########CD4_plus200#################
	#Step4d:  Index the (i) first of two consecutive CD4_plus200 (ii) first of two consecutive CD4_350
	if (length(CD4_vec1)>=2){
		#(i) index first of two consecutive CD4_plus200
		for (i in 1:(length(CD4_vec1)-1)){
			#print(i)#1-88 and 1-47
			if (CD4_vec1[i] >= CD4Baseline_vec2[i]+200 & CD4_vec1[i+1] >= CD4Baseline_vec2[i]+200){
				#print(CD4_vec1[i])
				#break NOTE:  break only break for loops, stop() stops everything
				#Step4e:  index is i already -_-, next retrieve the full row and index that for original database
				df1index_CD4_plus200 <- row.names(VLCD4_df3_uniqPATID[i,]) 
				df3vecindex_CD4_plus200 <- i
				CD4_plus200_event <- 1
				#Step4f:  Retrive coldate from df1 row index
				CD4_plus200_date <- VLCD4_df[df1index_CD4_plus200,]$COLDATE
				break
			}else{
				CD4_plus200_date <- "NA"
				CD4_plus200_event <- 0
			}
		}
		#(ii) index first of two consecutive CD4_350
		for (i in 1:(length(CD4_vec1)-1)){
			if (CD4_vec1[i] > 350 & CD4_vec1[i+1] > 350){
				df1index_CD4_350 <- row.names(VLCD4_df3_uniqPATID[i,]) 
				df3vecindex_CD4_350 <- i
				CD4_350_event <- 1
				#Step4f:  Retrive coldate from df1 row index
				CD4_350_date <- VLCD4_df[df1index_CD4_350,]$COLDATE
				break
			}else{
				CD4_350_date <- "NA"
				CD4_350_event <- 0				
			}
		}
	}else{
		CD4_plus200_date <- "NA1"
		CD4_plus200_event <- 0
		CD4_350_date <- "NA1"
		CD4_350_event <- 0
	}
	#####################CENSORDATE-START##############################
	if (length(VLCD4_df3_uniqPATID$COLDATE)==0){
		Censored_date <- "NA"
	}else{
		Censored_date <- VLCD4_df3_uniqPATID$COLDATE[length(VLCD4_df3_uniqPATID$COLDATE)]
	}
	#print(unique_id_list[n])
	######################CD4_Visit_Intervals############################
	if (length(CD4_vec1)>=2){
		COLDATE_vec3 <- VLCD4_df3_uniqPATID$COLDATE
		CD4_Interval_vec <- c()
		for (k in 1:length(COLDATE_vec3)-1){
			diff <- COLDATE_vec3[k+1] - COLDATE_vec3[k]
			CD4_Interval_vec <- append(CD4_Interval_vec,diff)
		}
		#print(CD4_Interval_vec)
		CD4_Interval_min <- min(CD4_Interval_vec)
		CD4_Interval_max <- max(CD4_Interval_vec)
		CD4_Interval_mean <- mean(CD4_Interval_vec)
		CD4_Interval_SD <- sd(CD4_Interval_vec)
	}else{
		CD4_Interval_min <- "NA"
		CD4_Interval_max <- "NA"
		CD4_Interval_mean <- "NA"
		CD4_Interval_SD <- "NA"
	}
	#######################Followup counts#######################
	if (length(CD4_vec1)>=1){
		CD4Followup_counts <- length(VLCD4_df3_uniqPATID$CD4)
	}else{
		CD4Followup_counts <- 0
	}
	#######################~the~END~#######################################
	row_current <- c(unique_id_list[n],as.character(CD4_plus200_date),CD4_plus200_event,as.character(CD4_350_date),CD4_350_event,as.character(Censored_date),CD4_Interval_min,CD4_Interval_max,CD4_Interval_mean,CD4_Interval_SD,CD4Followup_counts)
	#print(row_current)
	output_table <- rbind(output_table,row_current)
}

#print(output_table) #correct
write.table(output_table,"output_CD4_plus200_350_outcome.csv",sep=",")

