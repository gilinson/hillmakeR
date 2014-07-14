#   library(RODBC)
#   library(hillmakeR)
#   # Load Data
#   dbConn<- odbcConnect("EDNHCL") # Connect to NHCL Extract
#   tblEncounter<- sqlQuery(dbConn, "SELECT
#                           CONVERT(VARCHAR(19), T_CheckIn, 120) AS 'T_CheckIn',
#                           CONVERT(VARCHAR(19), T_Bed, 120) AS 'T_Bed',
#                           CONVERT(VARCHAR(19), T_Provider, 120) AS 'T_Provider',
#                           CONVERT(VARCHAR(19), T_Disch, 120) AS 'T_Disch',
#                           TriageLevel,
#                           Dispo
#                           FROM  ER_ENCOUNTER
#                           WHERE T_CheckIn >= '2010-10-01'
#                           AND   T_CheckIn < '2010-11-01'")
#   
#   if(class(tblEncounter) != "data.frame"){
#     stop(paste("Select Failed. \n", tblEncounter))
#   }
#   
#   # Convert some dates
#   warning(paste("NHCL Data Opened. ", nrow(tblEncounter), " records retreived."))
#   dateFormat = "%Y-%m-%d %H:%M:%s"
#   tblEncounter$T_CheckIn<- as.POSIXct(as.character(tblEncounter$T_CheckIn), format = dateFormat)
#   tblEncounter$T_Bed<- as.POSIXct(as.character(tblEncounter$T_Bed), format = dateFormat)
#   tblEncounter$T_Provider<- as.POSIXct(as.character(tblEncounter$T_Provider), format = dateFormat)
#   tblEncounter$T_Disch<- as.POSIXct(as.character(tblEncounter$T_Disch), format = dateFormat)
# 
#   procdata<-processdata(tblEncounter, timeCols=c("T_CheckIn", "T_Bed", "T_Provider", "T_Disch"), factors="Dispo")
#   occpat<-occupancyPattern(procdata, startCol="T_CheckIn", endCol="T_Disch")
#   
# 
# # # # 
# # #   tblEncounter$LOS<-as.numeric(tblEncounter$T_Disch - tblEncounter$T_CheckIn, units = "mins")
# # #   tblEncounter<-subset(tblEncounter, LOS > 0 & !is.na(LOS))
# # #   
# # #   system.time(
# # #     test<-createOccupancyFrame(tblEncounter, ArrivalCol = "T_CheckIn", DepartureCol = "T_Disch", factors = c("Dispo","TriageLevel"))
# # #   )