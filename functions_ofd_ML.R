#pipeline_OFD_analisis <- function(path, filename)
  
pipeline_OFT_ML <- function(path, filename){
  parameters_OFD_ML <- read.table("Analisis DLC/OFD/machine learning/parameters_OFD_ML.csv", sep = ",", header = T)
  point.info <-
  labeling.data <- 
  Tracking <- ReadDLCDataFromCSV(file = path, fps = parameters_OFD_ML[parameters_OFD_ML$filename == filename, 2])
  Tracking <- CalibrateTrackingData(Tracking, method = "area", in.metric = 42*42,points = c("tl","tr","br","bl"))
  Tracking <- AddOFTZones(Tracking, scale_center = 0.5,scale_periphery  = 0.75 ,scale_corners = 0.5, points = c("tl","tr","br","bl"))
  Tracking <- CleanTrackingData(Tracking, likelihoodcutoff = 0.95)
  Tracking <- AddLabelingData(Tracking, labeling.data[labeling.data$CSVname == Tracking$filename,])
  Tracking <- OFTAnalysis(Tracking, movement_cutoff = 5,integration_period = 5,points = "bodycentre")
  Tracking <- CreateSkeletonData_OFD(Tracking)
  Tracking <- CreateTrainingSet(Tracking, integration_period = 15)

  return(Tracking)
}

CreateSkeletonData_OFD <- function(t){
  if(!IsTrackingData(t)){
    stop("Object is not of type TrackingData")
  }
  dat <- data.frame(S1 = GetDistances(t,"nose","mouth"))
  dat$S2 <- GetDistances(t,"mouth","troath")
  dat$S3 <- GetDistances(t,"troath","bodycentre")
  dat$S4 <- GetDistances(t,"bodycentre","bcr")
  dat$S5 <- GetDistances(t,"bodycentre","bcl")
  dat$S6 <- GetDistances(t,"bodycentre","tailbase")
  dat$S7 <- GetDistances(t,"tailbase","hipr")
  dat$S8 <- GetDistances(t,"tailbase","hipl")
  dat$S9 <- GetDistances(t,"tailbase","tailcentre")
  dat$S10 <- GetDistances(t,"tailcentre","tailtip")
  dat$S11 <- GetDistances(t,"lfp","rfp")
  dat$S12 <- GetDistances(t, "nose","lfp")
  dat$S13 <- GetDistances(t,"nose","rfp")
  dat$S14 <- GetDistances(t,"nose", "lhp")
  dat$S15 <- GetDistances(t,"nose", "rhp")
  dat$S16 <- GetDistances(t, "nose", "bcr")
  dat$S17 <- GetDistances(t, "nose", "bcl")
  dat$S17 <- GetDistances(t, "nose", "tailbase")
  dat$A1 <- GetAngleTotal(t,"tailbase","tailcentre","tailcentre","tailtip")
  dat$A2 <- GetAngleTotal(t,"hipr","tailbase","tailbase","hipl")
  dat$A3 <- GetAngleTotal(t,"tailbase","bodycentre","bodycentre","troath")
  dat$A4 <- GetAngleTotal(t,"bcr","bodycentre","bodycentre","bcl")
  dat$A5 <- GetAngleTotal(t,"bodycentre","troath","troath","mouth")
  dat$A6 <- GetAngleTotal(t,"tailbase","bodycentre","troath","mouth")
  dat$Ar1 <- GetPolygonAreas(t,c("tailbase","hipr","hipl"))
  dat$Ar2 <- GetPolygonAreas(t,c("hipr","hipl","bcl","bcr"))
  dat$Ar3 <- GetPolygonAreas(t,c("bcr","mouth","bcl"))
  dat$Ar4 <- GetPolygonAreas(t,c("lfp","nose","rfp"))
  dat$Ac1 <- t$data[["nose"]]$acceleration
  dat$Ac2 <- t$data[["mouth"]]$acceleration
  dat$Ac4 <- t$data[["lfp"]]$acceleration
  dat$Ac5 <- t$data[["rfp"]]$acceleration
  dat$Ac6 <- t$data[["bodycentre"]]$acceleration
  dat$Ac11 <- t$data[["lhp"]]$acceleration
  dat$Ac12 <- t$data[["rhp"]]$acceleration
  dat$Ac13 <- t$data[["tailbase"]]$acceleration
  dat$Sp1 <- t$data[["nose"]]$speed
  dat$Sp2 <- t$data[["lfp"]]$speed
  dat$Sp3 <- t$data[["rfp"]]$speed
  dat$Sp4 <- t$data[["bodycentre"]]$speed
  dat$Sp5 <- t$data[["lhp"]]$speed
  dat$Sp6 <- t$data[["rfp"]]$speed
  dat$Sp7 <- t$data[["tailbase"]]$speed
  dat <- as.data.frame(dat) 
  t$features <- dat
  return(t)
}