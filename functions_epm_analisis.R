pipeline_EPManalisis <- function(path){
  parameters_analisis <- read.csv("Analisis DLC/EPM/parameters_analisis.csv", sep = ",", header = T)
  Tracking <- ReadDLCDataFromCSV(file = path, fps = parameters_analisis[parameters_analisis$filename == basename(path), 3])
  Tracking <- CutTrackingData(Tracking, start = parameters_analisis[parameters_analisis$filename == basename(path), 2], end = length(Tracking$frames) - Tracking$fps*300 - parameters_analisis[parameters_analisis$filename == basename(path), 2] -1)
  Tracking <- CalibrateTrackingData(Tracking, method = "distance", in.metric = 110, points = c("tl", "br"))
  zoneinfo <- read.table("Analisis DLC/EPM/EPM_zoneinfo.csv", sep = ";", header = T)
  Tracking <- AddZones(Tracking, zoneinfo)
  Tracking <- CleanTrackingData(Tracking, likelihoodcutoff = 0.95, existence.pol = ScalePolygon(Tracking$zones$arena, 1.8))
  Tracking <- AddBinData(Tracking, unit = "minute", binlength = 5)
  Tracking <- EPMAnalysis(Tracking, movement_cutoff = 5, integration_period = Tracking$fps %/% 2,points = "bodycentre", nosedips = TRUE)
  Tracking <- CreateSkeletonData_test(Tracking)
  Tracking <- ClassifyBehaviors(Tracking, modelEPM, MLDataEPM$parameters)
  Tracking <- automatic.SAP(Tracking, Tracking$fps %/% 2)
  Tracking$Report <- c(Tracking$Report, LabelReport(Tracking))
  return(Tracking)
}



pipeline_ML <- function(path){
  labeling.data <- read.table("Analisis DLC/EPM/machine learning/EPM-Labels_AMH.csv", header = T, sep = ",")
  parameters_ML <- read.table("Analisis DLC/EPM/machine learning/parameters_ML.csv", sep = ";", header = T)
  Tracking <- ReadDLCDataFromCSV(file = path, fps = parameters_ML[parameters_ML$filename == basename(path), 2])
  Tracking <- CalibrateTrackingData(Tracking, method = "distance", in.metric = 110, points = c("tl", "br"))
  zoneinfo <- read.table("Analisis DLC/EPM/EPM_zoneinfo.csv", sep = ";", header = T)
  Tracking <- AddZones(Tracking, zoneinfo)
  Tracking <- CleanTrackingData(Tracking, likelihoodcutoff = 0.95, existence.pol = ScalePolygon(Tracking$zones$arena, 1.8))
  Tracking <- AddLabelingData(Tracking, labeling.data[labeling.data$CSVname == Tracking$filename,])
  pointinfo <- read.table("Analisis DLC/EPM/EPM_pointinfo.csv", sep = (";"), header = T)
  Tracking <- AddPointInfo(Tracking, pointinfo)
  Tracking <- EPMAnalysis(Tracking, movement_cutoff = 5,integration_period = 5,points = "bodycentre", nosedips = TRUE)
  Tracking <- CreateSkeletonData_test(Tracking)
  Tracking <- CreateTrainingSet(Tracking, integration_period = 12)
  
  return(Tracking)
}

pipeline_test <- function(path){
  Tracking <- ReadDLCDataFromCSV(path, fps = 25)
  Tracking <- CutTrackingData(Tracking,start = 300,end = 300)
  Tracking <- CalibrateTrackingData(Tracking, "distance",in.metric = 65.5, c("tl","br"))
  Tracking <- CleanTrackingData(Tracking, likelihoodcutoff = 0.95)
  Tracking <- AddLabelingData(Tracking, labeling.data[labeling.data$DLCFile == Tracking$filename,])
  Tracking <- CreateSkeletonData_test(Tracking)
  zoneinfo <- read.table("Analisis DLC/EPM/EPM_zoneinfo.csv", sep = ";", header = T)
  Tracking <- AddZones(Tracking, zoneinfo)
  Tracking <- CreateTrainingSet(Tracking, integration_period = 20)
  pointinfo <- read.table("Analisis DLC/EPM/EPM_pointinfo.csv", sep = (";"), header = T)
  Tracking <- AddPointInfo(Tracking, pointinfo)
  Tracking <- EPMAnalysis(Tracking, movement_cutoff = 5, integration_period = 5, points = "bodycentre", nosedips = TRUE)
  
  return(Tracking)
}

CreateSkeletonData_test <- function(t){
  if(!IsTrackingData(t)){
    stop("Object is not of type TrackingData")
  }
  dat <- data.frame(S1 = GetDistances(t,"nose","headcentre"))
  dat$S2 <- GetDistances(t,"headcentre","neck")
  dat$S3 <- GetDistances(t,"neck","bodycentre")
  dat$S4 <- GetDistances(t,"bodycentre","bcr")
  dat$S5 <- GetDistances(t,"bodycentre","bcl")
  dat$S6 <- GetDistances(t,"bodycentre","tailbase")
  dat$S7 <- GetDistances(t,"tailbase","hipr")
  dat$S8 <- GetDistances(t,"tailbase","hipl")
  dat$S9 <- GetDistances(t, "tailbase", "tailcentre")
  dat$S10 <- GetDistances(t, "tailcentre", "tailtip" )
  dat$S11 <- GetDistances(t,"nose","tailbase")
  dat$S12 <- GetDistances(t, "bcr", "bcl")
  dat$D1 <- GetDelta(dat$S11)
  dat$R1 <- GetRatio(dat$S11,dat$S12)
  dat$E1 <- GetEccentricity(dat$S11,dat$S12)
  dat$A1 <- GetAngleTotal(t,"tailbase","tailcentre","tailcentre","tailtip")
  dat$A2 <- GetAngleTotal(t,"hipr","tailbase","tailbase","hipl")
  dat$A3 <- GetAngleTotal(t,"tailbase","bodycentre","bodycentre","neck")
  dat$A4 <- GetAngleTotal(t,"bcr","bodycentre","bodycentre","bcl")
  dat$A5 <- GetAngleTotal(t,"bodycentre","neck","neck","headcentre")
  dat$A6 <- GetAngleTotal(t,"tailbase","bodycentre","neck","headcentre")
  dat$Ar1 <- GetPolygonAreas(t,c("tailbase","hipr","hipl"))
  dat$Ar2 <- GetPolygonAreas(t,c("hipr","hipl","bcl","bcr"))
  dat$Ar3 <- GetPolygonAreas(t,c("bcr","earr","earl","bcl"))
  dat$Ar4 <- GetPolygonAreas(t,c("earr","nose","earl"))
  #dat$Ac1 <- t$data[["nose"]]$acceleration
  dat$Ac2 <- t$data[["headcentre"]]$acceleration
  #dat$Ac3 <- t$data[["neck"]]$acceleration
  dat$Ac6 <- t$data[["bodycentre"]]$acceleration
  #dat$Ac7 <- t$data[["bcl"]]$acceleration
  #dat$Ac8 <- t$data[["bcr"]]$acceleration
  #dat$Ac9 <- t$data[["hipl"]]$acceleration
  #dat$Ac10 <- t$data[["hipr"]]$acceleration
  dat$Ac11 <- t$data[["tailbase"]]$acceleration
  dat$Spd1 <- t$data[["headcentre"]]$speed
  dat$Spd2 <- t$data[["bodycentre"]]$speed
  dat$Spd3 <- t$data[["tailbase"]]$speed
  dat <- as.data.frame(dat) 
  t$features <- dat
  return(t)
}

plot_analisis <- function(TrackingAll){
  plot <- list()
  for (i in names(TrackingAll)){
    plot[[paste(i)]]<- OverviewPlot( TrackingAll[[i]], "bodycentre")
  }
  return(plot)
}

GetDelta <- function(f){
  c(0, diff(as.matrix(f)))
}

GetRatio <- function(a,b){
  a/b
}

GetEccentricity <- function(a,b){
  E <- ifelse(a == 0|b == 0, 0,
              ifelse(a >= b, sqrt(1-(b^2/a^2)), sqrt(1-(a^2/b^2)))
  )
  E[is.na(E)] <- 0
  return(E)
}
