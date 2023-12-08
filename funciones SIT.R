ReadDLCDataFromCSV_ma <- function(file,fps = 1){
  out <- list()
  out$data <- list()
  data.individuals <- read.table(file, sep =",", header = T, nrows = 1)
  data.bodyparts <- read.table(file, sep =",", header = T, skip = 1, nrows = 1)
  data.header <- paste(data.individuals, data.bodyparts, sep = ".")
  data.header <- data.frame(sapply(data.header, as.character), stringsAsFactors=FALSE)
  raw.data <- read.table(file, sep =",", header = T, skip = 3)
  for(i in seq(from = 2, to = nrow(data.header), by = 3)){
    out$data[[paste(data.header[i,])]] <- data.frame(frame = raw.data$coords, x = raw.data[,i], y = raw.data[,(i+1)], likelihood = raw.data[,(i+2)])
  }
  
  if(fps == 1){
    warning("no fps set. setting fps to 1. keep in mind that time based analyses are resolved in frames / second")
  }
  out$frames <- raw.data$coords
  out$fps <- fps
  out$seconds <- out$frames / fps
  
  out$median.data <- NULL
  for(i in names(out$data)){
    out$median.data <- rbind(out$median.data, data.frame(PointName = i, x = median(out$data[[i]]$x, na.rm = TRUE), y = median(out$data[[i]]$y, TRUE)))
  }
  rownames(out$median.data) <- out$median.data$PointName
  
  out$point.info <- data.frame(PointName = names(out$data), PointType = "NotDefined")
  out$distance.units <- "pixel"
  out$labels <- list()
  out$filename <- last(strsplit(file,split = "/")[[1]])
  out$object.type = "TrackingData"
  
  return(out)
  
}

pipelineSIT_ML <- function(PathSIT){
  ParametersSIT <- read.csv("Analisis DLC/SIT/SITParametersML.csv", header = TRUE, sep = ",")
  
  TrackingSIT <- ReadDLCDataFromCSV_ma(file = PathSIT, fps = ParametersSIT[ParametersSIT$file == basename(PathSIT), 2])
  
  if(TrackingSIT$fps != 14.29){
    TrackingSIT <- adjust_sample_rate(TrackingSIT)
  }
  
  TrackingSIT <- CalibrateTrackingData(TrackingSIT, method = "area", in.metric = 42*42, points = c("single.tl", "single.tr", "single.br", "single.bl" ) )
  
  TrackingSIT <- CleanTrackingData(TrackingSIT, likelihoodcutoff = 0.9)
  
  TrackingSIT <- CalculateMovement(TrackingSIT, movement_cutoff = 5, integration_period = 5)
  
  labeling.data <- read.table("E:/AGMOT/SIT/machine learning/SITLabels.csv", header = TRUE, sep = ",")
  
  TrackingSIT <- AddLabelingData(TrackingSIT, labeling.data[labeling.data$CSVname == TrackingSIT$filename,])
  
  TrackingSIT <- CreateSkeletonDataSIT(TrackingSIT)
  
  TrackingSIT <- CreateTrainingSet(TrackingSIT, integration_period = 10)
  
  return(TrackingSIT)
}

RelativePosition <- function(t){
  #crear par de coordenadas de cabeza y cuerpo de cada animal
  coordHead1 <- c(t$data$Rat1.headcentre$x, t$data$Rat1.headcentre$y)
  coordBodycentre1 <- c(t$data$Rat1.bodycentre$x, t$dataRat1.bodycentre$y)
  coordHead2 <- c(t$data$Rat2.headcentre$x, t$data$Rat2.headcentre$y)
  coordBodycentre2 <- c(t$data$Rat2.bodycentre$x, t$data$Rat2.bodycentre$y)
  
  #Calcular los vectores que van desde el centro de cuerpo a la cabeza de cada animal
  Vector1 <- coordHead1 - coordBodycentre1
  Vector2 <- coordHead2 - coordBodycentre2
  
  #Calcular el angulo entre ambos vectores
  PositionAngle <- abs(acos(rowsum(Vector1 * Vector2) / (sqrt(rowsum(Vector1^2)) * sqrt(rowsum(Vector2^2)))))
  
  return(PositionAngle)
}

RelativePosition <- function(t, a,b,c){
  coordHead1 <- c(t$data$Rat1.headcentre$x, t$data$Rat1.headcentre$y)
  coorBodycentre1 <- c(t$data$Rat1.bodycentre$x, t$data$Rat1.bodycentre$y)
  coordBodycentre2 <- c(t$data$Rat2.bodycentre$x, t$data$Rat2.bodycentre$y)
  
  Vector1 <- coordHead1 - coorBodycentre1
  vector2 <- coorBodycentre2 - coordBodycentre1
  
  PositionAngle <- cos(acos(sum(Vector1 * Vector2) / (sqrt(sum(Vector1^2)) * sqrt(sum(Vector2^2)))))
  }

CreateSkeletonDataSIT <- function(t){
  dat <- data.frame(S1 = GetDistances(t,"Rat1.nose","Rat2.nose"))
  dat$S2 <- GetDistances(t,"Rat1.bodycentre","Rat2.bodycentre")
  dat$S3 <- GetDistances(t,"Rat1.nose","Rat2.tailbase")
  dat$S4 <- GetDistances(t,"Rat2.nose","Rat1.tailbase")
  dat$A1 <- GetAngleTotal(t,"Rat1.bodycentre","Rat1.headcentre","Rat2.bodycentre","Rat2.headcentre")
  dat$A2 <- cospi(GetAngleTotal(t,"Rat1.bodycentre","Rat1.headcentre","Rat1.bodycentre","Rat2.bodycentre"))
  dat$A3 <- cospi(GetAngleTotal(t,"Rat2.bodycentre","Rat2.headcentre","Rat2.bodycentre", "Rat1.bodycentre"))
  dat$Sp1 <- t$data[["Rat1.bodycentre"]]$speed
  dat$Sp2 <- t$data[["Rat1.headcentre"]]$speed
  dat$Sp3 <- t$data[["Rat2.bodycentre"]]$speed
  dat$Sp4 <- t$data[["Rat2.bodycentre"]]$speed
  dat <- as.data.frame(dat) 
  t$features <- dat
  
  return(t)
}

pipelineSIT <-function(PathSIT){
  
  ParametersSIT <- read.table("Analisis DLC/SIT/SITparameters.csv", sep = ",", header = TRUE, )
  
  TrackingSIT <- ReadDLCDataFromCSV_ma(PathSIT, fps = ParametersSIT[ParametersSIT$filename == basename(PathSIT), 2])
  
  if(TrackingSIT$fps != 14.29){
    TrackingSIT <- adjust_sample_rate(TrackingSIT)
  }
  
  TrackingSIT <- CalibrateTrackingData(TrackingSIT, method = "area", in.metric = 42*42, points = c("single.tl", "single.tr", "single.br", "single.bl" ) )
  
  TrackingSIT <- CleanTrackingData(TrackingSIT, likelihoodcutoff = 0.9)
  
  TrackingSIT <- CutTrackingData(TrackingSIT, start = 570, end = length(TrackingSIT$frames) - 570 - 8574 - 1)
  
  TrackingSIT <- CalculateMovement(TrackingSIT, movement_cutoff = 5, integration_period = 5)
  
  TrackingSIT <- CreateSkeletonDataSIT(TrackingSIT)
  
  TrackingSIT <- ClassifyBehaviors(TrackingSIT, model, MLDataSIT$parameters)
  
  TrackingSIT$Report <- LabelReport(TrackingSIT, integration_period = 5)
  
  return(TrackingSIT)
}