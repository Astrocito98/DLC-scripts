adjust_sample_rate <- function(Tracking){
  
  rate <- Tracking$fps/14.29
  frames2keep <- round(seq(1, length(Tracking$frames), rate))
  
  for(i in names(Tracking$data)){
    Tracking$data[[i]] <- Tracking$data[[i]][frames2keep, ]
    Tracking$data[[i]]$frame <- seq(0, length(frames2keep)-1)
  }
  
  Tracking$frames <- seq(0, length(frames2keep)-1)
  Tracking$fps <- 14.29
  Tracking$seconds <- Tracking$frames/Tracking$fps
  
  return(Tracking)
}

FindAdjustRate <- function(TrackingOFD, TrackingOFU, Parameters){
  StartTimeOFD <- Parameters[Parameters$CSVname == TrackingOFD$filename, 3]
  StartTimeOFU <- Parameters[Parameters$CSVname == TrackingOFU$filename, 3]
  StartFrameOFD <- round(StartTimeOFD*TrackingOFD$fps)
  StartFrameOFU <- round(StartTimeOFU*TrackingOFU$fps)
  AdjustRate <- StartFrameOFD - StartFrameOFU
}

SynchFrames <- function(TrackingOFU, TrackingOFD, AdjustRate){
# Agregar o quitar filas al inicio de cada matriz de datos de Tracking_OFU segun el valor de adjust_rate
  for(i in names(TrackingOFU$data)){
    if(AdjustRate > 0) {
      matrix <- data.frame(matrix(0, nrow = AdjustRate, ncol = 4 ))
      colnames(matrix) <- list("frame","x", "y", "likelihood")
      TrackingOFU$data[[i]] <- rbind(matrix, TrackingOFU$data[[i]])
    } else if (AdjustRate < 0) {
      TrackingOFU$data[[i]] <- tail(TrackingOFU$data[[i]], AdjustRate)
    }
  }
  
# Agregar o quitar filas al final de cada matriz de datos de Tracking_OFU para que coincida con el número de frames de Tracking_OFD
  for (i in names(TrackingOFU$data)){
    if(length(TrackingOFU$frames) + AdjustRate < length(TrackingOFD$frames)){
      matrix_tail <- data.frame(matrix(0, nrow = length(TrackingOFD$frames) - length(TrackingOFU$frames) - AdjustRate, ncol = 4))
      colnames(matrix_tail) <- list("frame", "x", "y", "likelihood")
      TrackingOFU$data[[i]] <- rbind(TrackingOFU$data[[i]], matrix_tail)
    } else if (length(TrackingOFU$frames) + AdjustRate > length((TrackingOFD$frames))) {
      TrackingOFU$data[[i]] <- head(TrackingOFU$data[[i]], length(TrackingOFD$frames) - length(TrackingOFU$frames) - AdjustRate)
    }
  }
# Actualizar el indice frame de cada matriz de datos de Tracking_OFU
  for (i in names(TrackingOFU$data)){
    TrackingOFU$data[[i]]$frame <- seq(0, length(TrackingOFU$data[[i]]$frame) - 1)
  }
# Actualizar el valor frames de Tracking_OFU 
  TrackingOFU$frames <- seq(0, (length(TrackingOFD$frames) - 1))
  return(TrackingOFU)
}


CombineTrackingOF <- function(TrackingOFD,TrackingOFU){
  TrackingOFD$data <- c(TrackingOFD$data, TrackingOFU$data)
  TrackingOFD$median.data <- rbind(TrackingOFD$median.data, TrackingOFU$median.data)
  TrackingOFD$point.info <- rbind(TrackingOFD$point.info, TrackingOFU$point.info)
  TrackingOFD$data[c("OFD_tl","OFD_tr", "OFD_bl", "OFD_br")] <- NULL
  TrackingOFD$median.data <- subset(TrackingOFD$median.data[!rownames(TrackingOFD$median.data) %in% c("OFD_tl", "OFD_tr", "OFD_bl", "OFD_br"), ])
  TrackingOFD$point.info <- TrackingOFD$point.info[!TrackingOFD$point.info$PointName %in% c("OFD_tl","OFD_tr", "OFD_bl", "OFD_br"), ]
  
  return(TrackingOFD)
}


pipeline_combined_OF <- function(path_OFD, path_OFU){
# leer valores de fps para cada archivo
  parameters_OFD_ML <- read.table("Analisis DLC/OFD/machine learning/Start_labels.csv", sep = ",", header = T)

## sincronizar y combinar archivos de OFD y OFU  
  
  # leer archivos .csv de OFD y OFU
  Tracking_OFD <- ReadDLCDataFromCSV_OFD(file = path_OFD, fps = parameters_OFD_ML[parameters_OFD_ML$CSVname == basename(path_OFD), 8])
  Tracking_OFU <- ReadDLCDataFromCSV_OFU(file = path_OFU, fps = parameters_OFD_ML[parameters_OFD_ML$CSVname == basename(path_OFU), 8])

  # ajustar frecuencia de muestreo para los data frame si no es igual a 14.29  
  if(Tracking_OFD$fps != 14.29){
    Tracking_OFD <- adjust_sample_rate(Tracking_OFD)
  }
  
  if(Tracking_OFU$fps != 14.29){
    Tracking_OFU <- adjust_sample_rate(Tracking_OFU)
  }
  
# Encontrar el valor de ajuste (numero de frames)
  adjust_rate <- FindAdjustRate(Tracking_OFD, Tracking_OFU, parameters_OFD_ML)

# sincronizar los archivos de OFU para que coincida en numero de frames con OFD  
  Tracking_OFU <- SynchFrames(Tracking_OFU, Tracking_OFD, adjust_rate)

# combinar valores de OFD y OFU  
  TrackingCombinedOF <- CombineTrackingOF(Tracking_OFD, Tracking_OFU)
  
  
## limpiar, calibrar y analizar
  
# calibrar medidas del laberinto (con los puntos de OFU)
  TrackingCombinedOF <- CalibrateTrackingData(TrackingCombinedOF, method = "area", in.metric = 42*42,points = c("OFU_tl","OFU_tr","OFU_br","OFU_bl"))
  
# agregar zonas (centro y periferia)
  TrackingCombinedOF <- AddOFTZones(TrackingCombinedOF, scale_center = 0.5,scale_periphery  = 0.5 ,scale_corners = 0.5, points = c("OFU_tl","OFU_tr","OFU_br","OFU_bl"))

# limpiar datos
  TrackingCombinedOF <- CleanTrackingData(TrackingCombinedOF, likelihoodcutoff = 0.5)

# analisis de campo abierto
  TrackingCombinedOF <- OFTAnalysis(TrackingCombinedOF, movement_cutoff = 5,integration_period = 5, points = "OFU_bodycentre")
  
## Crear datos de ML
  
# leer labels de grooming
  labeling.data <- read.table("OFD/Machine learning/OFD_Labels_microGrooming.csv", sep = ",", header = T)
  labeling.data <- labeling.data[!(labeling.data$type == "Grooming"), ]
  labeling.data <- labeling.data[!(labeling.data$ID == "Pil-a2_OFD"), ]
  
# agregar valores de labels  
  TrackingCombinedOF <- AddLabelingData(TrackingCombinedOF, labeling.data[labeling.data$CSVname == TrackingCombinedOF$filename,])
  
# crear representación esqueletica  
  TrackingCombinedOF <- CreateSkeletonData_OF_combined(TrackingCombinedOF)

# crear training data set
TrackingCombinedOF <- CreateTrainingSet(TrackingCombinedOF, integration_period = 10)
  return(TrackingCombinedOF)
}


RunPipeline_combined_OF <- function(files_OFD, path_OFD, files_OFU, path_OFU, FUN){
  out <- list()
  for(j in files_OFD){
    out[[paste(j)]] <- FUN(paste(path_OFD,j,sep = ""), paste(path_OFU, files_OFU[str_extract(files_OFU, "\\d+") == str_extract(j, "\\d+")], sep = ""))
  }
  return(out)
}


ReadDLCDataFromCSV_OFU <- function(file,fps = 1){
  out <- list()
  out$data <- list()
  data.header <- read.table(file, sep =",", header = T, nrows = 1)
  data.header <- data.frame(sapply(data.header, as.character), stringsAsFactors=FALSE)
  raw.data <- read.table(file, sep =",", header = T, skip = 2)
  for(i in seq(from = 2, to = nrow(data.header), by = 3)){
    out$data[[paste("OFU", data.header[i,], sep = "_")]] <- data.frame(frame = raw.data$coords, x = raw.data[,i], y = raw.data[,(i+1)], likelihood = raw.data[,(i+2)])
  }
  
  if(fps == 1){
    warning("no fps set. setting fps to 1. keep in mind that time based analyses are resolved in frames / second")
  }
  out$frames <- raw.data$coords
  out$fps <- fps
  out$seconds <- out$frames / fps
  
  out$median.data <- NULL
  for(i in names(out$data)){
    out$median.data <- rbind(out$median.data, data.frame(PointName = i, x = median(out$data[[i]]$x), y = median(out$data[[i]]$y)))
  }
  rownames(out$median.data) <- out$median.data$PointName
  
  out$point.info <- data.frame(PointName = names(out$data), PointType = "NotDefined")
  out$distance.units <- "pixel"
  out$labels <- list()
  out$filename <- last(strsplit(file,split = "/")[[1]])
  out$object.type = "TrackingData"
  
  return(out)
}

ReadDLCDataFromCSV_OFD <- function(file,fps = 1){
  out <- list()
  out$data <- list()
  data.header <- read.table(file, sep =",", header = T, nrows = 1)
  data.header <- data.frame(sapply(data.header, as.character), stringsAsFactors=FALSE)
  raw.data <- read.table(file, sep =",", header = T, skip = 2)
  for(i in seq(from = 2, to = nrow(data.header), by = 3)){
    out$data[[paste("OFD", data.header[i,], sep = "_")]] <- data.frame(frame = raw.data$coords, x = raw.data[,i], y = raw.data[,(i+1)], likelihood = raw.data[,(i+2)])
  }
  
  if(fps == 1){
    warning("no fps set. setting fps to 1. keep in mind that time based analyses are resolved in frames / second")
  }
  out$frames <- raw.data$coords
  out$fps <- fps
  out$seconds <- out$frames / fps
  
  out$median.data <- NULL
  for(i in names(out$data)){
    out$median.data <- rbind(out$median.data, data.frame(PointName = i, x = median(out$data[[i]]$x), y = median(out$data[[i]]$y)))
  }
  rownames(out$median.data) <- out$median.data$PointName
  
  out$point.info <- data.frame(PointName = names(out$data), PointType = "NotDefined")
  out$distance.units <- "pixel"
  out$labels <- list()
  out$filename <- last(strsplit(file,split = "/")[[1]])
  out$object.type = "TrackingData"
  
  return(out)
}

CreateSkeletonData_OF_combined <- function(t){
  if(!IsTrackingData(t)){
    stop("Object is not of type TrackingData")
  }
  dat <- data.frame(S1 = GetDistances(t,"OFU_nose","OFU_headcentre"))
  #dat$S2 <- GetDistances(t,"OFU_headcentre","OFU_neck")
  #dat$S3 <- GetDistances(t,"OFU_neck","OFU_bodycentre")
  #dat$S4 <- GetDistances(t,"OFU_bodycentre","OFU_bcr")
  #dat$S5 <- GetDistances(t,"OFU_bodycentre","OFU_bcl")
  #dat$S6 <- GetDistances(t,"OFU_bodycentre","OFU_tailbase")
  #dat$S7 <- GetDistances(t,"OFU_tailbase","OFU_hipr")
  #dat$S8 <- GetDistances(t,"OFU_tailbase","OFU_hipl")
  #dat$S9 <- GetDistances(t,"OFU_tailbase","OFU_tailcentre")
  #dat$S10 <- GetDistances(t,"OFU_tailcentre","OFU_tailtip")
  dat$S11 <- GetDistances(t,"OFU_fpr", "OFU_nose")
  dat$S12 <- GetDistances(t,"OFU_fpl", "OFU_nose")
  dat$S13 <- GetDistances(t,"OFU_fpr","OFU_headcentre")
  dat$S14 <- GetDistances(t,"OFU_fpl","OFU_headcentre")
  dat$S15 <- GetDistances(t,"OFU_hpr","OFU_nose")
  dat$S16 <- GetDistances(t,"OFU_hpl","OFU_nose")
  dat$S17 <- GetDistances(t,"OFU_nose","OFU_hipr")
  dat$S18 <- GetDistances(t,"OFU_nose", "OFU_hipl")
  dat$S19 <- GetDistances(t,"OFD_rfp","OFD_nose")
  dat$S20 <- GetDistances(t,"OFD_lfp","OFD_nose")
  dat$S21 <- GetDistances(t,"OFD_rfp","OFD_headcentre")
  dat$S22 <- GetDistances(t,"OFD_lfp","OFD_headcentre")
  dat$S23 <- GetDistances(t,"OFD_rhp","OFD_nose")
  dat$S24 <- GetDistances(t,"OFD_lhp","OFD_nose")
  dat$S25 <- GetDistances(t,"OFD_nose","OFD_bodycentre")
  dat$S26 <- GetDistances(t,"OFD_nose", "OFD_hipr")
  dat$S27 <- GetDistances(t,"OFD_nose", "OFD_hipl")
  dat$S28 <- GetDistances(t,"OFD_nose","OFD_genitals")
  dat$S29 <- GetDistances(t,"OFU_fpr", "OFU_fpl")
  dat$S30 <- GetDistances(t,"OFD_rfp", "OFD_lfp")
  dat$S31 <- GetDistances(t,"OFU_fpr", "OFU_bodycentre")
  dat$S32 <- GetDistances(t,"OFU_fpl", "OFU_bodycentre")
  dat$S31 <- GetDistances(t,"OFD_rfp", "OFD_bodycentre")
  dat$S32 <- GetDistances(t,"OFD_lfp", "OFD_bodycentre")
  dat$S33 <- GetDistances(t,"OFD_rfp", "OFD_bodycentre")
  dat$S34 <- GetDistances(t,"OFD_lfp", "OFD_bodycentre")
  #dat$A1 <- GetAngleTotal(t,"OFU_tailbase","OFU_tailcentre","OFU_tailcentre","OFU_tailtip")
  #dat$A2 <- GetAngleTotal(t,"OFU_hipr","OFU_tailbase","OFU_tailbase","OFU_hipl")
  #dat$A3 <- GetAngleTotal(t,"OFU_tailbase","OFU_bodycentre","OFU_bodycentre","OFU_neck")
  dat$A4 <- GetAngleTotal(t,"OFU_bcr","OFU_bodycentre","OFU_bodycentre","OFU_bcl")
  dat$A5 <- GetAngleTotal(t,"OFU_bodycentre","OFU_neck","OFU_neck","OFU_headcentre")
  dat$A6 <- GetAngleTotal(t,"OFU_tailbase","OFU_bodycentre","OFU_neck","OFU_headcentre")
  #dat$Ar1 <- GetPolygonAreas(t,c("OFU_tailbase","OFU_hipr","OFU_hipl"))
  #dat$Ar2 <- GetPolygonAreas(t,c("OFU_hipr","OFU_hipl","OFU_bcl","OFU_bcr"))
  #dat$Ar3 <- GetPolygonAreas(t,c("OFU_bcr","OFU_earr","OFU_earl", "OFU_bcl"))
  #dat$Ar4 <- GetPolygonAreas(t,c("OFU_earr","OFU_nose","OFU_earl"))
  #dat$Ac1 <- t$data[["nose"]]$acceleration
  #dat$Ac2 <- t$data[["mouth"]]$acceleration
  #dat$Ac4 <- t$data[["lfp"]]$acceleration
  #dat$Ac5 <- t$data[["rfp"]]$acceleration
  #dat$Ac6 <- t$data[["bodycentre"]]$acceleration
  #dat$Ac11 <- t$data[["lhp"]]$acceleration
  #dat$Ac12 <- t$data[["rhp"]]$acceleration
  #dat$Ac13 <- t$data[["tailbase"]]$acceleration
  dat$Sp1 <- t$data[["OFU_fpr"]]$speed
  dat$Sp2 <- t$data[["OFU_fpl"]]$speed
  dat$Sp3 <- t$data[["OFU_hpr"]]$speed
  dat$Sp4 <- t$data[["OFU_hpl"]]$speed
  dat$Sp5 <- t$data[["OFD_rfp"]]$speed
  dat$Sp6 <- t$data[["OFD_lfp"]]$speed
  dat$Sp7 <- t$data[["OFD_rhp"]]$speed
  dat$Sp7 <- t$data[["OFD_lhp"]]$speed
  dat$Sp8 <- t$data[["OFU_bodycentre"]]$speed
  dat <- as.data.frame(dat) 
  t$features <- dat
  return(t)
}

GroomingReport <- function(t){
  t <-SmoothLabels(t, 5)
  GroomingLengthEncoding <- rle(t$labels$classifications)
  ShortNoneSequences <- which(GroomingLengthEncoding$lengths <= 70 & GroomingLengthEncoding$values == "None")
  NASequences <- which(is.na(GroomingLengthEncoding$values))
  GroomingLengthEncoding$lengths <- GroomingLengthEncoding$lengths[-c(ShortNoneSequences, NASequences)]
  GroomingLengthEncoding$values <- GroomingLengthEncoding$values[-c(ShortNoneSequences, NASequences)]
  #GroomingLengthEncoding$last <- cumsum(GroomingLengthEncoding$lenghts)
  #GroomingLengthEncoding$first <- c(1, head(GroomingLengthEncoding$last, -1) + 1)
  
  
  for(i in unique(GroomingLengthEncoding$values)){
    GroomingLengthEncoding[[paste("idx_",gsub(" ","",i), sep = "")]] <- which(GroomingLengthEncoding$values == i)
  }
  
  GroomingReport <- list()
  
  if(length(GroomingLengthEncoding$lengths) == 0)
    return(GroomingReport)
  
  #Patterns counts
  for (i in names(GroomingLengthEncoding[3:length(GroomingLengthEncoding)])){
    GroomingReport[[paste(str_extract(names(GroomingLengthEncoding[i]), "(?<=_)[^_]+$"), "Count", sep = ".")]] <- length(GroomingLengthEncoding[[i]])
  }
  
  #Reporte de Transiciones
  
  GroomingReport$correct_transitions <- 0
  
  GroomingReport$incorrect_transitions <- 0
  
  for(i in GroomingLengthEncoding$idx_None){
    if(isTRUE(GroomingLengthEncoding$values[i+1] == "Paws/Nose Grooming"))
      GroomingReport$correct_transitions = GroomingReport$correct_transitions + 1
    else
      GroomingReport$incorrect_transitions = GroomingReport$incorrect_transitions + 1
  }
  
  for(i in GroomingLengthEncoding$"idx_Paws/NoseGrooming"){
    if(isTRUE(GroomingLengthEncoding$values[i+1] == "Face/HeadWashing"))
      GroomingReport$correct_transitions <- GroomingReport$correct_transitions + 1
    else
      GroomingReport$incorrect_transitions <- GroomingReport$incorrect_transitions + 1
  }
  
  for(i in GroomingLengthEncoding$"idx_Face/HeadWashing"){
    if(GroomingLengthEncoding$values[i+1] == "BodyGrooming")
      GroomingReport$correct_transitions <- GroomingReport$correct_transitions + 1
    else
      GroomingReport$incorrect_transitions <- GroomingReport$incorrect_transitions + 1
  }
  
  for(i in GroomingLengthEncoding$"idx_BodyGrooming"){
    if(i == length(GroomingLengthEncoding$lengths))
      break
    if(GroomingLengthEncoding$values[i+1] == "GenitalsLicking")
      GroomingReport$correct_transitions <- GroomingReport$correct_transitions + 1
    else
      GroomingReport$incorrect_transitions <- GroomingReport$incorrect_transitions + 1
  }
  
  for(i in GroomingLengthEncoding$"idx_GenitalsLicking"){
    if(i == length(GroomingLengthEncoding$lengths))
      break
    if(GroomingLengthEncoding$values[i+1] == "None")
      GroomingReport$correct_transitions <- GroomingReport$correct_transitions + 1
    else
      GroomingReport$incorrect_transitions <- GroomingReport$incorrect_transitions + 1
  }
  
  GroomingReport$TransitionsRate <- GroomingReport$incorrect_transitions/(GroomingReport$correct_transitions + GroomingReport$incorrect_transitions)
  
  
  #Reporte de Grooming Bouts
  
  GroomingReport$BoutsCount <- sum(CalculateTransitions(GroomingLengthEncoding$values == "None", 0))/2
  
  GroomingReport$CompleteBouts <- 0
  
  CompletePattern <- c("None", "Paws/Nose Grooming", "Face/HeadWashing", "BodyGrooming", "GenitalsLicking","None")
  
  for(i in 1:(length(GroomingLengthEncoding$values) - length(CompletePattern) + 1)){
    if(length(GroomingLengthEncoding$values <= length(CompletePattern)))
       break
    if(all(GroomingLengthEncoding$values[i:(i+length(CompletePattern) - 1)] == CompletePattern)){
      GroomingReport$CompleteBouts <- GroomingReport$CompleteBouts + 1
    }
  }
  
  GroomingReport$IncompleteBouts <- GroomingReport$BoutsCount - GroomingReport$CompleteBouts
  
  GroomingReport$TimeGrooming <- sum(GroomingLengthEncoding$lengths[GroomingLengthEncoding$values != "None"])/t$fps
  
  GroomingReport$AvgBoutTime <- GroomingReport$TimeGrooming/GroomingReport$BoutsCount
  
  #GroomingReport$Interrupted$Bouts <- length(ShortNoneSequences)
  
  
  #Reporte por regiones cefalo/caudal
  
  GroomingReport$RostralPatterns <- sum(ifelse(is.null(GroomingReport$"Paws/NoseGrooming.Count"), 0, GroomingReport$"Paws/NoseGrooming.Count"),
                                        ifelse(is.null(GroomingReport$"Face/HeadWashing.Count"), 0, GroomingReport$"Face/HeadWashing.Count"))
  #length(GroomingLengthEncoding$values[GroomingLengthEncoding$values == "Nose/Paws Grooming" | GroomingLengthEncoding$values == "Face/Head Washing"])
  
  GroomingReport$CaudalPatterns <- sum(ifelse(is.null(GroomingReport$"BodyGrooming.Count"), 0 , GroomingReport$"BodyGrooming.Count"),
                                       ifelse(is.null(GroomingReport$"GenitalsLicking.Count"), 0 , GroomingReport$"GenitalsLicking.Count"),
                                       ifelse(is.null(GroomingReport$"HindPawScratching.Count"), 0, GroomingReport$"HindPawScratching.Count"))
  #length(GroomingLengthEncoding$values[GroomingLengthEncoding$values == "Body Grooming" | GroomingLengthEncoding$values =="Genitals Licking" | GroomingLengthEncoding$values == "Hind Paw Scratching"])
  
  GroomingReport$TotalPatterns <- GroomingReport$RostralPatterns + GroomingReport$CaudalPatterns
  
  GroomingReport$PercentRostralpatterns <- GroomingReport$RostralPatterns / GroomingReport$TotalPatterns
  
  GroomingReport$RostralTime <- sum(GroomingLengthEncoding$lengths[GroomingLengthEncoding$values == "Paws/Nose Grooming" | GroomingLengthEncoding$values == "Face/Head Washing"])/t$fps
  
  GroomingReport$CaudalTime <- sum(GroomingLengthEncoding$lengths[GroomingLengthEncoding$values == "Body Grooming" | GroomingLengthEncoding$values == "Genitals Licking" | GroomingLengthEncoding$values == "Hind Paw Scratching"])/t$fps
  
  GroomingReport$PercentRostralTime <- GroomingReport$RostralTime/GroomingReport$TimeGrooming
  
  #GroomingReport$AvgRostralTime <- RostralPatterns / RostralTime
  
  return(GroomingReport)
}

pipeline_OF <- function(pathOFD, pathOFU){
  # leer valores de fps para cada archivo
  ParametersOF <- read.table("E:/AGMOT/Analisis DLC/OFD/Start_labels.csv", sep = ",", header = T)
  
  ## sincronizar y combinar archivos de OFD y OFU  
  
  # leer archivos .csv de OFD y OFU
  TrackingOFD <- ReadDLCDataFromCSV_OFD(file = pathOFD, fps = ParametersOF[ParametersOF$CSVname == basename(pathOFD), 8])
  TrackingOFU <- ReadDLCDataFromCSV_OFU(file = pathOFU, fps = ParametersOF[ParametersOF$CSVname == basename(pathOFU), 8])
  
  # ajustar frecuencia de muestreo para los data frame si no es igual a 14.29  
  if(TrackingOFD$fps != 14.29){
    TrackingOFD <- adjust_sample_rate(TrackingOFD)
  }
  if(TrackingOFU$fps != 14.29){
    TrackingOFU <- adjust_sample_rate(TrackingOFU)
  
  }
  # Encontrar el valor de ajuste (numero de frames)
  AdjustRate <- FindAdjustRate(TrackingOFD, TrackingOFU, ParametersOF)
  
  # sincronizar los archivos de OFU para que coincida en numero de frames con OFD  
  TrackingOFU <- SynchFrames(TrackingOFU, TrackingOFD, AdjustRate)
  
  # combinar valores de OFD y OFU  
  TrackingCombinedOF <- CombineTrackingOF(TrackingOFD, TrackingOFU)

    
  ## limpiar, calibrar y analizar
  
  #cortar datos
  StartFrame <- round(ParametersOF[ParametersOF$CSVname == TrackingCombinedOF[["filename"]], 3] * TrackingCombinedOF$fps)
  
  TrackingCombinedOF <- CutTrackingData(TrackingCombinedOF, start = StartFrame, end = length(TrackingCombinedOF$frames) - TrackingCombinedOF$fps*1800 - StartFrame -1)
  
  # calibrar medidas del laberinto (con los puntos de OFU)
  TrackingCombinedOF <- CalibrateTrackingData(TrackingCombinedOF, method = "area", in.metric = 42*42,points = c("OFU_tl","OFU_tr","OFU_br","OFU_bl"))
  
  # agregar zonas (centro y periferia)
  TrackingCombinedOF <- AddOFTZones(TrackingCombinedOF, scale_center = 0.5,scale_periphery  = 0.5 ,scale_corners = 0.5, points = c("OFU_tl","OFU_tr","OFU_br","OFU_bl"))
  
  # limpiar datos
  TrackingCombinedOF <- CleanTrackingData(TrackingCombinedOF, likelihoodcutoff = 0.6)
  
  # analisis de campo abierto
  TrackingCombinedOF <- OFTAnalysis(TrackingCombinedOF, movement_cutoff = 5,integration_period = 5, points = "OFU_bodycentre")
  
  # crear representación esqueletica  
  TrackingCombinedOF <- CreateSkeletonData_OF_combined(TrackingCombinedOF)
  
  # Clasificar comportamientos
  TrackingCombinedOF <- ClassifyBehaviors(TrackingCombinedOF, model_grooming, MLData$parameters)
  
  GroomingReport <- GroomingReport(TrackingCombinedOF)
  
  TrackingCombinedOF$Report <- c(TrackingCombinedOF$Report, GroomingReport)
  
  #TrackingCombinedOF <- AddBinData(TrackingCombinedOF, unit = "minute", binlength = 5)
  
  #TrackingCombinedOF$BinReport <- BinAnalysis(TrackingCombinedOF, FUN = OFTAnalysis, movement_cutoff = 5,integration_period = 5, points = "OFU_bodycentre")

  return(TrackingCombinedOF)
}
