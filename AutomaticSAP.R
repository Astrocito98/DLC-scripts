#funcion para detectar Stretched Attend Posture basado en Holly, K.S. et. al (2016)
automatic.SAP <- function(t, integration_period){
  EccenThreslhold <- 0.96
  SpeedThreshold <- 9.0
  
  lenght.axis <- t$features$S11
  wide.axis <- t$features$S12
  
  Eccentricity <- ifelse(lenght.axis == 0| wide.axis== 0, 0,
              ifelse(lenght.axis >= wide.axis, sqrt(1-(wide.axis^2/lenght.axis^2)), sqrt(1-(lenght.axis^2/wide.axis^2)))
  )
  Eccentricity[is.na(Eccentricity)] <- 0
  

  t$labels$automatic.SAP <- avgbool(Eccentricity > EccenThreslhold & t$data$bodycentre$speed <= SpeedThreshold, integration_period)
  t$Report[["SAP"]] <- CalculateTransitions(t$labels$automatic.SAP,integration_period) / 2
  t$labels$automatic.SAP <- ifelse(t$labels$automatic.SAP == 1,"SAP","None")

  return(t)
}
  #t$labels$automatic.SAP <-ifelse(Eccentricity <= EccenThreshold, 'None',
  #                      ifelse(Speed > SpeedThreshold, 'None', 'SAP'))
