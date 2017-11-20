siri <- function(bodyDensity) {
  return(((4.95/bodyDensity)-4.5)*100)
}
sloan <- function(db) {
    sloanBD <- ifelse(db$cinsiyet==1,
                      1.0764 - 0.00081*db$dkk_silliak - 0.00088*db$dkk_triceps,
                      1.1043 - 0.001327*db$dkk_quadriceps - 0.001310*db$dkk_sscapula
                )
    
    return(siri(sloanBD))
}

wilmore <- function(db) {
    wilmoreBD <- ifelse(db$cinsiyet=='1',
                        1.06234 - 0.00068*db$dkk_sscapula - 0.00039*db$dkk_triceps -
                          0.00025*db$dkk_quadriceps,
                        1.08543 - 0.000886*db$dkk_abdomen - 0.00040*db$dkk_quadriceps 
                        )
    return(siri(wilmoreBD))
}

