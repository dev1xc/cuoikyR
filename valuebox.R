valuebox1 <- nrow(linelist)
valuebox1 

valuebox2 <- median(linelist$wt_kg)
valuebox3 <- median(linelist$ht_cm)

calc_survival_ratio <- round((sum(linelist$outcome == "Recover")/valuebox1)*100,0) 
valuebox4 <- paste0(calc_survival_ratio, " %")

valuebox5 <- round(median(linelist$age),2)
valuebox5 

valuebox6 <- round(median(linelist$bmi),2)
valuebox6 
