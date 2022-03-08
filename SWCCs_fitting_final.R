library(soilhypfit)
library(robustbase)
library(data.table)
library(tidyverse)
library(rgdal)
library(rgeos)
library(raster)# for metadata/attributes- vectors or rasters
library(dplyr)
library(dbplyr)
library(ggplot2)
library(sf)
library(raster)

final_dataset<- read.csv("C:/Users/guptasu.D/Documents/GSHP manuscript/WRC_dataset_surya_et_al_2021_final.csv")

final_dataset<- final_dataset[,c(1:34)]

urves441 <-final_dataset %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


YWYD<- urves441[urves441$SWCC_classes=="YWYD",]

YWYD$thetar.L<-0 

YWYD$thetar.U<-1

YWYD$thetas.L<-0 

YWYD$thetas.U<-1

lower.thetar.thetas_YWYD<- YWYD[,c(1,35,37)]

upper.thetar.thetas_YWYD<-YWYD[,c(1,36,38)]

rownames(lower.thetar.thetas_YWYD) <- rownames(upper.thetar.thetas_YWYD) <- levels(
  upper.thetar.thetas_YWYD$layer_id
)

lower.thetar.thetas_YWYD<-lower.thetar.thetas_YWYD %>% remove_rownames %>% column_to_rownames(var="layer_id")

upper.thetar.thetas_YWYD<-upper.thetar.thetas_YWYD %>% remove_rownames %>% column_to_rownames(var="layer_id")

colnames(lower.thetar.thetas_YWYD)[which(colnames(lower.thetar.thetas_YWYD) %in% c("thetar.L") )] <- 
  c("thetar")

colnames(lower.thetar.thetas_YWYD)[which(colnames(lower.thetar.thetas_YWYD) %in% c("thetas.L") )] <- 
  c("thetas")

colnames(upper.thetar.thetas_YWYD)[which(colnames(upper.thetar.thetas_YWYD) %in% c("thetar.U") )] <- 
  c("thetar")

colnames(upper.thetar.thetas_YWYD)[which(colnames(upper.thetar.thetas_YWYD) %in% c("thetas.U") )] <- 
  c("thetas")

YWND<- urves441[urves441$SWCC_classes=="YWND",]

restention_5curve1<-YWND

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Sureface area
##Clay = 7400000 cm2/g
##Silt = 11100 cm2/g
##Sand = 444 cm2/g

restention_5curve1$clay_SA<- restention_5curve1$clay_g_100g*7400000

restention_5curve1$sand_SA<- restention_5curve1$sand_g_100g*444

restention_5curve1$silt_SA<- restention_5curve1$silt_g_100g*11100

##Total surface area

restention_5curve1$SA<- restention_5curve1$sand_SA+restention_5curve1$silt_SA+restention_5curve1$clay_SA

#Transform from cm2/g to m2/kg 

restention_5curve1$SA_m2_kg<- restention_5curve1$SA /10

restention_5curve1$h_cube_root_lwr<- 3.5e-10

restention_5curve1$h_cube_root_upr<- 7e-10

## Estimation of gravimetric water contnet at 150 m suction head

restention_5curve1$thetar_lwr<- restention_5curve1$SA_m2_kg * 997 * restention_5curve1$h_cube_root_lwr

restention_5curve1$thetar_upr<- restention_5curve1$SA_m2_kg * 997 * restention_5curve1$h_cube_root_upr

## Convert gravimetric water content to percentage by multiplying 100

restention_5curve1$thetar_vol_lwr<- ((restention_5curve1$thetar_lwr) *restention_5curve1$db_od )

restention_5curve1$thetar_vol_upr<- ((restention_5curve1$thetar_upr) *restention_5curve1$db_od )

lower.thetar.thetas_YWND<- restention_5curve1[,c(1,47)]

upper.thetar.thetas_YWND<-restention_5curve1[,c(1,48)]

#kjkj<-  final_dataset[final_dataset$layer_id%in% lower.thetar.thetas_YWND$layer_id,]

lower.thetar.thetas_YWND<-lower.thetar.thetas_YWND %>% remove_rownames %>% column_to_rownames(var="layer_id")

upper.thetar.thetas_YWND<-upper.thetar.thetas_YWND %>% remove_rownames %>% column_to_rownames(var="layer_id")

lower.thetar.thetas_YWND$thetas<- 0

upper.thetar.thetas_YWND$thetas<-1

colnames(upper.thetar.thetas_YWND)

colnames(lower.thetar.thetas_YWND)[which(colnames(lower.thetar.thetas_YWND) %in% c("thetar_vol_lwr") )] <- 
  c("thetar")

colnames(upper.thetar.thetas_YWND)[which(colnames(upper.thetar.thetas_YWND) %in% c("thetar_vol_upr") )] <- 
  c("thetar")



## NWYD


NWYD<- urves441[urves441$SWCC_classes=="NWYD",]

NWYD1<- NWYD[!is.na(NWYD$sand_tot_psa_percent),]

NWYD2<- NWYD[is.na(NWYD$sand_tot_psa_percent),]

NWYD1$region <- factor(
  rep("temperate", NROW(NWYD1)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
NWYD1$region[NWYD1$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, NWYD1, interval = "prediction")

NWYD1<-cbind(NWYD1,pp)



NWYD2$region <- factor(
  rep("temperate", NROW(NWYD2)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
NWYD2$region[NWYD2$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, NWYD2, interval = "prediction")

NWYD2<-cbind(NWYD2,pp)

NWYD<- rbind(NWYD1, NWYD2)


lower.thetar.thetas_NWYD<- NWYD[,c(1,37)]

upper.thetar.thetas_NWYD<-NWYD[,c(1,38)]

lower.thetar.thetas_NWYD<-lower.thetar.thetas_NWYD %>% remove_rownames %>% column_to_rownames(var="layer_id")

upper.thetar.thetas_NWYD<-upper.thetar.thetas_NWYD %>% remove_rownames %>% column_to_rownames(var="layer_id")

lower.thetar.thetas_NWYD$thetar<- 0

upper.thetar.thetas_NWYD$thetar<-1


colnames(lower.thetar.thetas_NWYD)[which(colnames(lower.thetar.thetas_NWYD) %in% c("lwr") )] <- 
  c("thetas")


colnames(upper.thetar.thetas_NWYD)[which(colnames(upper.thetar.thetas_NWYD) %in% c("upr") )] <- 
  c("thetas")

## NWND

NWND<- urves441[urves441$SWCC_classes=="NWND",]

restention_5curve1<-NWND

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Sureface area
##Clay = 7400000 cm2/g
##Silt = 11100 cm2/g
##Sand = 444 cm2/g

restention_5curve1$clay_SA<- restention_5curve1$clay_g_100g*7400000

restention_5curve1$sand_SA<- restention_5curve1$sand_g_100g*444

restention_5curve1$silt_SA<- restention_5curve1$silt_g_100g*11100

##Total surface area

restention_5curve1$SA<- restention_5curve1$sand_SA+restention_5curve1$silt_SA+restention_5curve1$clay_SA

#Transform from cm2/g to m2/kg 

restention_5curve1$SA_m2_kg<- restention_5curve1$SA /10

restention_5curve1$h_cube_root_lwr<- 3.5e-10

restention_5curve1$h_cube_root_upr<- 7e-10

## Estimation of gravimetric water contnet at 150 m suction head

restention_5curve1$thetar_lwr<- restention_5curve1$SA_m2_kg * 997 * restention_5curve1$h_cube_root_lwr

restention_5curve1$thetar_upr<- restention_5curve1$SA_m2_kg * 997 * restention_5curve1$h_cube_root_upr

## Convert gravimetric water content to percentage by multiplying 100

restention_5curve1$thetar_vol_lwr<- ((restention_5curve1$thetar_lwr) *restention_5curve1$db_od )

restention_5curve1$thetar_vol_upr<- ((restention_5curve1$thetar_upr) *restention_5curve1$db_od )

restention_5curve1$region <- factor(
  rep("temperate", NROW(restention_5curve1)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
restention_5curve1$region[restention_5curve1$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, restention_5curve1, interval = "prediction")

restention_5curve1<-cbind(restention_5curve1,pp)

colnames(restention_5curve1)


lower.thetar.thetas_NWND<- restention_5curve1[,c(1,47,51)]

upper.thetar.thetas_NWND<-restention_5curve1[,c(1,48,52)]


lower.thetar.thetas_NWND<-lower.thetar.thetas_NWND %>% remove_rownames %>% column_to_rownames(var="layer_id")

upper.thetar.thetas_NWND<-upper.thetar.thetas_NWND %>% remove_rownames %>% column_to_rownames(var="layer_id")


colnames(lower.thetar.thetas_NWND)[which(colnames(lower.thetar.thetas_NWND) %in% c("lwr") )] <- 
  c("thetas")


colnames(upper.thetar.thetas_NWND)[which(colnames(upper.thetar.thetas_NWND) %in% c("upr") )] <- 
  c("thetas")

colnames(lower.thetar.thetas_NWND)[which(colnames(lower.thetar.thetas_NWND) %in% c("thetar_vol_lwr") )] <- 
  c("thetar")

colnames(upper.thetar.thetas_NWND)[which(colnames(upper.thetar.thetas_NWND) %in% c("thetar_vol_upr") )] <- 
  c("thetar")


lower.thetar.thetas_NWYD<- lower.thetar.thetas_NWYD[,c(2,1)]

upper.thetar.thetas_NWYD<- upper.thetar.thetas_NWYD[,c(2,1)]


lower.thetar.thetas<- rbind(lower.thetar.thetas_NWND, lower.thetar.thetas_NWYD, lower.thetar.thetas_YWND, lower.thetar.thetas_YWYD)

lower.thetar.thetas$thetar<- as.numeric(lower.thetar.thetas$thetar)

lower.thetar.thetas$thetas<- as.numeric(lower.thetar.thetas$thetas)

lower.thetar.thetas<- data.matrix(lower.thetar.thetas)

upper.thetar.thetas<- rbind(upper.thetar.thetas_NWND,upper.thetar.thetas_NWYD, upper.thetar.thetas_YWND, upper.thetar.thetas_YWYD)

upper.thetar.thetas<-data.matrix(upper.thetar.thetas)

str(lower.thetar.thetas)

str(upper.thetar.thetas)

sum(sel<- lower.thetar.thetas[, 1] > lower.thetar.thetas[, 2])

sel_thetas_zero<- lower.thetar.thetas[,2] == 0

sum(sel_thetas_zero)

lower.thetar.thetas[sel_thetas_zero,2]<- lower.thetar.thetas[sel_thetas_zero,1]

lower.thetar.thetas[sel,]

sum(sel<- upper.thetar.thetas[, 1] > upper.thetar.thetas[, 2])

sel_thetar_one<- upper.thetar.thetas[,1] == 1

sum(sel_thetar_one)

upper.thetar.thetas[sel_thetar_one,1]<- upper.thetar.thetas[sel_thetar_one,2]

YWND$layer_id


final_dataset$lab_head_m[final_dataset$lab_head_m == 0]<- 0.01

# # system.time(r.fit.uglobal.probenid.constrained123 <- fit_wrc_hcc(
# #   wrc_formula = lab_wrc ~ lab_head_m | layer_id,
# #   data = final_dataset ,
# #   lower_param = lower.thetar.thetas,
# #   upper_param = upper.thetar.thetas, verbose = 2,
# #   control = control_fit_wrc_hcc(
# #     min_nobs_wc = 4,
# #     keep_empty_fits = TRUE,
# #     nloptr = control_nloptr(maxeval = 250),
# #     param_bound = param_boundf(
# #       alpha = c(1.490116e-07 , 100.),
# #       n = c(1., 7.)
# #     )
# #   )
# # ))
# 
# write.csv(original_para1,"C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/r3_sand_YWYD_Global.csv" )
# 
# save(r.fit.uglobal.probenid.constrained12356,file = "C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/global_all.RData")
# 
# extract_error_messages()
# 
# select_failed_fits(r.fit.uglobal.probenid.constrained123)
# 
# summary(r.fit.uglobal.probenid.constrained123)
# 
# u.global<- coef(r.fit.uglobal.probenid.constrained123, se = TRUE)



system.time(r.fit.uglobal.probenid.constrained12356 <- fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = final_dataset ,
  lower_param = lower.thetar.thetas,
  upper_param = upper.thetar.thetas, verbose = 1,
  control = control_fit_wrc_hcc(
    min_nobs_wc = 4,
    keep_empty_fits = TRUE,
    nloptr = control_nloptr(maxeval = 250),
    param_bound = param_boundf(
      alpha = c(1.490116e-07 , 100.),
      n = c(1., 7.)
    )
  )
))

# save(r.fit.uglobal.probenid.constrained12356,file = "C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/global_all.RData")

u.global1<- coef(r.fit.uglobal.probenid.constrained12356, se = TRUE,gof = TRUE)

# write.csv(u.global1,"C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/global_coef.csv")

u.global2<- coef(r.fit.uglobal.probenid.constrained12356, se = TRUE,gof = TRUE)[, c("alpha", "n")]




r.fit.ulocal.probenid.constrained1 <- update(
  r.fit.uglobal.probenid.constrained12356,
  param = u.global2,
  control = control_fit_wrc_hcc(
    settings = "ulocal",
    min_nobs_wc = 4,
    keep_empty_fits = FALSE,
    param_tf = param_transf(alpha = "identity", n = "identity", tau = "identity"),
    param_bound = param_boundf(
      alpha = c(1.490116e-07 , 100.),
      n = c(1., 7.)
    )  
  )
)

# save(r.fit.ulocal.probenid.constrained1,file = "C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/locall_all.RData" )
# 
# write.csv(original_para,"C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/local_coef.csv")

original_para <- coef(r.fit.ulocal.probenid.constrained1, se = TRUE,  gof = TRUE)


### Likelihood-based confidence intervals 

local_CI_95<-confint(r.fit.ulocal.probenid.constrained1, level = 0.95)

local_CI_80<-confint(r.fit.ulocal.probenid.constrained1, level = 0.8)

local_CI_50<-confint(r.fit.ulocal.probenid.constrained1, level = 0.5)

# write.csv(local_CI_95,"C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/CI_95.csv")
# 
# write.csv(local_CI_80,"C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/CI_80.csv")
# 
# write.csv(local_CI_50,"C:/Users/guptasu.D/Downloads/Binary_SWCCs_dataset/CI_50.csv")

