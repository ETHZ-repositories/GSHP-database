library(robustbase)

## PTF_for thetas

Wrc_lab<- read.csv("C:/Users/guptasu.D/Downloads/WRC_dataset_surya_et_al_2021_final.csv")


##First_case

WRC_less_0.01m<-Wrc_lab %>% group_by(layer_id,site_key) %>%
  summarise(count = sum(lab_head_m<=0.01 ))

WRC_less_0.01m_1<- subset(WRC_less_0.01m, WRC_less_0.01m$count>0)

test1<-Wrc_lab[Wrc_lab$layer_id %in% WRC_less_0.01m_1$layer_id,]

WRC_greater_150m<-test1 %>% group_by(layer_id,site_key) %>%
  summarise(count = sum(lab_head_m>=150 ))

WRC_greater_150m_1<- subset(WRC_greater_150m, WRC_greater_150m$count>0)


test2<- test1[test1$layer_id %in% WRC_greater_150m_1$layer_id,]


Samples_without_error<- test2[!test2$tex_psda=="Error",]

Sand<- Samples_without_error[!is.na(Samples_without_error$sand_tot_psa_percent),]

Clay<- Sand[!is.na(Sand$clay_tot_psa_percent),]

unique(Clay$layer_id)

## 1925
## Tropical

Tropical_WRC<- Clay[Clay$climate_classes=="Tropical",] 

Tropical_WRC_clay<-Tropical_WRC[!is.na (Tropical_WRC$clay_tot_psa_percent), ]

unique(Tropical_WRC_clay$layer_id)

##Temperate

temp_WRC<- Clay[!Clay$climate_classes=="Tropical",] 

Temp_WRC_clay<-temp_WRC[!is.na (temp_WRC$clay_tot_psa_percent), ]

unique(Temp_WRC_clay$layer_id)

## Temp_trop

Hybras_temp<- Temp_WRC_clay[Temp_WRC_clay$source_db=="HYBRAS",]

unique(Hybras_temp$layer_id)

temperate<- Temp_WRC_clay[!Temp_WRC_clay$source_db=="HYBRAS",]

unique(temperate$layer_id)


## Fitting curves

## Tropical_curve

system.time(
  r1 <- fit_wrc_hcc(
    wrc_formula = lab_wrc ~ lab_head_m| layer_id, 
    data = Tropical_WRC_clay,
    control = control_fit_wrc_hcc(
      settings = "uglobal",
      min_nobs_wc = 4,
      keep_empty_fits = TRUE,
      pcmp = control_pcmp(ncores = 11),
      nloptr = control_nloptr(ranseed = 1)
    ), verbose = 0
  ))

r11 <- coef(r1,gof = TRUE)

r11$region<- "tropical"

## Temperate
system.time(
  r2 <- fit_wrc_hcc(
    wrc_formula = lab_wrc ~ lab_head_m| layer_id, 
    data = temperate,
    control = control_fit_wrc_hcc(
      settings = "uglobal",
      min_nobs_wc = 4,
      keep_empty_fits = TRUE,
      pcmp = control_pcmp(ncores = 11),
      nloptr = control_nloptr(ranseed = 1)
    ), verbose = 0
  ))

r21 <- coef(r2,gof = TRUE)

r21$region<- "temperate"

## Temperate/tropical

system.time(
  r4 <- fit_wrc_hcc(
    wrc_formula = lab_wrc ~ lab_head_m| layer_id, 
    data = Hybras_temp,
    control = control_fit_wrc_hcc(
      settings = "uglobal",
      min_nobs_wc = 4,
      keep_empty_fits = TRUE,
      pcmp = control_pcmp(ncores = 11),
      nloptr = control_nloptr(ranseed = 1)
    ), verbose = 0
  ))


r41 <- coef(r4,gof = TRUE)

r41$region<- "temperate.hybras"

Total_curves<- rbind(r11, r21, r41)

#  write.csv(Total_curves, "C:/Users/guptasu.D/Documents/Thetas_1925.csv")
#  
#  Thetas_1925<- read.csv("C:/Users/guptasu.D/Documents/Thetas_1925.csv")
#  
# Thetas_1925$layer_id<- Thetas_1925$X


Curves_1929<- merge(Clay, Total_curves, by = "layer_id")

Curves_1929_thetas <-Curves_1929 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

table(Curves_1929_thetas$Classes)

Curves_1929_thetas<- Curves_1929_thetas[!is.na(Curves_1929_thetas$db_od),]

# write.csv(Curves_1929_thetas, "C:/Users/guptasu.D/Documents/Final_thetas_1925.csv")
# 
# colnames(Curves_1929_thetas)

##lm_rob

summary(r.lmrob.texture.bd.texture.1<- lmrob(
  thetas.y ~ db_od+region+sand_tot_psa_percent+clay_tot_psa_percent, data = Curves_1929_thetas, scale= sse ))

Curves_1929_thetas$prediction<- predict(r.lmrob.texture.bd.texture.1, Curves_1929_thetas)

hhhkl<- Curves_1929_thetas[!is.na(Curves_1929_thetas$prediction),]

hexbinplot(thetas~ prediction, 
           panel = function(x, y, ...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y,span = 2/3, col.line = "blue",type="l", lty=2, lwd = 4)
             panel.abline(c(0, 1),lwd = 2)
           },
           data =Curves_1929_thetas,xlab = "Predicted thetas [m3/m3]", ylab = "Measured thetas [m3/m3]",cex.axis = 4, aspect="1", xbins=25,font.lab= 14, cex.labels = 1.5,font.axis = 40,
           colramp = 
             function(n) {viridis (8,  alpha = 1, begin = 0, end = 1, direction = -1,option = "C")},xlim=c(0.2,0.9), ylim=c(0.2,0.9),
           colorcut=c(0,0.01,0.03,0.07,0.15,0.25,0.5,0.75,1) )

# bias(hhhkl$thetas, hhhkl$prediction)
# 
# 
# Curves_1929_thetas$prediction<- as.numeric(Curves_1929_thetas$prediction, na.rm = "TRUE")
# 
# 
# str(Curves_1929_thetas)
