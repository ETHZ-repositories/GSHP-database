library(soilhypfit)

samples_with_bulk_density<- read.csv("C:/Users/guptasu.D/Downloads/WRC_dataset_surya_et_al_2021_final.csv")

samples_with_bulk_density<- samples_with_bulk_density[,c(1:29)]

soiltexture_sand<- samples_with_bulk_density[!is.na(samples_with_bulk_density$sand_tot_psa_percent),]

soiltexture_clay<- soiltexture_sand[!is.na(soiltexture_sand$clay_tot_psa_percent),]

soiltexture_silt<- soiltexture_clay[!is.na(soiltexture_clay$silt_tot_psa_percent),]

unique(soiltexture_silt$layer_id)

##11247 curves have soil texture information

## now I am making two sets

##first set = samples with soil texture information
## second set = samples with no soil texture information

set_with_texture_infor<- soiltexture_clay

soiltexture_sand1<- samples_with_bulk_density[is.na(samples_with_bulk_density$sand_tot_psa_percent),]

soiltexture_clay1<- soiltexture_sand[is.na(soiltexture_sand$clay_tot_psa_percent),]

set_without_texture_infor<- rbind(soiltexture_sand1,soiltexture_clay1)

unique(set_without_texture_infor$layer_id)


#write.csv(set_with_texture_infor, "E:/Andreas_ksat_data/nonsensical_data_review/samples_with_soil_texture_information.csv")

#set_with_texture_infor_1<- read.csv("E:/Andreas_ksat_data/nonsensical_data_review/samples_with_soil_texture_information.csv")

#unique(set_with_texture_infor_1$layer_id)

remove_Error_ST<- set_with_texture_infor[set_with_texture_infor$tex_psda=="Error",]

set_without_texture_infor<- rbind(set_without_texture_infor,remove_Error_ST )

unique(set_without_texture_infor$layer_id)

set_with_texture_infor_2<- set_with_texture_infor[!set_with_texture_infor$tex_psda=="Error",]

table(set_with_texture_infor_2$tex_psda)

unique(set_with_texture_infor_2$layer_id)

## Now I will divide three sets

##1: sandy soils (sand texture class)
##2: no sandy soils (except sand and loamy sand texture class)
##3: loamy sand (loamy sand texture class)

unique(non_sandy_soils$layer_id)

sandysoils<- set_with_texture_infor_2[set_with_texture_infor_2$tex_psda=="sand",]

loamysandsoils<- set_with_texture_infor_2[set_with_texture_infor_2$tex_psda=="loamy sand",]

non_sandy_soils<- set_with_texture_infor_2 %>% filter( !tex_psda %in% c("loamy sand","sand"))

## sandy soils fitting curves


## sandy soil samples with wet end 

Curves_with_less_0.2_sandy_soil<- subset(sandysoils,sandysoils$lab_head_m<=0.2)

Curves_with_thetas_2_sandy_soil<-sandysoils[sandysoils$layer_id %in% Curves_with_less_0.2_sandy_soil$layer_id,]

unique(Curves_with_thetas_2_sandy_soil$layer_id)

## sandy soil samples without wet end 

Curves_with_thetas_not_wetend_sandy_soil<-sandysoils[!sandysoils$layer_id %in% Curves_with_less_0.2_sandy_soil$layer_id,]

unique(Curves_with_thetas_not_wetend_sandy_soil$layer_id)

## now work with with wet end samples

## divided data into 2 parts

## where we have both wet and dry end
## where we have dry end

Curves_with_less_145_sandy_soil<- subset(Curves_with_thetas_2_sandy_soil,Curves_with_thetas_2_sandy_soil$lab_head_m>=140)

Curves_with_dry_145_sandy_soil<-Curves_with_thetas_2_sandy_soil[Curves_with_thetas_2_sandy_soil$layer_id %in% Curves_with_less_145_sandy_soil$layer_id,]

unique(Curves_with_dry_145_sandy_soil$layer_id)

## where we do not have dry end

Curves_without_dry_145_sandy_soil<-Curves_with_thetas_2_sandy_soil[!Curves_with_thetas_2_sandy_soil$layer_id %in% Curves_with_less_145_sandy_soil$layer_id,]

Curves_without_dry_145_sandy_soil<- Curves_without_dry_145_sandy_soil[!Curves_without_dry_145_sandy_soil$layer_id=="Miguel_Cooper14",]
unique(Curves_without_dry_145_sandy_soil$layer_id)

## going to start with sandy soil with both wet and dry end

## S_with_wet_end and dry_end------- 

Curves_with_dry_145_sandy_soil$lab_head_m[Curves_with_dry_145_sandy_soil$lab_head_m == 0]<- 0.01

unique(Curves_with_dry_145_sandy_soil$layer_id)

r3 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_dry_145_sandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = rbind(
    c(alpha = 0.001, n = 2)), upper_param = rbind(
      c(alpha = 100, n = 7))
)

original_para <- coef(r3,gof = TRUE)

#r.coef.sandy_soils_with_dry$class<- "YWYD"


pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/sandy_soil_1.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r3,draw_parameter = TRUE)
dev.off()

## Error test_case 1---------

rss.original_case1<-sapply(r3$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case1<- data.frame(original_para, rss.original_case1)

jj_case1$class<- "YWYD"

## S_with_wet_end and not_dry end-------

## calculation of thetar

restention_5curve1<-Curves_without_dry_145_sandy_soil

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Surface area
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

tmp0 <-restention_5curve1 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_without_dry_145_sandy_soil

selected_curves1_lwr$alpha<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

#tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(30,31)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

thetar<- as.numeric(tmp0$thetar_vol_lwr)
#thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetar)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_without_dry_145_sandy_soil

selected_curves1_upr$alpha<- selected_curves1_upr$tex_psda

selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 100

selected_curves1_upr$n<- selected_curves1_upr$tex_psda

selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
#tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(30,31)]

thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

#thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetar)

#write.csv(upr, "E:/Andreas_ksat_data/nonsensical_data_review/upr_alpha_n_thetas.csv")

Curves_without_dry_145_sandy_soil$lab_head_m[Curves_without_dry_145_sandy_soil$lab_head_m == 0]<- 0.01

unique(Curves_without_dry_145_sandy_soil$layer_id)

r4 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_without_dry_145_sandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_case2 <- coef(r4,gof = TRUE)

# r.coef.2$class<- "YWND"

# r.coef.2_n_7<- subset(r.coef.2, r.coef.2$n==7)
# 
# r.coef.2_n_2<- subset(r.coef.2, r.coef.2$n==2)
# 
# r.coef.2_alpha_30<- subset(r.coef.2, r.coef.2$alpha==30)

#r.coef.without.boxconstraint.thetas<- cbind(r.coef.without.boxconstraint.thetas,tc)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/sandy_soil_withoud_dry_all_thetar.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r4,draw_parameter = TRUE)

dev.off()

## Error test_case 2---------

rss.original_case2<-sapply(r4$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case2<- data.frame(original_para_case2, rss.original_case2)

jj_case2$class<- "YWND"
## sandy soils where we do not have wet ends

Curves_with_thetas_not_wetend_sandy_soil

## Divided this into two parts 

# S_Curves with dry end but not wet end----

Curves_with_thetas_not_wetend_sandy_soil_with_dry_end<- subset(Curves_with_thetas_not_wetend_sandy_soil,Curves_with_thetas_not_wetend_sandy_soil$lab_head_m>=140)

Curves_with_dry_145_sandy_soil<-Curves_with_thetas_not_wetend_sandy_soil[Curves_with_thetas_not_wetend_sandy_soil$layer_id %in% 
                                                                           Curves_with_thetas_not_wetend_sandy_soil_with_dry_end$layer_id,]

unique(Curves_with_dry_145_sandy_soil$layer_id)

## where we do not have dry end

Curves_without_dry_145_sandy_soil<-Curves_with_thetas_not_wetend_sandy_soil[!Curves_with_thetas_not_wetend_sandy_soil$layer_id %in% 
                                                                              Curves_with_thetas_not_wetend_sandy_soil_with_dry_end$layer_id,]
unique(Curves_without_dry_145_sandy_soil$layer_id)


## I will work with samples where we have dry end

## computation of thetas

Curves_with_dry_145_sandy_soil$region <- factor(
  rep("temperate", NROW(Curves_with_dry_145_sandy_soil)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_with_dry_145_sandy_soil$region[Curves_with_dry_145_sandy_soil$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_with_dry_145_sandy_soil, interval = "prediction")

Curves_with_dry_145_sandy_soil<-cbind(Curves_with_dry_145_sandy_soil,pp)

tmp <-Curves_with_dry_145_sandy_soil %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_with_dry_145_sandy_soil

selected_curves1_lwr$alpha<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

#thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_with_dry_145_sandy_soil

selected_curves1_upr$alpha<- selected_curves1_upr$tex_psda

selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 100

selected_curves1_upr$n<- selected_curves1_upr$tex_psda

selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

#thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas)

r5 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_dry_145_sandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_case3 <- coef(r5,gof = TRUE)

# r.coef.3$class<- "NWYD"
# 
# r.coef.3_n_7<- subset(r.coef.3, r.coef.3$n==7)
# 
# r.coef.3_n_2<- subset(r.coef.3, r.coef.3$n==2)
# 
# r.coef.3_alpha_30<- subset(r.coef.3, r.coef.3$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/sandy_soil_with_dry_without_thetas.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r5,draw_parameter = TRUE)

dev.off()

## Error test_case 3---------

rss.original_case3<-sapply(r5$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case3<- data.frame(original_para_case3, rss.original_case3)

jj_case3$class<- "NWYD"

## where we do not have dry end

# S_Curves with not dry end but not wet end----

Curves_without_dry_145_sandy_soil

## Thetar_calculations

restention_5curve1<-Curves_without_dry_145_sandy_soil

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Surface area
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

tmp0 <-restention_5curve1 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

## thetas_calculation

Curves_without_dry_145_sandy_soil$region <- factor(
  rep("temperate", NROW(Curves_without_dry_145_sandy_soil)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_without_dry_145_sandy_soil$region[Curves_without_dry_145_sandy_soil$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_without_dry_145_sandy_soil, interval = "prediction")

Curves_without_dry_145_sandy_soil<-cbind(Curves_without_dry_145_sandy_soil,pp)

tmp <-Curves_without_dry_145_sandy_soil %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_without_dry_145_sandy_soil

selected_curves1_lwr$alpha<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas,thetar)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_without_dry_145_sandy_soil

selected_curves1_upr$alpha<- selected_curves1_upr$tex_psda

selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 100

selected_curves1_upr$n<- selected_curves1_upr$tex_psda

selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas,thetar)

r6 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_without_dry_145_sandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_case4 <- coef(r6,gof = TRUE)

# r.coef.4$class<- "NWND"

# r.coef.4_n_7<- subset(r.coef.4, r.coef.4$n==7)
# 
# r.coef.4_n_2<- subset(r.coef.4, r.coef.4$n==2)
# 
# r.coef.4_alpha_30<- subset(r.coef.4, r.coef.4$alpha==30)


pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/sandy_soil_without_dry_without_thetas.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r6,draw_parameter = TRUE)

dev.off()

sand_coeff<- rbind(r.coef.sandy_soils_with_dry, r.coef.2, r.coef.3, r.coef.4)

ggplot(sand_coeff, aes(x=class, y=n)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Error test_case 4---------

rss.original_case4<-sapply(r6$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case4<- data.frame(original_para_case4, rss.original_case4)

jj_case4$class<- "NWND"
 

colnames(jj_case1)[which(names(jj_case1) == "rss.original_case1")] <- "RSS"

colnames(jj_case2)[which(names(jj_case2) == "rss.original_case2")] <- "RSS"

colnames(jj_case3)[which(names(jj_case3) == "rss.original_case3")] <- "RSS"

colnames(jj_case4)[which(names(jj_case4) == "rss.original_case4")] <- "RSS"

combine_error_sandy_soil<- rbind(jj_case1,jj_case2,jj_case3, jj_case4)

combine_error_sandy_soil$ST<- "sand_soil"


## Loamy sand samples constrained---------- 

loamysandsoils

## loamy sandy soil samples with wet end 

Curves_with_less_0.2_loamy_sand_soil<- subset(loamysandsoils,loamysandsoils$lab_head_m<=0.2)

Curves_with_thetas_2_loamy_sandy_soil<-loamysandsoils[loamysandsoils$layer_id %in% Curves_with_less_0.2_loamy_sand_soil$layer_id,]

unique(Curves_with_thetas_2_loamy_sandy_soil$layer_id)

## loamy sandy soil samples without wet end 

Curves_without_thetas_2_loamy_sandy_soil<-loamysandsoils[!loamysandsoils$layer_id %in% Curves_with_less_0.2_loamy_sand_soil$layer_id,]

unique(Curves_without_thetas_2_loamy_sandy_soil$layer_id)

## now work with with wet end samples

## divided data into 2 parts

## where we have both wet and dry end
## where we have dry end

Curves_with_less_145_loamysandy_soil<- subset(Curves_with_thetas_2_loamy_sandy_soil,Curves_with_thetas_2_loamy_sandy_soil$lab_head_m>=140)

Curves_with_dry_145_loamysandy_soil<-Curves_with_thetas_2_loamy_sandy_soil[Curves_with_thetas_2_loamy_sandy_soil$layer_id %in% Curves_with_less_145_loamysandy_soil$layer_id,]

unique(Curves_with_dry_145_loamysandy_soil$layer_id)

## where we do not have dry end

Curves_without_dry_145_loamysandy_soil<-Curves_with_thetas_2_loamy_sandy_soil[!Curves_with_thetas_2_loamy_sandy_soil$layer_id %in% Curves_with_less_145_loamysandy_soil$layer_id,]

unique(Curves_without_dry_145_sandy_soil$layer_id)


## going to start with sandy soil with both wet and dry end

## LS_with_wet_end and dry_end------- 

Curves_with_dry_145_loamysandy_soil$lab_head_m[Curves_with_dry_145_loamysandy_soil$lab_head_m == 0]<- 0.01

r12 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_dry_145_loamysandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = rbind(
    c(alpha = 0.001, n = 1)), upper_param = rbind(
      c(alpha = 100, n = 3.5))
)

original_para_LS <- coef(r12,gof = TRUE)

# r.coef.loamysandy_soils_with_dry$class<- "FC"

# r.coef.loamysandy_soils_with_dry_n_3_5<- subset(r.coef.loamysandy_soils_with_dry, r.coef.loamysandy_soils_with_dry$n==3.5)
# 
# r.coef.loamysandy_soils_with_dry_alpha_30<- subset(r.coef.loamysandy_soils_with_dry, r.coef.loamysandy_soils_with_dry$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/loamysandy_soil_1.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r12,draw_parameter = TRUE)
dev.off()

## Error test_LS_case 1---------

rss.original_LS_case1<-sapply(r12$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case1_LS<- data.frame(original_para_LS, rss.original_LS_case1)

jj_case1_LS$class<- "YWYD"

## LS_with_wet_end and not_dry end-------

## calculation of thetar

restention_5curve1<-Curves_without_dry_145_loamysandy_soil

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Surface area
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

tmp0 <-restention_5curve1 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_without_dry_145_loamysandy_soil

selected_curves1_lwr$alpha<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "loamy sand"]<- 0.001

selected_curves1_lwr$n<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$n[selected_curves1_lwr$n == "loamy sand"]<- 1

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

#tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(30,31)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

thetar<- as.numeric(tmp0$thetar_vol_lwr)
#thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetar)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_without_dry_145_loamysandy_soil

selected_curves1_upr$alpha<- selected_curves1_upr$tex_psda

selected_curves1_upr$alpha[selected_curves1_upr$alpha == "loamy sand"]<- 100

selected_curves1_upr$n<- selected_curves1_upr$tex_psda

selected_curves1_upr$n[selected_curves1_upr$n == "loamy sand"]<- 3.5

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
#tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(30,31)]

thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

#thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetar)

#write.csv(upr, "E:/Andreas_ksat_data/nonsensical_data_review/upr_alpha_n_thetas.csv")

Curves_without_dry_145_loamysandy_soil$lab_head_m[Curves_without_dry_145_loamysandy_soil$lab_head_m == 0]<- 0.01

unique(Curves_without_dry_145_loamysandy_soil$layer_id)

r13 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_without_dry_145_loamysandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_LS_2 <- coef(r13,gof = TRUE)

r.coef.2$class<- "ND"

# r.coef.2_n_3_5<- subset(r.coef.2, r.coef.2$n==3.5)
# 
# r.coef.2_alpha_30<- subset(r.coef.2, r.coef.2$alpha==30)

#r.coef.without.boxconstraint.thetas<- cbind(r.coef.without.boxconstraint.thetas,tc)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/loamysandy_soil_withoud_dry_all_thetar.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r13,draw_parameter = TRUE)

dev.off()

## Error test_LS_case 2---------


rss.original_LS_case2<-sapply(r13$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case2_LS<- data.frame(original_para_LS_2, rss.original_LS_case2)

jj_case2_LS$class<- "YWND"


## Divided this into two parts 

# LS_Curves with dry end but not wet end----

Curves_with_thetas_not_wetend_loamysandy_soil_with_dry_end<- subset(Curves_without_thetas_2_loamy_sandy_soil,
                                                                    Curves_without_thetas_2_loamy_sandy_soil$lab_head_m>=140)

Curves_with_dry_145_loamysandy_soil<-Curves_without_thetas_2_loamy_sandy_soil[Curves_without_thetas_2_loamy_sandy_soil$layer_id %in% 
                                                                                Curves_with_thetas_not_wetend_loamysandy_soil_with_dry_end$layer_id,]

unique(Curves_with_dry_145_loamysandy_soil$layer_id)

## where we do not have dry end

Curves_without_dry_145_loamysandy_soil<-Curves_without_thetas_2_loamy_sandy_soil[!Curves_without_thetas_2_loamy_sandy_soil$layer_id %in% 
                                                                                Curves_with_thetas_not_wetend_loamysandy_soil_with_dry_end$layer_id,]
unique(Curves_without_dry_145_loamysandy_soil$layer_id)

## I will work with samples where we have dry end

## computation of thetas

Curves_with_dry_145_loamysandy_soil$region <- factor(
  rep("temperate", NROW(Curves_with_dry_145_loamysandy_soil)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_with_dry_145_loamysandy_soil$region[Curves_with_dry_145_loamysandy_soil$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_with_dry_145_loamysandy_soil, interval = "prediction")


Curves_with_dry_145_loamysandy_soil<-cbind(Curves_with_dry_145_loamysandy_soil,pp)

tmp <-Curves_with_dry_145_loamysandy_soil %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_with_dry_145_loamysandy_soil

selected_curves1_lwr$alpha<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "loamy sand"]<- 0.001

selected_curves1_lwr$n<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$n[selected_curves1_lwr$n == "loamy sand"]<- 1

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

#thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_with_dry_145_loamysandy_soil

selected_curves1_upr$alpha<- selected_curves1_upr$tex_psda

selected_curves1_upr$alpha[selected_curves1_upr$alpha == "loamy sand"]<- 100

selected_curves1_upr$n<- selected_curves1_upr$tex_psda

selected_curves1_upr$n[selected_curves1_upr$n == "loamy sand"]<- 3.5

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

#thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas)

r14 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_dry_145_loamysandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_LS_3 <- coef(r14,gof = TRUE)

# r.coef.3$class<- "NWYD"

# r.coef.3_n_3_5<- subset(r.coef.3, r.coef.3$n==3.5)
# 
# r.coef.3_alpha_30<- subset(r.coef.3, r.coef.3$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/loamysandy_soil_with_dry_without_thetas.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r14,draw_parameter = TRUE)

dev.off()


# Loamysand_coeff<- rbind(r.coef.loamysandy_soils_with_dry, r.coef.2, r.coef.3)

## Error test_LS_case 3---------

rss.original_LS_case3<-sapply(r14$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case3_LS<- data.frame(original_para_LS_3, rss.original_LS_case3)

jj_case3_LS$class<- "NWYD"

## LS_curves_without_dry_end_without_wet_end----------

restention_5curve1<-Curves_without_dry_145_loamysandy_soil

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Surface area
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

tmp0 <-restention_5curve1 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

## thetas_calculation

Curves_without_dry_145_loamysandy_soil$region <- factor(
  rep("temperate", NROW(Curves_without_dry_145_loamysandy_soil)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_without_dry_145_loamysandy_soil$region[Curves_without_dry_145_loamysandy_soil$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_without_dry_145_loamysandy_soil, interval = "prediction")

Curves_without_dry_145_loamysandy_soil<-cbind(Curves_without_dry_145_loamysandy_soil,pp)

tmp <-Curves_without_dry_145_loamysandy_soil %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_without_dry_145_loamysandy_soil

selected_curves1_lwr$alpha<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "loamy sand"]<- 0.001

selected_curves1_lwr$n<- selected_curves1_lwr$tex_psda

selected_curves1_lwr$n[selected_curves1_lwr$n == "loamy sand"]<- 1

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas,thetar)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_without_dry_145_loamysandy_soil

selected_curves1_upr$alpha<- selected_curves1_upr$tex_psda

selected_curves1_upr$alpha[selected_curves1_upr$alpha == "loamy sand"]<- 100

selected_curves1_upr$n<- selected_curves1_upr$tex_psda

selected_curves1_upr$n[selected_curves1_upr$n == "loamy sand"]<- 3.5

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas,thetar)

r61 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_without_dry_145_loamysandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_LS_4 <- coef(r61,gof = TRUE)

# r.coef.3$class<- "NWYD"

# r.coef.3_n_3_5<- subset(r.coef.3, r.coef.3$n==3.5)
# 
# r.coef.3_alpha_30<- subset(r.coef.3, r.coef.3$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/loamysandy_soil_with_dry_without_thetas.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r14,draw_parameter = TRUE)

dev.off()


# Loamysand_coeff<- rbind(r.coef.loamysandy_soils_with_dry, r.coef.2, r.coef.3)

## Error test_LS_case 3---------

rss.original_LS_case4<-sapply(r61$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case4_LS<- data.frame(original_para_LS_4, rss.original_LS_case4)

jj_case4_LS$class<- "NWND"

colnames(jj_case1_LS)[which(names(jj_case1_LS) == "rss.original_LS_case1")] <- "RSS"

colnames(jj_case2_LS)[which(names(jj_case2_LS) == "rss.original_LS_case2")] <- "RSS"

colnames(jj_case3_LS)[which(names(jj_case3_LS) == "rss.original_LS_case3")] <- "RSS"

colnames(jj_case4_LS)[which(names(jj_case4_LS) == "rss.original_LS_case4")] <- "RSS"

combine_error_loamy_sandy_soil<- rbind(jj_case1_LS,jj_case2_LS,jj_case3_LS,jj_case4_LS)

combine_error_loamy_sandy_soil$ST<- "loamy_sand_soil"

## Samples with non_sandy soils-----------

non_sandy_soils

## nonsandy soil samples with wet end 

Curves_with_less_0.2_nonsandy_soil<- subset(non_sandy_soils,non_sandy_soils$lab_head_m<=0.2)

Curves_with_thetas_2_nonsandy_soil<-non_sandy_soils[non_sandy_soils$layer_id %in% Curves_with_less_0.2_nonsandy_soil$layer_id,]

unique(Curves_with_thetas_2_nonsandy_soil$layer_id)

## nonsandy soil samples without wet end 

Curves_with_thetas_not_wetend_nonsandy_soil<-non_sandy_soils[!non_sandy_soils$layer_id %in% Curves_with_less_0.2_nonsandy_soil$layer_id,]

unique(Curves_with_thetas_not_wetend_nonsandy_soil$layer_id)


## divided data into 2 parts

## where we have both wet and dry end
## where we have dry end

Curves_with_less_145_nonsandy_soil<- subset(Curves_with_thetas_2_nonsandy_soil,Curves_with_thetas_2_nonsandy_soil$lab_head_m>=140)

Curves_with_dry_145_nonsandy_soil<-Curves_with_thetas_2_nonsandy_soil[Curves_with_thetas_2_nonsandy_soil$layer_id %in% 
                                                                        Curves_with_less_145_nonsandy_soil$layer_id,]

unique(Curves_with_dry_145_nonsandy_soil$layer_id)

## where we do not have dry end

Curves_without_dry_145_nonsandy_soil<-Curves_with_thetas_2_nonsandy_soil[!Curves_with_thetas_2_nonsandy_soil$layer_id %in% 
                                                                        Curves_with_less_145_nonsandy_soil$layer_id,]

unique(Curves_without_dry_145_nonsandy_soil$layer_id)

## going to start with Nonsandy soil with both wet and dry end

## NS_with_wet_end and dry_end------- 

Curves_with_dry_145_nonsandy_soil$lab_head_m[Curves_with_dry_145_nonsandy_soil$lab_head_m == 0]<- 0.01

r15 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_dry_145_nonsandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = rbind(
    c(alpha = 0.001, n = 1)), upper_param = rbind(
      c(alpha = 30, n = 2.5))
)

original_para_NS <- coef(r15,gof = TRUE)
# 
# r.coef.nonsandy_soils_with_dry$class<- "FC"
# 
# r.coef.nonsandy_soils_with_dry_n_2_5<- subset(r.coef.nonsandy_soils_with_dry, r.coef.nonsandy_soils_with_dry$n==2.5)
# 
# r.coef.nonsandy_soils_with_dry_alpha_30<- subset(r.coef.nonsandy_soils_with_dry, r.coef.nonsandy_soils_with_dry$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/nonsandy_soil_1.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r15,draw_parameter = TRUE)
dev.off()

## Error test_NS_case 1---------

rss.original_NS_case1<-sapply(r15$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case1_NS<- data.frame(original_para_NS, rss.original_NS_case1)

jj_case1_NS$class<- "YWYD"

## NS_with_wet_end and not_dry end-------

## calculation of thetar

restention_5curve1<-Curves_without_dry_145_nonsandy_soil

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Surface area
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

tmp0 <-restention_5curve1 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_without_dry_145_nonsandy_soil

selected_curves1_lwr$alpha<- 0.001

#selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- 1

#selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

#tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(30,31)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

thetar<- as.numeric(tmp0$thetar_vol_lwr)
#thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetar)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_without_dry_145_nonsandy_soil

selected_curves1_upr$alpha<- 100

#selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 30

selected_curves1_upr$n<- 2.5

#selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
#tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(30,31)]

thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

#thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetar)

#write.csv(upr, "E:/Andreas_ksat_data/nonsensical_data_review/upr_alpha_n_thetas.csv")

Curves_without_dry_145_nonsandy_soil$lab_head_m[Curves_without_dry_145_nonsandy_soil$lab_head_m == 0]<- 0.01

r16 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_without_dry_145_nonsandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_NS_2 <- coef(r16,gof = TRUE)

# r.coef.2$class<- "ND"
# 
# r.coef.2_n_2_5<- subset(r.coef.2, r.coef.2$n==2.5)
# 
# r.coef.2_alpha_30<- subset(r.coef.2, r.coef.2$alpha==30)


#r.coef.without.boxconstraint.thetas<- cbind(r.coef.without.boxconstraint.thetas,tc)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/N_sandy_soil_without_dry_all_thetar.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r16,draw_parameter = TRUE)

dev.off()
### Error test_NS_case 2---------


rss.original_NS_case2<-sapply(r16$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case2_NS<- data.frame(original_para_NS_2, rss.original_NS_case2)

jj_case2_NS$class<- "YWND"


# NS_Curves with dry end but not wet end----

Curves_with_thetas_not_wetend_nonsandy_soil_with_dry_end<- subset(Curves_with_thetas_not_wetend_nonsandy_soil,
                                                                  Curves_with_thetas_not_wetend_nonsandy_soil$lab_head_m>=140)

Curves_with_dry_145_not_sandy_soil<-Curves_with_thetas_not_wetend_nonsandy_soil[Curves_with_thetas_not_wetend_nonsandy_soil$layer_id %in% 
                                                                              Curves_with_thetas_not_wetend_nonsandy_soil_with_dry_end$layer_id,]

unique(Curves_with_dry_145_not_sandy_soil$layer_id)

## where we do not have dry end

Curves_without_dry_145_nonsandy_soil<-Curves_with_thetas_not_wetend_nonsandy_soil[!Curves_with_thetas_not_wetend_nonsandy_soil$layer_id %in% 
                                                                                    Curves_with_thetas_not_wetend_nonsandy_soil_with_dry_end$layer_id,]
unique(Curves_without_dry_145_nonsandy_soil$layer_id)


## I will work with samples where we have dry end

## computation of thetas

Curves_with_dry_145_not_sandy_soil$region <- factor(
  rep("temperate", NROW(Curves_with_dry_145_not_sandy_soil)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_with_dry_145_not_sandy_soil$region[Curves_with_dry_145_not_sandy_soil$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_with_dry_145_not_sandy_soil, interval = "prediction")

Curves_with_dry_145_not_sandy_soil<-cbind(Curves_with_dry_145_not_sandy_soil,pp)

tmp <-Curves_with_dry_145_not_sandy_soil %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_with_dry_145_not_sandy_soil

selected_curves1_lwr$alpha<- 0.001

#selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- 1

#selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

#thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_with_dry_145_not_sandy_soil

selected_curves1_upr$alpha<- 100

#selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 30

selected_curves1_upr$n<- 2.5

#selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

#thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas)

r17 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_dry_145_not_sandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_NS_3 <- coef(r17,gof = TRUE)

# r.coef.3$class<- "NW"
# 
# r.coef.3_n_2_5<- subset(r.coef.3, r.coef.3$n==2.5)
# 
# r.coef.3_alpha_30<- subset(r.coef.3, r.coef.3$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/nonsandy_soil_with_dry_without_thetas.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r17,draw_parameter = TRUE)

dev.off()

## Error test_NS_case 3---------

rss.original_NS_case3<-sapply(r17$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case3_NS<- data.frame(original_para_NS_3, rss.original_NS_case3)

jj_case3_NS$class<- "NWYD"

#write.csv(jj_case3_NS,"E:/Andreas_ksat_data/nonsensical_data_review/non_sandy_soil_errors.csv" )

# NS_Curves with not dry end but not wet end----

Curves_without_dry_145_nonsandy_soil

## Thetar_calculations

restention_5curve1<-Curves_without_dry_145_nonsandy_soil

restention_5curve1$clay_g_100g<- restention_5curve1$clay_tot_psa_percent/100
restention_5curve1$sand_g_100g<- restention_5curve1$sand_tot_psa_percent/100
restention_5curve1$silt_g_100g<- restention_5curve1$silt_tot_psa_percent/100

#Surface area
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

tmp0 <-restention_5curve1 %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

## thetas_calculation

Curves_without_dry_145_nonsandy_soil$region <- factor(
  rep("temperate", NROW(Curves_without_dry_145_nonsandy_soil)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_without_dry_145_nonsandy_soil$region[Curves_without_dry_145_nonsandy_soil$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_without_dry_145_nonsandy_soil, interval = "prediction")

Curves_without_dry_145_nonsandy_soil<-cbind(Curves_without_dry_145_nonsandy_soil,pp)

tmp <-Curves_without_dry_145_nonsandy_soil %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_without_dry_145_nonsandy_soil

selected_curves1_lwr$alpha<- 0.001

#selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- 1

#selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas,thetar)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_without_dry_145_nonsandy_soil

selected_curves1_upr$alpha<- 100

#selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 30

selected_curves1_upr$n<- 2.5

#selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas,thetar)

r18 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_without_dry_145_nonsandy_soil,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_NS_4 <- coef(r18,gof = TRUE)

# r.coef.4$class<- "NWND"
# 
# r.coef.4_n_2_5<- subset(r.coef.4, r.coef.4$n==2.5)
# 
# r.coef.4_alpha_30<- subset(r.coef.4, r.coef.4$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/nonsandy_soil_without_dry_without_thetas1.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r18,draw_parameter = TRUE)

dev.off()

#nonsand_coeff<- rbind(r.coef.nonsandy_soils_with_dry, r.coef.2, r.coef.3, r.coef.4)


## Error test_NS_case 4---------

rss.original_NS_case4<-sapply(r18$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case4_NS<- data.frame(original_para_NS_4, rss.original_NS_case4)

jj_case4_NS$class<- "NWND"


colnames(jj_case1_NS)[which(names(jj_case1_NS) == "rss.original_NS_case1")] <- "RSS"

colnames(jj_case2_NS)[which(names(jj_case2_NS) == "rss.original_NS_case2")] <- "RSS"

colnames(jj_case3_NS)[which(names(jj_case3_NS) == "rss.original_NS_case3")] <- "RSS"

colnames(jj_case4_NS)[which(names(jj_case4_NS) == "rss.original_NS_case4")] <- "RSS"


combine_error_non_sandy_soil<- rbind(jj_case1_NS,jj_case2_NS,jj_case3_NS, jj_case4_NS)

combine_error_non_sandy_soil$ST<- "non_sandy_soil"

write.csv(combine_error_non_sandy_soil,"E:/Andreas_ksat_data/nonsensical_data_review/non_sand_soil_errors.csv" )


## Samples without soil texture information-----------

set_without_texture_infor

## nonsandy soil samples with wet end 

## sandy soil samples with wet end 

Curves_with_less_0.2_non_soil_texture<- subset(set_without_texture_infor,set_without_texture_infor$lab_head_m<=0.2)

Curves_with_thetas_2_non_soil_texture<-set_without_texture_infor[set_without_texture_infor$layer_id %in% 
                                                                   Curves_with_less_0.2_non_soil_texture$layer_id,]

unique(Curves_with_thetas_2_non_soil_texture$layer_id)

## sandy soil samples without wet end 

Curves_with_thetas_not_wetend_non_soil_texture<-set_without_texture_infor[!set_without_texture_infor$layer_id %in% 
                                                                      Curves_with_less_0.2_non_soil_texture$layer_id,]

unique(Curves_with_thetas_not_wetend_non_soil_texture$layer_id)

## now work with with wet end samples

## non_soil_texture_with_wet_end and dry_end------- 

Curves_with_thetas_2_non_soil_texture$lab_head_m[Curves_with_thetas_2_non_soil_texture$lab_head_m == 0]<- 0.01

r20 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_thetas_2_non_soil_texture,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = rbind(
    c(alpha = 0.001, n = 1)), upper_param = rbind(
      c(alpha = 100, n = 7))
)

original_para_NSTI1 <- coef(r20,gof = TRUE)

# r.coef.non_soil_texture_with_dry$class<- "FC"
# 
# r.coef.non_soil_texture_with_dry_n_7<- subset(r.coef.non_soil_texture_with_dry, r.coef.non_soil_texture_with_dry$n==7)
# 
# r.coef.non_soil_texture_with_dry_alpha_30<- subset(r.coef.non_soil_texture_with_dry, r.coef.non_soil_texture_with_dry$alpha==30)

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/non_soil_texture_soil_with_wet_end.pdf",
  width = 12, height = 9
)

par(mfrow = c(3, 4))
plot(x= r20,draw_parameter = TRUE)
dev.off()

## Error test_NSTI_case 1---------


rss.original_NSTI1_case1<-sapply(r20$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case1_NSTI1<- data.frame(original_para_NSTI1, rss.original_NSTI1_case1)

jj_case1_NSTI1$class<- "YWYD"


## non_soil_texture_without_wet_end------- 

## I will work with samples where we have dry end

## computation of thetas

Curves_with_thetas_not_wetend_non_soil_texture$region <- factor(
  rep("temperate", NROW(Curves_with_thetas_not_wetend_non_soil_texture)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_with_thetas_not_wetend_non_soil_texture$region[Curves_with_thetas_not_wetend_non_soil_texture$climate_classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_with_thetas_not_wetend_non_soil_texture, interval = "prediction")

Curves_with_thetas_not_wetend_non_soil_texture<-cbind(Curves_with_thetas_not_wetend_non_soil_texture,pp)

tmp <-Curves_with_thetas_not_wetend_non_soil_texture %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))


selected_curves1_lwr<- Curves_with_thetas_not_wetend_non_soil_texture

selected_curves1_lwr$alpha<- 0.001

#selected_curves1_lwr$alpha[selected_curves1_lwr$alpha == "sand"]<- 0.001

selected_curves1_lwr$n<- 1

#selected_curves1_lwr$n[selected_curves1_lwr$n == "sand"]<- 2

tmp1 <-selected_curves1_lwr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

tmp1$thetas<- tmp1$lwr

colnames(tmp1)

lwr<- tmp1[,c(34,35,36)]

alpha<- as.numeric(lwr$alpha)

n<- as.numeric(lwr$n)

#thetar<- as.numeric(tmp0$thetar_vol_lwr)
thetas<- as.numeric(lwr$thetas)

lwr<- cbind(alpha,n,thetas)

#write.csv(lwr, "E:/Andreas_ksat_data/nonsensical_data_review/lwr_alpha_n_thetas.csv")

selected_curves1_upr<- Curves_with_thetas_not_wetend_non_soil_texture

selected_curves1_upr$alpha<- 100

#selected_curves1_upr$alpha[selected_curves1_upr$alpha == "sand"]<- 30

selected_curves1_upr$n<- 7

#selected_curves1_upr$n[selected_curves1_upr$n == "sand"]<- 7

tmp2 <-selected_curves1_upr %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
tmp2$thetas<- tmp2$upr

colnames(tmp1)

upr<- tmp2[,c(34,35,36)]

#thetar<- as.numeric(tmp0$thetar_vol_upr)

alpha<- as.numeric(upr$alpha)

n<- as.numeric(upr$n)

thetas<- as.numeric(upr$thetas)

upr<- cbind(alpha,n,thetas)

r21 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_thetas_not_wetend_non_soil_texture,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

original_para_NSTI2 <- coef(r21,gof = TRUE)

# r.coef.3$class<- "NW"

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/non_soil_texture_soil_without_wet_end.pdf",
  width = 12, height = 9
)
par(mfrow = c(3, 4))
plot(x= r21,draw_parameter = TRUE)
dev.off()

# r.coef.3_n_7<- subset(r.coef.3, r.coef.3$n==7)
# 
# r.coef.3_alpha_30<- subset(r.coef.3, r.coef.3$alpha==30)
# 
# non_soil_texture_information<- rbind(r.coef.non_soil_texture_with_dry,r.coef.3 )

## Error test_NSTI_case 2---------


rss.original_NSTI2_case2<-sapply(r21$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case2_NSTI2<- data.frame(original_para_NSTI2, rss.original_NSTI2_case2)

jj_case2_NSTI2$class<- "NWYD"

#write.csv(jj_case2_NSTI2,"E:/Andreas_ksat_data/nonsensical_data_review/non_soil_texture_errors1.csv" )

colnames(jj_case1_NSTI1)[which(names(jj_case1_NSTI1) == "rss.original_NSTI1_case1")] <- "RSS"

colnames(jj_case2_NSTI2)[which(names(jj_case2_NSTI2) == "rss.original_NSTI2_case2")] <- "RSS"

combine_error_non_soil_texture<- rbind(jj_case1_NSTI1,jj_case2_NSTI2)

combine_error_non_soil_texture$ST<- "non_soil_texture"


#write.csv(combine_error_non_soil_texture,"E:/Andreas_ksat_data/nonsensical_data_review/non_soil_texture_errors.csv" )


combine_error_constraints<- rbind(combine_error_sandy_soil,combine_error_loamy_sandy_soil,
                                  combine_error_non_sandy_soil, combine_error_non_soil_texture )

write.csv(combine_error_constraints,"P:/guptasu/Andreas_ksat_data/nonsensical_data_review/final_WC.csv")

errors_1<-read.csv("P:/guptasu/Andreas_ksat_data/nonsensical_data_review/final_WC.csv")

colnames(errors_1)[which(names(errors_1) == "X")] <- "layer_id"

Error_with_cons<- merge(errors_1,samples_with_bulk_density1, by = "layer_id")

Error_with_cons$RMSE<- sqrt(Error_with_cons$RSS/Error_with_cons$values)

write.csv(Error_with_cons, "E:/Andreas_ksat_data/nonsensical_data_review/Final_errors_with_cons_19_06.csv" )


## case1_W_limit_non_soil_texture_with_wet_end_soil-----

Curves_with_thetas_2_non_soil_texture$lab_head_m[Curves_with_thetas_2_non_soil_texture$lab_head_m == 0]<- 0.01

r7 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data = Curves_with_thetas_2_non_soil_texture,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  )
)

r.coef.1_WL <- coef(r7,gof = TRUE)

r.coef.1_WL$class<- "FC"

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/Curves_with_without_cons/non_soil_texture_soil_with_wet_end_WC.pdf",
  width = 12, height = 9
)
par(mfrow = c(3, 4))
plot(x= r7,draw_parameter = TRUE)
dev.off()


## Error test_NSTI_case 1_WL---------

system.time(
  r7 <-fit_wrc_hcc(
    wrc_formula = lab_wrc ~ lab_head_m | layer_id,
    data = Curves_with_thetas_2_non_soil_texture,
    control = control_fit_wrc_hcc(
      settings = "uglobal", 
      min_nobs_wc = 4
    )
  )
)

original_para_NSTI1_WL<-coef(r7, gof = TRUE, se = TRUE, residual_se = TRUE, lc = TRUE)

rss.original_NSTI1_case1_WL<-sapply(r7$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case1_NSTI1_WL<- data.frame(original_para_NSTI1_WL, rss.original_NSTI1_case1_WL)

jj_case1_NSTI1_WL$class<- "FC"


## case2_W_limit_non_soil_texture_without_wet_end_soil-----

Curves_with_thetas_not_wetend_non_soil_texture$region <- factor(
  rep("temperate", NROW(Curves_with_thetas_not_wetend_non_soil_texture)), 
  levels = c("temperate", "tropical", "temperate.hybras")
)
Curves_with_thetas_not_wetend_non_soil_texture$region[Curves_with_thetas_not_wetend_non_soil_texture$Classes == "Tropical"] <- "tropical"


pp<- predict(r.lmrob.texture.bd.texture.1, Curves_with_thetas_not_wetend_non_soil_texture, interval = "prediction")

Curves_with_thetas_not_wetend_non_soil_texture<-cbind(Curves_with_thetas_not_wetend_non_soil_texture,pp)

tmp <-Curves_with_thetas_not_wetend_non_soil_texture %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))

lwr <-tmp[, "lwr", drop = FALSE]

upr <- tmp[, "upr", drop = FALSE]

colnames(lwr) <- colnames(upr) <- "thetas"

Curves_with_thetas_not_wetend_non_soil_texture$lab_head_m[Curves_with_thetas_not_wetend_non_soil_texture$lab_head_m == 0]<- 0.01

r9 <-fit_wrc_hcc(
  wrc_formula = lab_wrc ~ lab_head_m | layer_id,
  data =Curves_with_thetas_not_wetend_non_soil_texture,
  control = control_fit_wrc_hcc(
    settings = "uglobal", 
    min_nobs_wc = 4
  ),
  lower_param = lwr, upper_param = upr
)

r.coef.3_WL <- coef(r9,gof = TRUE)

r.coef.3_WL$class<- "NW"

pdf(
  file = "E:/Andreas_ksat_data/nonsensical_data_review/Curves_with_without_cons/non_soil_texture_soil_without_wet_end_WC.pdf",
  width = 12, height = 9
)
par(mfrow = c(3, 4))
plot(x= r9,draw_parameter = TRUE)
dev.off()

non_soil_texture_information_WL<- rbind(r.coef.1_WL,r.coef.3_WL )

non_soil_texture_information_WL$logalpha<- log10(non_soil_texture_information_WL$alpha)

non_soil_texture_information$logalpha<- log10(non_soil_texture_information$alpha)

ggplot(non_soil_texture_information_WL, aes(x=class, y=n)) + geom_boxplot()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(y= expression(paste("n [-]")), x = expression(paste("Classes")))+
  scale_y_continuous(limits = c(0,20), expand = c(0, 0))+theme(axis.text=element_text(size=16, color = "black"),
                                                              axis.title=element_text(size=16,face="bold"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x = 1, y = 10, label= "2107", size = 5)+
  annotate("text",x = 2, y =10, label= "1888", size = 5)
  

## Error test_NSTI_case 2_WL---------

system.time(
  r9 <-fit_wrc_hcc(
    wrc_formula = lab_wrc ~ lab_head_m | layer_id,
    data =Curves_with_thetas_not_wetend_non_soil_texture,
    control = control_fit_wrc_hcc(
      settings = "uglobal", 
      min_nobs_wc = 4
    ),
    lower_param = lwr, upper_param = upr
  )
  
)

original_para_NSTI2_WL<-coef(r9, gof = TRUE, se = TRUE, residual_se = TRUE, lc = TRUE)

rss.original_NSTI2_case2_WL<-sapply(r9$fit, function(x) attr(x$obj, "ssq_wc"))

jj_case2_NSTI2_WL<- data.frame(original_para_NSTI2_WL, rss.original_NSTI2_case2_WL)

jj_case2_NSTI2_WL$class<- "NW"

colnames(jj_case1_NSTI1_WL)[which(names(jj_case1_NSTI1_WL) == "rss.original_NSTI1_case1_WL")] <- "RSS"

colnames(jj_case2_NSTI2_WL)[which(names(jj_case2_NSTI2_WL) == "rss.original_NSTI2_case2_WL")] <- "RSS"


combine_error_non_soil_texture_WL<- rbind(jj_case1_NSTI1_WL,jj_case2_NSTI2_WL)

combine_error_non_soil_texture_WL$ST<- "non_soil_texture"


#write.csv(combine_error_non_soil_texture,"E:/Andreas_ksat_data/nonsensical_data_review/non_soil_texture_errors.csv" )


combine_error_constraints_WL<- rbind(combine_error_sandy_soil_WL,combine_error_loamy_sandy_soil_WL,
                                  combine_error_non_sandy_soil_WL, combine_error_non_soil_texture_WL )

write.csv(combine_error_constraints_WL,"E:/Andreas_ksat_data/nonsensical_data_review/ALL_errors_with_cons_WL.csv" )

errors_1<-read.csv("E:/Andreas_ksat_data/nonsensical_data_review/ALL_errors_with_cons_WL.csv" )

colnames(errors_1)[which(names(errors_1) == "X")] <- "layer_id"

Error_with_cons<- merge(errors_1,samples_with_bulk_density1, by = "layer_id")

colnames(Error_with_cons)

WRC11<-Error_with_cons[, c("layer_id","alpha","n","thetar","thetas","RSS","class","ST"),]

samples1<- read.csv("C:/Users/guptasu.D/Downloads/WRC_dataset_surya_et_al_2021_final.csv")

WRC22<-samples1[, c("layer_id","disturbed_undisturbed","climate_classes","profile_id","site_key","method","method_keywords",
             "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
             "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
             "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
             "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
             "keywords_total_porosity","source_db"),]

Error_with_cons11<- merge(WRC22, WRC11, by = "layer_id")

# #Error_with_cons$RMSE<- sqrt(Error_with_cons$RSS/Error_with_cons$values)
# 
# Final_errors_with_constraints_WL<-write.csv(Error_with_cons, "E:/Andreas_ksat_data/nonsensical_data_review/Final_errors_with_cons_WL.csv" )


write.csv(Error_with_cons11,"C:/Users/guptasu.D/Downloads/WRC_dataset_surya_et_al_2021_final.csv")
