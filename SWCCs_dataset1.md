Global soil hydraulic properties database
================
Surya Gupta, Andreas Papritz, Peter Lehmann, Tom Hengl, Sara Bonetti,
Dani Or

  - [*Target variables*](#target-variables)
  - [*AFSPDB*](#afspdb)
  - [*EGRPR*](#egrpr)
  - [*WOSIS*](#wosis)
  - [*Australia\_CSIRO*](#australia_csiro)
  - [*ETH imported data from
    literature*](#eth-imported-data-from-literature)
  - [*Bind the data*](#bind-the-data)

#### *Target variables*

``` r
site.names = c("site_key", "longitude_decimal_degrees", "latitude_decimal_degrees")
hor.names = c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                         "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                         "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa",
                                         "silt_tot_psa","clay_tot_psa","ph_h2o","ksat_field","ksat_lab",
                                         "porosity","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                         "keywords_total_porosity","source_db","location_accuracy_min", "location_accuracy_max")
## target structure:
col.names = c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                         "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                         "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa",
                                         "silt_tot_psa","clay_tot_psa","ph_h2o","ksat_field","ksat_lab",
                                         "porosity","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                         "keywords_total_porosity","source_db","location_accuracy_min", "location_accuracy_max")
```

  - `layer_id`: Unique id for layer or horizon  
    \-`disturbed_undisturbed`: disturbed or undisturbed sample
    -`profile_id`: Unique id for soil profile  
    \-`reference`: Source of data  
    \-`method`: Method used to analyze the data -`method_keywords`:
    Comments on the methods if applicable  
    \-`latitude_decimal_degrees`: Ranges up to +90 degrees down to -90
    degrees in Decimal degree -`longitude_decimal_degrees`: Ranges up to
    +180 degrees down to -180 degrees in Decimal degree -`hzn_desgn`:
    Horizon designation -`hzn_top_cm`: Top depth of soil sample in cm
    -`hzn_bot_cm`: Lower depth of soil sample in cm -`db_33`: Bulk
    density\_33kpa in g/cm3 -`db_od`: Bulk density\_dry in g/cm3
    -`oc_percent`: Soil organic carbon content in % -`tex_psda`: Soil
    texture classes based on USDA  
    \-`sand_tot_psa`: Mass of soil particle, \> 0.05 and \< 2 mm
    in dekagram/kg -`silt_tot_psa`: Mass of soil particles, \> 0.002 and
    \< 0.05 mm in dekagram/kg -`clay_tot_psa`: Mass of soil particles, \<
    0.002 mm in dekagram/kg -`ph_h2o`: Soil reaction ???- -`ksat_field`: Soil
    saturated hydraulic conductivity from field in cm/day -`ksat_lab`:
    Soil saturated hydraulic conductivity from lab in cm/day
    -`porosity`: Porosity in m3/m3 -`WG_33kpa`: Gravimetric water
    content at 33kpa in weight/weight -`lab_head_m`: Lab
    measured\_suction head in m -`lab_wrc Lab`: measured\_volumetric
    water content in vol/vol -`field_head_m`: Field measured\_suction
    head in m -`field_wrc`: Field measured\_volumetric water content in
    vol/vol -`keywords_total_porosity`: Reference to porosity  
    \-`source_db`: Reference to the database  
    \-`location_accuracy_min`: Minimum value of location accuracy
    -`location_accuracy_max`: Maximum value of location accuracy

#### *AFSPDB*

  - Leenaars, J. G., Van OOstrum, A. J. M., & Ruiperez Gonzalez, M.
    (2014). [Africa soil profiles database version 1.2. A compilation of
    georeferenced and standardized legacy soil profile data for
    Sub-Saharan Africa (with
    dataset)](https://www.isric.org/projects/africa-soil-profiles-database-afsp).
    Wageningen: ISRIC Report 2014/01; 2014.

<!-- end list -->

``` r
options(warn=-1)
require(foreign)
```

    ## Loading required package: foreign

``` r
afspdb.profiles <- read.dbf("C:/Users/guptasu.D/Downloads/AfSIS_SPDB/AfSP012Qry_Profiles.dbf", as.is=TRUE)
## approximate location error
afspdb.profiles$location_accuracy_min = afspdb.profiles$XYAccur * 1e5
afspdb.profiles$location_accuracy_min = ifelse(afspdb.profiles$location_accuracy_min < 20, NA, afspdb.profiles$location_accuracy_min)
afspdb.profiles$location_accuracy_max = NA
afspdb.layers <- read.dbf("C:/Users/guptasu.D/Downloads/AfSIS_SPDB/AfSP012Qry_Layers.dbf", as.is=TRUE)
## select columns of interest:
 afspdb.sel.prof <- afspdb.profiles[,c("ProfileID", "T_Year", "X_LonDD", "Y_LatDD", "location_accuracy_min", "location_accuracy_max"),]
## Convert to weight content
#summary(afspdb.layers$BlkDens)
## select layers
afspdb.sel.layer <- afspdb.layers[,c("LayerID", "ProfileID", "LayerNr", "UpDpth", "LowDpth", "HorDes", "BlkDens","Ksat","VMCpF00","VMCpF05","VMCpF10","VMCpF15","VMCpF17",
                                                     "VMCpF18", "VMCpF20", "VMCpF22","VMCpF23","VMCpF24", "VMCpF25","VMCpF27","VMCpF28","VMCpF29","VMCpF30",
                                                     "VMCpF33","VMCpF34","VMCpF35" ,"VMCpF36", "VMCpF37", "VMCpF40" ,"VMCpF42", "VMCpF50","VMCpF58", "LabTxtr",
                                                    "Clay", "Silt", "Sand","FldTxtr", "OrgC", "PHKCl", "PHH2O", "CecSoil")]
afspdb.layers_merge<- merge(afspdb.sel.prof, afspdb.sel.layer, by = "ProfileID")
afspdb.layers_merge[ afspdb.layers_merge == -9999] <- NA
##covert organic carbon in %
afspdb.layers_merge$OrgC<- (afspdb.layers_merge$OrgC)/10
## chnaged wide format to long format to extract the curves which have suction value 4 or more than 4
afspdb.layers_wide <- gather(afspdb.layers_merge, condition, measurement, VMCpF00: VMCpF58, factor_key=TRUE)
afspdb.layers_wide_or<- afspdb.layers_wide[order(afspdb.layers_wide$LayerID),]
afspdb.layers_wide_or<- afspdb.layers_wide_or[!is.na(afspdb.layers_wide_or$condition),]
afspdb.counts<- stack(by(afspdb.layers_wide_or$measurement, afspdb.layers_wide_or$LayerID, FUN=function(x) sum(!is.na(x))))
afspdb.counts_order<- afspdb.counts[order(afspdb.counts$values),]
afspdb.counts$LayerID<- afspdb.counts$ind
afspdb.counts<- afspdb.counts[afspdb.counts$values >= 4,]
# Final selected curves
afspdb.layers_merge<-afspdb.layers_merge[afspdb.layers_merge$LayerID%in% afspdb.counts$LayerID,]
afspdb.layers_merge<- afspdb.layers_merge[!is.na(afspdb.layers_merge$X_LonDD),]
## add missing columns
#for(j in c("usiteid")){  afspdb.profiles[,j] = NA }
#for(j in c("db_13b", "COLEws", "w15bfm", "adod", "wrd_ws13", "cec7_cly", "w15cly", "cec_nh4", "ksat_lab", "ksat_field")){  afspdb.layers[,j] = NA }
#hydrosprops.AfSPDB = plyr::join(afspdb.profiles[,afspdb.s.lst], afspdb.layers[,afspdb.h.lst])
#for(j in 1:ncol(hydrosprops.AfSPDB)){
# if(is.numeric(hydrosprops.AfSPDB[,j])) { hydrosprops.AfSPDB[,j] <- ifelse(hydrosprops.AfSPDB[,j] < -200, NA, hydrosprops.AfSPDB[,j]) }
#}
afspdb.layers_merge$source_db = "AfSPDB"
#hydrosprops.AfSPDB$confidence_degree = 5
  #hydrosprops.AfSPDB$OrgC = hydrosprops.AfSPDB$OrgC/10
  #summary(hydrosprops.AfSPDB$OrgC)
  #hydrosprops.AfSPDB = complete.vars(hydrosprops.AfSPDB, sel = c("VMCpF25", "VMCpF42"), coords = c("X_LonDD", "Y_LatDD"))
  #hydrosprops.AfSPDB$uuid = uuid::UUIDgenerate(use.time=TRUE, n=nrow(hydrosprops.AfSPDB))
#}
 # AFSPDB_long_format
  #afspdb.WRC_only<- afspdb.layers_merge[, c(5,3,4,5,11:34),]
afspdb.WRC_only_wide <- gather( afspdb.layers_merge, condition, measurement, VMCpF00: VMCpF58, factor_key=TRUE)
afspdb.WRC_only_wide_or<- afspdb.WRC_only_wide[order(afspdb.WRC_only_wide$LayerID),]
#afspdb.WRC_only_wide_or$lab_head_m<- tstrsplit(afspdb.WRC_only_wide_or$condition, "6")[[1]]
afspdb.WRC_only_wide_or$lab_head_m<- substr(afspdb.WRC_only_wide_or$condition, start = 6, stop = 7)
afspdb.WRC_only_wide_or$lab_head_m<-as.numeric(as.character(afspdb.WRC_only_wide_or$lab_head_m))
afspdb.WRC_only_wide_or$lab_head_m<-(10^(afspdb.WRC_only_wide_or$lab_head_m/10))/100
afspdb.WRC_only_wide_or$lab_wrc<-afspdb.WRC_only_wide_or$measurement/100
afspdb.WRC_only_wide_or$layer_id<- afspdb.WRC_only_wide_or$LayerID
afspdb.WRC_only_wide_or$latitude_decimal_degrees<- afspdb.WRC_only_wide_or$Y_LatDD
afspdb.WRC_only_wide_or$longitude_decimal_degrees<- afspdb.WRC_only_wide_or$X_LonDD
afspdb.WRC_only_wide_or$disturbed_undisturbed<-"disturbed"
afspdb.WRC_only_wide_or$db_33<-NA
afspdb.WRC_only_wide_or$porosity_percent<-NA
afspdb.WRC_only_wide_or$WG_33kpa<-NA
afspdb.WRC_only_wide_or$profile_id<- afspdb.WRC_only_wide_or$ProfileID
afspdb.WRC_only_wide_or$site_key<-"Leenaars_et_al_2014"
afspdb.WRC_only_wide_or$hzn_top<-afspdb.WRC_only_wide_or$UpDpth
afspdb.WRC_only_wide_or$hzn_bot<-afspdb.WRC_only_wide_or$LowDpth
afspdb.WRC_only_wide_or$db_od<-afspdb.WRC_only_wide_or$BlkDens
afspdb.WRC_only_wide_or$tex_psda<-afspdb.WRC_only_wide_or$LabTxtr
afspdb.WRC_only_wide_or$sand_tot_psa_percent<-afspdb.WRC_only_wide_or$Sand
afspdb.WRC_only_wide_or$silt_tot_psa_percent<-afspdb.WRC_only_wide_or$Silt
afspdb.WRC_only_wide_or$clay_tot_psa_percent<-afspdb.WRC_only_wide_or$Clay
afspdb.WRC_only_wide_or$ph_h2o<-afspdb.WRC_only_wide_or$PHH2O
afspdb.WRC_only_wide_or$total<- afspdb.WRC_only_wide_or$Sand+afspdb.WRC_only_wide_or$Silt+afspdb.WRC_only_wide_or$Clay
afspdb.WRC_only_wide_or$ksat_field<-NA
afspdb.WRC_only_wide_or$ksat_lab<-afspdb.WRC_only_wide_or$Ksat
afspdb.WRC_only_wide_or$field_head_m<- NA
afspdb.WRC_only_wide_or$field_wrc<- NA
afspdb.WRC_only_wide_or$hzn_desgn<-afspdb.WRC_only_wide_or$HorDes
afspdb.WRC_only_wide_or$oc<-afspdb.WRC_only_wide_or$OrgC
afspdb.WRC_only_wide_or$keywords_total_porosity<- NA
afspdb.WRC_only_wide_or$method<-"Pressure plate"
afspdb.WRC_only_wide_or$method_keywords<- NA
##merge the target variables
##merge the target variables
afspdb.WRC<- afspdb.WRC_only_wide_or[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                         "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                         "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                         "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                         "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                         "keywords_total_porosity","source_db","location_accuracy_min", "location_accuracy_max"),]
#options(scipen = 999)
#options(digits=1)
afspdb.WRC$lab_head_m<- as.numeric(afspdb.WRC$lab_head_m)

#saveRDS(afspdb.WRC, "E:/Soil_points/INT/USDA_NCSS/hydrosprops.AfSPDB_SG.rds")
#afspdb.WRC<-afspdb.WRC[!is.na(afspdb.WRC$lab_wrc),]
#write.csv(afspdb.WRC, "C:/Users/guptasu.D/Downloads/AfSIS_SPDB/AFSPDB_wrc_database.csv")
#write.csv(afspdb.WRC, "E:/Soil_points/INT/USDA_NCSS/AFSPDB_wrc_database.csv")
afspdb.WRC1<- read.csv("C:/Users/guptasu.D/Downloads/AfSIS_SPDB/AFSPDB_wrc_database.csv")
locations<- afspdb.WRC[, c("layer_id","location_accuracy_min", "location_accuracy_max")]
locations1 <-locations %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
afspdb.WRC2<- merge(afspdb.WRC1,locations1, by = "layer_id")
dim(afspdb.WRC2)
```

    ## [1] 55224    31

``` r
#table(afspdb.WRC1$tex_psda)
```

#### *EGRPR*

  - [Russian Federation: The Unified State Register of Soil Resources
    (EGRPR)](http://egrpr.esoil.ru/).

<!-- end list -->

``` r
#### EGRPR-----------

russ.HOR = read.csv("C:/Users/guptasu.D/Downloads/soil_data2.csv")
#russ.HOR = read.csv("E:/Soil_points/Russia/EGRPR/Russia_EGRPR_soil_pedons.csv")
russ.HOR$SOURCEID = paste(russ.HOR$CardID, russ.HOR$SOIL_ID, sep="_")
russ.HOR$SNDPPT <- russ.HOR$TEXTSAF + russ.HOR$TEXSCM
russ.HOR$SLTPPT <- russ.HOR$TEXTSIC + russ.HOR$TEXTSIM + 0.8 * russ.HOR$TEXTSIF
russ.HOR$CLYPPT <- russ.HOR$TEXTCL + 0.2 * russ.HOR$TEXTSIF
## Correct texture fractions:
sumTex <- rowSums(russ.HOR[,c("SLTPPT","CLYPPT","SNDPPT")])
russ.HOR$SNDPPT <- russ.HOR$SNDPPT / ((sumTex - russ.HOR$CLYPPT) /(100 - russ.HOR$CLYPPT))
russ.HOR$SLTPPT <- russ.HOR$SLTPPT / ((sumTex - russ.HOR$CLYPPT) /(100 - russ.HOR$CLYPPT))
russ.HOR$oc <- russ.HOR$ORGMAT/1.724
## add missing columns
#for(j in c("site_obsdate", "location_accuracy_min", "location_accuracy_max", "labsampnum", "db_13b", "COLEws", "w15bfm", "w6clod", "adod", "wrd_ws13", "cec7_cly", "w15cly", "tex_psda", "cec_nh4", "wpg2", "ksat_lab", "ksat_field")){  russ.HOR[,j] = NA }
#russ.sel.h = c("SOURCEID", "SOIL_ID", "site_obsdate", "LONG", "LAT", "location_accuracy_min", "location_accuracy_max", "labsampnum", "HORNMB", "HORTOP", "HORBOT", "HISMMN", "db_13b", "DVOL", "COLEws", "w6clod", "WR10", "WR33", "WR1500", "w15bfm", "adod", "wrd_ws13", "cec7_cly", "w15cly", "tex_psda", "CLYPPT", "SLTPPT", "SNDPPT", "oc", "PHSLT", "PHH2O", "CECST", "cec_nh4", "wpg2","ksat_lab", "ksat_field")
EGRPR.sel.layer <- russ.HOR[,c("SOIL_ID", "LAT", "LONG", "HORTOP","HORBOT", "PHH2O","SNDPPT", "SLTPPT","CLYPPT","oc","WR1",
                                             "WR10","WR100","WR33","WR1500")]
#hydrosprops.EGRPR = russ.HOR[,russ.sel.h]
EGRPR.sel.layer$source_db = "Russia_EGRPR"
EGRPR_dataset<-   EGRPR.sel.layer[complete.cases(EGRPR.sel.layer[ , c(11:14) ]),]
EGRPR_dataset<- EGRPR_dataset[!is.na(EGRPR_dataset$LAT),]
EGRPR_dataset$mean_depth<-(EGRPR_dataset$HORTOP+EGRPR_dataset$HORBOT)/2
EGRPR_dataset$layer_id<-paste(EGRPR_dataset$SOIL_ID,"_",EGRPR_dataset$mean_depth)
#hydrosprops.EGRPR$confidence_degree = 2
#hydrosprops.EGRPR <- complete.vars(hydrosprops.EGRPR, sel=c("WR33", "WR1500"), coords = c("LONG", "LAT"))
#summary(hydrosprops.EGRPR$WR1500)
#hydrosprops.EGRPR$uuid = uuid::UUIDgenerate(use.time=TRUE, n=nrow(hydrosprops.EGRPR))
##1138
##test_EGRPR_database
#EGRPR.WRC_only<- EGRPR_dataset[, c(18,2,3,11:15),]
EGRPR.WRC_only_wide <- gather(EGRPR_dataset, condition, measurement, WR1: WR1500, factor_key=TRUE)
EGRPR.WRC_only_wide_or<- EGRPR.WRC_only_wide[order(EGRPR.WRC_only_wide$layer_id),]
EGRPR.WRC_only_wide_or$theta_vol<- EGRPR.WRC_only_wide_or$measurement/100
EGRPR.WRC_only_wide_or$head_m<- substr(EGRPR.WRC_only_wide_or$condition, start = 3, stop = 6)
EGRPR.WRC_only_wide_or$head_m<-as.numeric(as.character(EGRPR.WRC_only_wide_or$head_m))
EGRPR.WRC_only_wide_or$latitude_decimal_degrees<- EGRPR.WRC_only_wide_or$LAT
EGRPR.WRC_only_wide_or$longitude_decimal_degrees<- EGRPR.WRC_only_wide_or$LONG
EGRPR.WRC_only_wide_or$disturbed_undisturbed<-"unknown"
EGRPR.WRC_only_wide_or$db_33<-NA
EGRPR.WRC_only_wide_or$porosity_percent<-NA
EGRPR.WRC_only_wide_or$WG_33kpa<-NA
EGRPR.WRC_only_wide_or$profile_id<- EGRPR.WRC_only_wide_or$SOIL_ID
EGRPR.WRC_only_wide_or$site_key<-"Russian Federation"
EGRPR.WRC_only_wide_or$hzn_top<-EGRPR.WRC_only_wide_or$HORTOP
EGRPR.WRC_only_wide_or$hzn_bot<-EGRPR.WRC_only_wide_or$HORBOT
EGRPR.WRC_only_wide_or$db_od<-DVOL
EGRPR.WRC_only_wide_or$tex_psda<-NA
EGRPR.WRC_only_wide_or$sand_tot_psa_percent<-EGRPR.WRC_only_wide_or$SNDPPT
EGRPR.WRC_only_wide_or$silt_tot_psa_percent<-EGRPR.WRC_only_wide_or$SLTPPT
EGRPR.WRC_only_wide_or$clay_tot_psa_percent<-EGRPR.WRC_only_wide_or$CLYPPT
EGRPR.WRC_only_wide_or$ph_h2o<-EGRPR.WRC_only_wide_or$PHH2O
EGRPR.WRC_only_wide_or$ksat_field<-NA
EGRPR.WRC_only_wide_or$ksat_lab<-NA
EGRPR.WRC_only_wide_or$field_head_m<- NA
EGRPR.WRC_only_wide_or$field_wrc<- NA
EGRPR.WRC_only_wide_or$oc<-EGRPR.WRC_only_wide_or$oc
EGRPR.WRC_only_wide_or$keywords_total_porosity<- NA
EGRPR.WRC_only_wide_or$hzn_desgn<-NA
EGRPR.WRC_only_wide_or$lab_head_m<-EGRPR.WRC_only_wide_or$head_m
EGRPR.WRC_only_wide_or$lab_wrc<-EGRPR.WRC_only_wide_or$theta_vol
EGRPR.WRC_only_wide_or$source_db = "Russia_EGRPR"
EGRPR.WRC_only_wide_or$method<- "unknown"
EGRPR.WRC_only_wide_or$method_keywords = NA
EGRPR.WRC_only_wide_or$location_accuracy_min<- NA
EGRPR.WRC_only_wide_or$location_accuracy_max = NA
##merge the target variables
 EGRPR.WRC<- EGRPR.WRC_only_wide_or[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                        "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                        "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                        "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                        "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                        "keywords_total_porosity","source_db","location_accuracy_min","location_accuracy_max"),]
EGRPR.WRC1<- EGRPR.WRC %>% filter( layer_id %in% c("152 _ 105", "35 _ 87.5", "8 _ 36","305 _ 35"))
EGRPR.WRC<-EGRPR.WRC[!EGRPR.WRC$layer_id%in% EGRPR.WRC1$layer_id,]
EGRPR.WRC2<- stack(by(EGRPR.WRC$lab_wrc, EGRPR.WRC$layer_id, FUN=function(x) sum(!is.na(x))))
EGRPR.WRC2$layer_id<- EGRPR.WRC2$ind
EGRPR.WRC2<- EGRPR.WRC2[EGRPR.WRC2$values > 5,]
EGRPR.WRC_R<- EGRPR.WRC[EGRPR.WRC$layer_id %in% EGRPR.WRC2$layer_id,]
EGRPR.WRC_R$layer_id<- paste(EGRPR.WRC_R$layer_id,"_",EGRPR.WRC_R$hzn_top)
EGRPR.WRC_R$layer_id<- paste(EGRPR.WRC_R$layer_id,"_",EGRPR.WRC_R$ph_h2o)
EGRPR.WRC_R<-EGRPR.WRC_R[order(EGRPR.WRC_R$layer_id),]
EGRPR.WRC<- EGRPR.WRC[!EGRPR.WRC$layer_id %in% EGRPR.WRC2$layer_id,]
EGRPR.WRC_R1<- EGRPR.WRC_R %>% filter( !layer_id %in% c("125 _ 15 _ 0 _ 7.3" ))
EGRPR.WRC<-rbind(EGRPR.WRC,EGRPR.WRC_R1)
#write.csv(EGRPR.WRC,"E:/Andreas_ksat_data/nonsensical_data_review/EGRPR.WRC.csv" )
#saveRDS(EGRPR.WRC, "E:/Soil_points/Russia/EGRPR/hydrosprops.EGRPR_SG.csv")
dim(EGRPR.WRC)
```

    ## [1] 5665   31

#### *WOSIS*

  - Batjes, N. H., Ribeiro, E., Oostrum, A. v., Leenaars, J., Hengl, T.,
    and Mendes de Jesus, J. (2017) [WoSIS: providing standardised soil
    profile datafor the
    world](https://www.isric.org/explore/wosis/accessing-wosis-derived-datasets).

<!-- end list -->

``` r
Table11<- read.table("C:/Users/guptasu.D/Downloads/WoSIS_2019_September/wosis_201909_attributes.tsv", sep = '\t', header = TRUE)
Table12<- read.table("C:/Users/guptasu.D/Downloads/WoSIS_2019_September/wosis_201909_layers_chemical.tsv", sep = '\t', header = TRUE,fill = TRUE)
Table13<- read.table("C:/Users/guptasu.D/Downloads/WoSIS_2019_September/wosis_201909_layers_physical.tsv", sep = '\t', header = TRUE,fill = TRUE,na.strings= c("999", "NA", " ", ""))
Table14<- read.table("C:/Users/guptasu.D/Downloads/WoSIS_2019_September/wosis_201909_profiles.tsv", sep = '\t', header = TRUE,fill = TRUE)
Table14$location_accuracy_id<- Table14$geom_accuracy*10000000
Table14$location_accuracy_min<- 0
Table14$location_accuracy_max<- (Table14$location_accuracy_id)/100
chemical<- Table12[, c("profile_layer_id","orgc_value_avg", "phaq_value_avg" )]
pp<-  merge(Table13, Table14, by="profile_id")
WRC_table2<-pp[,c("profile_id","upper_depth","profile_layer_id", "upper_depth", "latitude", "longitude",
                                "lower_depth", "bdfi33_value_avg","bdfiod_value_avg","bdws33_value_avg", "clay_value_avg",
                                "sand_value_avg", "silt_value_avg", "wv0100_value_avg", "wv0010_value_avg",
                                "wv1500_value_avg", "wv0200_value_avg","wv0500_value_avg", "wv0006_value_avg","wv0033_value_avg",
                                "country_id", "country_name", "wg0100_value_avg", "wg0010_value_avg", "wg1500_value_avg"
                                ,"wg0200_value_avg", "wg0033_value_avg"
                                , "wg0500_value_avg", "wg0006_value_avg","location_accuracy_min","location_accuracy_max")]
##for gravimetric_water content
WOSIS.layers_wide <- gather(WRC_table2, condition, measurement, wg0100_value_avg: wg0006_value_avg, factor_key=TRUE)
WOSIS.layers_wide_or<- WOSIS.layers_wide[order(WOSIS.layers_wide$profile_layer_id),]
WOSIS.layers_wide_or<- WOSIS.layers_wide_or[!is.na(WOSIS.layers_wide_or$condition),]
WOSIS.counts<- stack(by(WOSIS.layers_wide_or$measurement, WOSIS.layers_wide_or$profile_layer_id, FUN=function(x) sum(!is.na(x))))
WOSIS.counts_order<- WOSIS.counts[order(WOSIS.counts$values),]
WOSIS.counts_order$profile_layer_id<- WOSIS.counts_order$ind
WOSIS.counts_order<- WOSIS.counts_order[WOSIS.counts_order$values >= 4,]
# Final selected curves
WOSIS.layers_merge<-WRC_table2[WRC_table2$profile_layer_id%in% WOSIS.counts_order$profile_layer_id,]
WOSIS.layers_merge_bd<- WOSIS.layers_merge[!is.na(WOSIS.layers_merge$bdws33_value_avg),]
## remove WOSIS dataset overlap with African database
remove_WOSIS_points<- read.csv("C:/Users/guptasu/Documents/WRC_dataset/new_lit_13_10_20/Digitization/WOSIS_profiles_remove.csv")
WOSIS.layers_merge_bd<-WOSIS.layers_merge_bd[!WOSIS.layers_merge_bd$profile_id%in% remove_WOSIS_points$profile_id,]
## divided gravimetric water content by 100
WOSIS.layers_merge_bd$wg0100_value_avg<- WOSIS.layers_merge_bd$wg0100_value_avg/100
WOSIS.layers_merge_bd$wg0010_value_avg<- WOSIS.layers_merge_bd$wg0010_value_avg/100
WOSIS.layers_merge_bd$wg1500_value_avg<- WOSIS.layers_merge_bd$wg1500_value_avg/100
WOSIS.layers_merge_bd$wg0200_value_avg<- WOSIS.layers_merge_bd$wg0200_value_avg/100
WOSIS.layers_merge_bd$wg0033_value_avg<- WOSIS.layers_merge_bd$wg0033_value_avg/100
WOSIS.layers_merge_bd$wg0500_value_avg<- WOSIS.layers_merge_bd$wg0500_value_avg/100
WOSIS.layers_merge_bd$wg0006_value_avg<- WOSIS.layers_merge_bd$wg0006_value_avg/100
## we have 33 kPa BD. we will convert the 33 kPa BD to dry bulk density
## bd_33kPa/ 1+ gravimetric water content at 33 kPa
WOSIS.layers_merge_bd$db_dry<- as.numeric(WOSIS.layers_merge_bd$bdws33_value_avg)/(1+(WOSIS.layers_merge_bd$wg0033_value_avg))
WOSIS.layers_merge_bd$wv0100_value_avg<-WOSIS.layers_merge_bd$wg0100_value_avg*WOSIS.layers_merge_bd$db_dry
WOSIS.layers_merge_bd$wv0010_value_avg<-WOSIS.layers_merge_bd$wg0010_value_avg*WOSIS.layers_merge_bd$db_dry
WOSIS.layers_merge_bd$wv1500_value_avg<-WOSIS.layers_merge_bd$wg1500_value_avg*WOSIS.layers_merge_bd$db_dry
WOSIS.layers_merge_bd$wv0200_value_avg<-WOSIS.layers_merge_bd$wg0200_value_avg*WOSIS.layers_merge_bd$db_dry
WOSIS.layers_merge_bd$wv0500_value_avg<-WOSIS.layers_merge_bd$wg0500_value_avg*WOSIS.layers_merge_bd$db_dry
WOSIS.layers_merge_bd$wv0006_value_avg<-WOSIS.layers_merge_bd$wg0006_value_avg*WOSIS.layers_merge_bd$db_dry
WOSIS.layers_merge_bd$wv0033_value_avg<-WOSIS.layers_merge_bd$wg0033_value_avg*WOSIS.layers_merge_bd$db_dry
##for volumetric water content
WOSIS.layers_wide_vol <- gather(WRC_table2, condition, measurement, wv0100_value_avg: wv0033_value_avg, factor_key=TRUE)
WOSIS.layers_wide_or_vol<- WOSIS.layers_wide_vol[order(WOSIS.layers_wide_vol$profile_layer_id),]
WOSIS.layers_wide_or_vol<- WOSIS.layers_wide_or_vol[!is.na(WOSIS.layers_wide_or_vol$condition),]
WOSIS.counts_vol<- stack(by(WOSIS.layers_wide_or_vol$measurement, WOSIS.layers_wide_or_vol$profile_layer_id, FUN=function(x) sum(!is.na(x))))
WOSIS.counts_order_vol<- WOSIS.counts_vol[order(WOSIS.counts_vol$values),]
WOSIS.counts_order_vol$profile_layer_id<- WOSIS.counts_order_vol$ind
WOSIS.counts_order_vol<- WOSIS.counts_order_vol[WOSIS.counts_order_vol$values >= 4,]
# Final selected curves
WOSIS.layers_merge_vol<-WRC_table2[WRC_table2$profile_layer_id%in% WOSIS.counts_order_vol$profile_layer_id,]

## the SWCCs where volumetric water content was available is the repetition from the AFSPDB dataset. Therefore the count
#is Zero
#plot(WOSIS.layers_merge_bd$bdfi33_value_avg,WOSIS.layers_merge_bd$bdws33_value_avg )
  WOSIS.layers_merge_vol<-WOSIS.layers_merge_vol[!WOSIS.layers_merge_vol$profile_id%in%remove_WOSIS_points$profile_id,]
#WOSIS.layers_merge_vol<- afspdb.layers_merge[!is.na(afspdb.layers_merge$X_LonDD),]
Final_WOSIS_dataset<- rbind(WOSIS.layers_merge_bd,WOSIS.layers_merge_vol)
Final_WOSIS_dataset<- merge(Final_WOSIS_dataset,chemical, by = "profile_layer_id", all.x = TRUE )
Final_WOSIS_dataset <- gather(Final_WOSIS_dataset, condition, measurement, wv0100_value_avg: wv0033_value_avg, factor_key=TRUE)
Final_WOSIS_dataset<- Final_WOSIS_dataset[order(Final_WOSIS_dataset$profile_layer_id),]
Final_WOSIS_dataset$head1 = substr(Final_WOSIS_dataset$condition,3,6)
Final_WOSIS_dataset1= (as.numeric(as.character((Final_WOSIS_dataset$head1))))
Final_WOSIS_dataset<- cbind(Final_WOSIS_dataset,Final_WOSIS_dataset1)
Final_WOSIS_dataset$lab_head_m<- (Final_WOSIS_dataset$Final_WOSIS_dataset1)/10
Final_WOSIS_dataset$lab_wrc<- (Final_WOSIS_dataset$measurement)
Final_WOSIS_dataset$disturbed_undisturbed<-"disturbed"
Final_WOSIS_dataset$db_33<-Final_WOSIS_dataset$bdws33_value_avg
Final_WOSIS_dataset$porosity_percent<-NA
Final_WOSIS_dataset$WG_33kpa<-Final_WOSIS_dataset$wg0033_value_avg
Final_WOSIS_dataset$profile_id<-Final_WOSIS_dataset$profile_id
Final_WOSIS_dataset$layer_id<-Final_WOSIS_dataset$profile_layer_id
Final_WOSIS_dataset$site_key<-"Bates_et_al_2020"
Final_WOSIS_dataset$hzn_top<-Final_WOSIS_dataset$upper_depth
Final_WOSIS_dataset$hzn_bot<-Final_WOSIS_dataset$lower_depth
Final_WOSIS_dataset$db_od<-Final_WOSIS_dataset$db_dry
Final_WOSIS_dataset$tex_psda<-NA
Final_WOSIS_dataset$sand_tot_psa_percent<-Final_WOSIS_dataset$sand_value_avg
Final_WOSIS_dataset$silt_tot_psa_percent<-Final_WOSIS_dataset$silt_value_avg
Final_WOSIS_dataset$clay_tot_psa_percent<-Final_WOSIS_dataset$clay_value_avg
Final_WOSIS_dataset$ph_h2o<-Final_WOSIS_dataset$phaq_value_avg
Final_WOSIS_dataset$ksat_field<-NA
Final_WOSIS_dataset$ksat_lab<-NA
Final_WOSIS_dataset$field_head_m<- NA
Final_WOSIS_dataset$field_wrc<- NA
Final_WOSIS_dataset$oc<- (Final_WOSIS_dataset$orgc_value_avg)/10
Final_WOSIS_dataset$keywords_total_porosity<- NA
Final_WOSIS_dataset$hzn_desgn<-NA
Final_WOSIS_dataset$source_db = "WOSIS"
Final_WOSIS_dataset$latitude_decimal_degrees<- Final_WOSIS_dataset$latitude
Final_WOSIS_dataset$longitude_decimal_degrees<- Final_WOSIS_dataset$longitude
Final_WOSIS_dataset$method<- "Pressure plate"
Final_WOSIS_dataset$method_keywords<- NA
##merge the target variables
##merge the target variables
WOSIS.WRC<- Final_WOSIS_dataset[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                    "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                    "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                    "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                    "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                    "keywords_total_porosity","source_db","location_accuracy_min","location_accuracy_max"),]


WOSIS.WRC_1<- subset(WOSIS.WRC, lab_wrc>1)
WOSIS.WRC<- WOSIS.WRC[!WOSIS.WRC$layer_id %in% WOSIS.WRC_1$layer_id,]
WOSIS.WRC1<- WOSIS.WRC %>% filter( layer_id %in% c(598429, 598597, 598598, 598599, 601063, 598626, 598674, 599538, 604376, 604823, 604825,
                                                   611743, 615464, 620345, 634556, 641757, 642584, 642592, 642606, 642608, 642800, 642801,
                                                   642876, 642877, 654385, 656325, 656328, 656329, 656372, 656376, 658972, 658979, 658993,
                                                   658993, 659093, 660343, 660352, 660832, 660833, 660834, 660835, 660836, 660837, 660839,
                                                   660842, 660843, 660844, 660845, 660846, 660848, 660849,660912, 660859, 660864, 660868,
                                                   660943, 660951, 660954, 660955, 660957, 660957, 660960, 660963, 660967, 660968,660973,
                                                   660976, 660982, 660983, 660985, 660986, 660988, 686016, 686017, 686018, 686020, 813517,
                                                   813537,813603, 814143, 816561, 817793, 598680,598683))
WOSIS.WRC<-WOSIS.WRC[!WOSIS.WRC$layer_id%in% WOSIS.WRC1$layer_id,]
#write.csv(WOSIS.WRC,"P:/guptasu/Andreas_ksat_data/nonsensical_data_review/WOSIS.WRC.csv" )
#saveRDS(WOSIS.WRC, "E:/Soil_points1/INT/fwdwaterretentiondatafromkssl/KSSL_soil.lab_WRC/hydrosprops.WOSIS_SG.rds")
WOSIS.WRC1<-read.csv("P:/guptasu/Andreas_ksat_data/nonsensical_data_review/WOSIS.WRC.csv")

locations<- WOSIS.WRC[, c("layer_id","location_accuracy_min", "location_accuracy_max")]

locations1 <-locations %>%
  mutate(layer_id = layer_id) %>% # or maybe dmy, depending on your date format
  group_by(layer_id) %>%
  arrange(desc(layer_id)) %>%
  summarise_all(funs(na.omit(.)[1]))
## more SWCCs to remove

WOSIS.WRC1<- merge(WOSIS.WRC1,locations1, by = "layer_id")

WOSIS.WRC2<-WOSIS.WRC1 %>% filter(layer_id %in% c(599571, 614531, 629141,630746,630749, 630750, 630753, 630754, 630851, 30852, 656398, 661164, 630854, 633012, 633020, 633832,634548, 643310,647166, 647706, 647707, 672093, 647738, 647760, 647826,647827, 647828, 647829,647830, 655178, 656341, 656366, 656367, 656368, 656369, 656370, 656371,56373, 656374, 656375, 656377, 656378, 656379, 656380, 656381, 656282,854572,840527, 834053, 647825, 630748, 636268,636274, 827713,656308, 818625))
WOSIS.WRC1<-WOSIS.WRC1[!WOSIS.WRC1$layer_id%in% WOSIS.WRC2$layer_id,]
dim(WOSIS.WRC1)
```

    ## [1] 18186    31

#### *Australia\_CSIRO*

  - CSIRO (2020) [CSIRO National Soil Site Database, v4,
    CSIRO](https://data.csiro.au/collections/collection/CIcsiro:7526v004).

<!-- end list -->

``` r
NATSOIL.results = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/LAB_RESULTS.csv")
NATSOIL.Depths = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/SAMPLES.csv")
NATSOIL.Depths$ID<- paste(NATSOIL.Depths$s_id,NATSOIL.Depths$h_no)
NATSOIL.sites = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/OBSERVATIONS.csv")
NATSOIL.sites_<- NATSOIL.sites[,c(3,12,13)]
## selected the lab_codes belong to water retention curves "P3B3VL"
NATSOIL_WRC <- NATSOIL.results[grep("P3B3VL", NATSOIL.results$labm_code), ]
##After the selecting the curves, the project_code was noticed to find the other physical and chemical properties
NATSOIL_sel.profiles<- NATSOIL.results %>% filter(proj_code %in% c("Morphology", "SSM", "RAALS", "BILLABONG", "Forrest", "BGM_ESM",
                                                                                   "CSIRO_LW" ,"CL", "DD", "Billabong", "GYC"))
NATSOIL_sel_WRC<-NATSOIL_sel.profiles[NATSOIL_sel.profiles$s_id%in% NATSOIL_WRC$s_id,]
#NATSOIL_sel_WRC_loc<- merge(NATSOIL.sites_,NATSOIL_sel_WRC, by = "s_id")
## there are so many lab codes. Hence, to extract the all information, it is important to deal with each project separately.
## Morphology_site
NATSOIL_sel_morphology<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "Morphology",]
NATSOIL_sel_Mor_prof<- NATSOIL_sel_morphology %>%
filter(labm_code %in% c("P3A1", "P3B3VLb001", "P3B3VLb003", "P3B3VLb005",
                                       "P3B3VLb01", "P3B3VLb05","P3B3VLbSAT" ,"4A1", "6B2", "P10_S_2", "P10_S_63","P10_S_2000",
                                        "P3B2VL_15", "P3B2VL_5", "P4_50_McK"))
NATSOIL_sel_Mor_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_Mor_prof, by = "s_id")
NATSOIL_sel_Mor_prof_$ID1<-paste(NATSOIL_sel_Mor_prof_$s_id, NATSOIL_sel_Mor_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")
NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="Morphology")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_Mor_prof_<- merge(NATSOIL_sel_Mor_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_Mor_prof_$ID<- paste(NATSOIL_sel_Mor_prof_$s_id, NATSOIL_sel_Mor_prof_$h_no,NATSOIL_sel_Mor_prof_$samp_no, NATSOIL_sel_Mor_prof_$labr_no )
## introduce horizon_information
#NATSOIL.horizons = read.csv("E:/Soil_points/Australia/CSIRO/HORIZONS_.csv")
#colnames(NATSOIL.horizons)
NATSOIL_sel_Mor_prof_wide<-dcast(NATSOIL_sel_Mor_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_Mor_prof_wide<- NATSOIL_sel_Mor_prof_wide[!is.na(NATSOIL_sel_Mor_prof_wide$P3B3VLb001),]
NATSOIL_sel_Mor_prof_loc<- NATSOIL_sel_Mor_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID","h_desig_master","h_upper_depth","h_lower_depth" )]
NATSOIL_sel_Mor_prof_final<- merge(NATSOIL_sel_Mor_prof_wide,NATSOIL_sel_Mor_prof_loc, by = "ID")
NATSOIL_sel_Mor_prof_final <-NATSOIL_sel_Mor_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
##test_Morphology_database
Morphology.WRC_only_wide <- gather(NATSOIL_sel_Mor_prof_final, condition, measurement, P3B2VL_15:P3B3VLbSAT, factor_key=TRUE)
#Morphology.WRC_only_wide<- Morphology.WRC_only_wide[order(Morphology.WRC_only_wide$Group.1),]
Morphology.WRC_only_wide$condition <- as.character(Morphology.WRC_only_wide$condition)
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B2VL_15"]<- 150
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B2VL_5"]<- 50
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B3VLb001"]<- 0.1
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B3VLb003"]<- 0.3
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B3VLb005"]<- 0.5
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B3VLb01"]<- 1
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B3VLb05"]<- 5
Morphology.WRC_only_wide$condition[Morphology.WRC_only_wide$condition=="P3B3VLbSAT"]<- 0
Morphology.WRC_only_wide$head_m<- as.numeric(as.character(Morphology.WRC_only_wide$condition))
Morphology.WRC_only_wide$latitude_decimal_degrees<- Morphology.WRC_only_wide$o_latitude
Morphology.WRC_only_wide$longitude_decimal_degrees<- Morphology.WRC_only_wide$o_longitude
Morphology.WRC_only_wide$layer_id<- Morphology.WRC_only_wide$ID
Morphology.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
Morphology.WRC_only_wide$db_33<-NA
Morphology.WRC_only_wide$porosity_percent<-NA
Morphology.WRC_only_wide$WG_33kpa<-NA
Morphology.WRC_only_wide$profile_id<-NA
Morphology.WRC_only_wide$site_key<-"Australia_Morph"
Morphology.WRC_only_wide$hzn_top<-Morphology.WRC_only_wide$h_upper_depth
Morphology.WRC_only_wide$hzn_bot<-Morphology.WRC_only_wide$h_lower_depth
Morphology.WRC_only_wide$db_od<-Morphology.WRC_only_wide$P3A1
Morphology.WRC_only_wide$tex_psda<-NA
Morphology.WRC_only_wide$sand_tot_psa_percent<-NA
Morphology.WRC_only_wide$silt_tot_psa_percent<-NA
Morphology.WRC_only_wide$clay_tot_psa_percent<-NA
Morphology.WRC_only_wide$ph_h2o<-NA
Morphology.WRC_only_wide$ksat_field<-NA
Morphology.WRC_only_wide$ksat_lab<-NA
Morphology.WRC_only_wide$field_head_m<- NA
Morphology.WRC_only_wide$field_wrc<- NA
Morphology.WRC_only_wide$oc<-NA
Morphology.WRC_only_wide$keywords_total_porosity<- NA
Morphology.WRC_only_wide$hzn_desgn<-Morphology.WRC_only_wide$h_desig_master
Morphology.WRC_only_wide$lab_head_m<-Morphology.WRC_only_wide$head_m
Morphology.WRC_only_wide$lab_wrc<-Morphology.WRC_only_wide$measurement
Morphology.WRC_only_wide$source_db = "Australia_Morph"
Morphology.WRC_only_wide$method<- "Suction and pressure plate"
Morphology.WRC_only_wide$method_keywords = "0-5m suction plate and 50, 150 pressure plate"
##merge the target variables
Morphology.WRC<- Morphology.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                               "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                               "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                               "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                               "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                               "keywords_total_porosity","source_db"),]
dim(Morphology.WRC)
```

    ## [1] 2888   29

``` r
##SSM_site
NATSOIL_sel_SSM<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "SSM",]
NATSOIL_sel_SSM_prof<- NATSOIL_sel_SSM%>%
filter(labm_code %in% c("4B1", "6B3", "P3A1", "PWS63-212",
                                        "PWS20-63", "PWS212-425","PWS425-1mm" ,"PWS1-2mm", "P3B3VLc001", "P3B3VLc003", "P3B3VLc01","P3B3VLc005",
                                        "P3B3VLcSAT", "P3B3VLc03", "P3B3VLd3", "P3B3VLd5", "P3B3VLd15", "P3B3VLd06", "P3B3VLd1" ))
NATSOIL_sel_SSM_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_SSM_prof, by = "s_id")
NATSOIL_sel_SSM_prof_$ID1<-paste(NATSOIL_sel_SSM_prof_$s_id, NATSOIL_sel_SSM_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")
NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="SSM")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_SSM_prof_<- merge(NATSOIL_sel_SSM_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_SSM_prof_$ID<- paste(NATSOIL_sel_SSM_prof_$s_id, NATSOIL_sel_SSM_prof_$h_no,NATSOIL_sel_SSM_prof_$samp_no, NATSOIL_sel_SSM_prof_$labr_no )
NATSOIL_sel_SSM_prof_wide<-dcast(NATSOIL_sel_SSM_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_SSM_prof_wide<- NATSOIL_sel_SSM_prof_wide[!is.na(NATSOIL_sel_SSM_prof_wide$P3A1),]
NATSOIL_sel_SSM_prof_loc<- NATSOIL_sel_SSM_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_SSM_prof_final<- merge(NATSOIL_sel_SSM_prof_wide,NATSOIL_sel_SSM_prof_loc, by = "ID")
NATSOIL_sel_SSM_prof_final <-NATSOIL_sel_SSM_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
#NATSOIL_sel_SSM_prof_final<- aggregate(NATSOIL_sel_SSM_prof_final[, 1:24], list(NATSOIL_sel_SSM_prof_final$ID), mean)
##145
##test_SSM_database
NATSOIL.WRC_only_wide <- gather(NATSOIL_sel_SSM_prof_final, condition, measurement, P3B3VLc001:P3B3VLd1, factor_key=TRUE)
#NATSOIL.WRC_only_wide<- NATSOIL.WRC_only_wide [order(NATSOIL.WRC_only_wide $Group.1),]
NATSOIL.WRC_only_wide$condition <- as.character(NATSOIL.WRC_only_wide$condition)
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLc001"]<- 0.1
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLc003"]<- 0.3
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLc01"]<- 1
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLc005"]<- 0.5
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLcSAT"]<- 0
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLc03"]<- 3
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLd3"]<- 30
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLd5"]<- 50
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLd15"]<- 150
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLd06"]<-6
NATSOIL.WRC_only_wide$condition[NATSOIL.WRC_only_wide$condition=="P3B3VLd1"]<- 10
NATSOIL.WRC_only_wide$head_m<- as.numeric(as.character(NATSOIL.WRC_only_wide$condition))
NATSOIL.WRC_only_wide$latitude_decimal_degrees<- NATSOIL.WRC_only_wide$o_latitude
NATSOIL.WRC_only_wide$longitude_decimal_degrees<- NATSOIL.WRC_only_wide$o_longitude
NATSOIL.WRC_only_wide$layer_id<- NATSOIL.WRC_only_wide$ID
NATSOIL.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
NATSOIL.WRC_only_wide$db_33<-NATSOIL.WRC_only_wide$P3A1
NATSOIL.WRC_only_wide$porosity_percent<-NA
NATSOIL.WRC_only_wide$WG_33kpa<-NA
NATSOIL.WRC_only_wide$profile_id<-NA
NATSOIL.WRC_only_wide$site_key<-"Australia_SSM"
NATSOIL.WRC_only_wide$hzn_top<-NATSOIL.WRC_only_wide$h_upper_depth
NATSOIL.WRC_only_wide$hzn_bot<-NATSOIL.WRC_only_wide$h_lower_depth
NATSOIL.WRC_only_wide$db_od<-NATSOIL.WRC_only_wide$P3A1
NATSOIL.WRC_only_wide$tex_psda<-NA
NATSOIL.WRC_only_wide$sand_tot_psa_percent<-NA
NATSOIL.WRC_only_wide$silt_tot_psa_percent<-NA
NATSOIL.WRC_only_wide$clay_tot_psa_percent<-NA
NATSOIL.WRC_only_wide$ph_h2o<-NA
NATSOIL.WRC_only_wide$ksat_field<-NA
NATSOIL.WRC_only_wide$ksat_lab<-NA
NATSOIL.WRC_only_wide$field_head_m<- NA
NATSOIL.WRC_only_wide$field_wrc<- NA
NATSOIL.WRC_only_wide$oc<-NA
NATSOIL.WRC_only_wide$keywords_total_porosity<- NA
NATSOIL.WRC_only_wide$hzn_desgn<-NATSOIL.WRC_only_wide$h_desig_master
NATSOIL.WRC_only_wide$lab_head_m<-NATSOIL.WRC_only_wide$head_m
NATSOIL.WRC_only_wide$lab_wrc<-NATSOIL.WRC_only_wide$measurement
NATSOIL.WRC_only_wide$source_db = "Australia_SSM"
NATSOIL.WRC_only_wide$method<- "Suction and pressure plate"
NATSOIL.WRC_only_wide$method_keywords = "0-5m suction plate and after 50m pressure plate"
##merge the target variables
NATSOIL.WRC<- NATSOIL.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                          "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                          "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                          "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                          "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                          "keywords_total_porosity","source_db"),]
dim(NATSOIL.WRC)
```

    ## [1] 1160   29

``` r
## RAALS_site
NATSOIL_sel_RAALS<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "RAALS",]
NATSOIL_sel_RAALS_prof<- NATSOIL_sel_RAALS%>% 
filter(labm_code %in% c("P3A1", "P3B2VL_15", "P3B2VL_1", "P3B2VL_5", 
                            "P3B3VLb01", "P3B3VLb03","P3B3VLb06" ,"P3B3VLb001", "P3B3VLb005", "P3B3VLb003","P3B3VLc001",
                            "P3B3VLc003", "P3B3VLc005", "P3B3VLc01", "P3B3VLc03", "P3B3VLc06","6B3", "P4_sat_LOV", "P10_S_2", "P10_S_63",
                            "P10_S_2000"))
NATSOIL_sel_RAALS_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_RAALS_prof, by = "s_id")
NATSOIL_sel_RAALS_prof_$ID1<-paste(NATSOIL_sel_RAALS_prof_$s_id, NATSOIL_sel_RAALS_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")
NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="RAALS")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_RAALS_prof_<- merge(NATSOIL_sel_RAALS_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_RAALS_prof_$ID<- paste(NATSOIL_sel_RAALS_prof_$s_id, NATSOIL_sel_RAALS_prof_$h_no,
                                     NATSOIL_sel_RAALS_prof_$samp_no, NATSOIL_sel_RAALS_prof_$labr_no)
NATSOIL_sel_RAALS_prof_wide<-dcast(NATSOIL_sel_RAALS_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_RAALS_prof_wide<- NATSOIL_sel_RAALS_prof_wide[!is.na(NATSOIL_sel_RAALS_prof_wide$P3A1),]
NATSOIL_sel_RAALS_prof_loc<- NATSOIL_sel_RAALS_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID",
                                                          "h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_RAALS_prof_final<- merge(NATSOIL_sel_RAALS_prof_wide,NATSOIL_sel_RAALS_prof_loc, by = "ID")
#NATSOIL_sel_RAALS_prof_final<- aggregate(NATSOIL_sel_RAALS_prof_final[, 1:25], list(NATSOIL_sel_RAALS_prof_final$ID), mean)
NATSOIL_sel_RAALS_prof_final <-NATSOIL_sel_RAALS_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
##112
##test_RAALS_database
NATSOIL_RAALS.WRC_only_wide <- gather(NATSOIL_sel_RAALS_prof_final, condition, measurement, P3B2VL_15:P3B3VLc06, factor_key=TRUE)
#NATSOIL_RAALS.WRC_only_wide<- NATSOIL_RAALS.WRC_only_wide [order(NATSOIL_RAALS.WRC_only_wide $Group.1),]
NATSOIL_RAALS.WRC_only_wide$condition <- as.character(NATSOIL_RAALS.WRC_only_wide$condition)
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B2VL_15"]<- 150
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B2VL_1"]<- 10
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B2VL_5"]<- 50
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLb01"]<- 1
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLb03"]<-3
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLb06"]<- 6
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLb001"]<- 0.1
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLb005"]<- 0.5
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLb003"]<- 0.3
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLc001"]<- 0.1
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLc003"]<- 0.3
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLc005"]<-0.5
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLc01"]<- 1
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLc03"]<- 3
NATSOIL_RAALS.WRC_only_wide$condition[NATSOIL_RAALS.WRC_only_wide$condition=="P3B3VLc06"]<- 6
NATSOIL_RAALS.WRC_only_wide$head_m<- as.numeric(as.character(NATSOIL_RAALS.WRC_only_wide$condition))
NATSOIL_RAALS.WRC_only_wide$latitude_decimal_degrees<- NATSOIL_RAALS.WRC_only_wide$o_latitude
NATSOIL_RAALS.WRC_only_wide$longitude_decimal_degrees<- NATSOIL_RAALS.WRC_only_wide$o_longitude
NATSOIL_RAALS.WRC_only_wide$layer_id<- NATSOIL_RAALS.WRC_only_wide$ID
NATSOIL_RAALS.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
NATSOIL_RAALS.WRC_only_wide$db_33<-NA
NATSOIL_RAALS.WRC_only_wide$porosity_percent<-NA
NATSOIL_RAALS.WRC_only_wide$WG_33kpa<-NA
NATSOIL_RAALS.WRC_only_wide$profile_id<-NA
NATSOIL_RAALS.WRC_only_wide$site_key<-"Australia_RAALS"
NATSOIL_RAALS.WRC_only_wide$hzn_top<-NATSOIL_RAALS.WRC_only_wide$h_upper_depth
NATSOIL_RAALS.WRC_only_wide$hzn_bot<-NATSOIL_RAALS.WRC_only_wide$h_lower_depth
NATSOIL_RAALS.WRC_only_wide$db_od<-NATSOIL_RAALS.WRC_only_wide$P3A1
NATSOIL_RAALS.WRC_only_wide$sand_tot_psa_percent<-NA
NATSOIL_RAALS.WRC_only_wide$silt_tot_psa_percent<-NA
NATSOIL_RAALS.WRC_only_wide$clay_tot_psa_percent<-NA
NATSOIL_RAALS.WRC_only_wide$ph_h2o<-NA
NATSOIL_RAALS.WRC_only_wide$ksat_field<-NA
NATSOIL_RAALS.WRC_only_wide$ksat_lab<-(NATSOIL_RAALS.WRC_only_wide$P4_sat_LOV)*2.4
NATSOIL_RAALS.WRC_only_wide$field_head_m<- NA
NATSOIL_RAALS.WRC_only_wide$field_wrc<- NA
NATSOIL_RAALS.WRC_only_wide$oc<-NA
NATSOIL_RAALS.WRC_only_wide$keywords_total_porosity<- NA
NATSOIL_RAALS.WRC_only_wide$hzn_desgn<-NATSOIL_RAALS.WRC_only_wide$h_desig_master
NATSOIL_RAALS.WRC_only_wide$lab_head_m<-NATSOIL_RAALS.WRC_only_wide$head_m
NATSOIL_RAALS.WRC_only_wide$lab_wrc<-NATSOIL_RAALS.WRC_only_wide$measurement
NATSOIL_RAALS.WRC_only_wide$source_db = "Australia_RAALS"
NATSOIL_RAALS.WRC_only_wide$tex_psda<-NA
NATSOIL_RAALS.WRC_only_wide$method<- "Suction and pressure plate"
NATSOIL_RAALS.WRC_only_wide$method_keywords = "0-5m suction plate and after 50m pressure plate"
##merge the target variables
NATSOIL_RAALS<- NATSOIL_RAALS.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                                  "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                                  "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                                  "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                                  "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                                  "keywords_total_porosity","source_db"),]
dim(NATSOIL_RAALS)
```

    ## [1] 1568   29

``` r
#NATSOIL_RAALS_method$method<- "0-5m suction plate and after 50m pressure plate"
## CSIRO_LW
NATSOIL_sel_CSIRO_LW<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "CSIRO_LW",]
NATSOIL_sel_CSIRO_LW_prof<- NATSOIL_sel_CSIRO_LW%>%
filter(labm_code %in% c("4B2", "6B3", "P3A1", "P3B2VL_1 ",
                                        "P3B2VL_15", "P3B3VLb01","P3B3VLb03" ,"P10_S_2000", "P10_S_63", "P3B2VL_5", "P3B3VLb001","P3B3VLb003",
                                    "P3B3VLb005", "P3B3VLc03", "P3B3VLb06", "P3B3VLd5", "P3B3VLd15", "P3B3VLd06", "P3B3VLd1"))
NATSOIL_sel_CSIRO_LW_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_CSIRO_LW_prof, by = "s_id")
NATSOIL_sel_CSIRO_LW_prof_$ID1<-paste(NATSOIL_sel_CSIRO_LW_prof_$s_id, NATSOIL_sel_CSIRO_LW_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")
NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="CSIRO_LW")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_CSIRO_LW_prof_<- merge(NATSOIL_sel_CSIRO_LW_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_CSIRO_LW_prof_$ID<- paste(NATSOIL_sel_CSIRO_LW_prof_$s_id, NATSOIL_sel_CSIRO_LW_prof_$h_no,NATSOIL_sel_CSIRO_LW_prof_$samp_no, NATSOIL_sel_CSIRO_LW_prof_$labr_no )
NATSOIL_sel_CSIRO_LW_prof_wide<-dcast(NATSOIL_sel_CSIRO_LW_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_CSIRO_LW_prof_wide<- NATSOIL_sel_CSIRO_LW_prof_wide[!is.na(NATSOIL_sel_CSIRO_LW_prof_wide$P3B3VLb001),]
NATSOIL_sel_CSIRO_LW_prof_loc<- NATSOIL_sel_CSIRO_LW_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID",
                                                              "h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_CSIRO_LW_prof_final<- merge(NATSOIL_sel_CSIRO_LW_prof_wide,NATSOIL_sel_CSIRO_LW_prof_loc, by = "ID")
NATSOIL_sel_CSIRO_LW_prof_final <-NATSOIL_sel_CSIRO_LW_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
#NATSOIL_sel_CSIRO_LW_prof_final<- aggregate(NATSOIL_sel_CSIRO_LW_prof_final[, 1:18], list(NATSOIL_sel_CSIRO_LW_prof_final$ID), mean)
##24
 ##test_CSIRO_LW_database
NATSOIL_CSIRO_LW.WRC_only_wide <- gather(NATSOIL_sel_CSIRO_LW_prof_final, condition, measurement, P3B2VL_15:P3B3VLb06, factor_key=TRUE)
#NATSOIL_CSIRO_LW.WRC_only_wide<- NATSOIL_CSIRO_LW.WRC_only_wide [order(NATSOIL_CSIRO_LW.WRC_only_wide$Group.1),]
NATSOIL_CSIRO_LW.WRC_only_wide$condition <- as.character(NATSOIL_CSIRO_LW.WRC_only_wide$condition)
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B2VL_1"]<- 10
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B2VL_15"]<- 150
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLb01"]<- 1
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLb03"]<- 3
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B2VL_5"]<-50
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLb001"]<- 0.1
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLb003"]<- 0.3
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLb005"]<- 0.5
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLc03"]<- 3
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLb06"]<- 6
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLd5"]<- 50
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLd15"]<-150
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLd06"]<- 6
NATSOIL_CSIRO_LW.WRC_only_wide$condition[NATSOIL_CSIRO_LW.WRC_only_wide$condition=="P3B3VLd1"]<- 10
NATSOIL_CSIRO_LW.WRC_only_wide$head_m<- as.numeric(as.character(NATSOIL_CSIRO_LW.WRC_only_wide$condition))
NATSOIL_CSIRO_LW.WRC_only_wide$latitude_decimal_degrees<- NATSOIL_CSIRO_LW.WRC_only_wide$o_latitude
NATSOIL_CSIRO_LW.WRC_only_wide$longitude_decimal_degrees<- NATSOIL_CSIRO_LW.WRC_only_wide$o_longitude
NATSOIL_CSIRO_LW.WRC_only_wide$layer_id<- NATSOIL_CSIRO_LW.WRC_only_wide$ID
NATSOIL_CSIRO_LW.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
NATSOIL_CSIRO_LW.WRC_only_wide$db_33<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$porosity_percent<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$WG_33kpa<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$profile_id<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$site_key<-"Australia_CSIRO_LW"
NATSOIL_CSIRO_LW.WRC_only_wide$hzn_top<-NATSOIL_CSIRO_LW.WRC_only_wide$h_upper_depth
NATSOIL_CSIRO_LW.WRC_only_wide$hzn_bot<-NATSOIL_CSIRO_LW.WRC_only_wide$h_lower_depth
NATSOIL_CSIRO_LW.WRC_only_wide$db_od<-NATSOIL_CSIRO_LW.WRC_only_wide$P3A1
NATSOIL_CSIRO_LW.WRC_only_wide$sand_tot_psa_percent<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$silt_tot_psa_percent<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$clay_tot_psa_percent<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$ph_h2o<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$ksat_field<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$ksat_lab<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$field_head_m<- NA
NATSOIL_CSIRO_LW.WRC_only_wide$field_wrc<- NA
NATSOIL_CSIRO_LW.WRC_only_wide$oc<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$keywords_total_porosity<- NA
NATSOIL_CSIRO_LW.WRC_only_wide$hzn_desgn<-NATSOIL_CSIRO_LW.WRC_only_wide$h_desig_master
NATSOIL_CSIRO_LW.WRC_only_wide$lab_head_m<-NATSOIL_CSIRO_LW.WRC_only_wide$head_m
NATSOIL_CSIRO_LW.WRC_only_wide$lab_wrc<-NATSOIL_CSIRO_LW.WRC_only_wide$measurement
NATSOIL_CSIRO_LW.WRC_only_wide$source_db = "Australia_CSIRO_LW"
NATSOIL_CSIRO_LW.WRC_only_wide$tex_psda<-NA
NATSOIL_CSIRO_LW.WRC_only_wide$method = "Suction and pressure plate"
NATSOIL_CSIRO_LW.WRC_only_wide$method_keywords<-"0-5m suction plate and after 50m pressure plate"
##merge the target variables
NATSOIL_CSIRO<- NATSOIL_CSIRO_LW.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                                   "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                                   "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                                   "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                                   "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                                   "keywords_total_porosity","source_db"),]
dim(NATSOIL_CSIRO)
```

    ## [1] 192  29

``` r
## CL_site
NATSOIL_sel_CL<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "CL",]
NATSOIL_sel_CL_prof<- NATSOIL_sel_CL%>%
filter(labm_code %in% c("4A1", "6A1", "P3A1","P3A_NR","P3A1_C4","P3A1_CLOD","P3A1_e", "P3B3VLe004 ",
                                      "P3B3VLe01", "P3B3VLe03","P3B3VLe06" ,"P3B3VLe15", "P3B3VLe2", "P3B3VLe7", "P10_NR_C","P10_NR_CS",
                                   "P10_NR_FS", "P10_NR_Z"))
NATSOIL_sel_CL_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_CL_prof, by = "s_id")
NATSOIL_sel_CL_prof_$ID1<-paste(NATSOIL_sel_CL_prof_$s_id, NATSOIL_sel_CL_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")

NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="CL")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_CL_prof_<- merge(NATSOIL_sel_CL_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_CL_prof_$ID<- paste(NATSOIL_sel_CL_prof_$s_id, NATSOIL_sel_CL_prof_$h_no,NATSOIL_sel_CL_prof_$samp_no, NATSOIL_sel_CL_prof_$labr_no )
NATSOIL_sel_CL_prof_wide<-dcast(NATSOIL_sel_CL_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_CL_prof_wide<- NATSOIL_sel_CL_prof_wide[!is.na(NATSOIL_sel_CL_prof_wide$P3B3VLe01),]
NATSOIL_sel_CL_prof_loc<- NATSOIL_sel_CL_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID",
                                                  "h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_CL_prof_final<- merge(NATSOIL_sel_CL_prof_wide,NATSOIL_sel_CL_prof_loc, by = "ID")
NATSOIL_sel_CL_prof_final <-NATSOIL_sel_CL_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
#NATSOIL_sel_CL_prof_final<- aggregate(NATSOIL_sel_CL_prof_final[, 1:17], list(NATSOIL_sel_CL_prof_final$ID), mean)
##14
##test_CL_database
NATSOIL_CL.WRC_only_wide <- gather(NATSOIL_sel_CL_prof_final, condition, measurement, P3B3VLe01:P3B3VLe7, factor_key=TRUE)
#NATSOIL_CL.WRC_only_wide<- NATSOIL_CL.WRC_only_wide [order(NATSOIL_CL.WRC_only_wide$Group.1),]
NATSOIL_CL.WRC_only_wide$condition <- as.character(NATSOIL_CL.WRC_only_wide$condition)
NATSOIL_CL.WRC_only_wide$condition[NATSOIL_CL.WRC_only_wide$condition=="P3B3VLe01"]<- 1
NATSOIL_CL.WRC_only_wide$condition[NATSOIL_CL.WRC_only_wide$condition=="P3B3VLe03"]<- 3
NATSOIL_CL.WRC_only_wide$condition[NATSOIL_CL.WRC_only_wide$condition=="P3B3VLe06"]<- 6
NATSOIL_CL.WRC_only_wide$condition[NATSOIL_CL.WRC_only_wide$condition=="P3B3VLe15"]<- 150
NATSOIL_CL.WRC_only_wide$condition[NATSOIL_CL.WRC_only_wide$condition=="P3B3VLe2"]<-20
NATSOIL_CL.WRC_only_wide$condition[NATSOIL_CL.WRC_only_wide$condition=="P3B3VLe7"]<- 70
NATSOIL_CL.WRC_only_wide$head_m<- as.numeric(as.character(NATSOIL_CL.WRC_only_wide$condition))
NATSOIL_CL.WRC_only_wide$latitude_decimal_degrees<- NATSOIL_CL.WRC_only_wide$o_latitude
NATSOIL_CL.WRC_only_wide$longitude_decimal_degrees<- NATSOIL_CL.WRC_only_wide$o_longitude
NATSOIL_CL.WRC_only_wide$layer_id<- NATSOIL_CL.WRC_only_wide$ID
NATSOIL_CL.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
NATSOIL_CL.WRC_only_wide$db_33<-NA
NATSOIL_CL.WRC_only_wide$porosity_percent<-NA
NATSOIL_CL.WRC_only_wide$WG_33kpa<-NA
NATSOIL_CL.WRC_only_wide$profile_id<-NA
NATSOIL_CL.WRC_only_wide$site_key<-"Australia_CL"
NATSOIL_CL.WRC_only_wide$hzn_top<-NATSOIL_CL.WRC_only_wide$h_upper_depth
NATSOIL_CL.WRC_only_wide$hzn_bot<-NATSOIL_CL.WRC_only_wide$h_lower_depth
NATSOIL_CL.WRC_only_wide$db_od<-NATSOIL_CL.WRC_only_wide$P3A1
NATSOIL_CL.WRC_only_wide$sand_tot_psa_percent<-NATSOIL_CL.WRC_only_wide$P10_NR_FS+NATSOIL_CL.WRC_only_wide$P10_NR_CS
NATSOIL_CL.WRC_only_wide$silt_tot_psa_percent<-NATSOIL_CL.WRC_only_wide$P10_NR_Z
NATSOIL_CL.WRC_only_wide$clay_tot_psa_percent<-NATSOIL_CL.WRC_only_wide$P10_NR_C
NATSOIL_CL.WRC_only_wide$ph_h2o<-NATSOIL_CL.WRC_only_wide$"4A1"
NATSOIL_CL.WRC_only_wide$ksat_field<-NA
NATSOIL_CL.WRC_only_wide$ksat_lab<-NA
NATSOIL_CL.WRC_only_wide$field_head_m<- NA
NATSOIL_CL.WRC_only_wide$field_wrc<- NA
NATSOIL_CL.WRC_only_wide$oc<-NATSOIL_CL.WRC_only_wide$"6A1"
NATSOIL_CL.WRC_only_wide$keywords_total_porosity<- NA
NATSOIL_CL.WRC_only_wide$hzn_desgn<-NATSOIL_CL.WRC_only_wide$h_desig_master
NATSOIL_CL.WRC_only_wide$lab_head_m<-NATSOIL_CL.WRC_only_wide$head_m
NATSOIL_CL.WRC_only_wide$lab_wrc<-NATSOIL_CL.WRC_only_wide$measurement
NATSOIL_CL.WRC_only_wide$source_db = "Australia_CL"
NATSOIL_CL.WRC_only_wide$tex_psda<-NA
NATSOIL_CL.WRC_only_wide$method = "Suction and pressure plate"
NATSOIL_CL.WRC_only_wide$method_keywords<-"0-5m suction plate and after 50m pressure plate"
##merge the target variables
NATSOIL_CL<- NATSOIL_CL.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                            "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                            "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                            "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                            "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                            "keywords_total_porosity","source_db"),]
dim(NATSOIL_CL)
```

    ## [1] 84 29

``` r
## DD_site
NATSOIL_sel_DD<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "DD",]
NATSOIL_sel_DD_prof<- NATSOIL_sel_DD%>%
filter(labm_code %in% c("4A1", "6A1", "P3A1","P3A_NR","P3A1_C4","P3A1_CLOD","P3A1_e", "P3B3VLe004 ",
                                        "P3B3VLe01", "P3B3VLe03","P3B3VLe06" ,"P3B3VLe15", "P3B3VLe2", "P3B3VLe7", "P10_NR_C","P10_NR_CS",
                                        "P10_NR_FS", "P10_NR_Z"))
NATSOIL_sel_DD_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_DD_prof, by = "s_id")
NATSOIL_sel_DD_prof_$ID1<-paste(NATSOIL_sel_DD_prof_$s_id, NATSOIL_sel_DD_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")
NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="DD")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_DD_prof_<- merge(NATSOIL_sel_DD_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_DD_prof_$ID<- paste(NATSOIL_sel_DD_prof_$s_id, NATSOIL_sel_DD_prof_$h_no,NATSOIL_sel_DD_prof_$samp_no, NATSOIL_sel_DD_prof_$labr_no )
NATSOIL_sel_DD_prof_wide<-dcast(NATSOIL_sel_DD_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_DD_prof_wide<- NATSOIL_sel_DD_prof_wide[!is.na(NATSOIL_sel_DD_prof_wide$P3B3VLe01),]
NATSOIL_sel_DD_prof_loc<- NATSOIL_sel_DD_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID",
                                                  "h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_DD_prof_final<- merge(NATSOIL_sel_DD_prof_wide,NATSOIL_sel_DD_prof_loc, by = "ID")
NATSOIL_sel_DD_prof_final <-NATSOIL_sel_DD_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
#NATSOIL_sel_DD_prof_final<- aggregate(NATSOIL_sel_DD_prof_final[, 1:17], list(NATSOIL_sel_DD_prof_final$ID), mean)
##13
##test_DD_database
NATSOIL_DD.WRC_only_wide <- gather(NATSOIL_sel_DD_prof_final, condition, measurement, P3B3VLe01:P3B3VLe7, factor_key=TRUE)
#NATSOIL_DD.WRC_only_wide<- NATSOIL_DD.WRC_only_wide [order(NATSOIL_DD.WRC_only_wide$Group.1),]
NATSOIL_DD.WRC_only_wide$condition <- as.character(NATSOIL_DD.WRC_only_wide$condition)
NATSOIL_DD.WRC_only_wide$condition[NATSOIL_DD.WRC_only_wide$condition=="P3B3VLe01"]<- 1
NATSOIL_DD.WRC_only_wide$condition[NATSOIL_DD.WRC_only_wide$condition=="P3B3VLe03"]<- 3
NATSOIL_DD.WRC_only_wide$condition[NATSOIL_DD.WRC_only_wide$condition=="P3B3VLe06"]<- 6
NATSOIL_DD.WRC_only_wide$condition[NATSOIL_DD.WRC_only_wide$condition=="P3B3VLe15"]<- 150
NATSOIL_DD.WRC_only_wide$condition[NATSOIL_DD.WRC_only_wide$condition=="P3B3VLe2"]<-20
NATSOIL_DD.WRC_only_wide$condition[NATSOIL_DD.WRC_only_wide$condition=="P3B3VLe7"]<- 70
NATSOIL_DD.WRC_only_wide$head_m<- as.numeric(as.character(NATSOIL_DD.WRC_only_wide$condition))
NATSOIL_DD.WRC_only_wide$latitude_decimal_degrees<- NATSOIL_DD.WRC_only_wide$o_latitude
NATSOIL_DD.WRC_only_wide$longitude_decimal_degrees<- NATSOIL_DD.WRC_only_wide$o_longitude
NATSOIL_DD.WRC_only_wide$layer_id<- NATSOIL_DD.WRC_only_wide$ID
NATSOIL_DD.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
NATSOIL_DD.WRC_only_wide$db_33<-NA
NATSOIL_DD.WRC_only_wide$porosity_percent<-NA
NATSOIL_DD.WRC_only_wide$WG_33kpa<-NA
NATSOIL_DD.WRC_only_wide$profile_id<-NA
NATSOIL_DD.WRC_only_wide$site_key<-"Australia_DD"
NATSOIL_DD.WRC_only_wide$hzn_top<-NATSOIL_DD.WRC_only_wide$h_upper_depth
NATSOIL_DD.WRC_only_wide$hzn_bot<-NATSOIL_DD.WRC_only_wide$h_lower_depth
NATSOIL_DD.WRC_only_wide$db_od<-NATSOIL_DD.WRC_only_wide$P3A1
NATSOIL_DD.WRC_only_wide$sand_tot_psa_percent<-NATSOIL_DD.WRC_only_wide$P10_NR_FS+NATSOIL_DD.WRC_only_wide$P10_NR_CS
NATSOIL_DD.WRC_only_wide$silt_tot_psa_percent<-NATSOIL_DD.WRC_only_wide$P10_NR_Z
NATSOIL_DD.WRC_only_wide$clay_tot_psa_percent<-NATSOIL_DD.WRC_only_wide$P10_NR_C
NATSOIL_DD.WRC_only_wide$ph_h2o<-NATSOIL_DD.WRC_only_wide$"4A1"
NATSOIL_DD.WRC_only_wide$ksat_field<-NA
NATSOIL_DD.WRC_only_wide$ksat_lab<-NA
NATSOIL_DD.WRC_only_wide$field_head_m<- NA
NATSOIL_DD.WRC_only_wide$field_wrc<- NA
NATSOIL_DD.WRC_only_wide$oc<-NATSOIL_DD.WRC_only_wide$"6A1"
NATSOIL_DD.WRC_only_wide$keywords_total_porosity<- NA
NATSOIL_DD.WRC_only_wide$hzn_desgn<-NATSOIL_DD.WRC_only_wide$h_desig_master
NATSOIL_DD.WRC_only_wide$lab_head_m<-NATSOIL_DD.WRC_only_wide$head_m
NATSOIL_DD.WRC_only_wide$lab_wrc<-NATSOIL_DD.WRC_only_wide$measurement
NATSOIL_DD.WRC_only_wide$source_db = "Australia_DD"
NATSOIL_DD.WRC_only_wide$tex_psda<-NA
NATSOIL_DD.WRC_only_wide$method = "Suction and pressure plate"
NATSOIL_DD.WRC_only_wide$method_keywords<-"0-5m suction plate and after 50m pressure plate"
##merge the target variables
NATSOIL_DD<- NATSOIL_DD.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                            "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                            "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                            "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                            "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                            "keywords_total_porosity","source_db"),]
dim(NATSOIL_DD)
```

    ## [1] 78 29

``` r
##GYC_site
NATSOIL_sel_GYC<- NATSOIL_sel_WRC[NATSOIL_sel_WRC$proj_code == "GYC",]
NATSOIL_sel_GYC_prof<- NATSOIL_sel_GYC%>%
filter(labm_code %in% c("4A1", "6A1", "P3A1","P3A_NR","P3A1_C4","P3A1_CLOD","P3A1_e", "P3B3VLe004",
                                        "P3B3VLe01", "P3B3VLe03","P3B3VLe06" ,"P3B3VLe15", "P3B3VLe2", "P3B3VLe7", "P10_NR_C","P10_NR_CS",
                                        "P10_NR_FS", "P10_NR_Z"))
NATSOIL_sel_GYC_prof_<- merge(NATSOIL.sites_,NATSOIL_sel_GYC_prof, by = "s_id")
NATSOIL_sel_GYC_prof_$ID1<-paste(NATSOIL_sel_GYC_prof_$s_id, NATSOIL_sel_GYC_prof_$h_no )
NATSOIL.horizons = read.csv("C:/Users/guptasu.D/Downloads/CSIRO (1)/CSIRO/HORIZONS.csv")
NATSOIL.horizons$ID1<- paste(NATSOIL.horizons$s_id, NATSOIL.horizons$h_no )
mor_pr_code<- subset(NATSOIL.horizons,NATSOIL.horizons$proj_code=="GYC")
mor_pr_code<- mor_pr_code[,c("ID1","h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_GYC_prof_<- merge(NATSOIL_sel_GYC_prof_,mor_pr_code, by = "ID1")
NATSOIL_sel_GYC_prof_$ID<- paste(NATSOIL_sel_GYC_prof_$s_id, NATSOIL_sel_GYC_prof_$h_no,NATSOIL_sel_GYC_prof_$samp_no, NATSOIL_sel_GYC_prof_$labr_no )
NATSOIL_sel_GYC_prof_wide<-dcast(NATSOIL_sel_GYC_prof_, ID ~ labm_code, value.var = "labr_value")
NATSOIL_sel_GYC_prof_wide<- NATSOIL_sel_GYC_prof_wide[!is.na(NATSOIL_sel_GYC_prof_wide$P3B3VLe01),]
NATSOIL_sel_GYC_prof_loc<- NATSOIL_sel_GYC_prof_[,c("s_id", "o_latitude","o_longitude","proj_code","ID",
                                                    "h_desig_master","h_upper_depth","h_lower_depth")]
NATSOIL_sel_GYC_prof_final<- merge(NATSOIL_sel_GYC_prof_wide,NATSOIL_sel_GYC_prof_loc, by = "ID")
NATSOIL_sel_GYC_prof_final <-NATSOIL_sel_GYC_prof_final %>%
mutate(ID = ID) %>% # or maybe dmy, depending on your date format
group_by(ID) %>%
arrange(desc(ID)) %>%
summarise_all(funs(na.omit(.)[1]))
#NATSOIL_sel_GYC_prof_final<- aggregate(NATSOIL_sel_GYC_prof_final[, 1:17], list(NATSOIL_sel_GYC_prof_final$ID), mean)
##5
##test_GYC_database
NATSOIL_GYC.WRC_only_wide <- gather(NATSOIL_sel_GYC_prof_final, condition, measurement, P3B3VLe01:P3B3VLe7, factor_key=TRUE)
#NATSOIL_GYC.WRC_only_wide<- NATSOIL_GYC.WRC_only_wide [order(NATSOIL_GYC.WRC_only_wide$Group.1),]
NATSOIL_GYC.WRC_only_wide$condition <- as.character(NATSOIL_GYC.WRC_only_wide$condition)
NATSOIL_GYC.WRC_only_wide$condition[NATSOIL_GYC.WRC_only_wide$condition=="P3B3VLe01"]<- 1
NATSOIL_GYC.WRC_only_wide$condition[NATSOIL_GYC.WRC_only_wide$condition=="P3B3VLe03"]<- 3
NATSOIL_GYC.WRC_only_wide$condition[NATSOIL_GYC.WRC_only_wide$condition=="P3B3VLe06"]<- 6
NATSOIL_GYC.WRC_only_wide$condition[NATSOIL_GYC.WRC_only_wide$condition=="P3B3VLe15"]<- 150
NATSOIL_GYC.WRC_only_wide$condition[NATSOIL_GYC.WRC_only_wide$condition=="P3B3VLe2"]<-20
NATSOIL_GYC.WRC_only_wide$condition[NATSOIL_GYC.WRC_only_wide$condition=="P3B3VLe7"]<- 70
NATSOIL_GYC.WRC_only_wide$head_m<- as.numeric(as.character(NATSOIL_GYC.WRC_only_wide$condition))
NATSOIL_GYC.WRC_only_wide$latitude_decimal_degrees<- NATSOIL_GYC.WRC_only_wide$o_latitude
NATSOIL_GYC.WRC_only_wide$longitude_decimal_degrees<- NATSOIL_GYC.WRC_only_wide$o_longitude
NATSOIL_GYC.WRC_only_wide$layer_id<- NATSOIL_GYC.WRC_only_wide$ID
NATSOIL_GYC.WRC_only_wide$disturbed_undisturbed<-"undisturbed"
NATSOIL_GYC.WRC_only_wide$db_33<-NA
NATSOIL_GYC.WRC_only_wide$porosity_percent<-NA
NATSOIL_GYC.WRC_only_wide$WG_33kpa<-NA
NATSOIL_GYC.WRC_only_wide$profile_id<-NA
NATSOIL_GYC.WRC_only_wide$site_key<-"Australia_GYC"
NATSOIL_GYC.WRC_only_wide$hzn_top<-NATSOIL_GYC.WRC_only_wide$h_upper_depth
NATSOIL_GYC.WRC_only_wide$hzn_bot<-NATSOIL_GYC.WRC_only_wide$h_lower_depth
NATSOIL_GYC.WRC_only_wide$db_od<-NATSOIL_GYC.WRC_only_wide$P3A1
NATSOIL_GYC.WRC_only_wide$sand_tot_psa_percent<-NATSOIL_GYC.WRC_only_wide$P10_NR_FS+NATSOIL_GYC.WRC_only_wide$P10_NR_CS
NATSOIL_GYC.WRC_only_wide$silt_tot_psa_percent<-NATSOIL_GYC.WRC_only_wide$P10_NR_Z
NATSOIL_GYC.WRC_only_wide$clay_tot_psa_percent<-NATSOIL_GYC.WRC_only_wide$P10_NR_C
NATSOIL_GYC.WRC_only_wide$ph_h2o<-NATSOIL_GYC.WRC_only_wide$"4A1"
NATSOIL_GYC.WRC_only_wide$ksat_field<-NA
NATSOIL_GYC.WRC_only_wide$ksat_lab<-NA
NATSOIL_GYC.WRC_only_wide$field_head_m<- NA
NATSOIL_GYC.WRC_only_wide$field_wrc<- NA
NATSOIL_GYC.WRC_only_wide$oc<-NATSOIL_GYC.WRC_only_wide$"6A1"
NATSOIL_GYC.WRC_only_wide$keywords_total_porosity<- NA
NATSOIL_GYC.WRC_only_wide$hzn_desgn<-NATSOIL_GYC.WRC_only_wide$h_desig_master
NATSOIL_GYC.WRC_only_wide$lab_head_m<-NATSOIL_GYC.WRC_only_wide$head_m
NATSOIL_GYC.WRC_only_wide$lab_wrc<-NATSOIL_GYC.WRC_only_wide$measurement
NATSOIL_GYC.WRC_only_wide$source_db = "Australia_GYC"
NATSOIL_GYC.WRC_only_wide$tex_psda<-NA
NATSOIL_GYC.WRC_only_wide$method = "Suction and pressure plate"
NATSOIL_GYC.WRC_only_wide$method_keywords<-"0-5m suction plate and after 50m pressure plate"
##merge the target variables
NATSOIL_GYC<- NATSOIL_GYC.WRC_only_wide[, c("layer_id","disturbed_undisturbed","profile_id","site_key","method","method_keywords",
                                              "latitude_decimal_degrees","longitude_decimal_degrees","hzn_desgn","hzn_top",
                                              "hzn_bot","db_33","db_od","oc","tex_psda","sand_tot_psa_percent",
                                              "silt_tot_psa_percent","clay_tot_psa_percent","ph_h2o","ksat_field","ksat_lab",
                                              "porosity_percent","WG_33kpa","lab_head_m","lab_wrc","field_head_m","field_wrc",
                                              "keywords_total_porosity","source_db"),]
dim(NATSOIL_GYC)
```

    ## [1] 30 29

``` r
##rbind all the data from Australia
Aulstr_WRC<- rbind(NATSOIL_GYC,NATSOIL_DD,NATSOIL_CL,NATSOIL_CSIRO,NATSOIL_RAALS,NATSOIL.WRC,Morphology.WRC, guess_max = 10000)
Aulstr_WRC1<- Aulstr_WRC %>% filter( layer_id %in% c("CP339 7 2 3", "SSM1 2 3 1","10000"))
Aulstr_WRC<-Aulstr_WRC[!Aulstr_WRC$layer_id%in% Aulstr_WRC1$layer_id,]
#write.csv(Aulstr_WRC,"E:/Andreas_ksat_data/nonsensical_data_review/Aulstr_WRC_TOM.csv" )
Aulstr_WRC<- read.csv("P:/guptasu/Andreas_ksat_data/nonsensical_data_review/Aulstr_WRC_TOM.csv")
Aulstr_WRC$hzn_top<- Aulstr_WRC$hzn_top*100
Aulstr_WRC$hzn_bot<- Aulstr_WRC$hzn_bot*100
Aulstr_WRC$location_accuracy_min<- NA
Aulstr_WRC$location_accuracy_max<- NA
dim(Aulstr_WRC)
```

    ## [1] 5984   31

#### *ETH imported data from literature*

``` r
ETH_literature<- readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Dataset_SWCCs_Literature", guess_max = 10000)
ETH_literature1<- ETH_literature %>% filter( layer_id %in% c("kool_105", "kool_106","kool_108","kool_109","kool_123","kool_142","kool_176",
                                                             "kool_19","kool_197","kool_198","kool_32","kool_33","kool_55","kool_60",
                                                             "kool_66","kool_68","kool_199","kool_200","kool_204","kool_24","kool_121"))
ETH_literature<-ETH_literature[!ETH_literature$layer_id%in% ETH_literature1$layer_id,]
##Holten dataset
Holten<- readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Holten", guess_max = 10000)
Holten1<- Holten %>% filter( layer_id %in% c("Holten1767", "Holten1216","Holten1497","Holten1651", "Holten1828"))
Holten<-Holten[!Holten$layer_id%in% Holten1$layer_id,]
## convert gravimetric to volumetric by converting BD33 kPa to dry bulk density
Holten$GWC<- Holten$lab_wrc/Holten$db_33
holten_30kpa<- subset(Holten,Holten$lab_head_m==3)
holten_30kpa$db_new<- as.numeric(holten_30kpa$db_33/(1+(holten_30kpa$lab_wrc)))
holten_2<- holten_30kpa[,c(1,33)]
Holten<- merge(Holten,holten_2, by = "layer_id")
Holten$lab_wrc<- Holten$GWC*Holten$db_new
Holten$db_od<- Holten$db_new
Holten<- Holten[,c(1:31)]
##UNSODA
Unsoda<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "UNSODA",guess_max = 10000)
Unsoda1<- Unsoda %>% filter( layer_id %in% c("UNSODA1161", "UNSODA1162","UNSODA1166","UNSODA1165", "UNSODA1460","UNSODA3050"))
Unsoda<-Unsoda[!Unsoda$layer_id%in% Unsoda1$layer_id,]
##HYBRAS dataset
hybras<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "HYBRAS",guess_max = 10000)
hybras1<- hybras %>% filter( layer_id %in% c("HYBRAS1008", "HYBRAS1015","HYBRAS1018","HYBRAS1019", "HYBRAS1021","HYBRAS1022","HYBRAS1025",
                                             "HYBRAS1027","HYBRAS1032","HYBRAS1039","HYBRAS1042","HYBRAS525","HYBRAS994","HYBRAS996","HYBRAS998"))
hybras<-hybras[!hybras$layer_id%in% hybras1$layer_id,]
##SWISS dataset
Swiss_data<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Swiss_database",guess_max = 10000)
##Zalf dataset
Zalf<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Zalf_database",guess_max = 10000)
##Belgiam dataset
Belgiam<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Vereecken_database",guess_max = 10000)
Florida<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Florida_database",guess_max = 10000)
Florida1<-Florida %>% filter( layer_id %in% c("Florida_ksat_data_2197", "Florida_ksat_data_226","Florida_ksat_data_2262","Florida_ksat_data_227",
                                              "Florida_ksat_data_2322","Florida_ksat_data_2326","Florida_ksat_data_2327","Florida_ksat_data_2430",
                                              "Florida_ksat_data_2452","Florida_ksat_data_2458","Florida_ksat_data_2459","Florida_ksat_data_2460",
                                              "Florida_ksat_data_2461","Florida_ksat_data_2480","Florida_ksat_data_2481","Florida_ksat_data_2482",
                                              "Florida_ksat_data_1135","Florida_ksat_data_1190","Florida_ksat_data_1191","Florida_ksat_data_1227",
                                              "Florida_ksat_data_1238","Florida_ksat_data_1298","Florida_ksat_data_1318","Florida_ksat_data_1413",
                                              "Florida_ksat_data_1458","Florida_ksat_data_1460","Florida_ksat_data_1461","Florida_ksat_data_1467",
                                              "Florida_ksat_data_1483","Florida_ksat_data_1520","Florida_ksat_data_1713","Florida_ksat_data_1721",
                                              "Florida_ksat_data_1915","Florida_ksat_data_197","Florida_ksat_data_2154","Florida_ksat_data_2155",
                                              "Florida_ksat_data_2183","Florida_ksat_data_2483","Florida_ksat_data_2484","Florida_ksat_data_2516",
                                              "Florida_ksat_data_2516","Florida_ksat_data_2571","Florida_ksat_data_2595","Florida_ksat_data_2596",
                                              "Florida_ksat_data_2602","Florida_ksat_data_2609","Florida_ksat_data_2627","Florida_ksat_data_2628",
                                              "Florida_ksat_data_269","Florida_ksat_data_270","Florida_ksat_data_301","Florida_ksat_data_305",
                                              "Florida_ksat_data_306","Florida_ksat_data_3127","Florida_ksat_data_3165","Florida_ksat_data_3208",
                                              "Florida_ksat_data_3209","Florida_ksat_data_3329","Florida_ksat_data_3335","Florida_ksat_data_3395",
                                              "Florida_ksat_data_3714","Florida_ksat_data_3717","Florida_ksat_data_3802","Florida_ksat_data_3803",
                                              "Florida_ksat_data_3815","Florida_ksat_data_4019","Florida_ksat_data_4035","Florida_ksat_data_4192",
                                              "Florida_ksat_data_4200","Florida_ksat_data_4215","Florida_ksat_data_4244","Florida_ksat_data_4270",
                                              "Florida_ksat_data_4314","Florida_ksat_data_4324","Florida_ksat_data_4325","Florida_ksat_data_4328",
                                              "Florida_ksat_data_4446","Florida_ksat_data_4489","Florida_ksat_data_4492","Florida_ksat_data_4493",
                                              "Florida_ksat_data_4510","Florida_ksat_data_4512","Florida_ksat_data_4513","Florida_ksat_data_4520",
                                              "Florida_ksat_data_4526","Florida_ksat_data_4833","Florida_ksat_data_4913","Florida_ksat_data_4917",
                                              "Florida_ksat_data_4529","Florida_ksat_data_5371","Florida_ksat_data_5415","Florida_ksat_data_558",
                                              "Florida_ksat_data_575","Florida_ksat_data_5751","Florida_ksat_data_5753","Florida_ksat_data_5754",
                                              "Florida_ksat_data_5758","Florida_ksat_data_5757","Florida_ksat_data_581","Florida_ksat_data_620",
                                              "Florida_ksat_data_625","Florida_ksat_data_626","Florida_ksat_data_627","Florida_ksat_data_628",
                                              "Florida_ksat_data_638","Florida_ksat_data_640","Florida_ksat_data_641","Florida_ksat_data_642",
                                              "Florida_ksat_data_643","Florida_ksat_data_65","Florida_ksat_data_653","Florida_ksat_data_654",
                                              "Florida_ksat_data_655","Florida_ksat_data_670","Florida_ksat_data_671","Florida_ksat_data_672",
                                              "Florida_ksat_data_675","Florida_ksat_data_676","Florida_ksat_data_677","Florida_ksat_data_678",
                                              "Florida_ksat_data_740","Florida_ksat_data_741","Florida_ksat_data_746","Florida_ksat_data_747",
                                              "Florida_ksat_data_843","Florida_ksat_data_844","Florida_ksat_data_845","Florida_ksat_data_926",
                                              "Florida_ksat_data_927","Florida_ksat_data_928","Florida_ksat_data_1317","Florida_ksat_data_3715"))
Florida<-Florida[!Florida$layer_id%in% Florida1$layer_id,]
#write.csv(Florida,"E:/Andreas_ksat_data/nonsensical_data_review/Florida_wrc.csv")
Australian<-readxl::read_excel("C:/Users/guptasu.D/Downloads/WRC_dataset_2021.xlsx", sheet = "Australian",guess_max = 10000)
Australian1<-Australian %>% filter( layer_id %in% c("Forrest_JA_1985_34", "Forrest_JA_1985_36","Forrest_JA_1985_4"))
Australian<-Australian[!Australian$layer_id%in% Australian1$layer_id,]
eth.tbl <- rbind(ETH_literature, Unsoda,hybras, Swiss_data, Zalf,Belgiam, Florida, Australian, Holten)
eth.tbl1<- eth.tbl[eth.tbl$lab_wrc<=1,]
dim(eth.tbl1)
```

    ## [1] 112904     31

## *Bind the data*

``` r
WRC_dataset<-rbind(afspdb.WRC2,EGRPR.WRC,Aulstr_WRC,eth.tbl1, WOSIS.WRC1)
WRC_dataset$locationid<- paste(WRC_dataset$latitude_decimal_degrees,"_", WRC_dataset$longitude_decimal_degrees)
WRC_dataset<-WRC_dataset[!is.na(WRC_dataset$latitude_decimal_degrees),]
WRC_dataset<-WRC_dataset[!is.na(WRC_dataset$lab_wrc),]
WRC_dataset<-WRC_dataset[!WRC_dataset$latitude_decimal_degrees==0,]
WRC.counts<- stack(by(WRC_dataset$lab_wrc, WRC_dataset$layer_id, FUN=function(x) sum(!is.na(x))))
WRC.counts_order<- WRC.counts[order(WRC.counts$values),]
WRC.counts$layer_id<- WRC.counts$ind
WRC.counts<- WRC.counts[WRC.counts$values >= 4,]

## Removed the SWCCs without wet-end having no information of bulk density 
WRC_dataset<-WRC_dataset[WRC_dataset$layer_id%in% WRC.counts$layer_id,]
Curves_with_less_0.2<- subset(WRC_dataset,WRC_dataset$lab_head_m<=0.2)
Curves_with_thetas_2<-WRC_dataset[WRC_dataset$layer_id %in% Curves_with_less_0.2$layer_id,]
Curves_with_thetas_not_wetend<-WRC_dataset[!WRC_dataset$layer_id %in% Curves_with_less_0.2$layer_id,]
Bulk_density<- Curves_with_thetas_not_wetend[!is.na(Curves_with_thetas_not_wetend$db_od),]
samples_with_bulk_density<- rbind(Curves_with_thetas_2,Bulk_density)

## These SWCCs are fitted using the 'soilhypfit' R package and estimated the vG parameters. Few SWCCs are discarded due to large RMSE (greater than 1 m3/m3). The total SWCCs presented in this database are 15,259. 
```
