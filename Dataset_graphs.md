Global soil hydraulic properties (GSHP) database
================
Surya Gupta, Andreas Papritz, Peter Lehmann, Tom Hengl, Sara Bonetti,
Dani Or

  - [Bulk density (g/cm3) vs Data
    sources](#bulk-density-gcm3-vs-data-sources)
  - [Organic carbon (%) vs Data
    sources](#organic-carbon--vs-data-sources)
  - [Sand content (%) vs Data sources](#sand-content--vs-data-sources)
  - [Silt content (%) vs Data sources](#silt-content--vs-data-sources)
  - [Clay content (%) vs Data sources](#clay-content--vs-data-sources)
  - [pH vs Data sources](#ph-vs-data-sources)
  - [Field measured saturated hydraulic conductivity (cm/day) vs Data
    sources](#field-measured-saturated-hydraulic-conductivity-cmday-vs-data-sources)
  - [Lab measured saturated hydraulic conductivity (cm/day) vs Data
    sources](#lab-measured-saturated-hydraulic-conductivity-cmday-vs-data-sources)
  - [Porosity (%) vs Data sources](#porosity--vs-data-sources)
  - [VG shape parameter (1/m) vs Data
    sources](#vg-shape-parameter-1m-vs-data-sources)
  - [VG shape parameter vs Data
    sources](#vg-shape-parameter-vs-data-sources)
  - [Saturated water content (m3/m3) vs Data
    sources](#saturated-water-content-m3m3-vs-data-sources)
  - [Residual water content (m3/m3) vs Data
    sources](#residual-water-content-m3m3-vs-data-sources)
  - [Root mean square error (m3/m3) vs Data
    sources](#root-mean-square-error-m3m3-vs-data-sources)

Plots of soil water characteristics data set. Total curves = 15153

``` r
library(ggplot2)

WRC<- read.csv("C:/Users/guptasu.D/Downloads/WRC_dataset_surya_et_al_2021_final.csv")

dim(WRC)
```

    ## [1] 136063     40

``` r
colnames(WRC)
```

    ##  [1] "layer_id"                  "disturbed_undisturbed"    
    ##  [3] "climate_classes"           "profile_id"               
    ##  [5] "site_key"                  "method"                   
    ##  [7] "method_keywords"           "latitude_decimal_degrees" 
    ##  [9] "longitude_decimal_degrees" "hzn_desgn"                
    ## [11] "hzn_top"                   "hzn_bot"                  
    ## [13] "db_33"                     "db_od"                    
    ## [15] "oc"                        "tex_psda"                 
    ## [17] "sand_tot_psa_percent"      "silt_tot_psa_percent"     
    ## [19] "clay_tot_psa_percent"      "ph_h2o"                   
    ## [21] "ksat_field"                "ksat_lab"                 
    ## [23] "porosity_percent"          "WG_33kpa"                 
    ## [25] "lab_head_m"                "lab_wrc"                  
    ## [27] "field_head_m"              "field_wrc"                
    ## [29] "keywords_total_porosity"   "alpha"                    
    ## [31] "n"                         "thetar"                   
    ## [33] "thetas"                    "RSS"                      
    ## [35] "RMSE"                      "SWCC_classes"             
    ## [37] "SWCC_ST_classes"           "source_db"                
    ## [39] "location_accuracy_min"     "location_accuracy_max"

``` r
unique(WRC$source_db)
```

    ##  [1] "Russia_EGRPR"        "WOSIS"               "ETH_Literature"     
    ##  [4] "Australia_DD"        "Australia_CL"        "Australia_GYC"      
    ##  [7] "AfSPDB"              "swiss_database"      "Australia_Morph"    
    ## [10] "Australia_CSIRO_LW"  "Australia_RAALS"     "Florida_database"   
    ## [13] "Australian_database" "HYBRAS"              "Australia_SSM"      
    ## [16] "UNSODA"              "Belgium_database"    "ZALF_database"

``` r
unique(WRC$site_key)
```

    ##  [1] "Stolbovoy_et_al_2016"        "Bates_et_al_2020"           
    ##  [3] "AL-Kayssi_A_W_2021"          "CSIRO_2020"                 
    ##  [5] "BAMBRA_A_ 2016"              "Wickland_et_al_2006"        
    ##  [7] "Leenaars_et_al_2014"         "Bhushan and Sharma_2005"    
    ##  [9] "Macinnis-NG_et_al_2010"      "Richard and Lüscher_1983_87"
    ## [11] "Ng_C. W. W_2020"             "Zhang et al_2018"           
    ## [13] "Tobon_et_al_2011"            "Medina_et_al_2002"          
    ## [15] "Sulaeman_et_al_2021"         "Grunwald_2020"              
    ## [17] "Forrest_JA_1985"             "Mosquera_et_al_2021"        
    ## [19] "Li et al_2012"               "Holten_1968"                
    ## [21] "Ottoni_et_al_2018"           "Ismail_1991"                
    ## [23] "Basile_et_al_1997"           "Elliott and Price_2020"     
    ## [25] "Kassaye_et_al_2021"          "Jha_et_al_2010"             
    ## [27] "Xia_et_al_2017"              "Toriyama_et_al_2011"        
    ## [29] "Karup_et_al_2017"            "Seki_et_al_2010"            
    ## [31] "Kool_1954"                   "Xing et al_2018"            
    ## [33] "Li et al_2011"               "Oliveira et al_2019"        
    ## [35] "Lozano et al_2014"           "Eden_et_al_2020"            
    ## [37] "Lowe_et_al_2019"             "Cooper_et_al_2017"          
    ## [39] "Mikko_Jauhiainen_2004"       "Abid and Lal et al_2009"    
    ## [41] "Nano_et_al_2016"             "Are_et_al_2018"             
    ## [43] "Guzman et al_2019"           "Bescansa et al_2006"        
    ## [45] "Macvicar_et_al_2019"         "Dlapa_et_al_2020"           
    ## [47] "Quang and Jansson_2012"      "Wang_et_al_2017"            
    ## [49] "Asswad_et_al_1993"           "Kumar_et_al_2010"           
    ## [51] "Konyai_et_al_2006"           "Saha and Kukal_2013"        
    ## [53] "Noguchi_et_al_1997"          "Simmons_L_A_2014"           
    ## [55] "Smeettem_Gregory_1996"       "Mondal_et_al_2016"          
    ## [57] "McBeath_et_al_2010"          "Talat_et_al_2020"           
    ## [59] "Pan_et_al_2019"              "Thakur_et_al_2010"          
    ## [61] "Glab et al_2020"             "Tyagi_et_al_2013"           
    ## [63] "Nemes_et_al_2001"            "Vereecken_et_al_2017"       
    ## [65] "Schindler_and_Müller_2017"   "Li_et_al_2019"              
    ## [67] "Nyamangara_et_al_2001"

## Bulk density (g/cm3) vs Data sources

``` r
options(warn=-1)

ggplot(WRC, aes(x=source_db, y=db_od)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Organic carbon (%) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=oc)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Sand content (%) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=sand_tot_psa_percent)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Silt content (%) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=silt_tot_psa_percent)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Clay content (%) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=clay_tot_psa_percent)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## pH vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=ph_h2o)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Field measured saturated hydraulic conductivity (cm/day) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=ksat_field)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Lab measured saturated hydraulic conductivity (cm/day) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=ksat_lab)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Porosity (%) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=porosity_percent)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## VG shape parameter (1/m) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=alpha)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## VG shape parameter vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=n)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Saturated water content (m3/m3) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=thetas)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Residual water content (m3/m3) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=thetar)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Root mean square error (m3/m3) vs Data sources

``` r
ggplot(WRC, aes(x=source_db, y=RMSE)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Dataset_graphs_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
