---
title: "Marketing Research of Brazilian Ecommerce"
author: "Andrew Pierson"
date: "May 4, 2019"
output: 
  html_document:
    keep_md: true
---

## Exploratory Data Analysis

The dataset of products contained 34,218 observations of 6 dimensions. This dataset was created by joining the reviews, payments, items, products, sellers, geolocations, and translations to the orders dataset. After joining these I grouped them by product ID and seller ID, and averaged the price, freight value, review score, product weight, photo quantity and payment installments to see these product attributes that were sold by each distributor. This dataset was purely numeric.
The first step I took with this dataset was to remove products that had null IDs and filter out canceled orders. Upon inspection of some initial relationships I found, unsurprisingly, that the average price of a product increased the greater a review score that it was given.
A rather interesting relationship that I found was between the average payments made on a product and the price of that product. I found that the average price appeared to increase until about 10 payments and then it decreased suddenly. I find this unusual because I would expect the price of a product to be very high if a customer was making 10 to 20 payments on the item. 


```r
# Load libraries
library(readxl)
library(dplyr)
library(NbClust)
library(dummies)
library(factoextra)
library(ggplot2)
library(ggExtra)
library(ggridges)

# Read in data
sellers <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_sellers_dataset.csv")
orders <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_orders_dataset.csv")
geolocation <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_geolocation_dataset.csv")
reviews <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_reviews_dataset.csv")
items <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_items_dataset.csv")
products <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_products_dataset.csv")
payments <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_payments_dataset.csv")
translation <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/product_category_name_translation.csv")

# Get the average review score for each product
productreviews = orders %>% left_join(reviews, by = "order_id") %>% left_join(payments, 
    by = "order_id") %>% left_join(items, by = "order_id") %>% left_join(products, 
    by = "product_id") %>% filter(order_status != "canceled")

# Subset dataset to get product information
cols <- c("seller_id", "product_id", "price", "freight_value", "product_weight_g", 
    "product_photos_qty", "payment_installments", "review_score")
productreviews <- productreviews[cols]
productattributes <- productreviews %>% na.omit(product_id) %>% group_by(product_id, 
    seller_id) %>% summarise(avg_price = mean(price), product_revenue = sum(price), 
    avg_freight_value = mean(freight_value), total_freight_value = sum(freight_value), 
    avg_review_score = mean(review_score), avg_weight = mean(product_weight_g), 
    avg_photo_qty = mean(product_photos_qty), avg_payments = mean(payment_installments)) %>% 
    left_join(products, by = "product_id") %>% left_join(translation, by = c(product_category_name = "ï..product_category_name"))  #%>% select(-c('product_category_name'))

library(plyr)
# Rename seller states
sellers$seller_state <- revalue(sellers$seller_state, c(AC = "Acre", AM = "Amazonas", 
    BA = "Bahia", CE = "Ceará", DF = "Distrito Federal", ES = "Espírito Santo", 
    GO = "Goiás", MA = "Maranhão", MG = "Minas Gerais", MS = "Mato Grosso do Sul", 
    MT = "Mato Grosso", PA = "Pará", PB = "Paraíba", PE = "Pernambuco", PI = "Piauí", 
    PR = "Paraná", RJ = "Rio de Janeiro", RN = "Rio Grande do Norte", RO = "Rondônia", 
    RS = "Rio Grande do Sul", SC = "Santa Catarina", SE = "Sergipe", SP = "São Paulo"))

# Read in geolocation data
geolocations <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/Tests/geolocations.csv")

# Join the geolocation data to the product-seller attributes
productattributes <- productattributes %>% left_join(sellers, by = "seller_id") %>% 
    left_join(geolocations, by = c(seller_zip_code_prefix = "geolocation_zip_code_prefix"))

# Check out the dimensions of the data
names(productattributes)
```

```
##  [1] "product_id"                    "seller_id"                    
##  [3] "avg_price"                     "product_revenue"              
##  [5] "avg_freight_value"             "total_freight_value"          
##  [7] "avg_review_score"              "avg_weight"                   
##  [9] "avg_photo_qty"                 "avg_payments"                 
## [11] "product_category_name"         "product_name_lenght"          
## [13] "product_description_lenght"    "product_photos_qty"           
## [15] "product_weight_g"              "product_length_cm"            
## [17] "product_height_cm"             "product_width_cm"             
## [19] "product_category_name_english" "seller_zip_code_prefix"       
## [21] "seller_city"                   "seller_state"                 
## [23] "X"                             "geo_lng"                      
## [25] "geo_lat"                       "output"
```

```r
summary(productattributes)
```

```
##                             product_id   
##  69455f41626a745aea9ee9164cb9eafd:    8  
##  d285360f29ac7fd97640bf0baef03de0:    8  
##  36f60d45225e60c7da4558b070ce4b60:    7  
##  4298b7e67dc399c200662b569563a2b2:    7  
##  656e0eca68dcecf6a31b8ececfabe3e8:    7  
##  dbb67791e405873b259e4656bf971246:    6  
##  (Other)                         :33574  
##                             seller_id       avg_price      
##  4a3ca9315b744ce9f8e9374361493884:  399   Min.   :   0.85  
##  cca3071e3e9bb7d12640c9fbe2301306:  322   1st Qu.:  39.99  
##  d91fb3b7d041e83b64a00a3edfb37e4f:  315   Median :  79.90  
##  fa1c13f2614d7b5c4749cbc52fecda94:  289   Mean   : 145.88  
##  7142540dd4c91e2237acb7e911c4eba2:  266   3rd Qu.: 157.50  
##  6560211a19b47992c3666cc44a7e94c0:  256   Max.   :6735.00  
##  (Other)                         :31770                    
##  product_revenue    avg_freight_value total_freight_value avg_review_score
##  Min.   :    2.29   Min.   :  0.01    Min.   :    0.01    Min.   :1.000   
##  1st Qu.:   62.50   1st Qu.: 13.59    1st Qu.:   16.11    1st Qu.:3.643   
##  Median :  139.94   Median : 16.73    Median :   27.34    Median :4.500   
##  Mean   :  415.87   Mean   : 21.15    Mean   :   69.26    Mean   :4.055   
##  3rd Qu.:  339.80   3rd Qu.: 21.84    3rd Qu.:   59.31    3rd Qu.:5.000   
##  Max.   :68810.00   Max.   :409.68    Max.   :14623.87    Max.   :5.000   
##                                                                           
##    avg_weight    avg_photo_qty     avg_payments     product_category_name
##  Min.   :    0   Min.   : 1.000   Min.   : 0.9091   Length:33617         
##  1st Qu.:  300   1st Qu.: 1.000   1st Qu.: 1.0000   Class :character     
##  Median :  700   Median : 1.000   Median : 2.0000   Mode  :character     
##  Mean   : 2241   Mean   : 2.169   Mean   : 3.1507                        
##  3rd Qu.: 1850   3rd Qu.: 3.000   3rd Qu.: 4.0000                        
##  Max.   :40425   Max.   :20.000   Max.   :24.0000                        
##                                                                          
##  product_name_lenght product_description_lenght product_photos_qty
##  Min.   : 5.0        Min.   :   4               Min.   : 1.000    
##  1st Qu.:42.0        1st Qu.: 341               1st Qu.: 1.000    
##  Median :51.0        Median : 595               Median : 1.000    
##  Mean   :48.4        Mean   : 772               Mean   : 2.169    
##  3rd Qu.:57.0        3rd Qu.: 971               3rd Qu.: 3.000    
##  Max.   :76.0        Max.   :3992               Max.   :20.000    
##                                                                   
##  product_weight_g product_length_cm product_height_cm product_width_cm
##  Min.   :    0    Min.   :  7.00    Min.   :  2.00    Min.   :  6.00  
##  1st Qu.:  300    1st Qu.: 18.00    1st Qu.:  8.00    1st Qu.: 15.00  
##  Median :  700    Median : 25.00    Median : 14.00    Median : 20.00  
##  Mean   : 2241    Mean   : 30.64    Mean   : 16.91    Mean   : 23.07  
##  3rd Qu.: 1850    3rd Qu.: 38.00    3rd Qu.: 20.00    3rd Qu.: 30.00  
##  Max.   :40425    Max.   :105.00    Max.   :105.00    Max.   :118.00  
##                                                                       
##  product_category_name_english seller_zip_code_prefix
##  bed_bath_table : 3106         Min.   : 1001         
##  sports_leisure : 2993         1st Qu.: 7032         
##  furniture_decor: 2660         Median :14050         
##  health_beauty  : 2596         Mean   :26484         
##  housewares     : 2363         3rd Qu.:31730         
##  (Other)        :19888         Max.   :99730         
##  NA's           :   11                               
##          seller_city               seller_state         X        
##  sao paulo     : 7856   São Paulo        :23139   Min.   :    1  
##  ibitinga      : 2264   Paraná           : 3011   1st Qu.: 3465  
##  curitiba      : 1113   Minas Gerais     : 2651   Median : 5270  
##  rio de janeiro:  848   Rio de Janeiro   : 1497   Mean   : 6749  
##  belo horizonte:  715   Santa Catarina   : 1429   3rd Qu.: 8223  
##  ribeirao preto:  673   Rio Grande do Sul:  761   Max.   :18977  
##  (Other)       :20148   (Other)          : 1129   NA's   :63     
##     geo_lng          geo_lat       
##  Min.   :-67.81   Min.   :-32.079  
##  1st Qu.:-48.83   1st Qu.:-23.613  
##  Median :-46.78   Median :-23.431  
##  Mean   :-47.36   Mean   :-22.937  
##  3rd Qu.:-46.52   3rd Qu.:-21.866  
##  Max.   :-34.86   Max.   : -2.501  
##  NA's   :63       NA's   :63       
##                                                                                       output     
##  R. José Custódio, 846 - Centro, Ibitinga - SP, 14940-000, Brazil                        : 2264  
##  R. Sd. Genivaldo José Damasceno, 16 - Cidade Ocian, Praia Grande - SP, 11704-360, Brazil:  316  
##  Rua Luiz José Duarte, 553 - Jardim Sao Carlos, Sumaré - SP, 13170-001, Brazil           :  296  
##  R. Dr. Bueno de Azevedo, 120 - Jardim Germania, São Paulo - SP, 05849-270, Brazil       :  267  
##  R. Anchieta, 1651, Penápolis - SP, 16300-000, Brazil                                    :  266  
##  (Other)                                                                                 :30145  
##  NA's                                                                                    :   63
```

```r
str(productattributes)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	33617 obs. of  26 variables:
##  $ product_id                   : Factor w/ 32951 levels "00066f42aeeb9f3007548bb9d3f33c38",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ seller_id                    : Factor w/ 3095 levels "0015a82c2db000af6aaaf3ae2ecb0532",..: 1057 1392 882 776 1703 2175 2498 1709 2549 1695 ...
##  $ avg_price                    : num  101.7 129.9 229 58.9 199 ...
##  $ product_revenue              : num  102 130 229 118 199 ...
##  $ avg_freight_value            : num  18.6 13.9 13.1 19.6 19.3 ...
##  $ total_freight_value          : num  18.6 13.9 13.1 39.2 19.3 ...
##  $ avg_review_score             : num  5 4 1 5 5 ...
##  $ avg_weight                   : num  300 1225 300 550 250 ...
##  $ avg_photo_qty                : num  6 4 2 3 4 1 1 1 1 3 ...
##  $ avg_payments                 : num  3 1 10 2.5 3 ...
##  $ product_category_name        : chr  "perfumaria" "automotivo" "cama_mesa_banho" "utilidades_domesticas" ...
##  $ product_name_lenght          : int  53 56 50 25 48 58 42 53 42 45 ...
##  $ product_description_lenght   : int  596 752 266 364 613 177 2461 274 253 520 ...
##  $ product_photos_qty           : int  6 4 2 3 4 1 1 1 1 3 ...
##  $ product_weight_g             : int  300 1225 300 550 250 100 700 600 6000 600 ...
##  $ product_length_cm            : int  20 55 45 19 22 16 25 30 40 26 ...
##  $ product_height_cm            : int  16 10 15 24 11 15 5 20 4 8 ...
##  $ product_width_cm             : int  16 26 35 12 15 16 15 20 30 22 ...
##  $ product_category_name_english: Factor w/ 71 levels "agro_industry_and_commerce",..: 60 6 8 50 71 6 21 17 8 40 ...
##  $ seller_zip_code_prefix       : int  3694 16301 14940 85603 88370 3880 14050 1212 14940 2443 ...
##  $ seller_city                  : Factor w/ 611 levels "04482255","abadia de goias",..: 519 403 228 198 357 519 443 519 228 519 ...
##  $ seller_state                 : Factor w/ 23 levels "Acre","Amazonas",..: 23 23 23 16 21 23 23 23 23 23 ...
##  $ X                            : int  1490 5749 5463 16741 17393 1563 5281 104 5463 618 ...
##  $ geo_lng                      : num  -46.5 -50.1 -48.8 -53.1 -48.7 ...
##  $ geo_lat                      : num  -23.5 -21.4 -21.8 -26 -26.9 ...
##  $ output                       : Factor w/ 18899 levels "0 - Asa Norte Comércio Local Norte 207 - Asa Norte, Brasília - DF, 70852-520, Brazil",..: 17361 5032 10602 4962 11782 13948 5739 14190 10602 1989 ...
##  - attr(*, "groups")=Classes 'tbl_df', 'tbl' and 'data.frame':	32134 obs. of  2 variables:
##   ..$ product_id: Factor w/ 32951 levels "00066f42aeeb9f3007548bb9d3f33c38",..: 1 2 3 4 5 6 7 8 9 10 ...
##   ..$ .rows     :List of 32134
##   .. ..$ : int 1
##   .. ..$ : int 2
##   .. ..$ : int 3
##   .. ..$ : int 4
##   .. ..$ : int 5
##   .. ..$ : int 6
##   .. ..$ : int 7
##   .. ..$ : int 8
##   .. ..$ : int 9
##   .. ..$ : int 10
##   .. ..$ : int 11
##   .. ..$ : int 12
##   .. ..$ : int 13
##   .. ..$ : int 14
##   .. ..$ : int 15
##   .. ..$ : int 16
##   .. ..$ : int 17
##   .. ..$ : int 18
##   .. ..$ : int 19
##   .. ..$ : int 20
##   .. ..$ : int 21
##   .. ..$ : int 22
##   .. ..$ : int 23
##   .. ..$ : int 24
##   .. ..$ : int 25
##   .. ..$ : int 26
##   .. ..$ : int 27
##   .. ..$ : int 28
##   .. ..$ : int 29
##   .. ..$ : int 30
##   .. ..$ : int 31
##   .. ..$ : int 32
##   .. ..$ : int 33
##   .. ..$ : int 34
##   .. ..$ : int 35
##   .. ..$ : int 36
##   .. ..$ : int 37
##   .. ..$ : int 38
##   .. ..$ : int 39
##   .. ..$ : int 40
##   .. ..$ : int 41
##   .. ..$ : int 42
##   .. ..$ : int 43
##   .. ..$ : int 44
##   .. ..$ : int 45
##   .. ..$ : int 46
##   .. ..$ : int 47
##   .. ..$ : int 48
##   .. ..$ : int 49
##   .. ..$ : int 50
##   .. ..$ : int 51
##   .. ..$ : int 52
##   .. ..$ : int 53
##   .. ..$ : int 54
##   .. ..$ : int 55
##   .. ..$ : int 56
##   .. ..$ : int 57
##   .. ..$ : int 58
##   .. ..$ : int 59
##   .. ..$ : int 60
##   .. ..$ : int 61
##   .. ..$ : int 62
##   .. ..$ : int 63
##   .. ..$ : int 64
##   .. ..$ : int 65
##   .. ..$ : int 66
##   .. ..$ : int 67
##   .. ..$ : int 68
##   .. ..$ : int 69
##   .. ..$ : int 70
##   .. ..$ : int 71
##   .. ..$ : int 72
##   .. ..$ : int 73
##   .. ..$ : int 74
##   .. ..$ : int 75
##   .. ..$ : int 76
##   .. ..$ : int 77
##   .. ..$ : int 78
##   .. ..$ : int 79
##   .. ..$ : int 80
##   .. ..$ : int 81
##   .. ..$ : int 82
##   .. ..$ : int 83
##   .. ..$ : int 84
##   .. ..$ : int 85
##   .. ..$ : int 86
##   .. ..$ : int 87
##   .. ..$ : int 88
##   .. ..$ : int 89
##   .. ..$ : int 90
##   .. ..$ : int 91
##   .. ..$ : int 92
##   .. ..$ : int 93
##   .. ..$ : int 94
##   .. ..$ : int 95
##   .. ..$ : int 96
##   .. ..$ : int 97
##   .. ..$ : int 98
##   .. ..$ : int 99
##   .. .. [list output truncated]
##   ..- attr(*, ".drop")= logi TRUE
```

```r
# Do prices vary depending on region?
ggplot(productattributes, aes(x = avg_price, y = seller_state, fill = seller_state, 
    main = "Average Product Price Across Regions")) + geom_density_ridges() + 
    theme_ridges() + theme(legend.position = "none")
```

![](Final_files/figure-html/EDA-1.png)<!-- -->

```r
# Are revenue and freight price related?
ggplot(productattributes, aes(x = avg_payments, y = avg_price, main = "Payments Versus Revenue")) + 
    geom_point(color = "red", fill = "blue", shape = 21, alpha = 0.2, size = 2, 
        stroke = 2)
```

![](Final_files/figure-html/EDA-2.png)<!-- -->

```r
# Are weight and freight price related?
ggplot(productattributes, aes(x = avg_review_score, y = avg_price, main = "Review Score Versus Revenue")) + 
    geom_point(color = "blue", fill = "green", shape = 21, alpha = 0.2, size = 2, 
        stroke = 2)
```

![](Final_files/figure-html/EDA-3.png)<!-- -->

## Cluster Analysis

To prepare the data numeric variables were chosen for analysis and scaled to a -1 to 1 distribution including product averages for the number of payments, price, photo quantity, freight value, weight, and review score. The data was them sampled to free up memory for computing the clusters, using a subset of 25% of the original dataset. Using Euclidean distance I then implemented the "elbow" method to determine the optimal number of clusters. I chose to use 4 clusters for the Complete hierarchical clustering, 3 clusters for Ward's hierarchical clustering, and 3 clusters for Kmeans clustering.
In addition to determining the optimal number of clusters, I also compared Complete, Ward's, and Kmeans clustering algorithms to determine which would be the better choice for the numerical dataset by inspecting the distribution of each cluster. The result from the comparison of the algorithms cluster distribution revealed that that the Kmeans algorithm produced a more even distribution among clusters. It was observed that the Complete and Ward's linkage methods for hierarchical clustering resulted in similar variable attributes for clusters 3 and 4, and 2 and 3, respectively. Further comparing these two methods reveals that the first cluster of the Ward's linkage method is a combination of the first and second clusters of the Complete linkage method. 
After choosing to inspect the Kmeans clustering method I found that the first cluster contained products that had the highest average price, freight values, photo quantity, and payments while also corresponding to the lowest review score. The second cluster from the Kmeans method indicated that products had the cheapest price, least photos, name length, payments, and freight value while corresponding to the highest review scores. This is unsurprising that the lowest priced products have the highest review scores. Those attributes for the third cluster using the Kmeans method still indicated a relatively low freight value and average amount of payments. Contrastingly, the third cluster observed a large product name length and review score.
Based on clustering of numeric data and inspection of the Kmeans clusters I would recommend that sellers provide products that have greater variation of pricing. One idea that sellers could use is to provide products that have incremental product features for customers to choose from. The customers would be more likely to give more accurate review scores and the seller could adjust their product offerings accordingly. Another idea might be to offer lower shipping prices for products in the first cluster by differentiating freight options. Sellers might offer slower shipping for customers that are only willing to pay less and premium values for those in a hurry. In addition, varying shipment options might increase average review scores or provide more insight for sellers.


```r
# Load libraries
library(imputeTS)
library(data.table)
library(cluster)
library(compareGroups)

# Subset numeric product dataset
productattributes.numeric <- data.frame(productattributes[, c(3, 5, 7, 9, 10, 
    12)])

# Remove missing values
productattributes.numeric <- na.omit(productattributes.numeric)
set.seed(123)
# Define sampling size of product data
sampleSize <- 4000
# Gather the random sampling of house data
productattributes.numeric <- productattributes.numeric[sample(nrow(productattributes.numeric), 
    sampleSize, replace = FALSE), ]
productattributes.numeric2 <- productattributes.numeric
productattributes.numeric <- scale(productattributes.numeric)

# Calculate distance matrix using euclidean distance
dis <- dist(productattributes.numeric, method = "euclidean")

# Find the best number of clusters according to the complete linkage method
numeric.complete <- NbClust(productattributes.numeric, distance = "euclidean", 
    min.nc = 2, max.nc = 10, method = "complete", index = "all")
```

![](Final_files/figure-html/Numeric Cluster Analysis-1.png)<!-- -->

```
## *** : The Hubert index is a graphical method of determining the number of clusters.
##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
##                 significant increase of the value of the measure i.e the significant peak in Hubert
##                 index second differences plot. 
## 
```

![](Final_files/figure-html/Numeric Cluster Analysis-2.png)<!-- -->

```
## *** : The D index is a graphical method of determining the number of clusters. 
##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
##                 second differences plot) that corresponds to a significant increase of the value of
##                 the measure. 
##  
## ******************************************************************* 
## * Among all indices:                                                
## * 8 proposed 2 as the best number of clusters 
## * 3 proposed 3 as the best number of clusters 
## * 8 proposed 4 as the best number of clusters 
## * 1 proposed 6 as the best number of clusters 
## * 2 proposed 8 as the best number of clusters 
## * 1 proposed 9 as the best number of clusters 
## 
##                    ***** Conclusion *****                            
##  
## * According to the majority rule, the best number of clusters is  2 
##  
##  
## *******************************************************************
```

```r
# Perform hierarchical clustering using complete linkage method
numeric.complete.hc <- hclust(dis, method = "complete")
# Cut the tree to the optimal number of clusters
numeric.complete.hc4 <- cutree(numeric.complete.hc, 4)
# Compute cophentic distance for complete linkage method
complete.hc.coph <- cophenetic(numeric.complete.hc)
# Compute the correlation between the cophenetic distance and original
# distance
cor(dis, complete.hc.coph)
```

```
## [1] 0.7008687
```

```r
# Find the best number of clusters according to wards linkage method
numeric.wards <- NbClust(productattributes.numeric, distance = "euclidean", 
    min.nc = 2, max.nc = 10, method = "ward.D2", index = "all")
```

![](Final_files/figure-html/Numeric Cluster Analysis-3.png)<!-- -->

```
## *** : The Hubert index is a graphical method of determining the number of clusters.
##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
##                 significant increase of the value of the measure i.e the significant peak in Hubert
##                 index second differences plot. 
## 
```

![](Final_files/figure-html/Numeric Cluster Analysis-4.png)<!-- -->

```
## *** : The D index is a graphical method of determining the number of clusters. 
##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
##                 second differences plot) that corresponds to a significant increase of the value of
##                 the measure. 
##  
## ******************************************************************* 
## * Among all indices:                                                
## * 7 proposed 2 as the best number of clusters 
## * 4 proposed 3 as the best number of clusters 
## * 3 proposed 4 as the best number of clusters 
## * 2 proposed 6 as the best number of clusters 
## * 3 proposed 7 as the best number of clusters 
## * 3 proposed 10 as the best number of clusters 
## 
##                    ***** Conclusion *****                            
##  
## * According to the majority rule, the best number of clusters is  2 
##  
##  
## *******************************************************************
```

```r
# Perform hierarchical clustering using wards linkage method
numeric.wards.hc <- hclust(dis, method = "ward.D2")
# Cut the tree to the optimal number of clusters
numeric.wards.hc3 <- cutree(numeric.complete.hc, 3)
# Compute cophentic distance for complete linkage method
wards.hc.coph <- cophenetic(numeric.wards.hc)
# Compute the correlation between the cophenetic distance and original
# distance
cor(dis, wards.hc.coph)
```

```
## [1] 0.4991892
```

```r
# Find the best number of clusters according to the kmeans linkage method
numeric.kmeans <- NbClust(productattributes.numeric, distance = "euclidean", 
    min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
```

![](Final_files/figure-html/Numeric Cluster Analysis-5.png)<!-- -->

```
## *** : The Hubert index is a graphical method of determining the number of clusters.
##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
##                 significant increase of the value of the measure i.e the significant peak in Hubert
##                 index second differences plot. 
## 
```

![](Final_files/figure-html/Numeric Cluster Analysis-6.png)<!-- -->

```
## *** : The D index is a graphical method of determining the number of clusters. 
##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
##                 second differences plot) that corresponds to a significant increase of the value of
##                 the measure. 
##  
## ******************************************************************* 
## * Among all indices:                                                
## * 7 proposed 2 as the best number of clusters 
## * 3 proposed 3 as the best number of clusters 
## * 2 proposed 4 as the best number of clusters 
## * 2 proposed 5 as the best number of clusters 
## * 6 proposed 6 as the best number of clusters 
## * 1 proposed 7 as the best number of clusters 
## * 1 proposed 8 as the best number of clusters 
## * 1 proposed 9 as the best number of clusters 
## * 1 proposed 10 as the best number of clusters 
## 
##                    ***** Conclusion *****                            
##  
## * According to the majority rule, the best number of clusters is  2 
##  
##  
## *******************************************************************
```

```r
# Perform kmeans clustering
numeric.kmeans.km <- kmeans(productattributes.numeric, 3, nstart = 25)
# Compute cophentic distance for complete linkage method
numeric.kmeans.km.coph <- cophenetic(numeric.kmeans.km$cluster)
```

```
## Error in as.hclust.default(x): argument 'x' cannot be coerced to class "hclust"character(0)
```

```r
# Compute the correlation between the cophenetic distance and original
# distance
cor(dis, numeric.kmeans.km.coph)
```

```
## Error in is.data.frame(y): object 'numeric.kmeans.km.coph' not found
```

```r
# Cluster sizes
table(numeric.complete.hc4)
```

```
## numeric.complete.hc4
##    1    2    3    4 
## 3724  245   18   13
```

```r
table(numeric.wards.hc3)
```

```
## numeric.wards.hc3
##    1    2    3 
## 3969   18   13
```

```r
table(numeric.kmeans.km$cluster)
```

```
## 
##    1    2    3 
##  251 2488 1261
```

```r
# Look at the groupings of the complete clusters
complete.cluster <- as.data.frame(cbind(productattributes.numeric2, numeric.complete.hc4))
colnames(complete.cluster)[7] <- "clusters"
group.complete <- compareGroups(clusters ~ ., data = complete.cluster)
clustab.complete = createTable(group.complete, digits = 3, show.p.overall = FALSE)
clustab.complete
```

```
## 
## --------Summary descriptives table by 'clusters'---------
## 
## ____________________________________________________________________________________________ 
##                             1                 2                 3                  4         
##                          N=3724             N=245              N=18              N=13        
## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯ 
## avg_price           135.134 (178.154) 123.868 (119.941) 2176.635 (712.286) 827.368 (333.279) 
## avg_freight_value    20.512 (14.613)   22.147 (14.091)   62.144 (33.046)   178.894 (43.716)  
## avg_review_score      4.055 (1.205)     4.219 (1.029)     4.222 (1.287)      4.465 (0.621)   
## avg_photo_qty         1.848 (1.171)     6.800 (1.989)     3.444 (2.357)      2.846 (2.075)   
## avg_payments          3.109 (2.585)     3.045 (2.567)     6.046 (3.948)      7.448 (2.791)   
## product_name_lenght  48.150 (10.251)   53.314 (7.930)     51.611 (4.717)    54.462 (5.882)   
## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
```

```r
# Look at the groupings of the wards clusters
wards.cluster <- as.data.frame(cbind(productattributes.numeric2, numeric.wards.hc3))
colnames(wards.cluster)[7] <- "clusters"
group.wards <- compareGroups(clusters ~ ., data = wards.cluster)
clustab.wards = createTable(group.wards, digits = 3, show.p.overall = FALSE)
clustab.wards
```

```
## 
## --------Summary descriptives table by 'clusters'---------
## 
## __________________________________________________________________________ 
##                             1                 2                  3         
##                          N=3969              N=18              N=13        
## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯ 
## avg_price           134.438 (175.131) 2176.635 (712.286) 827.368 (333.279) 
## avg_freight_value    20.613 (14.585)   62.144 (33.046)   178.894 (43.716)  
## avg_review_score      4.065 (1.195)     4.222 (1.287)      4.465 (0.621)   
## avg_photo_qty         2.154 (1.718)     3.444 (2.357)      2.846 (2.075)   
## avg_payments          3.105 (2.584)     6.046 (3.948)      7.448 (2.791)   
## product_name_lenght  48.468 (10.198)    51.611 (4.717)    54.462 (5.882)   
## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
```

```r
# Look at the groupings of the kmeans clusters
kmeans.cluster <- as.data.frame(cbind(productattributes.numeric2, numeric.kmeans.km$cluster))
colnames(kmeans.cluster)[7] <- "clusters"
group.kmeans <- compareGroups(clusters ~ ., data = kmeans.cluster)
clustab.kmeans = createTable(group.kmeans, digits = 3, show.p.overall = FALSE)
clustab.kmeans
```

```
## 
## --------Summary descriptives table by 'clusters'---------
## 
## ________________________________________________________________________ 
##                             1                 2                3         
##                           N=251            N=2488            N=1261      
## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯ 
## avg_price           729.706 (570.412) 110.361 (101.735) 99.751 (105.371) 
## avg_freight_value    64.825 (41.813)   18.667 (8.767)    17.876 (9.112)  
## avg_review_score      3.871 (1.392)     4.095 (1.154)    4.051 (1.227)   
## avg_photo_qty         2.566 (1.872)     2.356 (1.867)    1.699 (1.242)   
## avg_payments          6.938 (3.439)     3.008 (2.402)    2.619 (2.171)   
## product_name_lenght  48.052 (9.699)    54.606 (4.422)    36.548 (7.402)  
## ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
```


## Market Basket Analysis

The initial dataset that was transformed, comprised of the items, products and translations joined to the orders data. The next step to prepare the data was to subset the joined dataset to include the order id and product categories since the product id was not beneficial in the final analysis. The product categories were then factored and grouped by order ids which were separated by commas to create a transactional dataset. This dataset contained 99,442 transactions of 23 variables 13 of which were factor dimensions, 7 numeric dimensions, and 2 numeric dimensions. For this dataset I chose not to filter out the canceled orders because customers still took an interest in certain products and this might provide important insight for sellers if omitted.
Subsequently, the transactional dataset was read in and the top ten transactions were inspected. The first ten transactions revealed single item purchases. Another step that was taken to explore the transactional dataset was to view the most frequently purchased product categories. The observed transactions showed that bed/bath/table, health/beauty, sports/leisure, computers/accessories, and furniture/décor. This is interesting because a majority of these products are small items and are most likely easy to ship to customers.
The next step in the process of market basket analysis was to create apriori rules for the transactional dataset. The apriori indicated that there were 103 rules with parameters set to a minimum support of 1e-4 and a confidence of 1e-4. The top ten rules indicate that many of the rules are based on single item purchases. The top ten rules were not highly supported by the apriori and gave a small lift, providing minimal information of customer purchase behavior. Sorting the rules by lift gave insight to purchase relationships of items. I found that there were strong associations between customers that purchased home/comfort products also buying bed/bath/table products. Additionally, I observed associations between do-it-yourself improvement products, home and household, and personal enjoyment items. An interesting and particularly comical relationship that I observed was that baby products and products categorized as cool were purchased together. I think this might be a similar relationship that has been studied in other common cases where diapers and beer are purchased together. 
Based on my analysis of the rules for purchase transactions in the Brazilian E-Commerce dataset, I would recommend that the online sellers encourage customers to purchase multiple items by offering bundling packages. One of the bundles that I might suggest would be to start with some specific items from the bed/bath/table and home/comfort categories due to their association support. To test this marketing campaign, they should consider selling products individually and selling a package deal that contains combinations of the items. This will allow them to study the effects of a small marketing campaign by conducting A/B testing on the controlled group of individuals and the packaged deal. I would also suggest sellers to promote popular household items and electronics because they seem to be a hot seller in the Brazilian market. In addition, I would offer a markdown for buying multiple of the same products during a single transaction. For all these marketing approaches I would make sure that they were targeted towards a specific subset of customers based on location, one method of doing this might be to target advertising geographically.


```r
# Load libraries
library(readxl)
library(arules)
library(arulesViz)
library(plyr)
library(lubridate)
library(tidyverse)

# Read in data
orders <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_orders_dataset.csv")
items <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_items_dataset.csv")
products <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_products_dataset.csv")
translation <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/product_category_name_translation.csv")

# Rename column to be able to join on common name
names(translation) <- c("product_category_name", "product_category_name_english")

# Merge data based on data schema diagram
ordersitems = merge(x = orders, y = items, by = "order_id", all.x = TRUE)
ordersitems = merge(x = ordersitems, y = products, by = "product_id", all.x = TRUE)
ordersitems = merge(x = ordersitems, y = translation, by = "product_category_name", 
    all.x = TRUE)

# Subset the ordersitems dataset
oicols <- c("order_id", "product_category_name_english")
ecommerce <- ordersitems[oicols]

# Discretize the products as.factor(ecommerce$product_category_name_english)

# Create transaction list
ecommerce.item.list <- ddply(ecommerce, c("order_id"), function(df1) paste(df1$product_category_name_english, 
    collapse = ","))

# Remove columns because they are not needed
ecommerce.item.list$order_id <- NULL

# Name column of transactions 'items'
colnames(ecommerce.item.list) <- c("items")

# Write the csv of transactions
write.csv(ecommerce.item.list, "E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/Tests/market_basket.csv", 
    quote = FALSE, row.names = TRUE)

# Read in the transactions
ecommerce.transactions <- read.transactions("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/Tests/market_basket.csv", 
    format = "basket", sep = ",", rm.duplicates = TRUE)
```

```
## distribution of transactions with duplicates:
## items
##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   19 
## 7064 1226  474  196  198   19    6    5    7    3    5    1    2    2    2 
##   20 
##    1
```

```r
# Check out the dimensions of the data
inspect(ecommerce.transactions[1:10])
```

```
##      items              
## [1]  {items}            
## [2]  {1,cool_stuff}     
## [3]  {2,pet_shop}       
## [4]  {3,furniture_decor}
## [5]  {4,perfumery}      
## [6]  {5,garden_tools}   
## [7]  {6,housewares}     
## [8]  {7,telephony}      
## [9]  {8,garden_tools}   
## [10] {9,health_beauty}
```

```r
# Inspect the item frequency plot
itemFrequencyPlot(ecommerce.transactions, topN = 20, type = "absolute")
```

![](Final_files/figure-html/Market Basket Analysis-1.png)<!-- -->

```r
# Create apriori rules
ecommercerules <- apriori(ecommerce.transactions, parameter = list(support = 1e-04, 
    conf = 1e-04, target = "rules"))
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen
##       1e-04    0.1    1 none FALSE            TRUE       5   1e-04      1
##  maxlen target   ext
##      10  rules FALSE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 9 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[99513 item(s), 99442 transaction(s)] done [0.08s].
## sorting and recoding items ... [69 item(s)] done [0.00s].
## creating transaction tree ... done [0.01s].
## checking subsets of size 1 2 3 done [0.00s].
## writing ... [103 rule(s)] done [0.00s].
## creating S4 object  ... done [0.01s].
```

```r
# Check out the summary of the rules
summary(ecommercerules)
```

```
## set of 103 rules
## 
## rule length distribution (lhs + rhs):sizes
##  1  2 
## 69 34 
## 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    1.00    1.00    1.33    2.00    2.00 
## 
## summary of quality measures:
##     support            confidence             lift        
##  Min.   :0.0001006   Min.   :0.0001207   Min.   :0.01315  
##  1st Qu.:0.0001961   1st Qu.:0.0014164   1st Qu.:0.17917  
##  Median :0.0009754   Median :0.0027533   Median :1.00000  
##  Mean   :0.0096321   Mean   :0.0123627   Mean   :0.73210  
##  3rd Qu.:0.0050381   3rd Qu.:0.0076024   3rd Qu.:1.00000  
##  Max.   :0.0946984   Max.   :0.1083123   Max.   :1.14376  
##      count       
##  Min.   :  10.0  
##  1st Qu.:  19.5  
##  Median :  97.0  
##  Mean   : 957.8  
##  3rd Qu.: 501.0  
##  Max.   :9417.0  
## 
## mining info:
##                    data ntransactions support confidence
##  ecommerce.transactions         99442   1e-04      1e-04
```

```r
# Inspect the top 10 rules
inspect(ecommercerules[1:50])
```

```
##      lhs    rhs                                       support     
## [1]  {}  => {cds_dvds_musicals}                       0.0001206734
## [2]  {}  => {la_cuisine}                              0.0001307295
## [3]  {}  => {arts_and_craftmanship}                   0.0002312906
## [4]  {}  => {home_comfort_2}                          0.0002413467
## [5]  {}  => {diapers_and_hygiene}                     0.0002715151
## [6]  {}  => {fashion_sport}                           0.0002715151
## [7]  {}  => {flowers}                                 0.0002916273
## [8]  {}  => {furniture_mattress_and_upholstery}       0.0003821323
## [9]  {}  => {music}                                   0.0003821323
## [10] {}  => {party_supplies}                          0.0003921884
## [11] {}  => {fashio_female_clothing}                  0.0003921884
## [12] {}  => {books_imported}                          0.0005329740
## [13] {}  => {dvds_blu_ray}                            0.0005933107
## [14] {}  => {cine_photo}                              0.0006536474
## [15] {}  => {small_appliances_home_oven_and_coffee}   0.0007542085
## [16] {}  => {tablets_printing_image}                  0.0007944329
## [17] {}  => {furniture_bedroom}                       0.0009553307
## [18] {}  => {costruction_tools_tools}                 0.0009754430
## [19] {}  => {fashion_male_clothing}                   0.0011262847
## [20] {}  => {fashion_underwear_beach}                 0.0012167897
## [21] {}  => {christmas_supplies}                      0.0012871825
## [22] {}  => {signaling_and_security}                  0.0014078558
## [23] {}  => {construction_tools_safety}               0.0016793709
## [24] {}  => {computers}                               0.0018201565
## [25] {}  => {agro_industry_and_commerce}              0.0018302126
## [26] {}  => {costruction_tools_garden}                0.0019508859
## [27] {}  => {art}                                     0.0020313348
## [28] {}  => {fixed_telephony}                         0.0021821765
## [29] {}  => {food_drink}                              0.0022827377
## [30] {}  => {home_appliances_2}                       0.0023531305
## [31] {}  => {industry_commerce_and_business}          0.0023631866
## [32] {}  => {fashion_shoes}                           0.0024134671
## [33] {}  => {kitchen_dining_laundry_garden_furniture} 0.0024939161
## [34] {}  => {construction_tools_lights}               0.0024536916
## [35] {}  => {air_conditioning}                        0.0025441966
## [36] {}  => {books_technical}                         0.0026145894
## [37] {}  => {market_place}                            0.0028157117
## [38] {}  => {drinks}                                  0.0029866656
## [39] {}  => {audio}                                   0.0035196396
## [40] {}  => {home_confort}                            0.0039922769
## [41] {}  => {furniture_living_room}                   0.0042436797
## [42] {}  => {food}                                    0.0045252509
## [43] {}  => {home_construction}                       0.0049274954
## [44] {}  => {books_general_interest}                  0.0051487299
## [45] {}  => {musical_instruments}                     0.0063152390
## [46] {}  => {small_appliances}                        0.0063353513
## [47] {}  => {construction_tools_construction}         0.0075219726
## [48] {}  => {home_appliances}                         0.0076828704
## [49] {}  => {luggage_accessories}                     0.0103980210
## [50] {}  => {consoles_games}                          0.0106795921
##      confidence   lift count
## [1]  0.0001206734 1      12 
## [2]  0.0001307295 1      13 
## [3]  0.0002312906 1      23 
## [4]  0.0002413467 1      24 
## [5]  0.0002715151 1      27 
## [6]  0.0002715151 1      27 
## [7]  0.0002916273 1      29 
## [8]  0.0003821323 1      38 
## [9]  0.0003821323 1      38 
## [10] 0.0003921884 1      39 
## [11] 0.0003921884 1      39 
## [12] 0.0005329740 1      53 
## [13] 0.0005933107 1      59 
## [14] 0.0006536474 1      65 
## [15] 0.0007542085 1      75 
## [16] 0.0007944329 1      79 
## [17] 0.0009553307 1      95 
## [18] 0.0009754430 1      97 
## [19] 0.0011262847 1     112 
## [20] 0.0012167897 1     121 
## [21] 0.0012871825 1     128 
## [22] 0.0014078558 1     140 
## [23] 0.0016793709 1     167 
## [24] 0.0018201565 1     181 
## [25] 0.0018302126 1     182 
## [26] 0.0019508859 1     194 
## [27] 0.0020313348 1     202 
## [28] 0.0021821765 1     217 
## [29] 0.0022827377 1     227 
## [30] 0.0023531305 1     234 
## [31] 0.0023631866 1     235 
## [32] 0.0024134671 1     240 
## [33] 0.0024939161 1     248 
## [34] 0.0024536916 1     244 
## [35] 0.0025441966 1     253 
## [36] 0.0026145894 1     260 
## [37] 0.0028157117 1     280 
## [38] 0.0029866656 1     297 
## [39] 0.0035196396 1     350 
## [40] 0.0039922769 1     397 
## [41] 0.0042436797 1     422 
## [42] 0.0045252509 1     450 
## [43] 0.0049274954 1     490 
## [44] 0.0051487299 1     512 
## [45] 0.0063152390 1     628 
## [46] 0.0063353513 1     630 
## [47] 0.0075219726 1     748 
## [48] 0.0076828704 1     764 
## [49] 0.0103980210 1    1034 
## [50] 0.0106795921 1    1062
```

```r
# Force rules to be data frame
ecommercerules1 <- as(ecommercerules, "data.frame")

# Load packages
library(arulesViz)

# Rules
newRules <- ecommercerules[70:81]

## 2D matrix with shading
plot(newRules, method = "graph")
```

![](Final_files/figure-html/Market Basket Analysis-2.png)<!-- -->


## Geospatial Analysis

The dataset generated from the joins in the geospatial analysis resulted from a similar schema to the product cluster dataset. However, a reverse geocoding method was used to obtain address information from Google's public API and appended to the dataset of products sold. In addition, a shapefile of the regions in Brazil was imported and then merged with the product attributes dataset. The resulting dataset contained 34,218 observations with 6 product dimensions including product weight, average product review score, product description length, product photo quantity, average product payment installments, and product category. The dimensions were comprised of 5 numeric and one factor variable.  
	The first step in exploring the geospatial data was to inspect a couple of the dimensions by region. I chose to create a quantile choropleth plot of the revenue produced by products in the regions around Brazil. Interestingly, I found that most of the revenue was near the capital city Sao Paulo and the surrounding south-eastern regions.
After observing the amount of revenue clustered around one area I decided to compute the Local Moran's I and plot the results. From the Local Moran's I plot of Revenue by State there is significant autocorrelation in the south-eastern regions of Brazil.
Another plot that was created was to investigate the fluctuations of freight cost by region. This plot shows that the south-eastern regions have a relatively low cost to ship to, while the north-eastern regions indicate a relatively high freight cost. This might be due to the location of the distribution centers of sellers in the ecommerce market. Similarly to the Revenue by State plot, I also calculated the Local Moran I's values for the average seller freight costs. The choropleth of Local Moran's I values for Average Seller Freight Cost by State indicates that there are two significant clusters. Figure 16 shows that there is significant autocorrelation among freight cost values in the northeast and the southeast regions. These regions are respectively correlated to high average shipping costs and low average shipping costs.
To efficiently compute the geographically weighted regression I chose to pick a random sample of data that equated to around 25% of the dataset. The next step was to determine which of the dimensions would provide adequate information to be a useful predictor in the geographically weighted regression. I chose to create linear regression models to see their relationships with the revenue associated with products. Using the p-value I chose to include only significant dimensions that were greater that 1e-10. The resulting dimensions included product weight, product description length, average product payment installments, and product category.
Based on my geospatial analysis I would recommend that sellers strategically position their distribution centers to decrease the average cost of freight. The sellers could also align their product needs by region to ensure that they have enough stock in areas that they expand their distribution centers to. In addition, I think it would be beneficial to perform a cost-benefit analysis of adding another distribution center versus targeting sales efforts in the Sao Paulo region. It might also be interesting to explore dynamic shipping models to customers or offer discounted shipping on large orders with high priced transportation costs.


```r
# Read in libararies
library(plyr)
library(dplyr)
library(tidyr)
library(ggmap)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(GISTools)
library(moments)
library(GWmodel)
```

```
## Error: package or namespace load failed for 'GWmodel':
##  package slot missing from signature for generic 'coerce'
## and classes STFDF, RasterBrick
## cannot use with duplicate class names (the package may need to be re-installed)
```

```r
library(sp)
library(spgwr)

# Read in data
sellers <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_sellers_dataset.csv")
orders <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_orders_dataset.csv")
geolocation <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_geolocation_dataset.csv")
reviews <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_reviews_dataset.csv")
items <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_items_dataset.csv")
products <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_products_dataset.csv")
payments <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/olist_order_payments_dataset.csv")
translation <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/product_category_name_translation.csv")

# Get the average review score for each product
productreviews = orders %>% left_join(reviews, by = "order_id") %>% left_join(payments, 
    by = "order_id") %>% left_join(items, by = "order_id") %>% left_join(products, 
    by = "product_id") %>% filter(order_status != "canceled")

# Subset dataset to get product information
cols <- c("seller_id", "product_id", "price", "freight_value", "product_weight_g", 
    "product_photos_qty", "payment_installments", "review_score")
productreviews <- productreviews[cols]
productattributes <- productreviews %>% na.omit(product_id) %>% group_by(product_id, 
    seller_id) %>% summarise(avg_price = mean(price), product_revenue = sum(price), 
    avg_freight_value = mean(freight_value), total_freight_value = sum(freight_value), 
    avg_review_score = mean(review_score), avg_weight = mean(product_weight_g), 
    avg_photo_qty = mean(product_photos_qty), avg_payments = mean(payment_installments)) %>% 
    left_join(products, by = "product_id") %>% left_join(translation, by = c(product_category_name = "ï..product_category_name"))  #%>% select(-c('product_category_name'))
```

```
## Error: `by` can't contain join column `product_id` which is missing from LHS
```

```r
# Clean and pinpoint each zip code location geolocations <- geolocation %>%
# drop_na(geolocation_zip_code_prefix) %>%
# group_by(geolocation_zip_code_prefix) %>% summarise(geo_lng =
# mean(geolocation_lng), geo_lat = mean(geolocation_lat))

# Reverse geocode latitude and longitude coordinates
register_google(key = "AIzaSyA1jFDGVKkTAPzcOI8x4FHZF5dMwtbpx8Q")
# output <- cbind(geolocations, do.call(rbind, lapply(1:nrow(geolocations),
# function(i)revgeocode(as.numeric(geolocations[i,2:3]), output =
# 'address')[c('administrative_area_level_1','locality')])))

# Join addresses to zipcode prefixes and save to avoid running again
# geolocations <- cbind(geolocations, output) write.csv(geolocations,
# 'E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and
# Analytics/FInal Project/Tests/geolocations.csv')

# Read in geolocation data
geolocations <- read.csv("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/Tests/geolocations.csv")

# Rename seller states
sellers$seller_state <- revalue(sellers$seller_state, c(AC = "Acre", AM = "Amazonas", 
    BA = "Bahia", CE = "Ceará", DF = "Distrito Federal", ES = "Espírito Santo", 
    GO = "Goiás", MA = "Maranhão", MG = "Minas Gerais", MS = "Mato Grosso do Sul", 
    MT = "Mato Grosso", PA = "Pará", PB = "Paraíba", PE = "Pernambuco", PI = "Piauí", 
    PR = "Paraná", RJ = "Rio de Janeiro", RN = "Rio Grande do Norte", RO = "Rondônia", 
    RS = "Rio Grande do Sul", SC = "Santa Catarina", SE = "Sergipe", SP = "São Paulo"))

# Join the geolocation data to the product-seller attributes
final <- productattributes %>% left_join(sellers, by = "seller_id") %>% left_join(geolocations, 
    by = c(seller_zip_code_prefix = "geolocation_zip_code_prefix"))
```

```
## Error: `by` can't contain join column `seller_zip_code_prefix` which is missing from LHS
```

```r
# Open the shapefile and find coordinate reference system
brazilstates <- st_read("E:/Rockhurst University/WS19/B Term/MK 6460 Marketing Research and Analytics/FInal Project/Central-West Region_AL3-AL4.shp")
```

```
## Reading layer `Central-West Region_AL3-AL4' from data source `E:\Rockhurst University\WS19\B Term\MK 6460 Marketing Research and Analytics\FInal Project\Central-West Region_AL3-AL4.shp' using driver `ESRI Shapefile'
## Simple feature collection with 32 features and 14 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: -73.98306 ymin: -33.75107 xmax: -28.84761 ymax: 5.284287
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```r
st_crs(brazilstates)
```

```
## Coordinate Reference System:
##   EPSG: 4326 
##   proj4string: "+proj=longlat +datum=WGS84 +no_defs"
```

```r
# Merge data and shapefile
brazilstatesdata <- merge(brazilstates, final, by.x = "locname", by.y = "seller_state")
```

```
## Error in merge(brazilstates, final, by.x = "locname", by.y = "seller_state"): object 'final' not found
```

```r
st_crs(brazilstatesdata)
```

```
## Error in st_crs(brazilstatesdata): object 'brazilstatesdata' not found
```

```r
# Simplify data to get revenue by state
cols2 <- c("seller_state", "product_revenue")
revenue3 <- final[cols2]
```

```
## Error in eval(expr, envir, enclos): object 'final' not found
```

```r
revenue3 <- aggregate(revenue3$product_revenue, by = list(seller_state = revenue3$seller_state), 
    FUN = sum)
```

```
## Error in aggregate(revenue3$product_revenue, by = list(seller_state = revenue3$seller_state), : object 'revenue3' not found
```

```r
names(revenue3) <- c("seller_state", "Revenue")
```

```
## Error in names(revenue3) <- c("seller_state", "Revenue"): object 'revenue3' not found
```

```r
# Plot the Revenue shapefile boundaries
revenuemap <- append_data(brazilstates, revenue3, key.shp = "locname", key.data = "seller_state")
```

```
## Error in key.data %in% names(data): object 'revenue3' not found
```

```r
tm_shape(revenuemap) + tm_polygons("Revenue", id = "seller_state", style = "quantile")
```

```
## Error in as.list.environment(environment()): object 'revenuemap' not found
```

```r
# GEOSPATIAL CLUSTER ANALYSIS FOR REVENUE
revenue.indicator <- revenue3$Revenue
```

```
## Error in eval(expr, envir, enclos): object 'revenue3' not found
```

```r
data.sp <- merge(brazilstates, revenue3, by.x = "locname", by.y = "seller_state")
```

```
## Error in merge(brazilstates, revenue3, by.x = "locname", by.y = "seller_state"): object 'revenue3' not found
```

```r
data.sp <- as_Spatial(data.sp)
```

```
## Error in as_Spatial(data.sp): object 'data.sp' not found
```

```r
data.sp.revenue <- data.sp$Revenue
```

```
## Error in eval(expr, envir, enclos): object 'data.sp' not found
```

```r
data.sp.listw <- nb2listw(poly2nb(data.sp))
```

```
## Error in extends(class(pl), "SpatialPolygons"): object 'data.sp' not found
```

```r
# Local Moran's I for Revenue
r.li <- localmoran(data.sp.revenue, data.sp.listw)
```

```
## Error in is.vector(x): object 'data.sp.revenue' not found
```

```r
# Compute a shading scheme
sleep.shade <- auto.shading(c(r.li[, 1], -r.li[, 1]), cols = brewer.pal(5, "PRGn"))
```

```
## Error in quantile(x, probs): object 'r.li' not found
```

```r
# Plot the Local Moran's I Revenue values
par(mar = c(0, 0, 0, 0))
choropleth(data.sp, r.li[, 1], shading = sleep.shade)
```

```
## Error in choropleth(data.sp, r.li[, 1], shading = sleep.shade): object 'sleep.shade' not found
```

```r
choro.legend(-83.77, 37.87, sleep.shade, fmt = "%6.2f", title = "Local Moran's I")
```

```
## Error in choro.legend(-83.77, 37.87, sleep.shade, fmt = "%6.2f", title = "Local Moran's I"): object 'sleep.shade' not found
```

```r
title("\r\nRevenue by State (Local Moran's I)", cex.main = 1)
```

```
## Error in title("\r\nRevenue by State (Local Moran's I)", cex.main = 1): plot.new has not been called yet
```

```r
# Simplify data to get average freight value by state
cols3 <- c("seller_state", "avg_freight_value")
avgfreight <- final[cols3]
```

```
## Error in eval(expr, envir, enclos): object 'final' not found
```

```r
avgfreight <- aggregate(avgfreight$avg_freight_value, by = list(seller_state = avgfreight$seller_state), 
    FUN = mean)
```

```
## Error in aggregate(avgfreight$avg_freight_value, by = list(seller_state = avgfreight$seller_state), : object 'avgfreight' not found
```

```r
names(avgfreight) <- c("seller_state", "Average Freight Value")
```

```
## Error in names(avgfreight) <- c("seller_state", "Average Freight Value"): object 'avgfreight' not found
```

```r
# Plot the shapefile boundaries
freightmap <- append_data(brazilstates, avgfreight, key.shp = "locname", key.data = "seller_state")
```

```
## Error in key.data %in% names(data): object 'avgfreight' not found
```

```r
tmap_mode("view")
tm_shape(freightmap) + tm_polygons("Average Freight Value", id = "NAME", palette = "-RdYlBu", 
    legend.title = "Average Freight Value")
```

```
## Error in as.list.environment(environment()): object 'freightmap' not found
```

```r
# GEOSPATIAL CLUSTER ANALYSIS FOR FREIGHT VALUE
freight.indicator <- avgfreight$`Average Freight Value`
```

```
## Error in eval(expr, envir, enclos): object 'avgfreight' not found
```

```r
freight.sp <- merge(brazilstates, avgfreight, by.x = "locname", by.y = "seller_state")
```

```
## Error in merge(brazilstates, avgfreight, by.x = "locname", by.y = "seller_state"): object 'avgfreight' not found
```

```r
freight.sp <- as_Spatial(freight.sp)
```

```
## Error in as_Spatial(freight.sp): object 'freight.sp' not found
```

```r
freight.sp.value <- freight.sp$Average.Freight.Value
```

```
## Error in eval(expr, envir, enclos): object 'freight.sp' not found
```

```r
freight.sp.listw <- nb2listw(poly2nb(freight.sp))
```

```
## Error in extends(class(pl), "SpatialPolygons"): object 'freight.sp' not found
```

```r
# Local Moran's I Freight Values
f.li <- localmoran(freight.sp.value, freight.sp.listw)
```

```
## Error in is.vector(x): object 'freight.sp.value' not found
```

```r
# Compute a shading scheme
sleep.shade <- auto.shading(c(f.li[, 1], -f.li[, 1]), cols = brewer.pal(5, "PRGn"))
```

```
## Error in quantile(x, probs): object 'f.li' not found
```

```r
# Plot the local moran's I values
par(mar = c(0, 0, 0, 0))
choropleth(freight.sp, f.li[, 1], shading = sleep.shade)
```

```
## Error in choropleth(freight.sp, f.li[, 1], shading = sleep.shade): object 'sleep.shade' not found
```

```r
choro.legend(-83.77, 37.87, sleep.shade, fmt = "%6.2f", title = "Local Moran's I")
```

```
## Error in choro.legend(-83.77, 37.87, sleep.shade, fmt = "%6.2f", title = "Local Moran's I"): object 'sleep.shade' not found
```

```r
title("\r\nFreight Value by State (Local Moran's I)", cex.main = 1)
```

```
## Error in title("\r\nFreight Value by State (Local Moran's I)", cex.main = 1): plot.new has not been called yet
```

```r
# Geographically weighted regression Set a random seed start
set.seed(123)

# Define sampling size of house data
sampleSize <- 8555

# Gather the random sampling of house data
brazilsatessample <- brazilstatesdata[sample(nrow(brazilstatesdata), sampleSize, 
    replace = FALSE), ]
```

```
## Error in eval(expr, envir, enclos): object 'brazilstatesdata' not found
```

```r
# Determine good predictors of revenue
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$avg_weight))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$avg_review_score))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$product_description_lenght))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$avg_photo_qty))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$avg_payments))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$product_category_name_english))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
summary(lm(brazilsatessample$product_revenue ~ brazilsatessample$avg_price))
```

```
## Error in eval(predvars, data, env): object 'brazilsatessample' not found
```

```r
# Set a random seed start
set.seed(123)

# Define sampling size of house data
sampleSize <- 500

# Gather the random sampling of house data
finalsample <- final[sample(nrow(final), sampleSize, replace = FALSE), ]
```

```
## Error in eval(expr, envir, enclos): object 'final' not found
```

```r
finalsample <- finalsample %>% drop_na(geo_lat) %>% drop_na(geo_lng)
```

```
## Error in eval(lhs, parent, parent): object 'finalsample' not found
```

```r
# Convert the SpatialPointsDataFrame to a SpatialPoints object.
# finalsample.points <- as(finalsample,'SpatialPoints')

# First make spatial points and set the projection (more on that below)
spoints <- SpatialPoints(coords = finalsample[, c("geo_lng", "geo_lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
```

```
## Error in coordinates(coords): object 'finalsample' not found
```

```r
# Then make a SpatialPointsDataFrame by addint the data frame back in
finalsampledf <- SpatialPointsDataFrame(spoints, finalsample)
```

```
## Error in is(coords, "SpatialPoints"): object 'spoints' not found
```

```r
# Determine optimal bandwidth
optimal.band <- gwr.sel(finalsampledf$product_revenue ~ finalsampledf$avg_weight + 
    finalsampledf$product_description_lenght + finalsampledf$avg_payments, data = finalsampledf, 
    coords = cbind(finalsample$geo_lng, finalsample$geo_lat), longlat = TRUE, 
    adapt = T)
```

```
## Error in is(data, "Spatial"): object 'finalsampledf' not found
```

```r
optimal.band
```

```
## Error in eval(expr, envir, enclos): object 'optimal.band' not found
```

```r
# Predict geographically weighted regression with most significant
# predictors using a bandwidth of 10
gwr.res10 <- gwr.basic(finalsampledf@data$product_revenue ~ finalsampledf@data$avg_weight + 
    finalsampledf@data$product_description_lenght + finalsampledf@data$avg_payments, 
    data = finalsampledf, bw = optimal.band, kernel = "gaussian")
```

```
## Error in gwr.basic(finalsampledf@data$product_revenue ~ finalsampledf@data$avg_weight + : could not find function "gwr.basic"
```

```r
gwr.res10
```

```
## Error in eval(expr, envir, enclos): object 'gwr.res10' not found
```
