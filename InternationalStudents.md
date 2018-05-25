106-2 大數據分析方法 作業二
================
Yi-Ju Tseng

作業完整說明[連結](https://docs.google.com/document/d/1aLGSsGXhgOVgwzSg9JdaNz2qGPQJSoupDAQownkGf_I/edit?usp=sharing)

學習再也不限定在自己出生的國家，台灣每年有許多學生選擇就讀國外的大專院校，同時也有人多國外的學生來台灣就讀，透過分析[大專校院境外學生人數統計(我國大學有多少境外生)](https://data.gov.tw/dataset/6289)、[大專校院本國學生出國進修交流數](https://data.gov.tw/dataset/24730)、[世界各主要國家之我國留學生人數統計表](https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv)可以了解103年以後各大專院校國際交流的情形。請同學分析以下議題，並以視覺化的方式呈現分析結果，呈現103年以後大專院校國際交流的情形。

來台境外生分析
--------------

### 資料匯入與處理

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.4.4

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.4

``` r
library(choroplethrMaps)
```

    ## Warning: package 'choroplethrMaps' was built under R version 3.4.4

``` r
library(choroplethr)
```

    ## Warning: package 'choroplethr' was built under R version 3.4.4

    ## Loading required package: acs

    ## Warning: package 'acs' was built under R version 3.4.4

    ## Loading required package: stringr

    ## Warning: package 'stringr' was built under R version 3.4.4

    ## Loading required package: XML

    ## 
    ## Attaching package: 'acs'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:base':
    ## 
    ##     apply

``` r
library(ggmap)
```

    ## Warning: package 'ggmap' was built under R version 3.4.4

``` r
library(maps)
```

    ## Warning: package 'maps' was built under R version 3.4.4

``` r
library(rgdal)
```

    ## Warning: package 'rgdal' was built under R version 3.4.4

    ## Loading required package: sp

    ## Warning: package 'sp' was built under R version 3.4.4

    ## rgdal: version: 1.2-20, (SVN revision 725)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.2.3, released 2017/11/20
    ##  Path to GDAL shared files: C:/Users/hh770/Documents/R/R-3.4.3/library/sf/gdal
    ##  GDAL binary built with GEOS: TRUE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: C:/Users/hh770/Documents/R/R-3.4.3/library/sf/proj
    ##  Linking to sp version: 1.2-7

``` r
library(rgeos) 
```

    ## Warning: package 'rgeos' was built under R version 3.4.4

    ## rgeos version: 0.3-26, (SVN revision 560)
    ##  GEOS runtime version: 3.6.1-CAPI-1.10.1 r0 
    ##  Linking to sp version: 1.2-7 
    ##  Polygon checking: TRUE

``` r
library(maptools)
```

    ## Warning: package 'maptools' was built under R version 3.4.4

    ## Checking rgeos availability: TRUE

``` r
library(plotly)
```

    ## Warning: package 'plotly' was built under R version 3.4.4

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggmap':
    ## 
    ##     wind

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.4.4

``` r
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 3.4.4

``` r
#匯入境外生來台資料(國別)(第一題)
Data103<- read_csv("C:/Users/hh770/Downloads/25f64d5125016dcd6aed42e50c972ed0_export.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   洲別 = col_character(),
    ##   國別 = col_character(),
    ##   `學位生-正式修讀學位外國生` = col_integer(),
    ##   `學位生-僑生(含港澳)` = col_integer(),
    ##   `學位生-正式修讀學位陸生` = col_integer(),
    ##   `非學位生-外國交換生` = col_integer(),
    ##   `非學位生-外國短期研習及個人選讀` = col_integer(),
    ##   `非學位生-大專附設華語文中心學生` = col_integer(),
    ##   `非學位生-大陸研修生` = col_integer(),
    ##   `非學位生-海青班` = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
Data104<- read_csv("C:/Users/hh770/Downloads/104_ab104_C.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   洲別 = col_character(),
    ##   國別 = col_character(),
    ##   `學位生-正式修讀學位外國生` = col_integer(),
    ##   `學位生-僑生(含港澳)` = col_integer(),
    ##   `學位生-正式修讀學位陸生` = col_integer(),
    ##   `非學位生-外國交換生` = col_integer(),
    ##   `非學位生-外國短期研習及個人選讀` = col_integer(),
    ##   `非學位生-大專附設華語文中心學生` = col_integer(),
    ##   `非學位生-大陸研修生` = col_integer(),
    ##   `非學位生-海青班` = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
Data105<- read_csv("C:/Users/hh770/Downloads/105_ab105_C.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   洲別 = col_character(),
    ##   國別 = col_character(),
    ##   學位生_正式修讀學位外國生 = col_integer(),
    ##   `學位生_僑生(含港澳)` = col_integer(),
    ##   學位生_正式修讀學位陸生 = col_integer(),
    ##   非學位生_外國交換生 = col_integer(),
    ##   非學位生_外國短期研習及個人選讀 = col_integer(),
    ##   非學位生_大專附設華語文中心學生 = col_integer(),
    ##   非學位生_大陸研修生 = col_integer(),
    ##   非學位生_海青班 = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
Data106<- read_csv("C:/Users/hh770/Downloads/106_ab105_C.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   洲別 = col_character(),
    ##   國別 = col_character(),
    ##   學位生_正式修讀學位外國生 = col_integer(),
    ##   `學位生_僑生(含港澳)` = col_integer(),
    ##   學位生_正式修讀學位陸生 = col_integer(),
    ##   非學位生_外國交換生 = col_integer(),
    ##   非學位生_外國短期研習及個人選讀 = col_integer(),
    ##   非學位生_大專附設華語文中心學生 = col_integer(),
    ##   非學位生_大陸研修生 = col_integer(),
    ##   非學位生_海青班 = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
#匯入境外生來台資料(校別)(第一題)
Data103a<-read_csv("C:/Users/hh770/Downloads/103_ab103_S.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   學校類型 = col_character(),
    ##   學校代碼 = col_character(),
    ##   學校名稱 = col_character(),
    ##   `學位生-正式修讀學位外國生` = col_integer(),
    ##   `學位生-僑生(含港澳)` = col_integer(),
    ##   `學位生-正式修讀學位陸生` = col_integer(),
    ##   `非學位生-外國交換生` = col_integer(),
    ##   `非學位生-外國短期研習及個人選讀` = col_integer(),
    ##   `非學位生-大專附設華語文中心學生` = col_integer(),
    ##   `非學位生-大陸研修生` = col_character(),
    ##   `非學位生-海青班` = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
Data104a<-read_csv("C:/Users/hh770/Downloads/104_ab104_S.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   學校類型 = col_character(),
    ##   學校代碼 = col_character(),
    ##   學校名稱 = col_character(),
    ##   `學位生-正式修讀學位外國生` = col_integer(),
    ##   `學位生-僑生(含港澳)` = col_integer(),
    ##   `學位生-正式修讀學位陸生` = col_integer(),
    ##   `非學位生-外國交換生` = col_integer(),
    ##   `非學位生-外國短期研習及個人選讀` = col_integer(),
    ##   `非學位生-大專附設華語文中心學生` = col_integer(),
    ##   `非學位生-大陸研修生` = col_character(),
    ##   `非學位生-海青班` = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
Data105a<-read_csv("C:/Users/hh770/Downloads/105_ab105_S.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   學校類型 = col_character(),
    ##   學校代碼 = col_character(),
    ##   學校名稱 = col_character(),
    ##   學位生_正式修讀學位外國生 = col_integer(),
    ##   `學位生_僑生(含港澳)` = col_integer(),
    ##   學位生_正式修讀學位陸生 = col_integer(),
    ##   非學位生_外國交換生 = col_integer(),
    ##   非學位生_外國短期研習及個人選讀 = col_integer(),
    ##   非學位生_大專附設華語文中心學生 = col_integer(),
    ##   非學位生_大陸研修生 = col_integer(),
    ##   非學位生_海青班 = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
Data106a<-read_csv("C:/Users/hh770/Downloads/106_ab105_S.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   學校類型 = col_character(),
    ##   學校代碼 = col_character(),
    ##   學校名稱 = col_character(),
    ##   學位生_正式修讀學位外國生 = col_integer(),
    ##   `學位生_僑生(含港澳)` = col_integer(),
    ##   學位生_正式修讀學位陸生 = col_integer(),
    ##   非學位生_外國交換生 = col_integer(),
    ##   非學位生_外國短期研習及個人選讀 = col_integer(),
    ##   非學位生_大專附設華語文中心學生 = col_integer(),
    ##   非學位生_大陸研修生 = col_integer(),
    ##   非學位生_海青班 = col_integer(),
    ##   境外專班 = col_integer()
    ## )

``` r
#匯入大專校院本國學生出國進修交流數(第四題)
Student_RPT<- read.csv("C:/Users/hh770/Downloads/Student_RPT_07 .csv")

#匯入世界各主要國家之我國留學生人數統計表(第七題)
StudyAbord105<- read_csv("C:/Users/hh770/Downloads/105abc .csv")
```

    ## Warning: Missing column names filled in: 'X4' [4], 'X5' [5], 'X6' [6]

    ## Parsed with column specification:
    ## cols(
    ##   洲別 = col_character(),
    ##   國別 = col_character(),
    ##   總人數 = col_number(),
    ##   X4 = col_character(),
    ##   X5 = col_character(),
    ##   X6 = col_character()
    ## )

``` r
StudyAbord105<-StudyAbord105[,-c(4,5,6)]
```

### 哪些國家來台灣唸書的學生最多呢？

``` r
#這是R Code Chunk

#(第一題第一小題)哪些國家來台灣唸書的學生最多呢？請取出前十名的國家與總人數，由大到小排序
#加總各國境外生總人數(第一題)
Data103$ForeignStudentSum103<-0
for (n in 1:149) {
  Data103[n,12]<-sum(Data103[n,3:11])
}
Data104$ForeignStudentSum104<-0
for (n in 1:159) {
  Data104[n,12]<-sum(Data104[n,3:11])
}
Data105$ForeignStudentSum105<-0
for (n in 1:165) {
  Data105[n,12]<-sum(Data105[n,3:11])
}
Data106$ForeignStudentSum106<-0
for (n in 1:167) {
  Data106[n,12]<-sum(Data106[n,3:11])
}
#以國家為基準結合四個年度的境外生人數(第一題)
CountryBaseSum103<-Data103[,c("國別","ForeignStudentSum103")]
CountryBaseSum104<-Data104[,c("國別","ForeignStudentSum104")]
CountryBaseSum105<-Data105[,c("國別","ForeignStudentSum105")]
CountryBaseSum106<-Data106[,c("國別","ForeignStudentSum106")]
CountryBaseSum<-merge(CountryBaseSum103,CountryBaseSum104,by="國別",all=T)
CountryBaseSum<-merge(CountryBaseSum,CountryBaseSum105,by="國別",all=T)  
CountryBaseSum<-merge(CountryBaseSum,CountryBaseSum106,by="國別",all=T)
CountryBaseSum$CountrySum<-0
for (n in 1:177) {
  CountryBaseSum[n,6]<-sum(CountryBaseSum[n,2:5],na.rm = T)#加總四個年度資料
}

head(arrange(CountryBaseSum,desc(CountrySum)),10)
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

    ##        國別 ForeignStudentSum103 ForeignStudentSum104 ForeignStudentSum105
    ## 1  中國大陸                33288                41951                41981
    ## 2  馬來西亞                13385                15054                16311
    ## 3      香港                 6286                 8233                 8660
    ## 4      日本                 5816                 6455                 7542
    ## 5      越南                 4005                 4459                 5342
    ## 6      澳門                 4723                 5152                 5286
    ## 7      印尼                 3559                 4454                 5154
    ## 8      南韓                 3587                 4062                 4575
    ## 9      美國                 3328                 4003                 3701
    ## 10     泰國                 1535                 1591                 1771
    ##    ForeignStudentSum106 CountrySum
    ## 1                 35304     152524
    ## 2                 17281      62031
    ## 3                  8761      31940
    ## 4                  8387      28200
    ## 5                  7864      21670
    ## 6                  5141      20302
    ## 7                  6453      19620
    ## 8                  4724      16948
    ## 9                  3814      14846
    ## 10                 2138       7035

``` r
knitr::kable(head(arrange(CountryBaseSum,desc(CountrySum)),10))
```

| 國別     |  ForeignStudentSum103|  ForeignStudentSum104|  ForeignStudentSum105|  ForeignStudentSum106|  CountrySum|
|:---------|---------------------:|---------------------:|---------------------:|---------------------:|-----------:|
| 中國大陸 |                 33288|                 41951|                 41981|                 35304|      152524|
| 馬來西亞 |                 13385|                 15054|                 16311|                 17281|       62031|
| 香港     |                  6286|                  8233|                  8660|                  8761|       31940|
| 日本     |                  5816|                  6455|                  7542|                  8387|       28200|
| 越南     |                  4005|                  4459|                  5342|                  7864|       21670|
| 澳門     |                  4723|                  5152|                  5286|                  5141|       20302|
| 印尼     |                  3559|                  4454|                  5154|                  6453|       19620|
| 南韓     |                  3587|                  4062|                  4575|                  4724|       16948|
| 美國     |                  3328|                  4003|                  3701|                  3814|       14846|
| 泰國     |                  1535|                  1591|                  1771|                  2138|        7035|

### 哪間大學的境外生最多呢？

``` r
#這是R Code Chunk
Data103a$`非學位生-大陸研修生`<-gsub("…",NA,Data103a$`非學位生-大陸研修生`)
Data104a$`非學位生-大陸研修生`<-gsub("…",NA,Data104a$`非學位生-大陸研修生`)
Data103a$SchoolForeignStudentSum103<-0
for (n in 1:153) {
  Data103a[n,13]<-sum(Data103a[n,4:9],na.rm = T)
}
Data104a$SchoolForeignStudentSum104<-0
Data104a$`非學位生-大陸研修生`<-as.numeric(Data104a$`非學位生-大陸研修生`)
for (n in 1:154) {
  Data104a[n,13]<-sum(Data104a[n,4:10],na.rm = T)
}
Data105a$SchoolForeignStudentSum105<-0
for (n in 1:154) {
  Data105a[n,13]<-sum(Data105a[n,4:10],na.rm = T)
} 
Data106a$SchoolForeignStudentSum106<-0
for (n in 1:154) {
  Data106a[n,13]<-sum(Data106a[n,4:10],na.rm = T)
}

#以學校為基準結合四個年度的境外生人數(第一題)
SchoolBaseSum103<-Data103a[,c("學校名稱","SchoolForeignStudentSum103")]
SchoolBaseSum104<-Data104a[,c("學校名稱","SchoolForeignStudentSum104")]
SchoolBaseSum105<-Data105a[,c("學校名稱","SchoolForeignStudentSum105")]
SchoolBaseSum106<-Data106a[,c("學校名稱","SchoolForeignStudentSum106")]
SchoolBaseSum<-merge(SchoolBaseSum103,SchoolBaseSum104,by="學校名稱",all=T)
SchoolBaseSum<-merge(SchoolBaseSum,SchoolBaseSum105,by="學校名稱",all=T)
SchoolBaseSum<-merge(SchoolBaseSum,SchoolBaseSum106,by="學校名稱",all=T)
SchoolBaseSum$SchoolSum<-0
for(n in 1:177){
  SchoolBaseSum[n,6]<-sum(SchoolBaseSum[n,2:5],na.rm = T)#加總四個年度資料
}

head(arrange(SchoolBaseSum,desc(SchoolSum)),10)
```

    ##            學校名稱 SchoolForeignStudentSum103 SchoolForeignStudentSum104
    ## 1      無法區分校別                         NA                      34114
    ## 2  國立臺灣師範大學                       4648                       5328
    ## 3      國立臺灣大學                       3800                       4514
    ## 4      中國文化大學                       3878                       3958
    ## 5          銘傳大學                       3026                       3682
    ## 6          淡江大學                       2986                       3254
    ## 7      國立政治大學                       2705                       2871
    ## 8      國立成功大學                       2385                       2554
    ## 9          輔仁大學                       2276                       2271
    ## 10         逢甲大學                       1679                       2154
    ##    SchoolForeignStudentSum105 SchoolForeignStudentSum106 SchoolSum
    ## 1                       32648                      25824     92586
    ## 2                        5709                       6428     22113
    ## 3                        4817                       5068     18199
    ## 4                        3820                       3479     15135
    ## 5                        3723                       3795     14226
    ## 6                        3727                       3920     13887
    ## 7                        2989                       3061     11626
    ## 8                        2864                       3179     10982
    ## 9                        2518                       2434      9499
    ## 10                       2148                       2244      8225

``` r
knitr::kable(head(arrange(SchoolBaseSum,desc(SchoolSum)),10))
```

| 學校名稱         |  SchoolForeignStudentSum103|  SchoolForeignStudentSum104|  SchoolForeignStudentSum105|  SchoolForeignStudentSum106|  SchoolSum|
|:-----------------|---------------------------:|---------------------------:|---------------------------:|---------------------------:|----------:|
| 無法區分校別     |                          NA|                       34114|                       32648|                       25824|      92586|
| 國立臺灣師範大學 |                        4648|                        5328|                        5709|                        6428|      22113|
| 國立臺灣大學     |                        3800|                        4514|                        4817|                        5068|      18199|
| 中國文化大學     |                        3878|                        3958|                        3820|                        3479|      15135|
| 銘傳大學         |                        3026|                        3682|                        3723|                        3795|      14226|
| 淡江大學         |                        2986|                        3254|                        3727|                        3920|      13887|
| 國立政治大學     |                        2705|                        2871|                        2989|                        3061|      11626|
| 國立成功大學     |                        2385|                        2554|                        2864|                        3179|      10982|
| 輔仁大學         |                        2276|                        2271|                        2518|                        2434|       9499|
| 逢甲大學         |                        1679|                        2154|                        2148|                        2244|       8225|

### 各個國家來台灣唸書的學生人數條狀圖

``` r
#這是R Code Chunk
#(第二題)用bar chart呈現各個國家(全部)來台灣唸書的學生人數
ggplot(CountryBaseSum,
       aes(x=國別,y=CountrySum))+
  geom_bar(stat = "identity")+
  labs(x="國家",y="總計",title="各個國家來台灣念書的學生人數")+
  theme(axis.text.x = element_text(size = 6,angle = 90))+
  theme(panel.border = element_blank())
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryBar-1.png)

### 各個國家來台灣唸書的學生人數面量圖

``` r
#這是R Code Chunk
```

台灣學生國際交流分析
--------------------

### 資料匯入與處理

``` r
#這是R Code Chunk
```

### 台灣大專院校的學生最喜歡去哪些國家進修交流呢？

``` r
#這是R Code Chunk
#(第四題第一小題)大專院校的學生最喜歡去哪些國家交流？請取出前十名的國家與總人數，由大到小排序
PRTCountrySum<-group_by(Student_RPT,對方學校.機構.國別.地區.)%>%
  summarise(countrycount=sum(小計))
head(arrange(PRTCountrySum,desc(countrycount)),10)
```

    ## # A tibble: 10 x 2
    ##    對方學校.機構.國別.地區. countrycount
    ##    <fct>                           <int>
    ##  1 日本                            12430
    ##  2 中國大陸                        10429
    ##  3 美國                             8916
    ##  4 大陸地區                         5996
    ##  5 南韓                             2498
    ##  6 法國                             2415
    ##  7 大韓民國(南韓)                   2131
    ##  8 德國                             1706
    ##  9 德意志聯邦共和國                 1458
    ## 10 英國                             1416

``` r
knitr::kable(head(arrange(PRTCountrySum,desc(countrycount)),10))
```

| 對方學校.機構.國別.地區. |  countrycount|
|:-------------------------|-------------:|
| 日本                     |         12430|
| 中國大陸                 |         10429|
| 美國                     |          8916|
| 大陸地區                 |          5996|
| 南韓                     |          2498|
| 法國                     |          2415|
| 大韓民國(南韓)           |          2131|
| 德國                     |          1706|
| 德意志聯邦共和國         |          1458|
| 英國                     |          1416|

### 哪間大學的出國交流學生數最多呢？

``` r
#這是R Code Chunk
PRTSchoolSum<-group_by(Student_RPT,學校名稱)%>%
  summarise(schoolcount=sum(小計))
head(arrange(PRTSchoolSum,desc(schoolcount)),10)
```

    ## # A tibble: 10 x 2
    ##    學校名稱     schoolcount
    ##    <fct>              <int>
    ##  1 國立臺灣大學        4719
    ##  2 淡江大學            3794
    ##  3 國立政治大學        3479
    ##  4 逢甲大學            2646
    ##  5 東海大學            1881
    ##  6 元智大學            1864
    ##  7 國立交通大學        1513
    ##  8 東吳大學            1457
    ##  9 國立成功大學        1397
    ## 10 國立臺北大學        1397

``` r
knitr::kable(head(arrange(PRTSchoolSum,desc(schoolcount)),10))
```

| 學校名稱     |  schoolcount|
|:-------------|------------:|
| 國立臺灣大學 |         4719|
| 淡江大學     |         3794|
| 國立政治大學 |         3479|
| 逢甲大學     |         2646|
| 東海大學     |         1881|
| 元智大學     |         1864|
| 國立交通大學 |         1513|
| 東吳大學     |         1457|
| 國立成功大學 |         1397|
| 國立臺北大學 |         1397|

### 台灣大專院校的學生最喜歡去哪些國家進修交流條狀圖

``` r
#這是R Code Chunk
#(第五題)請用bar chart呈現台灣大專院校(全部)的學生去各國家進修交流人數
ggplot(PRTSchoolSum,
       aes(x=學校名稱,y=schoolcount))+
  geom_bar(stat = "identity")+
  labs(x="學校名稱",y="總計",title="台灣大專院校的學生去各國家進修交流人數")+
  theme(axis.text.x = element_text(size = 6,angle = 90))+
  theme(panel.border = element_blank())
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryBar-1.png)

### 台灣大專院校的學生最喜歡去哪些國家進修交流面量圖

``` r
#這是R Code Chunk
```

台灣學生出國留學分析
--------------------

### 資料匯入與處理

``` r
#這是R Code Chunk
```

### 台灣學生最喜歡去哪些國家留學呢？

``` r
#這是R Code Chunk

#(第七題)台灣學生最喜歡去哪些國家留學呢？請取出前十名的國家與總人數，由大到小排序
head(arrange(StudyAbord105,desc(總人數)),10)
```

    ## # A tibble: 10 x 3
    ##    洲別   國別     總人數
    ##    <chr>  <chr>     <dbl>
    ##  1 美洲   美國     21127.
    ##  2 大洋洲 澳大利亞 13582.
    ##  3 亞洲   日本      8444.
    ##  4 美洲   加拿大    4827.
    ##  5 歐洲   英國      3815.
    ##  6 歐洲   德國      1488.
    ##  7 大洋洲 紐西蘭    1106.
    ##  8 歐洲   波蘭       561.
    ##  9 亞洲   馬來西亞   502.
    ## 10 歐洲   奧地利     419.

``` r
knitr::kable(head(arrange(StudyAbord105,desc(總人數)),10))
```

| 洲別   | 國別     | 總人數 |
|:-------|:---------|:------:|
| 美洲   | 美國     |  21127 |
| 大洋洲 | 澳大利亞 |  13582 |
| 亞洲   | 日本     |  8444  |
| 美洲   | 加拿大   |  4827  |
| 歐洲   | 英國     |  3815  |
| 歐洲   | 德國     |  1488  |
| 大洋洲 | 紐西蘭   |  1106  |
| 歐洲   | 波蘭     |   561  |
| 亞洲   | 馬來西亞 |   502  |
| 歐洲   | 奧地利   |   419  |

### 台灣學生最喜歡去哪些國家留學面量圖

``` r
#這是R Code Chunk
```

綜合分析
--------

請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。
