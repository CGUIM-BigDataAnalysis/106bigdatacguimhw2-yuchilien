library(readr)
library(dplyr)
library(ggplot2)
library(choroplethrMaps)
library(choroplethr)
library(ggmap)
library(maps)
library(rgdal)
library(rgeos) 
library(maptools)
library(plotly)
library(knitr)
library(jsonlite)
#匯入境外生來台資料(國別)(第一題)
Data103<- read_csv("C:/Users/hh770/Downloads/25f64d5125016dcd6aed42e50c972ed0_export.csv")
Data104<- read_csv("C:/Users/hh770/Downloads/104_ab104_C.csv")
Data105<- read_csv("C:/Users/hh770/Downloads/105_ab105_C.csv")
Data106<- read_csv("C:/Users/hh770/Downloads/106_ab105_C.csv")

#匯入境外生來台資料(校別)(第一題)
Data103a<-read_csv("C:/Users/hh770/Downloads/103_ab103_S.csv")
Data104a<-read_csv("C:/Users/hh770/Downloads/104_ab104_S.csv")
Data105a<-read_csv("C:/Users/hh770/Downloads/105_ab105_S.csv")
Data106a<-read_csv("C:/Users/hh770/Downloads/106_ab105_S.csv")

#匯入大專校院本國學生出國進修交流數(第四題)
Student_RPT<- read.csv("C:/Users/hh770/Downloads/Student_RPT_07 .csv")

#匯入世界各主要國家之我國留學生人數統計表(第七題)
StudyAbord105<- read_csv("C:/Users/hh770/Downloads/105abc .csv")
StudyAbord105<-StudyAbord105[,-c(4,5,6)]
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
knitr::kable(head(arrange(CountryBaseSum,desc(CountrySum)),10))


#(第一題第二小題)又哪間大學的境外生最多呢？請取出前十名的大學與總人數，由大到小排序
#加總各校境外生總人數(第一題)
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
knitr::kable(head(arrange(SchoolBaseSum,desc(SchoolSum)),10))

#(第二題)用bar chart呈現各個國家(全部)來台灣唸書的學生人數
ggplot(CountryBaseSum,
       aes(x=國別,y=CountrySum))+
  geom_bar(stat = "identity")+
  labs(x="國家",y="總計",title="各個國家來台灣念書的學生人數")+
  theme(axis.text.x = element_text(size = 6,angle = 90))+
  theme(panel.border = element_blank())
#(第三題)用面量圖呈現各個國家來台灣唸書的學生人數，人數越多顏色越深
#把國家名字換成英文
CountryName<-fromJSON("C:/Users/hh770/Downloads/countries.json")
CountryBaseSum$country<-""

for (n in 1:177) {
  for (m in 1:250) {
    if(CountryBaseSum[n,1]==CountryName$Taiwan[m])
      CountryBaseSum[n,7]<-CountryName$ISO3[m]
  }
}
#手動輸入
CountryBaseSum[5,7]<-"CHN"
CountryBaseSum[8,7]<-"PNG"
CountryBaseSum[42,7]<-"TUV"
CountryBaseSum[48,7]<-"HRV"
CountryBaseSum[50,7]<-"HND"
CountryBaseSum[67,7]<-"BIH"
CountryBaseSum[72,7]<-"SAU"
CountryBaseSum[80,7]<-"KOR"
CountryBaseSum[81,7]<-"SSD"
CountryBaseSum[87,7]<-"KOS"
CountryBaseSum[96,7]<-"COD"
CountryBaseSum[108,7]<-"NAM"
CountryBaseSum[112,7]<-"SLB"
CountryBaseSum[117,7]<-"MHL"
CountryBaseSum[119,7]<-"MLT"
CountryBaseSum[120,7]<-"FSM"
CountryBaseSum[139,7]<-"SRB"
CountryBaseSum[143,7]<-"SGP"
CountryBaseSum[144,7]<-"SLE"
CountryBaseSum[148,7]<-"VCT"
CountryBaseSum[150,7]<-"KNA"
CountryBaseSum[153,7]<-"COM"
CountryBaseSum[167,7]<-"AUS"
CountryBaseSum[173,7]<-"CYP"

l <- list(color = toRGB("grey"), width = 0.8)
g <- list(
  showframe=FALSE,
  showcountries=TRUE,
  showcoastlines=TRUE,
  coastlinecolor=toRGB("grey"),
  countrycolor=toRGB("grey"))



plot_ly(CountryBaseSum, 
        z=~CountrySum, 
        text=~country, 
        locations=~country, 
        type= 'choropleth',
        color=~CountrySum, 
        colors='Greens', 
        marker = list(line = l)) %>%
  layout(title='各個國家來台灣唸書的學生人數', geo=g)



#(第四題第一小題)大專院校的學生最喜歡去哪些國家交流？請取出前十名的國家與總人數，由大到小排序
PRTCountrySum<-group_by(Student_RPT,對方學校.機構.國別.地區.)%>%
  summarise(countrycount=sum(小計))
head(arrange(PRTCountrySum,desc(countrycount)),10)
knitr::kable(head(arrange(PRTCountrySum,desc(countrycount)),10))
#(第四題第二小題)哪間大學的出國交流學生數最多呢？請取出前十名的大學與總人數，由大到小排序
PRTSchoolSum<-group_by(Student_RPT,學校名稱)%>%
  summarise(schoolcount=sum(小計))
head(arrange(PRTSchoolSum,desc(schoolcount)),10)
knitr::kable(head(arrange(PRTSchoolSum,desc(schoolcount)),10))
#(第五題)請用bar chart呈現台灣大專院校(全部)的學生去各國家進修交流人數
ggplot(PRTSchoolSum,
       aes(x=學校名稱,y=schoolcount))+
  geom_bar(stat = "identity")+
  labs(x="學校名稱",y="總計",title="台灣大專院校的學生去各國家進修交流人數")+
  theme(axis.text.x = element_text(size = 6,angle = 90))+
  theme(panel.border = element_blank())
#(第六題)請用面量圖呈現台灣大專院校的學生去各國家進修交流人數，人數越多顏色越深
#把中文國家名字改成英文
PRTCountrySum$country<-""
for (n in 2:149) {
  for (m in 1:250) {
    if(PRTCountrySum[n,1]==CountryName$Taiwan[m])
      PRTCountrySum[n,3]<-CountryName$ISO3[m]
  }
}
#手動輸入
PRTCountrySum[3,3]<-"TUR"
PRTCountrySum[4,3]<-"CHN"
PRTCountrySum[5,3]<-"KOR"
PRTCountrySum[6,3]<-"CHN"
PRTCountrySum[8,3]<-"DNK"
PRTCountrySum[10,3]<-"ECU"
PRTCountrySum[12,3]<-"BRA"
PRTCountrySum[14,3]<-"PAN"
PRTCountrySum[17,3]<-"BEL"
PRTCountrySum[24,3]<-"SWZ"
PRTCountrySum[27,3]<-"GMB"
PRTCountrySum[28,3]<-"BRL"
PRTCountrySum[30,3]<-"LTU"
PRTCountrySum[31,3]<-"IRN"
PRTCountrySum[33,3]<-"ISL"
PRTCountrySum[35,3]<-"LIE"
PRTCountrySum[37,3]<-"HUN"
PRTCountrySum[38,3]<-"IDN"
PRTCountrySum[40,3]<-"IDN"
PRTCountrySum[41,3]<-"IND"
PRTCountrySum[43,3]<-"ESP"
PRTCountrySum[c(44,45),3]<-"HRV"
PRTCountrySum[47,3]<-"GRC"
PRTCountrySum[49,3]<-"BRN"
PRTCountrySum[51,3]<-"SAU"
PRTCountrySum[55,3]<-"PLW"
PRTCountrySum[57,3]<-"LVA"
PRTCountrySum[60,3]<-"POL"
PRTCountrySum[62,3]<-"FIN"
PRTCountrySum[64,3]<-"OMN"
PRTCountrySum[65,3]<-"IRN"
PRTCountrySum[67,3]<-"RUS"
PRTCountrySum[69,3]<-"ZAF"
PRTCountrySum[70,3]<-"KOR"
PRTCountrySum[72,3]<-"KHM"
PRTCountrySum[74,3]<-"KOS"
PRTCountrySum[76,3]<-"TUN"
PRTCountrySum[78,3]<-"JOR"
PRTCountrySum[84,3]<-"CRI"
PRTCountrySum[86,3]<-"EGY"
PRTCountrySum[88,3]<-"NOR"
PRTCountrySum[90,3]<-"THA"
PRTCountrySum[94,3]<-"SLB"
PRTCountrySum[97,3]<-"MLT"
PRTCountrySum[99,3]<-"CZE"
PRTCountrySum[101,3]<-"NLD"
PRTCountrySum[102,3]<-"MOZ"
PRTCountrySum[104,3]<-"FJI"
PRTCountrySum[106,3]<-"LKA"
PRTCountrySum[108,3]<-"SVK"
PRTCountrySum[110,3]<-"SVN"
PRTCountrySum[113,3]<-"PHL"
PRTCountrySum[115,3]<-"VNV"
PRTCountrySum[116,3]<-"SYC"
PRTCountrySum[117,3]<-"SRB"
PRTCountrySum[119,3]<-"AUT"
PRTCountrySum[121,3]<-"EST"
PRTCountrySum[123,3]<-"IRL"
PRTCountrySum[c(124,125),3]<-"SGP"
PRTCountrySum[128,3]<-"SWE"
PRTCountrySum[130,3]<-"ITA"
PRTCountrySum[132,3]<-"STP"
PRTCountrySum[134,3]<-"PRT"
PRTCountrySum[136,3]<-"MNG"
PRTCountrySum[138,3]<-"MEX"
PRTCountrySum[140,3]<-"DEU"
PRTCountrySum[142,3]<-"MAR"
PRTCountrySum[144,3]<-"MUS"
PRTCountrySum[145,3]<-"MMR"
PRTCountrySum[146,3]<-"AUS"
PRTCountrySum[148,3]<-"LUX"
#用ptrcountrysum
plot_ly(PRTCountrySum, 
        z=~countrycount, 
        text=~country, 
        locations=~country, 
        type= 'choropleth',
        color=~countrycount, 
        colors='Greens', 
        marker = list(line = l)) %>%
  layout(title='台灣大專院校的學生去各國家進修交流人數', geo=g)
#(第七題)台灣學生最喜歡去哪些國家留學呢？請取出前十名的國家與總人數，由大到小排序
head(arrange(StudyAbord105,desc(總人數)),10)
knitr::kable(head(arrange(StudyAbord105,desc(總人數)),10))
#(第八題)請用面量圖呈現台灣學生去各國家留學人數，人數越多顏色越深(畫世界)
#改國家名字
StudyAbord105$country<-""
for (n in 1:29) {
  for (m in 1:250) {
    if(StudyAbord105[n,2]==CountryName$Taiwan[m])
      StudyAbord105[n,4]<-CountryName$ISO3[m]
  }
}
#手動輸入
StudyAbord105[15,4]<-"AUS"
StudyAbord105[18,4]<-"KOR"
StudyAbord105[20,4]<-"SGP"

plot_ly(StudyAbord105, 
        z=~'總人數', 
        text=~country, 
        locations=~country, 
        type= 'choropleth',
        color=~'總人數', 
        colors='Greens', 
        marker = list(line = l)) %>%
  layout(title='台灣學生去各國家留學人數', geo=g)

#(第九題)請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？
StudyAbord105#(OUT)
CountryBaseSum#(IN)

StudyTrend<-merge(StudyAbord105,CountryBaseSum,by="國別",all=TRUE)

ggplot(StudyTrend, aes(x = 國別)) +
  geom_line(aes(y = 總人數)) +
  geom_line(aes(y = CountrySum))+
  theme(axis.text.x = element_text(size = 6,angle = 90))+
  theme(panel.border = element_blank())
#想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。
