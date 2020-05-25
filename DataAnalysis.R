install.packages("jsonlite")
library(jsonlite)
library(dplyr)
library(readr)

#匯入資料
Data104<-read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
Data107<-read.csv("/Users/tyson/Desktop/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
#整理資料(統一職業名稱)
Data104$大職業別<-gsub("部門","",Data104$大職業別)
Data104$大職業別<-gsub("、","_",Data104$大職業別)
Data104$大職業別<-gsub(" -","-",Data104$大職業別)
Data104$大職業別<-gsub("資訊及通訊傳播業","出版、影音製作、傳播及資通訊服務業",Data104$大職業別)
Data107$大職業別<-gsub("營建工程","營造業",Data107$大職業別)
Data107$大職業別<-gsub("醫療保健業","醫療保健服務業",Data107$大職業別)
Data107$大職業別<-gsub("教育業","教育服務業",Data107$大職業別)
#join兩個資料
Data<-full_join(Data104,Data107,by="大職業別")
#更改column name名稱
colnames(Data)[1]<-"104年度"
colnames(Data)[15]<-"107年度"

#Q1
#新增欄位計算107年度/104年度的大學畢業薪資
Data$大學畢業薪資比例<-as.numeric(Data$`大學-薪資`)/as.numeric(Data$大學.薪資)
#由大到小排序跟呈現前10名的資料
Data<-arrange(Data,desc(大學畢業薪資比例))
head(Data,10)
#前10名的大職業別
head(Data$大職業別,10)

#兩年度薪資比例 >1.05的欄位
more_than_5_percent<-filter(Data,大學畢業薪資比例>1.05)
more_than_5_percent$大職業別

#取出大職業別中"-" 前面的字串
strsplit(more_than_5_percent$大職業別,"-")%>%
  sapply("[", 1)
#出現次數
table(more_than_5_percent$大職業別)

#Q2
#先將104及107年度的大學男女薪資比例轉成數字
Data$`大學-女/男`<-as.numeric(Data$`大學-女/男`)
Data$大學.女.男<-as.numeric(Data$大學.女.男)
#將104年度大學畢業男女薪資比例由小到大排序
Data[complete.cases(Data),]%>%
  filter(`大學-女/男`<100)%>%
  arrange(`大學-女/男`)%>%
  select(大職業別,`大學-女/男`)%>%
  head(10)%>%
  View()
#將107年度大學畢業男女薪資比例由小到大排序 
Data[complete.cases(Data),]%>%
  filter(大學.女.男<100)%>%
  arrange(大學.女.男)%>%
  select(大職業別,大學.女.男)%>%
  head(10)%>%
  View()
##以上兩個的結果是小於100的都是個行業中男生薪資比例比女生高的，
##及最後取出前１０個做成表格並由小到大排序

#將104年度大學畢業男女薪資比例由大到小排序
Data[complete.cases(Data),]%>%
  filter(`大學-女/男`>100)%>%
  arrange(`大學-女/男`)%>%
  select(大職業別,`大學-女/男`)%>%
  head(10)%>%
  View()
#將107年度大學畢業男女薪資比例由大到小排序 
Data[complete.cases(Data),]%>%
  filter(大學.女.男>100)%>%
  arrange(大學.女.男)%>%
  select(大職業別,大學.女.男)%>%
  head(10)%>%
  View()
##在１０４年度的資料中只有１個職業是女生薪資比男生高的，
##在１０７年度則是１個都沒有。由此可見，男女同工不同酬的問題不只沒有變好，
##反而是越來越差

#Q3
#取出大學薪資欄位與研究所薪資欄位
salary<-select(Data,大職業別,`大學.薪資`,`研究所.薪資`)
##用select取出１０７年度的大學薪資及研究所薪資並創建一個salary的表格

#計算研究所薪資 / 大學薪資，並用此值在表格中新增一個欄位
salary$薪資差異比例<-as.numeric(salary$`研究所.薪資`)/as.numeric(salary$`大學.薪資`)
##將兩個薪資相除，大於１的代表研究所在此職業的薪資比大學還高

#用薪資差異比例做大到小排序，並取前１０筆
arrange(salary,desc(薪資差異比例))%>%
  head(10)%>%
  View()
##將比例由大道小做排序，可以知道薪資差異最多的前１０名職業
#	在其他服務業的薪資差異比例是最高的，比其他的職業多出了大約３％，所以這個職業不念研究所比較划算

#Q4
#有興趣的職業別篩選，呈現薪資
interested<-salary[grepl("專業_科學及技術服務業",salary$大職業別),]
interested$研究所.薪資<-as.numeric(interested$研究所.薪資)
interested$大學.薪資<-as.numeric(interested$大學.薪資)

interested_1<-salary[grepl("出版、影音製作、傳播及資通訊服務業",salary$大職業別),]
interested_1$研究所.薪資<-as.numeric(interested_1$研究所.薪資)
interested_1$大學.薪資<-as.numeric(interested_1$大學.薪資)

# 這些職業別研究所薪資與大學薪資差多少呢？
interested$薪資差異<-interested$研究所.薪資-interested$大學.薪資
interested_1$薪資差異<-interested_1$研究所.薪資-interested_1$大學.薪資

