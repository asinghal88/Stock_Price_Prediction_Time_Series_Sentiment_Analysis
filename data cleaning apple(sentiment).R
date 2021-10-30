##Preparing dataset for analysis
setwd("C:/Users/Nisant Manepalli/Downloads")
i=1
filename_list = list.files(path = 'C://Users/Nisant Manepalli/Downloads/n/')
final_file = data.frame(V1 = NA,company_apple = F, date = NA)
for(filename in filename_list){
  a=read.delim(paste('n/',filename,sep=''),header = F)
  a$company_apple = stringr::str_detect(string = a$V1,pattern = '^AAPL')
  a = a[a$company_apple,]
  if(nrow(a) != 0){
    a$date = filename
    final_file = rbind(final_file,a)
  }
print(i)

i=i+1
}
#creating the file
library(stringr)
mon=final_file
mon=mon[-1,]
mon$company_apple=NULL
mon$date=stringr::str_replace(mon$date,pattern = '.csv',replacement = '') 
mon$date2=NULL 
mon$Date=mon$date
mon$date=NULL
a1=read.csv('2.csv')
a1$Date=as.Date(a1$Date,'%d-%m-%Y')
mon$Date=as.Date(mon$Date,'%Y-%m-%d')
df2=merge(mon,a1)
