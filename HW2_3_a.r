install.packages("data.table")
library("data.table")
require(data.table)

consumption=fread('C:/Users/Administrator/Desktop/gozde/hw2/hw2.csv')
setnames(consumption,names(consumption)[3],'value')
consumption[,date:=as.Date(Tarih,'%d.%m.%Y')]
consumption[,hour:=as.numeric(substr(Saat,1,2))]
consumption=consumption[,list(date,hour,value)]
consumption[,value:=gsub(".","",value,fixed=TRUE)]
consumption[,value:=as.numeric(gsub(",",".",value,fixed=TRUE))]
head(consumption)

train.con =consumption [date >= "2016-01-01" & date <= "2020-03-01"]
test.con168 =consumption [date >= "2020-03-02" & date <= "2020-05-19"]
test.con48 =consumption [date >= "2020-03-02" & date <= "2020-05-19"]

test.con168[,lag_168:=shift(value,168)]
test.con168[,mape:=(abs((value-lag_168)/value)*100)]

test.con48[,lag_48:=shift(value,48)]
test.con48[,mape:=(abs((value-lag_48)/value)*100)]

boxplot(test.con48$mape,test.con168$mape)

test.con168_2=consumption [date >= "2020-03-02" & date <= "2020-05-19"]
head(test.con168_2)
test.con168_2[,lag_168:=shift(value,168)]
test.con168_2_full=test.con168_2[complete.cases(test.con168_2)]
test.con168_2_full[,mape:=(abs((value-lag_168)/value)*100)]

test.con48_2=consumption [date >= "2020-03-02" & date <= "2020-05-19"]
head(test.con48_2)
test.con48_2[,lag_48:=shift(value,48)]
test.con48_2_full=test.con48_2[complete.cases(test.con48_2)]
test.con48_2_full[,mape:=(abs((value-lag_48)/value)*100)]

boxplot(test.con48_2_full$mape,test.con168_2_full$mape)

summary(test.con48)

summary(test.con168)


