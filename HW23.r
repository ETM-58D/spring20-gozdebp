#parta
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

#part b
consumption_b=consumption
consumption_b[,lag_48:=shift(value,48)]
consumption_b[,lag_168:=shift(value,168)]
train.conb=consumption_b[date >= "2016-01-01" & date <= "2020-03-01"]
test.conb=consumption_b [date >= "2020-03-02" & date <= "2020-05-19"]
train.conb_full=train.conb[complete.cases(train.conb)]
test.conb_full=test.conb[complete.cases(test.conb)]

model=lm(value~lag_48+lag_168,train.conb_full)
pred_con=predict(model,newdata=test.conb_full)
summary(model)

head(pred_con)

test.conb_full2=test.conb[complete.cases(test.conb)]
test.conb_full2[,pred:=pred_con]
head(test.conb_full2)

test.conb_full2[,mape:=(abs((value-pred)/value)*100)]
head(test.conb_full2)

boxplot(test.conb_full2$mape)

#part c
train.conb_full=train.conb[complete.cases(train.conb)]
test.conb_full=test.conb[complete.cases(test.conb)]

train.conb_full0=train.conb_full[hour==0]
train.conb_full1=train.conb_full[hour==1]
train.conb_full2=train.conb_full[hour==2]
train.conb_full3=train.conb_full[hour==3]
train.conb_full4=train.conb_full[hour==4]
train.conb_full5=train.conb_full[hour==5]
train.conb_full6=train.conb_full[hour==6]
train.conb_full7=train.conb_full[hour==7]
train.conb_full8=train.conb_full[hour==8]
train.conb_full9=train.conb_full[hour==9]
train.conb_full10=train.conb_full[hour==10]
train.conb_full11=train.conb_full[hour==11]
train.conb_full12=train.conb_full[hour==12]
train.conb_full13=train.conb_full[hour==13]
train.conb_full14=train.conb_full[hour==14]
train.conb_full15=train.conb_full[hour==15]
train.conb_full16=train.conb_full[hour==16]
train.conb_full17=train.conb_full[hour==17]
train.conb_full18=train.conb_full[hour==18]
train.conb_full19=train.conb_full[hour==19]
train.conb_full20=train.conb_full[hour==20]
train.conb_full21=train.conb_full[hour==21]
train.conb_full22=train.conb_full[hour==22]
train.conb_full23=train.conb_full[hour==23]

test.conb_full0=test.conb_full[hour==0]
test.conb_full1=test.conb_full[hour==1]
test.conb_full2=test.conb_full[hour==2]
test.conb_full3=test.conb_full[hour==3]
test.conb_full4=test.conb_full[hour==4]
test.conb_full5=test.conb_full[hour==5]
test.conb_full6=test.conb_full[hour==6]
test.conb_full7=test.conb_full[hour==7]
test.conb_full8=test.conb_full[hour==8]
test.conb_full9=test.conb_full[hour==9]
test.conb_full10=test.conb_full[hour==10]
test.conb_full11=test.conb_full[hour==11]
test.conb_full12=test.conb_full[hour==12]
test.conb_full13=test.conb_full[hour==13]
test.conb_full14=test.conb_full[hour==14]
test.conb_full15=test.conb_full[hour==15]
test.conb_full16=test.conb_full[hour==16]
test.conb_full17=test.conb_full[hour==17]
test.conb_full18=test.conb_full[hour==18]
test.conb_full19=test.conb_full[hour==19]
test.conb_full20=test.conb_full[hour==20]
test.conb_full21=test.conb_full[hour==21]
test.conb_full22=test.conb_full[hour==22]
test.conb_full23=test.conb_full[hour==23]

head(test.conb_full22)

model0=lm(value~lag_48+lag_168,train.conb_full0)
model1=lm(value~lag_48+lag_168,train.conb_full1)
model2=lm(value~lag_48+lag_168,train.conb_full2)
model3=lm(value~lag_48+lag_168,train.conb_full3)
model4=lm(value~lag_48+lag_168,train.conb_full4)
model5=lm(value~lag_48+lag_168,train.conb_full5)
model6=lm(value~lag_48+lag_168,train.conb_full6)
model7=lm(value~lag_48+lag_168,train.conb_full7)
model8=lm(value~lag_48+lag_168,train.conb_full8)
model9=lm(value~lag_48+lag_168,train.conb_full9)
model10=lm(value~lag_48+lag_168,train.conb_full10)
model11=lm(value~lag_48+lag_168,train.conb_full11)
model12=lm(value~lag_48+lag_168,train.conb_full12)
model13=lm(value~lag_48+lag_168,train.conb_full13)
model14=lm(value~lag_48+lag_168,train.conb_full14)
model15=lm(value~lag_48+lag_168,train.conb_full15)
model16=lm(value~lag_48+lag_168,train.conb_full16)
model17=lm(value~lag_48+lag_168,train.conb_full17)
model18=lm(value~lag_48+lag_168,train.conb_full18)
model19=lm(value~lag_48+lag_168,train.conb_full19)
model20=lm(value~lag_48+lag_168,train.conb_full20)
model21=lm(value~lag_48+lag_168,train.conb_full21)
model22=lm(value~lag_48+lag_168,train.conb_full22)
model23=lm(value~lag_48+lag_168,train.conb_full23)

pred_con0=predict(model0,newdata=test.conb_full0)
pred_con1=predict(model1,newdata=test.conb_full1)
pred_con2=predict(model2,newdata=test.conb_full2)
pred_con3=predict(model3,newdata=test.conb_full3)
pred_con4=predict(model4,newdata=test.conb_full4)
pred_con5=predict(model5,newdata=test.conb_full5)
pred_con6=predict(model6,newdata=test.conb_full6)
pred_con7=predict(model7,newdata=test.conb_full7)
pred_con8=predict(model8,newdata=test.conb_full8)
pred_con9=predict(model9,newdata=test.conb_full9)
pred_con10=predict(model10,newdata=test.conb_full10)
pred_con11=predict(model11,newdata=test.conb_full11)
pred_con12=predict(model12,newdata=test.conb_full12)
pred_con13=predict(model13,newdata=test.conb_full13)
pred_con14=predict(model14,newdata=test.conb_full14)
pred_con15=predict(model15,newdata=test.conb_full15)
pred_con16=predict(model16,newdata=test.conb_full16)
pred_con17=predict(model17,newdata=test.conb_full17)
pred_con18=predict(model18,newdata=test.conb_full18)
pred_con19=predict(model19,newdata=test.conb_full19)
pred_con20=predict(model20,newdata=test.conb_full20)
pred_con21=predict(model21,newdata=test.conb_full21)
pred_con22=predict(model22,newdata=test.conb_full22)
pred_con23=predict(model23,newdata=test.conb_full23)

test.conb_full0[,pred:=pred_con0]
test.conb_full1[,pred:=pred_con1]
test.conb_full2[,pred:=pred_con2]
test.conb_full3[,pred:=pred_con3]
test.conb_full4[,pred:=pred_con4]
test.conb_full5[,pred:=pred_con5]
test.conb_full6[,pred:=pred_con6]
test.conb_full7[,pred:=pred_con7]
test.conb_full8[,pred:=pred_con8]
test.conb_full9[,pred:=pred_con9]
test.conb_full10[,pred:=pred_con10]
test.conb_full11[,pred:=pred_con11]
test.conb_full12[,pred:=pred_con12]
test.conb_full13[,pred:=pred_con13]
test.conb_full14[,pred:=pred_con14]
test.conb_full15[,pred:=pred_con15]
test.conb_full16[,pred:=pred_con16]
test.conb_full17[,pred:=pred_con17]
test.conb_full18[,pred:=pred_con18]
test.conb_full19[,pred:=pred_con19]
test.conb_full20[,pred:=pred_con20]
test.conb_full21[,pred:=pred_con21]
test.conb_full22[,pred:=pred_con22]
test.conb_full23[,pred:=pred_con23]

test.conb_full0[,mape:=(abs((value-pred)/value)*100)]
test.conb_full1[,mape:=(abs((value-pred)/value)*100)]
test.conb_full2[,mape:=(abs((value-pred)/value)*100)]
test.conb_full3[,mape:=(abs((value-pred)/value)*100)]
test.conb_full4[,mape:=(abs((value-pred)/value)*100)]
test.conb_full5[,mape:=(abs((value-pred)/value)*100)]
test.conb_full6[,mape:=(abs((value-pred)/value)*100)]
test.conb_full7[,mape:=(abs((value-pred)/value)*100)]
test.conb_full8[,mape:=(abs((value-pred)/value)*100)]
test.conb_full9[,mape:=(abs((value-pred)/value)*100)]
test.conb_full10[,mape:=(abs((value-pred)/value)*100)]
test.conb_full11[,mape:=(abs((value-pred)/value)*100)]
test.conb_full12[,mape:=(abs((value-pred)/value)*100)]
test.conb_full13[,mape:=(abs((value-pred)/value)*100)]
test.conb_full14[,mape:=(abs((value-pred)/value)*100)]
test.conb_full15[,mape:=(abs((value-pred)/value)*100)]
test.conb_full16[,mape:=(abs((value-pred)/value)*100)]
test.conb_full17[,mape:=(abs((value-pred)/value)*100)]
test.conb_full18[,mape:=(abs((value-pred)/value)*100)]
test.conb_full19[,mape:=(abs((value-pred)/value)*100)]
test.conb_full20[,mape:=(abs((value-pred)/value)*100)]
test.conb_full21[,mape:=(abs((value-pred)/value)*100)]
test.conb_full22[,mape:=(abs((value-pred)/value)*100)]
test.conb_full23[,mape:=(abs((value-pred)/value)*100)]

mape_table=data.table(
  observation=1:79,
  mape0= test.conb_full0[,mape],
  mape1= test.conb_full1[,mape],
  mape2= test.conb_full2[1:79,mape],
  mape3= test.conb_full3[1:79,mape],
  mape4= test.conb_full4[1:79,mape],
  mape5= test.conb_full5[1:79,mape],
  mape6= test.conb_full6[1:79,mape],
  mape7= test.conb_full7[1:79,mape],
  mape8= test.conb_full8[1:79,mape],
  mape9= test.conb_full9[1:79,mape],
  mape10= test.conb_full10[1:79,mape],
  mape11= test.conb_full11[1:79,mape],
  mape12= test.conb_full12[1:79,mape],
  mape13= test.conb_full13[1:79,mape],
  mape14= test.conb_full14[1:79,mape],
  mape15= test.conb_full15[1:79,mape],
  mape16= test.conb_full16[1:79,mape],
  mape17= test.conb_full17[1:79,mape],
  mape18= test.conb_full18[1:79,mape],
  mape19= test.conb_full19[1:79,mape],
  mape20= test.conb_full20[1:79,mape],
  mape21= test.conb_full21[1:79,mape],
  mape22= test.conb_full22[1:79,mape],
  mape23= test.conb_full23[1:79,mape]
)

head(mape_table)

summary(mape_table)
