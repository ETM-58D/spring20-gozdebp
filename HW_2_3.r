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

#part d
full_con=consumption[complete.cases(consumption)]
install.packages("glmnet", dependencies=TRUE)
library(glmnet)

long_tr = full_con[date < '2020-03-01']
long_te = full_con[date >= '2020-03-01']
wide_pen_tr = merge(dcast(long_tr, date~paste0('lag_48_hour_', hour), value.var='lag_48'), dcast(long_tr, date~paste0('lag_168_hour_', hour), value.var='lag_168'), by = 'date')
wide_pen_tr_ac = dcast(long_tr, date~paste0('actual_', hour), value.var='value')
wide_pen_te = merge(dcast(long_te, date~paste0('lag_48_hour_', hour), value.var='lag_48'), dcast(long_te, date~paste0('lag_168_hour_', hour), value.var='lag_168'), by = 'date')
wide_pen_te_ac = dcast(long_te, date~paste0('actual_', hour), value.var='value')

wide_pen_tr = merge(dcast(long_tr, date~paste0('lag_48_hour_', hour), value.var='lag_48'), dcast(long_tr, date~paste0('lag_168_hour_', hour), value.var='lag_168'), by = 'date')
wide_pen_tr_ac = dcast(long_tr, date~paste0('actual_', hour), value.var='value')
wide_pen_te = merge(dcast(long_te, date~paste0('lag_48_hour_', hour), value.var='lag_48'), dcast(long_te, date~paste0('lag_168_hour_', hour), value.var='lag_168'), by = 'date')
wide_pen_te_ac = dcast(long_te, date~paste0('actual_', hour), value.var='value')

wide_tr_mat = data.matrix(wide_pen_tr)
wide_tr_ac_mat = data.matrix(wide_pen_tr_ac)
wide_te_mat = data.matrix(wide_pen_te)

wide_fit_0 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 2], alpha = 1)
wide_fit_1 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 3], alpha = 1)
wide_fit_2 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 14], alpha = 1)
wide_fit_3 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 19], alpha = 1)
wide_fit_4 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 20], alpha = 1)
wide_fit_5 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 21], alpha = 1)
wide_fit_6 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 22], alpha = 1)
wide_fit_7 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 23], alpha = 1)
wide_fit_8 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 24], alpha = 1)
wide_fit_9 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 25], alpha = 1)
wide_fit_10 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 4], alpha = 1)
wide_fit_11 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 5], alpha = 1)
wide_fit_12 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 6], alpha = 1)
wide_fit_13 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 7], alpha = 1)
wide_fit_14 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 8], alpha = 1)
wide_fit_15 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 9], alpha = 1)
wide_fit_16 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 10], alpha = 1)
wide_fit_17 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 11], alpha = 1)
wide_fit_18 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 12], alpha = 1)
wide_fit_19 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 13], alpha = 1)
wide_fit_20 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 15], alpha = 1)
wide_fit_21 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 16], alpha = 1)
wide_fit_22 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 17], alpha = 1)
wide_fit_23 = cv.glmnet(wide_tr_mat, wide_tr_ac_mat[, 18], alpha = 1)

plot(wide_fit_0)
wide_fit_0$lambda.min
wide_fit_0$lambda.1se

lasso_model_0 = glmnet(wide_tr_mat, wide_tr_ac_mat[,2], alpha = 1, family = "gaussian", lambda = wide_fit_0$lambda.1se)
lasso_model_1 = glmnet(wide_tr_mat, wide_tr_ac_mat[,3], alpha = 1, family = "gaussian", lambda = wide_fit_1$lambda.1se)
lasso_model_2 = glmnet(wide_tr_mat, wide_tr_ac_mat[,14], alpha = 1, family = "gaussian", lambda = wide_fit_2$lambda.1se)
lasso_model_3 = glmnet(wide_tr_mat, wide_tr_ac_mat[,19], alpha = 1, family = "gaussian", lambda = wide_fit_3$lambda.1se)
lasso_model_4 = glmnet(wide_tr_mat, wide_tr_ac_mat[,20], alpha = 1, family = "gaussian", lambda = wide_fit_4$lambda.1se)
lasso_model_5 = glmnet(wide_tr_mat, wide_tr_ac_mat[,21], alpha = 1, family = "gaussian", lambda = wide_fit_5$lambda.1se)
lasso_model_6 = glmnet(wide_tr_mat, wide_tr_ac_mat[,22], alpha = 1, family = "gaussian", lambda = wide_fit_6$lambda.1se)
lasso_model_7 = glmnet(wide_tr_mat, wide_tr_ac_mat[,23], alpha = 1, family = "gaussian", lambda = wide_fit_7$lambda.1se)
lasso_model_8 = glmnet(wide_tr_mat, wide_tr_ac_mat[,24], alpha = 1, family = "gaussian", lambda = wide_fit_8$lambda.1se)
lasso_model_9 = glmnet(wide_tr_mat, wide_tr_ac_mat[,25], alpha = 1, family = "gaussian", lambda = wide_fit_9$lambda.1se)
lasso_model_10 = glmnet(wide_tr_mat, wide_tr_ac_mat[,4], alpha = 1, family = "gaussian", lambda = wide_fit_10$lambda.1se)
lasso_model_11 = glmnet(wide_tr_mat, wide_tr_ac_mat[,5], alpha = 1, family = "gaussian", lambda = wide_fit_11$lambda.1se)
lasso_model_12 = glmnet(wide_tr_mat, wide_tr_ac_mat[,6], alpha = 1, family = "gaussian", lambda = wide_fit_12$lambda.1se)
lasso_model_13 = glmnet(wide_tr_mat, wide_tr_ac_mat[,7], alpha = 1, family = "gaussian", lambda = wide_fit_13$lambda.1se)
lasso_model_14 = glmnet(wide_tr_mat, wide_tr_ac_mat[,8], alpha = 1, family = "gaussian", lambda = wide_fit_14$lambda.1se)
lasso_model_15 = glmnet(wide_tr_mat, wide_tr_ac_mat[,9], alpha = 1, family = "gaussian", lambda = wide_fit_15$lambda.1se)
lasso_model_16 = glmnet(wide_tr_mat, wide_tr_ac_mat[,10], alpha = 1, family = "gaussian", lambda = wide_fit_16$lambda.1se)
lasso_model_17 = glmnet(wide_tr_mat, wide_tr_ac_mat[,11], alpha = 1, family = "gaussian", lambda = wide_fit_17$lambda.1se)
lasso_model_18 = glmnet(wide_tr_mat, wide_tr_ac_mat[,12], alpha = 1, family = "gaussian", lambda = wide_fit_18$lambda.1se)
lasso_model_19 = glmnet(wide_tr_mat, wide_tr_ac_mat[,13], alpha = 1, family = "gaussian", lambda = wide_fit_19$lambda.1se)
lasso_model_20 = glmnet(wide_tr_mat, wide_tr_ac_mat[,15], alpha = 1, family = "gaussian", lambda = wide_fit_20$lambda.1se)
lasso_model_21 = glmnet(wide_tr_mat, wide_tr_ac_mat[,16], alpha = 1, family = "gaussian", lambda = wide_fit_21$lambda.1se)
lasso_model_22 = glmnet(wide_tr_mat, wide_tr_ac_mat[,17], alpha = 1, family = "gaussian", lambda = wide_fit_22$lambda.1se)
lasso_model_23 = glmnet(wide_tr_mat, wide_tr_ac_mat[,18], alpha = 1, family = "gaussian", lambda = wide_fit_23$lambda.1se)

lasso_predicted = wide_pen_te_ac
lasso_predicted = lasso_predicted[, lasso_predicted_0 := predict.glmnet(lasso_model_0, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_1 := predict.glmnet(lasso_model_1, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_2 := predict.glmnet(lasso_model_2, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_3 := predict.glmnet(lasso_model_3, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_4 := predict.glmnet(lasso_model_4, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_5 := predict.glmnet(lasso_model_5, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_6 := predict.glmnet(lasso_model_6, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_7 := predict.glmnet(lasso_model_7, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_8 := predict.glmnet(lasso_model_8, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_9 := predict.glmnet(lasso_model_9, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_10 := predict.glmnet(lasso_model_10, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_11 := predict.glmnet(lasso_model_11, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_12 := predict.glmnet(lasso_model_12, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_13 := predict.glmnet(lasso_model_13, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_14 := predict.glmnet(lasso_model_14, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_15 := predict.glmnet(lasso_model_15, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_16 := predict.glmnet(lasso_model_16, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_17 := predict.glmnet(lasso_model_17, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_18 := predict.glmnet(lasso_model_18, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_19 := predict.glmnet(lasso_model_19, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_20 := predict.glmnet(lasso_model_20, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_21 := predict.glmnet(lasso_model_21, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_22 := predict.glmnet(lasso_model_22, wide_te_mat)]
lasso_predicted = lasso_predicted[, lasso_predicted_23 := predict.glmnet(lasso_model_23, wide_te_mat)]

lasso_predicted = lasso_predicted[, APE_L0:=(abs(lasso_predicted$actual_0-lasso_predicted$lasso_predicted_0)/abs(lasso_predicted$actual_0))*100]
lasso_predicted = lasso_predicted[, APE_L1:=(abs(lasso_predicted$actual_1-lasso_predicted$lasso_predicted_1)/abs(lasso_predicted$actual_1))*100]
lasso_predicted = lasso_predicted[, APE_L2:=(abs(lasso_predicted$actual_2-lasso_predicted$lasso_predicted_2)/abs(lasso_predicted$actual_2))*100]
lasso_predicted = lasso_predicted[, APE_L3:=(abs(lasso_predicted$actual_3-lasso_predicted$lasso_predicted_3)/abs(lasso_predicted$actual_3))*100]
lasso_predicted = lasso_predicted[, APE_L4:=(abs(lasso_predicted$actual_4-lasso_predicted$lasso_predicted_4)/abs(lasso_predicted$actual_4))*100]
lasso_predicted = lasso_predicted[, APE_L5:=(abs(lasso_predicted$actual_5-lasso_predicted$lasso_predicted_5)/abs(lasso_predicted$actual_5))*100]
lasso_predicted = lasso_predicted[, APE_L6:=(abs(lasso_predicted$actual_6-lasso_predicted$lasso_predicted_6)/abs(lasso_predicted$actual_6))*100]
lasso_predicted = lasso_predicted[, APE_L7:=(abs(lasso_predicted$actual_7-lasso_predicted$lasso_predicted_7)/abs(lasso_predicted$actual_7))*100]
lasso_predicted = lasso_predicted[, APE_L8:=(abs(lasso_predicted$actual_8-lasso_predicted$lasso_predicted_8)/abs(lasso_predicted$actual_8))*100]
lasso_predicted = lasso_predicted[, APE_L9:=(abs(lasso_predicted$actual_9-lasso_predicted$lasso_predicted_9)/abs(lasso_predicted$actual_9))*100]
lasso_predicted = lasso_predicted[, APE_L10:=(abs(lasso_predicted$actual_10-lasso_predicted$lasso_predicted_10)/abs(lasso_predicted$actual_10))*100]
lasso_predicted = lasso_predicted[, APE_L11:=(abs(lasso_predicted$actual_11-lasso_predicted$lasso_predicted_11)/abs(lasso_predicted$actual_11))*100]
lasso_predicted = lasso_predicted[, APE_L12:=(abs(lasso_predicted$actual_12-lasso_predicted$lasso_predicted_12)/abs(lasso_predicted$actual_12))*100]
lasso_predicted = lasso_predicted[, APE_L13:=(abs(lasso_predicted$actual_13-lasso_predicted$lasso_predicted_13)/abs(lasso_predicted$actual_13))*100]
lasso_predicted = lasso_predicted[, APE_L14:=(abs(lasso_predicted$actual_14-lasso_predicted$lasso_predicted_14)/abs(lasso_predicted$actual_14))*100]
lasso_predicted = lasso_predicted[, APE_L15:=(abs(lasso_predicted$actual_15-lasso_predicted$lasso_predicted_15)/abs(lasso_predicted$actual_15))*100]
lasso_predicted = lasso_predicted[, APE_L16:=(abs(lasso_predicted$actual_16-lasso_predicted$lasso_predicted_16)/abs(lasso_predicted$actual_16))*100]
lasso_predicted = lasso_predicted[, APE_L17:=(abs(lasso_predicted$actual_17-lasso_predicted$lasso_predicted_17)/abs(lasso_predicted$actual_17))*100]
lasso_predicted = lasso_predicted[, APE_L18:=(abs(lasso_predicted$actual_18-lasso_predicted$lasso_predicted_18)/abs(lasso_predicted$actual_18))*100]
lasso_predicted = lasso_predicted[, APE_L19:=(abs(lasso_predicted$actual_19-lasso_predicted$lasso_predicted_19)/abs(lasso_predicted$actual_19))*100]
lasso_predicted = lasso_predicted[, APE_L20:=(abs(lasso_predicted$actual_20-lasso_predicted$lasso_predicted_20)/abs(lasso_predicted$actual_20))*100]
lasso_predicted = lasso_predicted[, APE_L21:=(abs(lasso_predicted$actual_21-lasso_predicted$lasso_predicted_21)/abs(lasso_predicted$actual_21))*100]
lasso_predicted = lasso_predicted[, APE_L22:=(abs(lasso_predicted$actual_22-lasso_predicted$lasso_predicted_22)/abs(lasso_predicted$actual_22))*100]
lasso_predicted = lasso_predicted[, APE_L23:=(abs(lasso_predicted$actual_23-lasso_predicted$lasso_predicted_23)/abs(lasso_predicted$actual_23))*100]

lasso_MAPE_0 = mean(lasso_predicted$APE_L0)
lasso_MAPE_1 = mean(lasso_predicted$APE_L1)
lasso_MAPE_2 = mean(lasso_predicted$APE_L2)
lasso_MAPE_3 = mean(lasso_predicted$APE_L3)
lasso_MAPE_4 = mean(lasso_predicted$APE_L4)
lasso_MAPE_5 = mean(lasso_predicted$APE_L5)
lasso_MAPE_6 = mean(lasso_predicted$APE_L6)
lasso_MAPE_7 = mean(lasso_predicted$APE_L7)
lasso_MAPE_8 = mean(lasso_predicted$APE_L8)
lasso_MAPE_9 = mean(lasso_predicted$APE_L9)
lasso_MAPE_10 = mean(lasso_predicted$APE_L10)
lasso_MAPE_11 = mean(lasso_predicted$APE_L11)
lasso_MAPE_12 = mean(lasso_predicted$APE_L12)
lasso_MAPE_13 = mean(lasso_predicted$APE_L13)
lasso_MAPE_14 = mean(lasso_predicted$APE_L14)
lasso_MAPE_15 = mean(lasso_predicted$APE_L15)
lasso_MAPE_16 = mean(lasso_predicted$APE_L16)
lasso_MAPE_17 = mean(lasso_predicted$APE_L17)
lasso_MAPE_18 = mean(lasso_predicted$APE_L18)
lasso_MAPE_19 = mean(lasso_predicted$APE_L19)
lasso_MAPE_20 = mean(lasso_predicted$APE_L20)
lasso_MAPE_21 = mean(lasso_predicted$APE_L21)
lasso_MAPE_22 = mean(lasso_predicted$APE_L22)
lasso_MAPE_23 = mean(lasso_predicted$APE_L23)

summary(lasso_predicted[,50:73])

quantile_hour0=quantile(lasso_predicted$APE_L0, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour1=quantile(lasso_predicted$APE_L1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour2=quantile(lasso_predicted$APE_L2, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour3=quantile(lasso_predicted$APE_L3, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour4=quantile(lasso_predicted$APE_L4, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour5=quantile(lasso_predicted$APE_L5, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour6=quantile(lasso_predicted$APE_L6, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour7=quantile(lasso_predicted$APE_L7, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour8=quantile(lasso_predicted$APE_L8, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour9=quantile(lasso_predicted$APE_L9, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour10=quantile(lasso_predicted$APE_L10, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour11=quantile(lasso_predicted$APE_L11, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour12=quantile(lasso_predicted$APE_L12, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour13=quantile(lasso_predicted$APE_L13, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour14=quantile(lasso_predicted$APE_L14, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour15=quantile(lasso_predicted$APE_L15, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour16=quantile(lasso_predicted$APE_L16, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour17=quantile(lasso_predicted$APE_L17, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour18=quantile(lasso_predicted$APE_L18, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour19=quantile(lasso_predicted$APE_L19, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour20=quantile(lasso_predicted$APE_L20, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour21=quantile(lasso_predicted$APE_L21, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour22=quantile(lasso_predicted$APE_L22, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile_hour23=quantile(lasso_predicted$APE_L23, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

q_all=cbind(quantile_hour0,
            quantile_hour1,
            quantile_hour2,
            quantile_hour3,
            quantile_hour4,
            quantile_hour5,
            quantile_hour6,
            quantile_hour7,
            quantile_hour8,
            quantile_hour9,
            quantile_hour10,
            quantile_hour11,
            quantile_hour12,
            quantile_hour13,
            quantile_hour14,
            quantile_hour15,
            quantile_hour16,
            quantile_hour17,
            quantile_hour18,
            quantile_hour19,
            quantile_hour20,
            quantile_hour21,
            quantile_hour22,
            quantile_hour23)

q_all

boxplot(lasso_predicted[,50:73])
