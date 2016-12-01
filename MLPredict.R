#Cforest
forest_Data<-read.csv("C:\\Users\\gowtham\\Downloads\\RankMiner\\Week4\\013_Agent116_Call_FeaturesErikCombo.csv")
install.packages("party")
library(party)
# forest_Data_mat<-data.matrix(forest_Data)
# set.seed(2)
# split<-sample(nrow(forest_Data_mat),floor(0.7*nrow(forest_Data_mat)))
# train<-forest_Data[split,]
# test<-forest_Data[-split,]
# nrow(test)
# train<-train[complete.cases(train),]
# test<-test[complete.cases(test),]
# typeof(train)
# attach(forest_Data)
# attach(train)

# fit_cforst=cforest(as.factor(target_value)~Unresolved_Perc+PTP_Arranged_Perc+
#                      Operator_Transfer_Perc+CUST_RPC_PTP_1_Perc+CUST_4_Perc+AGENT_CUST_3_Perc+
#                      Attorney_Handling_Perc+Debtor_dispute_Perc+DNC_Perc+Bankrupt_Perc+
#                      Deceased_Perc+train$Feat1_Callstat_mean+train$Feat2_NegEmo_mean+
#                      train$Feat3_PosEmo_mean+train$Feat4_PropEmo_mean,data=train,controls=cforest_unbiased(ntree=100, mtry=3))
# test$Feat1_Callstat_mean

fit_cforst=cforest(as.factor(target_value)~Unresolved_Perc+PTP_Arranged_Perc+
                     Operator_Transfer_Perc+CUST_RPC_PTP_1_Perc+CUST_4_Perc+AGENT_CUST_3_Perc+
                     Attorney_Handling_Perc+Debtor_dispute_Perc+DNC_Perc+Bankrupt_Perc+
                     Deceased_Perc+erik_data$Feat1_Callstat_mean+erik_data$Feat2_NegEmo_mean+
                     erik_data$Feat3_PosEmo_mean+erik_data$Feat4_PropEmo_mean,data=forest_Data,controls=cforest_unbiased(ntree=100, mtry=3))
names(train)
Prediction_cforest<-predict(fit_cforst,type="response")
table(forest_Data$target_value,Prediction_cforest)
?cforest
nrow(test)
table(forest_Data$target_value,Prediction_cforest)


-----------------------------------------------------------------------------------------
  
  #Week-3
#Finding unique agent_id's to get set of feature values 
  FinalAgent_PCA<-NULL
FinalAgent_PCA=read.csv("C:\\Users\\gowtham\\Downloads\\011_Agent_Call_Features_Clean_Scaled.csv")

st=FinalAgent$agent_id
st=as.character(st)
st=st[!duplicated(st)]

l[[3]]
length(l)
#Create dataframe for each agent with all variables
for(i in 1:116)
{
  print(i)
  l[[i]]<-assign(paste0("AgentID_PC1_",st[i]),value =FinalAgent_PCA[which(FinalAgent_PCA$agent_id==st[i]),])
}
l[[1]]
temp<-NULL
#Finding mean values of each feature
temp<-NULL
for(i in 1:116)
{
  temp<-as.data.frame(l[i])
  
  for (n in 2:177)
  {
    temp[[paste0("feature_value_mean",n)]]<-mean(temp[,n])
  }
  l[[i]]<-temp
}

temp1=as.data.frame(l[1])[1,]
temp1<-NULL
for(i in 1:116)
{
  temp1<-rbind(temp1,as.data.frame(l[i])[1,])
  print(temp1$agent_id)
}
nrow(temp1)
write.csv(temp1,"C:\\Users\\gowtham\\Downloads\\008_PCA.csv")
names(temp1)
-----------------------------------------------------------------------------------------------------------------
#PCA :
PCA_data_final<-NULL
PCA_data_final<-read.csv("C:\\Users\\gowtham\\Downloads\\008_PCA.csv")
PCA_data_final<-PCA_data_final[,-177]
PCA_data_final<-PCA_data_final[,-1]

names(PCA_data_final)
PCZ<-NULL
PCZ<-prcomp(PCA_data_final,cor = TRUE,scores = TRUE)
summary(PCZ)  
PC1<-NULL  
write.csv(PCZ$x,"C:\\Users\\gowtham\\Downloads\\008_PCA_2.csv")
-----------------------------------------------------------------------------------------------------------------------
#GLM on PCA values 
tPC1<-NULL
tPC1<-read.csv("C:\\Users\\gowtham\\Downloads\\008_PCA_2.csv")
attach(tPC1)
tPC1=tPC1[-1,]
nrow(tPC1)
Mean_Fit<-glm(target_value~tPC1$PC1+PC2+PC3+PC4+PC5)
summary(Mean_Fit)  
PMean<-predict(Mean_Fit,response="TRUE")  
model_pred_target<-rep(0,116)
model_pred_target[PMean>0.5]=1
model_pred_target<-NULL
table(tPC1$target_value,model_pred_target)


length(tPC1$target_value)
length(model_pred_target)
screeplot(PCZ,type="line",main = "Screen Plot")

