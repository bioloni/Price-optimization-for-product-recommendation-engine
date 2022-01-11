#validation of ranges
ranges<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//best_parameters.csv")
rango<-length(ranges$Model)

sensitivity_train<-ranges$Sensitivity
specificity_train<-ranges$Specificity

df2<-data.frame()
jnum=0
for (i in 1:rango){
  
  training1<-validation[validation$Product.Category..Enhanced.E.commerce.==ranges$Category[i],]
  
  #progress-----------------------
  jnum=jnum+1
 
  progres<-(jnum/rango)*100
  
  print(paste0(round(progres, digits = 1)," % complete"))
  #-----------------------------------
    
  model_stats<-vector()
  sens_stats<-vector()
  spec_stats<-vector()
    
  #pre-calculations
  
  a<-training1[training1$Mobile.Device.Info==ranges$Model[i],]
  b<-a[a$Unique.Purchases>0,] #bought
  sb<-b[b$Product.Detail.Views>0,] #saw and bought
  db<-a[a$Unique.Purchases==0,] #didn't bought
  sdb<-db[db$Product.Detail.Views>0,] #saw and didn't buy
    
  j<-ranges$Upper_Value[i]
  #if they see something in range they are classified as buyers
  sbir<-sb[sb$Avg..Price<=j,]  #If they buy it #TP
  sdbir<-sdb[sdb$Avg..Price<=j,]#If they don't buy it #FP
      
  #If they see something outside of range they are classified as non buyers
  sbor<-sb[sb$Avg..Price>j,]# if they buy it #FN
  sdbor<-sdb[sdb$Avg..Price>j,] #if they don't buy it #TN
      
      

  #-------use all the sensisbility and specificity values to calculate which range gives maximum
  #------ revenue
        
  #we have how many people saw something OR and didn't buy it, what if they had seen something IR
  #How many of those people would have made a purchase (hidden buyers)?
  #First We need to calculate probability of purchase for this group:
  #this can be calculated as n° of purchases IR/n° of views IR
  conv_prob<-as.numeric(length(sbir$Product)/(length(sbir$Product)+length(sdbir$Product)))
      
  #Then we can multiply this probability by the total number of non-buying users that saw a product
  #outside of range
  hidden_buyers<-length(sdbor$Product)*conv_prob
      
  #if we then multiply the number of hidden buyers by the avg. product price purchased for that range,
  #we get the approximate revenue that this group would bring
  avg_price<-mean(sbir$Avg..Price)
  prob_revenue<-avg_price*hidden_buyers
        
  #We are also affecting users that were buying products even though they were OR (SBOR)
  #This sbor users will become: non-buyers, sbir, or stay sbor
  #First we calculate avg price of sbor buyers
  avg_lost_price<-mean(sbor$Avg..Price)
        
  #First outcome, we lose all customers from SBOR
  pesimist_lost_revenue<-avg_lost_price*length(sbor$Mobile.Device.Info)
        
  #Second outcome, they transform to SBIR, instead of paying SBOR avg price, they pay SBIR avg. price
  moderate_lost_revenue<-pesimist_lost_revenue-(avg_price*length(sbor$Mobile.Device.Info))
        
  #Third outcome, they stay as SBOR buyers, in this scenario we dont lose revenue
  #optimist_lost_revenue<-0
        
        
  #output
  stats2<-c(ranges$Model[i],ranges$Category[i],j,prob_revenue,moderate_lost_revenue,pesimist_lost_revenue,
                  length(sbir$Product),length(sdbir$Product),length(sdbor$Product),length(sbor$Product),
                  conv_prob,hidden_buyers,avg_price,avg_lost_price)
  df2<-rbind(df2,stats2)
        
        
        
   
        
      
  
    
  
  
}

#cleaning
colnames(df2)<-c("Model","Category","Upper_Value","Gain_Revenue","Moderate_Lost_revenue","Pesimist_lost_revenue",
                 "TP","FP","TN","FN","Conv_Probability","Hidden_Buyers","Avg.SBIR_price","Avg.SDBOR_price")
df2$Gain_Revenue<-as.numeric(df2$Gain_Revenue)
df2$Moderate_Lost_revenue<-as.numeric(df2$Moderate_Lost_revenue)
df2$Pesimist_lost_revenue<-as.numeric(df2$Pesimist_lost_revenue)
df2$Conv_Probability<-as.numeric(df2$Conv_Probability)
df2$Hidden_Buyers<-as.numeric(df2$Hidden_Buyers)
df2$TP<-as.numeric(df2$TP)
df2$FP<-as.numeric(df2$FP)
df2$TN<-as.numeric(df2$TN)
df2$FN<-as.numeric(df2$FN)
df2$Avg.SBIR_price<-as.numeric(df2$Avg.SBIR_price)
df2$Avg.SDBOR_price<-as.numeric(df2$Avg.SDBOR_price)

#extra calculation
df2$Sensitivity<-df2$TP/(df2$TP+df2$FN)
df2$Specificity<-df2$TN/(df2$TN+df2$FP)
df2$Accuracy<-(df2$TN+df2$TP)/(df2$TN+df2$FN+df2$TP+df2$FP)
df2$avg<-(df2$Sensitivity+df2$Specificity)/2
df2$net_revenue_pesimist<-df2$Gain_Revenue-df2$Pesimist_lost_revenue
df2$net_revenue_moderate<-df2$Gain_Revenue-df2$Moderate_Lost_revenue

#remove nan
df2$Gain_Revenue[which(is.na(df2$Gain_Revenue))]<-0
df2$Moderate_Lost_revenue[which(is.na(df2$Moderate_Lost_revenue))]<-0
df2$Pesimist_lost_revenue[which(is.na(df2$Pesimist_lost_revenue))]<-0
df2$net_revenue_pesimist[which(is.na(df2$net_revenue_pesimist))]<-0
df2$net_revenue_moderate[which(is.na(df2$net_revenue_moderate))]<-0
df2$Sensitivity[which(is.na(df2$Sensitivity))]<-0
df2$Specificity[which(is.na(df2$Specificity))]<-0
df2$avg[which(is.na(df2$avg))]<-0
df2$Conv_Probability[which(is.na(df2$Conv_Probability))]<-0
df2$Hidden_Buyers[which(is.na(df2$Hidden_Buyers))]<-0
df2$Avg.SBIR_price[which(is.na(df2$Avg.SBIR_price))]<-0
df2$Avg.SDBOR_price[which(is.na(df2$Avg.SDBOR_price))]<-0


write.csv(df2,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//validation.csv")


#--------------------------------comparison


dif_sensitivity<-abs(df2$Sensitivity-sensitivity_train)
dif_specificity<-abs(df2$Specificity-specificity_train)

df3<-data.frame(dif_sensitivity,dif_specificity)
df3
write.csv(df3,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//val_results.csv")
