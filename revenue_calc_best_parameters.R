#calclate revenue if we used optimal parameters
performance<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//performance.product.model.newusers.csv") 
best_parameters<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//best_parameters.csv") 
price_list<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//price.per.sku.csv")
category<-price_list


best_parameters


category<-category[,c(1,3)]
unique(category$Product.Category..Enhanced.E.commerce.) #we have 3 different contact lenses

#we change them all to the same
category$Product.Category..Enhanced.E.commerce.[category$Product.Category..Enhanced.E.commerce.=="Contact-lenses"]<-"Contact lenses"
category$Product.Category..Enhanced.E.commerce.[category$Product.Category..Enhanced.E.commerce.=="Contact-Lenses"]<-"Contact lenses"

unique(category$Product.Category..Enhanced.E.commerce.) #all good now

#add categories to performance dataset
performance<-merge(performance,category,by = "Product.SKU" )

#filter prices, by minimums seen in website

#add prices to non buyers
non_buyers<-performance[performance$Unique.Purchases==0,]
buyers<-performance[performance$Unique.Purchases>0,]
non_buyers[,12]<-NULL

#Load price list-------------------------------------

price_list<-price_list[,c(1,4)]
non_buyers<-merge(non_buyers,price_list,by = "Product.SKU" )
#-------------------------------------------
performance<-rbind(non_buyers,buyers)


sunglasses<-performance[performance$Product.Category..Enhanced.E.commerce.=="Sunglasses",]
glasses<-performance[performance$Product.Category..Enhanced.E.commerce.=="Optical",]
accesories<-performance[performance$Product.Category..Enhanced.E.commerce.=="Accessories",]
contact_lenses<-performance[performance$Product.Category..Enhanced.E.commerce.=="Contact lenses",]


sunglasses<-sunglasses[sunglasses$Avg..Price>=10,]
glasses<-glasses[glasses$Avg..Price>=25,]
accesories<-accesories[accesories$Avg..Price>=1,]
contact_lenses<-contact_lenses[contact_lenses$Avg..Price>=8,]

performance_clean<-rbind(sunglasses,glasses,accesories,contact_lenses)

#only keep products that were actually seen by users
performance_clean<-performance_clean[performance_clean$Product.Detail.Views>0,]

#Some users have more than one view, which could bias the results, the buy or no buy 
#may depend on the number of detail views instead of the price, we can filter for users that
#only viewed the product once

hj<-performance_clean[performance_clean$Product.Detail.Views==1,]
length(performance_clean$Product.Detail.Views)-length(hj$Product.Detail.Views)

performance_clean<-hj
#----------------------------------------------------------

rango<-length(best_parameters$Model)


df2<-data.frame()
jnum=0
for (i in 1:rango){
  
  training1<-performance_clean[performance_clean$Product.Category..Enhanced.E.commerce.==best_parameters$Category[i],]
  
  #progress-----------------------
  jnum=jnum+1
  
  progres<-(jnum/rango)*100
  
  print(paste0(round(progres, digits = 1)," % complete"))
  #-----------------------------------
  
  model_stats<-vector()
  sens_stats<-vector()
  spec_stats<-vector()
  
  #pre-calculations
  
  a<-training1[training1$Mobile.Device.Info==best_parameters$Model[i],]
  b<-a[a$Unique.Purchases>0,] #bought
  sb<-b[b$Product.Detail.Views>0,] #saw and bought
  db<-a[a$Unique.Purchases==0,] #didn't bought
  sdb<-db[db$Product.Detail.Views>0,] #saw and didn't buy
  
  j<-best_parameters$Upper_Value[i]
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
  stats2<-c(best_parameters$Model[i],best_parameters$Category[i],j,prob_revenue,moderate_lost_revenue,pesimist_lost_revenue,
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



write.csv(df2,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//best_parameters_revenue.csv")

