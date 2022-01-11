#learning algorithm, done with new users
performance<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//performance.product.model.newusers.csv")
price_list<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//price.per.sku.csv")
category<-price_list
all_results<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1///v4.5//test//complete_raw_results.csv")


#-----------------cleanning
#----------------------------------------
#the minimum sunglasses price in the website is 10pounds, prices below than that will be filtered
#this prices may comes from errors, or discounts
#cheapest glasses are 25 pounds, again we will filter glasses by this value
#cheapest contact lenses are 8pounds, we will filter by these
#cheapest accesory is 1 pounds

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
#-------------------------------------------------------------------------
#----------------------------------------------------------
#Load ranges
ranges1<-seq(from=1, to=100, by= 2)
ranges2<-seq(from=101, to=200, by= 10)
ranges<-c(ranges1,ranges2)

#get categories
uniq_categories<-unique(performance_clean$Product.Category..Enhanced.E.commerce.)
#-----------------------------------------

#there are a lot of models, some have very little data, we will filter those

g<-as.data.frame(table(performance_clean$Mobile.Device.Info))
g<-g[g$Freq>1000,]
performance_clean<-performance_clean[which(performance_clean$Mobile.Device.Info%in%g$Var1),]

#get models list
modelos<-unique(performance_clean$Mobile.Device.Info)
length(modelos)
#--------------------------------------------------------------------

total<-as.numeric(length(uniq_categories))*as.numeric(length(modelos))
df2<-data.frame()
jnum=0
for (o in uniq_categories){
  
  
  training1<-performance_clean[performance_clean$Product.Category..Enhanced.E.commerce.==o,]
  
  
  
  
  for (i in 1:length(modelos)){
    
    #progress-----------------------
    jnum=jnum+1
    progres<-jnum/total*100
    print(paste0(round(progres, digits = 1)," % complete"))
    #-----------------------------------
    
    model_stats<-vector()
    sens_stats<-vector()
    spec_stats<-vector()
    
    #pre-calculations
    #a<-performance1[performance1$Mobile.Device.Info==modelos[i],]
    
    a<-training1[training1$Mobile.Device.Info==modelos[i],]
    b<-a[a$Unique.Purchases>0,] #bought
    db<-a[a$Unique.Purchases==0,] #didn't bought

    
    for (j in ranges ){
      
      
      #if they see something in range they are classified as buyers
      sbir<-b[b$Avg..Price<=j,]  #If they buy it #TP
      
      sdbir<-db[db$Avg..Price<=j,]#If they don't buy it #FP
      
      #If they see something outside of range they are classified as non buyers
      sbor<-b[b$Avg..Price>j,]# if they buy it #FN
      sdbor<-db[sdb$Avg..Price>j,] #if they don't buy it #TN
      
      
      TP<-as.numeric(length(sbir$Product.SKU))
      TN<-as.numeric(length(sdbor$Product.SKU))
      FP<-as.numeric(length(sdbir$Product.SKU))
      FN<-as.numeric(length(sbor$Product.SKU))
      
      
      #suma_conv<-as.numeric(length(sbir$Product.SKU))+as.numeric(length(sdbir$Product.SKU))
      suma_tuto<-TP+TN+FP+FN
      
      
      
      if (suma_tuto>40){
        if (TP > 0){
          if (TN >0){
            if (FP>0){
              if (FN >0){
                #-------use all the sensisbility and specificity values to calculate which range gives maximum
                #------ revenue
                
                #we have how many people saw something OR and didn't buy it, what if they had seen something IR
                #How many of those people would have made a purchase (hidden buyers)?
                #First We need to calculate probability of purchase for this group:
                #this can be calculated as n° of purchases IR/n° of views IR
                conv_prob<-TP/(TP+FP)
                
                #Then we can multiply this probability by the total number of non-buying users that saw a product
                #outside of range
                hidden_buyers<-TN*conv_prob
                
                #if we then multiply the number of hidden buyers by the avg. product price purchased for that range,
                #we get the approximate revenue that this group would bring
                avg_price<-mean(sbir$Avg..Price)
                prob_revenue<-avg_price*hidden_buyers
                
                #We are also affecting users that were buying products even though they were OR (SBOR)
                #This sbor users will become: non-buyers, sbir, or stay sbor
                #First we calculate avg price of sbor buyers
                avg_lost_price<-mean(sbor$Avg..Price)
                
                #First outcome, we lose all customers from SBOR
                pesimist_lost_revenue<-avg_lost_price*FN
                
                #Second outcome, they transform to SBIR, instead of paying SBOR avg price, they pay SBIR avg. price
                moderate_lost_revenue<-pesimist_lost_revenue-(avg_price*FN)
                
                #Third outcome, they stay as SBOR buyers, in this scenario we dont lose revenue
                #optimist_lost_revenue<-0
                
                
                #output
                stats2<-c(modelos[i],o,j,prob_revenue,moderate_lost_revenue,pesimist_lost_revenue,
                          conv_prob,hidden_buyers,avg_price,avg_lost_price,TP,FP,TN,FN)
                df2<-rbind(df2,stats2)
                
                
              }
            }
          }
        }
        
        
       
        
        
        
        
        
        
      }
    }
    
  } 
  
}



colnames(df2)<-c("Model","Category","Upper_Value","Gain_revenue","Mod_Lost_revenue","Pes_Lost_revenue","Conv. Prob","Hidden_buyers",
                 "Avg_SBIR_price","Avg_SBOR_price")


df99<-df2

df2$Gain_revenue<-as.numeric(df2$Gain_revenue)
df2$Mod_Lost_revenue<-as.numeric(df2$Mod_Lost_revenue)
df2$Pes_Lost_revenue<-as.numeric(df2$Pes_Lost_revenue)
df2$net_revenue_pesimist<-df2$Gain_revenue-df2$Pes_Lost_revenue
df2$net_revenue_moderate<-df2$Gain_revenue-df2$Mod_Lost_revenue

df2
#find ranges that give the maximum revenue

largodf2<-length(df2$Model)

best_price_pes<-data.frame()
#Pesimist
for (i in 1:largodf2 ){
    
    df3<-df2[df2$Category==df2$Category[i],]
    df3<-df3[df3$Model==df2$Model[i],]
    max_revenue<-max(df3$net_revenue_pesimist)
    df3
    if (max_revenue >0){
      max_index<-which(df3$net_revenue_pesimist==max_revenue)
      if (length(max_index==1)){
        best_price_pes<-rbind(best_price_pes,df3[max_index,])
      if (length(max_index>1)){
        po<-tail(df3[max_index,],n=1L)
        best_price_pes<-rbind(best_price_pes,po)
      }
        
        
      }
      
    }
    
}

best_price_pes<-(unique(best_price_pes))
best_price_pes<-best_price_pes[best_price_pes$net_revenue_pesimist>0,]

write.csv(best_price_pes,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//best_price_pes.csv")

best_price_mod<-data.frame()
#Pesimist
for (i in 1:largodf2 ){
  
  df3<-df2[df2$Category==df2$Category[i],]
  df3<-df3[df3$Model==df2$Model[i],]
  max_revenue<-max(df3$net_revenue_moderate)
  df3
  if (max_revenue >0){
    max_index<-which(df3$net_revenue_moderate==max_revenue)
    if (length(max_index==1)){
      best_price_mod<-rbind(best_price_mod,df3[max_index,])
      if (length(max_index>1)){
        po<-tail(df3[max_index,],n=1L)
        best_price_mod<-rbind(best_price_mod,po)
      }
      
      
    }
    
  }
  
}

best_price_mod<-(unique(best_price_mod))
best_price_mod<-best_price_mod[best_price_mod$net_revenue_moderate>0,]

write.csv(best_price_pes,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//best_price_mod.csv")
