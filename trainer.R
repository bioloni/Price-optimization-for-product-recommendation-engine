#trainer
performance<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//performance.product.model.newusers.csv")
price_list<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//price.per.sku.csv")
category<-price_list

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
#-------------Learning algorithm---------------------------------------------------------------------------------------
#Model will try to classify users in buyers or non buyers based on the price of the product they see
#--------------------------------------------------

#Classes are unbalanced-------------------------------------------------------------------------------------------------
dataset_buyers<-performance_clean[performance_clean$Unique.Purchases>0,]
dataset_nonbuyers<-performance_clean[performance_clean$Unique.Purchases==0,]

rownames(dataset_buyers)<-NULL
rownames(dataset_nonbuyers)<-NULL

a<-length(dataset_buyers$Product.SKU)
b<-length(dataset_nonbuyers$Product.SKU)
#--------------------------------------------------
#how does it look like if i undersample the non buyers
#largo<-a/2


#library(dplyr)
#training_buy<-sample_n(dataset_buyers,largo)
#validation_buy<-dataset_buyers[!row.names(dataset_buyers)%in%row.names(training_buy),]
#length(training_buy$Product.SKU)
#length(validation_buy$Product.SKU)

#training_nobuy<-sample_n(dataset_nonbuyers,largo) #random dataset of same length as buyers class
#validation_nobuy<-dataset_nonbuyers[!row.names(dataset_nonbuyers)%in%row.names(training_nobuy),] #rest of data
#length(validation_nobuy$Product.SKU)
#validation_nobuy<-sample_n(validation_nobuy,length(validation_buy$Product.SKU)) #reduce size of rest of data to saame length as buyers class

#length(training_nobuy$Product.SKU)
#length(validation_nobuy$Product.SKU)

#assemble datasets
#training<-rbind(training_buy,training_nobuy)
#validation<-rbind(validation_buy,validation_nobuy)

#length(training$Product.SKU)
#length(validation$Product.SKU)
#--------------------------------------------------
#how does it look like if i oversample the buyers

c<-round(b/a,digits = 0) # how many times longer is the non buyers list

db<-data.frame()
for (i in 1:c){
  kk<-i/c*100
  kk
  print(paste(round(kk,digits=1)," % complete"))
  db<-rbind(db,dataset_buyers)
  
}
dataset_buyers<-db

d<-length(dataset_nonbuyers$Product.SKU)-length(db$Product.SKU)
dl<-length(dataset_nonbuyers$Product.SKU)
d
library(dplyr)
e<-sample(1:dl,d)# generar x cantidad de indices entre 1 y el maximo de observaicones de non buyer
e
f<-dataset_nonbuyers[!row.names(dataset_nonbuyers)%in%e,] # le saco las observaciones que sobran
f
dataset_nonbuyers<-f
length(dataset_nonbuyers$Product.SKU)-length(db$Product.SKU)

largo<-length(dataset_buyers$Product.SKU)         
largo
length(dataset_nonbuyers$Product.SKU)         

seq1<-1:largo
ind1<-sample(1:largo,largo/2)
ind2<-seq1[!seq1%in%ind1]
ind1
ind2
row.names(dataset_buyers)<-NULL
row.names(dataset_nonbuyers)<-NULL
training_buy<-dataset_buyers[row.names(dataset_buyers)%in%ind1,]
training_nobuy<-dataset_nonbuyers[row.names(dataset_nonbuyers)%in%ind1,]
length(training_buy$Product.SKU)
length(training_nobuy$Product.SKU)

validation_buy<-dataset_buyers[row.names(dataset_buyers)%in%ind2,]
validation_nobuy<-dataset_nonbuyers[row.names(dataset_nonbuyers)%in%ind2,]
length(validation_nobuy$Product.SKU)
length(validation_buy$Product.SKU)


training<-rbind(training_buy,training_nobuy)
validation<-rbind(validation_buy,validation_nobuy)


length(training$Product.SKU)
length(validation$Product.SKU)
training
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

total<-as.numeric(length(uniq_categories))*as.numeric(length(modelos))
df2<-data.frame()
jnum=0
for (o in uniq_categories){
  
  
  training1<-training[training$Product.Category..Enhanced.E.commerce.==o,]
  #performance1<-performance_clean[performance_clean$Product.Category..Enhanced.E.commerce.==o,]
  
  
   
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
    sb<-b[b$Product.Detail.Views>0,] #saw and bought
    db<-a[a$Unique.Purchases==0,] #didn't bought
    sdb<-db[db$Product.Detail.Views>0,] #saw and didn't buy
    

    for (j in ranges ){
      
      
      #if they see something in range they are classified as buyers
      sbir<-sb[sb$Avg..Price<=j,]  #If they buy it #TP
      sdbir<-sdb[sdb$Avg..Price<=j,]#If they don't buy it #FP
      
      #If they see something outside of range they are classified as non buyers
      sbor<-sb[sb$Avg..Price>j,]# if they buy it #FN
      sdbor<-sdb[sdb$Avg..Price>j,] #if they don't buy it #TN
      
      #suma_conv<-as.numeric(length(sbir$Product.SKU))+as.numeric(length(sdbir$Product.SKU))
      suma_tuto<-as.numeric(length(sbir$Product.SKU))+as.numeric(length(sdbir$Product.SKU))+
        as.numeric(length(sbor$Product.SKU))+as.numeric(length(sdbor$Product.SKU))
      
      
      if (suma_tuto>40){
        
        #output
        stats2<-c(modelos[i],o,j,length(sbir$Product),length(sdbir$Product),length(sdbor$Product),length(sbor$Product))
        df2<-rbind(df2,stats2)
        
        
        
        
        
        
      }
    }
    
  } 
        
}
      

df2
#cleaning
colnames(df2)<-c("Model","Category","Upper_Value","TP","FP","TN","FN")
df

df2$TP<-as.numeric(df2$TP)
df2$FP<-as.numeric(df2$FP)
df2$TN<-as.numeric(df2$TN)
df2$FN<-as.numeric(df2$FN)
df2$total_counts<-df2$TP+df2$FP+df2$TN+df2$FN
#extra calculation
df2$Sensitivity<-df2$TP/(df2$TP+df2$FN)
df2$Specificity<-df2$TN/(df2$TN+df2$FP)
df2$Accuracy<-(df2$TN+df2$TP)/(df2$TN+df2$FN+df2$TP+df2$FP)
df2$avg<-(df2$Sensitivity+df2$Specificity)/2

#remove nan
df2$Sensitivity[which(is.na(df2$Sensitivity))]<-0
df2$Specificity[which(is.na(df2$Specificity))]<-0
df2$avg[which(is.na(df2$avg))]<-0



write.csv(df2,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//complete_raw_results.csv")



#check distribution of variables-------------------
#par(mfrow=c(1,2))
#for (i in 5:6){
#  u<-df2[df2$Model==modelos[i],]
#  u<-u[u$Category=="Sunglasses",]
#  
#  plot(u$Upper_Value,u$Gain_Revenue,main = paste(modelos[i]))
#}
#---------------------------------------------
#Extra filter to account for low values that pump the prob of conversion
#optimize for sensitivity and specificity

#clean all values obtained from last step
#filter observations with values too small that give inaccurate sensitivity calculations


df3<-df2[df2$avg!=0,]
df3<-df3[df3$FN>0,]
df3<-df3[df3$TP>0,]
df3<-df3[df3$FP>0,]
df3<-df3[df3$TN>0,]


best_parameters<-data.frame()
ran<-length(df3$Model)
for (i in 1:ran){
  #----------------------------
  kk<-i/ran*100
  print(paste(round(kk,digits=1)," % complete"))
  #----------------------------
  mode<-df3$Model[i]
  cat<-df3$Category[i]
  df4<-df3[df3$Category==cat,]
  df4<-df4[df4$Model==mode,]
  df4
  max_avg<-max(df4$avg)
  b<-which(df4$avg==max_avg)
  if (max_avg>0){
    if(length(b)==1){
      best_parameters<-rbind(best_parameters,df4[b,])
        
      }
    if(length(b)>1){
      b<-tail(b,n=1)
      best_parameters<-rbind(best_parameters,df4[b,])
      }
      
    }
  
  
}
best_parameters<-unique(best_parameters)





write.csv(best_parameters,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//best_parameters.csv",row.names = F)











#TN (vieron un product OR y no lo compraron)
#






