performance<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//performance.product.model.newusers.csv") 

performance<-performance[performance$Unique.Purchases>0,]



library(dplyr)
library("ggpubr")

t<-unique(performance$Mobile.Device.Info)
t<-as.vector(t)
brands_original<-performance


promedios<-data.frame()
pvalores<-data.frame()
counts<-vector()
modelos_ok<-vector()
contador=0



for (h in t) {
  model<-h
  model
  p<-brands_original[brands_original$Mobile.Device.Info==model,]
  g<-length(p$Mobile.Device.Info)
  if (g>=40){
    brands<-brands_original
    brands$Mobile.Device.Info[brands$Mobile.Device.Info!=model]<-"Other"
    brands$Mobile.Device.Info[brands$Mobile.Device.Info==model]<-"Model"
    
    b<-pairwise.wilcox.test(brands$Avg..Price, brands$Mobile.Device.Info,
                            p.adjust.method = "BH")
    
    if (b$p.value<0.05) {
      contador=contador+1
      a<-group_by(brands, Mobile.Device.Info) %>%
        summarise(
          count = n(),
          mean = mean(Avg..Price, na.rm = TRUE),
          sd = sd(Avg..Price, na.rm = TRUE),
          median = median(Avg..Price, na.rm = TRUE),
          IQR = IQR(Avg..Price, na.rm = TRUE)
        )
      
      #write.csv(as.data.frame(a),file=paste0("C://Users//admin//Desktop//resultados//",model,"-means",".csv"))
      #png(filename=paste0("C://Users//admin//Desktop//resultados//",model,"-boxplot",".png"))
      
      #y<-ggboxplot(brands, x = "Mobile.Device.Info", y = "Avg..Price", 
       #            color = "Mobile.Device.Info", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
        #           order =  ,
         #          ylab = "Avg. Price", xlab = " Model")
      #print(ggpar(y, x.text.angle = 40, legend="none"))
      #dev.off()
      #png(filename=paste0("C://Users//admin//Desktop//resultados//",model,"-meanplot",".png"))
      #z<-ggline(brands, x = "Mobile.Device.Info", y = "Avg..Price", 
      #          add = c("mean_se", "jitter"), 
       #         order = unique(brands$Mobile.Device.Info),
        #        ylab = "Avg. Price", 
         #       xlab = "Device")
      
      #print(z)
      
      #dev.off()
      #write.csv(b$p.value,file=paste0("C://Users//admin//Desktop//resultados//",model,"p-value",".csv"))
      
      print(a)
      promedios[contador,1]<-a$mean[1] #modelo
      promedios[contador,2]<-a$mean[2] #otros
      pvalores[contador,1]<-b$p.value[1]
      modelos_ok[contador]<-model
      counts[contador]<-a$count[1]
      
    }
    
    
  }
  
}


rownames(promedios)<-modelos_ok


promedios
promedios[,3]<-pvalores
promedios[,4]<-counts

colnames(promedios)<-c("Model","Other","P values","Model Counts")
promedios

write.csv(promedios, file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//stat_test_vs_all.csv")
#------------------------------------------------------------------------------------------------


