performance<-read.csv("C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//performance.product.model.newusers.csv")
buyers<-performance[performance$Unique.Purchases>0,]

model_counts<-as.data.frame(table(buyers$Mobile.Device.Info))
filter_model_counts<-model_counts[model_counts$Freq>40,]
models<-unique(as.vector(filter_model_counts$Var1))
models



filter_buyers<-buyers[(buyers$Mobile.Device.Info%in%models),]
filter_buyers

a<-pairwise.wilcox.test(filter_buyers$Avg..Price, filter_buyers$Mobile.Device.Info,
                     p.adjust.method = "BH")

write.csv(a$p.value,file="C://Users//escoloni//Desktop//grandvision//project_1//v4.5//test//stat_differences_models.csv")

