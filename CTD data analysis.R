p1=read.csv("E:/mcmc_test/Nutrient'_Nabo_Apu/CTD.csv")
p2=read.csv("E:\\mcmc_test//CTD_2.csv")
common1=p2 %>% 
  inner_join(p1,by=c("Latitude","Longitude"))
write.csv(common1,file ="E:/mcmc_test/Nutrient11_1.csv",row.names = F )
attach(p2)
head(p2)
p3=p2 %>% 
  dplyr::select(Latitude,Longitude,Month,Temperature,Chl_A,Salinity) %>% 
  dplyr::filter(p2$Press<=1 & Salinity>12 & Temperature>25 & Month==2)
p4=p3 %>% 
  dplyr::group_by(Latitude,Longitude) %>% 
  dplyr::summarise(mean_temp=mean(Temperature),
            mean_Sal=mean(Salinity),
            mean_chla=mean(Chl_A),
            mean_month=mean(Month))

write.csv(p3,file ="E:/mcmc_test/feb.csv",row.names = F )
#########################################################################
d1=read.csv("E:/mcmc_test/Last_Analysis/CTD_2.csv")
d2=read.csv("E:/mcmc_test/Last_Analysis/Location.csv")
d3=read.csv("E:/mcmc_test/Last_Analysis/CTD_3.csv")
c1=d1%>% 
  inner_join(d2,by=c("Latitude","Longitude"))
write.csv(c1,file ="E:/mcmc_test/Last_Analysis/CTD_3.csv",row.names = F )

head(d3)

p3=d1 %>% 
  dplyr::group_by(Latitude,Longitude) %>%
  dplyr::filter(Press>=0.5&Salinity>=2) %>% 
  dplyr::select(Latitude,Longitude,Month,Temperature,Chl_A,Salinity)
p4=p3 %>% 
  dplyr::group_by(Latitude,Longitude) %>% 
  dplyr::summarise(mean_temp=mean(Temperature),
                   mean_Sal=mean(Salinity),
                   mean_chla=mean(Chl_A))
##########################Sample_extraction####################################
sample_size <- 180
sampled_data <- p3[sample(nrow(p3), sample_size), ]
hist(sampled_data$Salinity)
boxplot(sampled_data$Temperature)
write.csv(sampled_data,file ="E:/mcmc_test/Last_Analysis/data.csv",row.names = F )
######################## outliers check###########
sample_size <- 180
Q1 <- quantile(p3$Temperature, 0.25)
Q3 <- quantile(p3$Temperature, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
no_outliers_data <-p3$Temperature[p3$Temperature >= lower_bound & p3$Temperature <= upper_bound]
sampled_SST <- no_outliers_data[sample(length(no_outliers_data), sample_size)]
str(sampled_SST)
hist(sampled_SST, main="Histogram of Sampled Data (No Outliers)", xlab="Salinity")
boxplot(sampled_SST)
data=data.frame(SST=sampled_SST)
data = data[,c(1,3)]
data=cbind(data,Sal=sampled_Sal)

######## For Sal #####################
sample_size <- 180
Q1 <- quantile(p3$Salinity, 0.25)
Q3 <- quantile(p3$Salinity, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
no_outliers_data <-p3$Salinity[p3$Salinity >= lower_bound & p3$Salinity <= upper_bound]
sampled_Sal <- no_outliers_data[sample(length(no_outliers_data), sample_size)]
str(sampled_Sal)
hist(sampled_Sal, main="Histogram of Sampled Data (No Outliers)", xlab="Salinity")
boxplot(sampled_Sal)

##########################################################
data= cbind(data,p2[,3:9])













write.csv(p4,file ="E:/mcmc_test/Last_Analysis/CTD_mean.csv",row.names = F )

#####################################################################################
d1=read.csv("E:/mcmc_test/Last_Analysis/all data.csv")
attach(d1)
 
# Create a directory to save the CSV files (change the path accordingly)
output_dir <- "E:/mcmc_test/Last_Analysis"
dir.create(output_dir, showWarnings = FALSE)

# Split the dataset by Month and save each group to a separate CSV file
split_data <- split(d1, d1$Month)

for (Month in names(split_data)) {
  month_data <- split_data[[Month]]
  month_csv_file <- file.path(output_dir, paste0(Month, ".csv"))
  write.csv(month_data, month_csv_file, row.names = FALSE)
}

###############################Pivot-longer 
c1=read.csv("E:/mcmc_test/Station Smaples/Station 4.csv")
attach(c1)
head(c1)
c=c1 %>% 
  pivot_longer(cols = c(Coscinodiscus.argus:Chaetoceros.sp),
               names_to = "month",
               values_to = "abundance")
write.csv(c,file ="E:/mcmc_test/Station Smaples/Station_4.csv",row.names = F )



################################################## pivot_Wider###################################
p1=read.csv("E:/mcmc_test/Last_Analysis/combine_fixed1.csv")
head(p1)
attach(p1)

p2=p1 %>%
  dplyr::group_by(Species,Abundance) %>% 
  tidyr::summarize() %>% 
  pivot_wider(names_from = Species,values_from  = Abundance)
  
