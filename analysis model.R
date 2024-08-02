getwd()
setwd("E:/Model for Paper")
x=readRDS("model_g_5.RDS",file = "E:/My Reserach Project/model_4.rda")
r=read.csv("E:/My Reserach Project/r.csv")
v2=round(matrix(x3$BUGSoutput$mean$gamma, byrow = TRUE, ncol = 20) ,6)
v3=as.data.frame(v2)
#################################################################
rm() 
gc()
memory.limit()
memory.limit(size=5000000000000)
x7=x$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress1")) %>% 
  dplyr::summarize(mean=mean(value)) %>% 
  mutate(species = rep(rep(unique(p2$Species2),180),400))
x1=mod_2$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress1")) %>% 
  dplyr::summarize(mean=mean(value))
x2=x$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress2")) %>% 
  dplyr::summarize(mean=mean(value)) 
x3=x$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress3")) %>% 
  dplyr::summarize(mean=mean(value)) 
x4=x$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress4")) %>% 
  dplyr::summarize(mean=mean(value)) 
x5=x$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress5")) %>% 
  dplyr::summarize(mean=mean(value)) 
x6=mod_2$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  #dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "gamma")) %>% 
  #dplyr::summarize(mean=mean(value)) 


df=list(x1,x2,x3,x4,x5,x6)
df2 = df %>% reduce(full_join,by=c("parameter","mean"))
df2$parameter[df2$parameter == paste0("stress",1,"[",10,"]")] <- 'Temperature'
df2
df2$parameter[df2$parameter == 'stress1[10]'] <- 'stress1[10]'


######################################################################
####################Step_1###############
df=list()
for (i in 1:5){
  data_1=mod_2$BUGSoutput$sims.matrix %>%
    as_tibble() %>%
    pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
    dplyr::group_by(parameter) %>% 
    dplyr::filter( str_detect(parameter, paste0("stress","[",i,"]"))) %>% 
    dplyr::filter(value>=0.3) %>% 
    dplyr::summarize(mean=mean(value))
  df[[i]]=data_1
}

df2 = df %>% reduce(full_join,by=c("parameter","mean"))
####################Step_2###############
for (i in 1:5) {
  for (j in 1:30) {
    if (i == 1) {
      df2$parameter[df2$parameter == paste0("stress",i,"[",j,"]")] <- 'Temperature'
    } else if (i == 2) {
      df2$parameter[df2$parameter == paste0("stress",i,"[",j,"]")] <- 'Salinity'
    }
    else if (i == 3) {
      df2$parameter[df2$parameter == paste0("stress",i,"[",j,"]")] <- 'Silicate'
    }
    else if (i == 4) {
      df2$parameter[df2$parameter == paste0("stress",i,"[",j,"]")] <- 'Nitrate'
    }
    else if (i == 5) {
      df2$parameter[df2$parameter == paste0("stress",i,"[",j,"]")] <- 'Phosphate'
    }
  }
}
####################Step_3###############
df2 %>% 
  mutate(where(is.numeric),~round(.,3)) %>% 
  mutate(species = rep(unique(v1$Species),5)) %>% 
  #arrange(-`mean`) %>% 
  ggplot(aes(x = fct_inorder(parameter), y = fct_inorder(species),fill= mean)) +
  geom_raster()+ 
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  #scale_fill_gradient2(low=muted('red'), mid='white', high=muted('blue'))+
  labs(x="Variable",y="Species")+
  theme_classic2()+
  #guides(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="italic",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))
#############################Explain Variance##############################################
df2 %>%
  dplyr::group_by(parameter) %>%
  dplyr::summarise(mean_value = mean(mean)) %>% 
  dplyr::mutate(percentage=(mean_value/sum(mean_value))*100) %>% 
  dplyr::select('parameter','percentage') %>% 
  ggplot(aes(x=fct_inorder(parameter),y=percentage,fill=parameter))+geom_col()+
  scale_x_discrete(limits = c('Temperature','Salinity','Silicate','Nitrate','Phosphate'))+
  labs(x="Variable",y="Explained Variance(%)")+theme_classic2()+
  theme(legend.position = 'None')+
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="italic",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))
sum(df2$mean)
write.csv(df2,file ="E:/mcmc_test/var.csv",row.names = F )
head(df3)
#################################### beta ##########################################

x1=x$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "gamma"))
  #dplyr::filter(value>0.1) %>% 
  #dplyr::summarize(mean=mean(value))
tempdir()
.libPaths("C:/Users/Mamun/Documents/R/new-library")
x1$parameter=gsub("\\[.*\\]", "",x1$parameter)
devtools::install_github("rpruim/CalvinBayes")
x1 %>% 
  ggplot(aes(x= value,y=parameter,fill = stat(x)))+
  geom_density_ridges(fill = "gray")
  #geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  #scale_fill_viridis_c(option = "C")

  #geom_bar()
  geom_density(linewidth=0.8,color='red')+
  #x=expression(Species~interaction~(alpha)))
  labs(y="Density",x='Growth rate(r)')+theme_bw()+theme(legend.position = 'None')+
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))

write.csv(x1,file ="E:/My Reserach Project/alpha.csv",row.names = F )
skewness(x1$value)
plot(x, trace = FALSE)




head(posterior(x))
gf_density(~ `gamma[2,5]`, data = posterior(x))
glimpse(posterior(x))
gf_dhistogram(~`gamma[2,5]`, data = posterior(x), bins = 50) %>% 
  gf_dens(~`gamma[2,5]`, size = 1.5, alpha = 0.8) %>%
  gf_dist("beta", shape1 = 16, shape2 = 36, color = "red")


plot_list <- list()
for (i in 1:5) {
  for (j in 1:30){
    param_name <- paste0("beta","[",j,",",i,"]")
    p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 20) %>% 
      gf_dens(~get(param_name), size = 1.5, alpha = 0.8)+xlab(param_name)
    #print(p)
    plot_list[[length(plot_list) + 1]] <- p
    }
  }

combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 6)
print(combined_plot)


plot_list <- list()
for (i in 1:5) {
  for (j in 1:30){
    param_name <- paste0("stress", i, ".", j)
    p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 50) %>% 
      gf_dens(~get(param_name), size = 1.5, alpha = 0.8)+xlab(param_name)
    #print(p)
    plot_list[[length(plot_list) + 1]] <- p
  }
  
}



##########trace plot
mcmc_trace(x, pars = 'beta[1,1]') %>%
  gf_facet_grid(chain~.) %>%
  gf_refine(scale_color_viridis_d())

mcmc_samples <- as.mcmc(x)
plot(x)
summary(x)
beta_samples <- x[, grep("beta", colnames(x))]
plot(mcmc(x, thin = 2))

#sample_size=150
#sampled_data <- x1[sample(nrow(x1), sample_size,replace = T), ]
#x1=sampled_data
###############################################################
split_df <- strsplit(as.character(x1$parameter), "[,\\[]")
split_df <- do.call(rbind.data.frame, split_df)
colnames(split_df) <- c("Value", "Index1", "Index2")
x1=cbind(split_df,x1)

############# Column selection #####################
x1=x1 %>% 
  dplyr::select("Index1","mean")
######################################## sorting ####################
x1$Index1=as.numeric(x1$Index1)
x2 = x1[order(x1$Index1),]
############################# row fill with different parameter ##############
x2 <- x2 %>%
  dplyr::mutate(
    Parameter = case_when(
      row_number() %in% 1:30 ~ 'Temperature',
      row_number() %in% 31:60 ~ 'Salinity',
      row_number() %in% 61:90 ~ 'Silicate',
      row_number() %in% 91:120 ~ 'Nitrate',
      row_number() %in% 121:150 ~ 'Phosphate'
    )
)
str(x2)
#############################################################
mycol <- c("navy", "blue", "cyan", "lightcyan", "white", "red", "red4")
x2 %>% 
  dplyr::select(mean,Parameter) %>% 
  mutate(where(is.numeric),~round(.,3)) %>% 
  mutate(species = rep(unique(v1$Species),5)) %>% 
  ggplot(aes(x = fct_inorder(Parameter), y = fct_inorder(species),fill= mean)) +
  geom_raster()+
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.1)+
  #scale_fill_gradientn(colours = mycol)+
  #scale_fill_gradient2(low="navy", mid="white", high="red")+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  #scale_fill_gradient2(low=muted('blue'), mid='white', high=muted('red'))+
  labs(x="Variable",y="Species")+theme_classic2()+
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="italic",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))
write.csv(x2,file ="E:/Model for Paper/beta.csv",row.names = F )
########################################################################################
x2 = cbind(x2,sort=1:150)

x2[104,2]=11.2
max(x2$mean)


##################################################################
df3=df2 %>% 
  mutate(Species=rep(species$Species,6))

x %>% 
  ggplot(aes(x=parameter,y=value))+geom_boxplot()

max(x$value)
x3$BUGSoutput$sims.list$prop.stress
tail(x3)
summary(x3$BUGSoutput$sims.list$Abundance1)
update(x3, 1000) #Burn-in
s <- coda.samples(x3, "p", n.iter=1000)
write.csv(x, file = "k.csv", row.names = FALSE)
##################################################
x5=mod_2$BUGSoutput$sims.matrix %>%
  as_tibble() %>%
  pivot_longer(cols =everything(),  values_to = "value", names_to = "parameter") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::filter( str_detect(parameter, "stress1"))
  #mutate(species = rep(rep(unique(p2$Species2),1),139000))
min(x5$value)
x5$species <- gsub("\\.", " ", x5$species)
x5 %>% 
  #dplyr::filter(value<5) %>% 
  dplyr::group_by(value,species) %>% 
  ggplot(aes(x=fct_inorder(species),y= value))+stat_boxplot(geom="errorbar")+
  geom_boxplot(fill="gray")+
  labs(x="Species",y=expression(paste("k(mgC",m^-3,")")))+theme_bw()+
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="italic"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))
  
dplyr::summarize(medianN = median(value),
 lci = quantile(value, probs = 25/100),
  uci = quantile(value, probs = 75/100)) 


round(matrix(x3$BUGSoutput$mean$r, byrow = TRUE, ncol =1) ,5)
for (p in 1:20){
  x5$parameter[x5$parameter==paste0("stress2[", p, "]")]= p
  print(x5)
  
}

x5$parameter=as.numeric(x5$parameter)
print(x5)
x5=x5[order(x5$parameter,decreasing = F),]
write.csv(x4, file = "stress1.1.csv", row.names = FALSE)
x5=x5 %>% 
  na.omit() %>% 
  mutate(species=unique(p2$Species2)) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  dplyr::select(-median_n)


print(x5)

for(x in 1:100){
  print(x)
}


##########################
p1=x5%>%
  dplyr::group_by(parameter) %>% 
  dplyr::summarize(mean=mean(value))

x2=subset(x,value==0)
x4=x2 %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::summarize(mean=mean(value))
p4=x%>% 
  separate(parameter,into = c("parameter","row","column"),sep = "\\[|,|\\]")

p3=p2 %>% 
  dplyr::select(beta,column,mean) %>% 
  filter(column==1) %>% 
  mutate(beta=v1$Species)

#################################################  
columns=c(1,2,3,4,5,6)
combined_results <- data.frame()
for (col_value in columns) {
  result <- p2 %>%
    dplyr::select(beta, column, mean) %>%
    filter(column == col_value) %>%
    mutate(beta = v1$Species)
  
  combined_results <- bind_rows(combined_results, result)
}  
write.csv(p4, file = "beta_final.csv", row.names = FALSE)

c1=read.csv("beta_final.csv")
attach(c1)
head(c1)
ggplot(c1,aes(x = fct_inorder(Parameter), y = fct_inorder(Species),fill= mean)) +
  geom_tile(stat = "identity",
            position = "identity")+scale_fill_gradientn(colours=c("#f8961e","#c36f09","#f94144",
                                 "white",
                                 "black","#471ca8","lightblue"),
                       values=rescale(c(0.00254, 0.00100602, 1.291445,
                                        0.0001,
                                        -1.081973, -0.0042564,-0.0012865 
                                         )),
                       guide="colorbar")+
  #geom_raster(interpolate = F,hjust = 0.2,vjust = 0.45)+ 
  #scale_fill_gradient2(low = "blue",mid="yellow",high = "red")+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1.5,barheight = 20,
                                title = "High",label = FALSE,ticks = FALSE))+
 labs(x="Variable",y="Species")+theme_classic2()+
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="italic",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  



sorted_c1 <- c1 %>%
  arrange(desc(mean), Parameter) %>%
  arrange(desc(mean), Species)



install.packages("BBmisc")


c1=read.csv("explained.csv")
head(c1)
c2=c1 %>% 
dplyr::group_by(Parameter) %>% 
  dplyr::summarize(mean=var(Value)*100)
head(c2)
c1 %>% na.omit() %>% 
  ggplot(aes(x=Var ,y=Value,fill=Var))+geom_col()+
  labs(x="Variable",y="Explained Variance(%)")+theme_classic()+
  theme(legend.position = "None",axis.text.x = element_text(angle =30,hjust =0.5,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))


### Explain Variance ###################################################
head(df3)
ggplot(df3,aes(x = fct_inorder(parameter), y = fct_inorder(Species),fill= mean)) +
  geom_tile(stat = "identity",
            position = "identity")+scale_fill_gradientn(colours=c("#ffffff","#f5f3f4","#d3d3d3","#b1a7a6","#e5383b","#ba181b",
                                                                  "#a4161a","#660708","#f6ae2d","darkgreen"),
                                                        values=rescale(c(0.000852, 0.001220602,0.01216,
                                                                         0.12756,0.1319222,
                                                                         0.13657,
                                                                         0.138526,
                                                                         0.145735,
                                                                         0.1597566,
                                                                         0.2144
                                                        )),
                                                        guide="colorbar")+
  #geom_raster(interpolate = F,hjust = 0.2,vjust = 0.45)+ 
  #scale_fill_gradient2(low = "blue",mid="yellow",high = "red")+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1.5,barheight = 20,
                                title = "High",label = FALSE,ticks = FALSE))+
  labs(x="Variable",y="Species")+theme_classic2()+
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.6,color = "black",size = 14,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="italic",size = 14,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))



################################
d3=as.data.frame(boxcox_data)
ggplot(d3, aes(x = boxcox_data)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot of Phosphate", x = "Phosphate", y = "Density")


boxcox_result <- boxcox(p2$Sal~ 1)  # Find optimal lambda
lambda <- boxcox_result$x[which.max(boxcox_result$y)]  # Get optimal lambda


# Apply Box-Cox transformation with the optimal lambda
boxcox_data <- (p2$Sal ^lambda-0.56 ) / lambda

# Check distribution after transformation
hist(boxcox_data, main = "Histogram of Box-Cox-transformed Data", xlab = "Box-Cox-transformed values")
shapiro.test(boxcox_data)
