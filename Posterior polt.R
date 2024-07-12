getwd()
setwd("E:/Model for Paper")
x=readRDS("model_g_5.RDS",file = "E:/My Reserach Project/model_4.rda")
head(posterior(x))


gf_density(~ r.1, data = posterior(x))
glimpse(posterior(x))
gf_dhistogram(~`gamma[2,5]`, data = posterior(x), bins = 5,alpha = 0.1) %>% 
  gf_dens(~`gamma[2,5]`, size = 1.5, alpha = 0.8) %>%
  gf_dist("beta", shape1 = 16, shape2 = 36, color = "red")

gf_dhistogram(~`beta[18,5]`, data = posterior(x), bins = 80,alpha = 0.4) %>% 
  gf_dens(~`beta[18,5]`, linewidth  = 1.1, alpha = 0.8,color = 'blue')

### For beta ###############################################################
plot_list <- list()
for (i in 1:5) {
  for (j in 1:30){
    param_name <- paste0("beta","[",j,",",i,"]")
    p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 50) %>% 
      gf_dens(~get(param_name), linewidth  = 1, alpha = 0.8,color = 'black')+xlab(param_name)
    #print(p)
    plot_list[[length(plot_list) + 1]] <- p
  }
}

combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 6)
print(combined_plot)

#################### For Gamma #############################################
plot_list <- list()
for (i in 1:5) {
  for (j in 1:30){
    param_name <- paste0("gamma","[",j,",",i,"]")
    p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 8,alpha = 0.8)+
      xlab(param_name)
      #gf_dens(~get(param_name), linewidth  = 1, alpha = 0.8,color = 'black')
    #print(p)
    plot_list[[length(plot_list) + 1]] <- p
  }
}

combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 6)
print(combined_plot)

##############Growth Rate ##################################################
plot_list <- list()
for ( i in 1:30){
  param_name <- paste0("r",".",i)
  p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 50) %>% 
    gf_dens(~get(param_name), linewidth  = 1, alpha = 0.8,color = 'black')+xlab(param_name)
  #print(p)
  plot_list[[length(plot_list) + 1]] <- p
  
}
combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 6)
print(combined_plot)

##################### Carrying Capacity ####################################

plot_list <- list()
for ( i in 1:30){
  param_name <- paste0("k",".",i)
  p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 50) %>% 
    gf_dens(~get(param_name), linewidth  = 1, alpha = 0.8,color = 'black')+xlab(param_name)
  #print(p)
  plot_list[[length(plot_list) + 1]] <- p
  
}
combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 6)
print(combined_plot)

########## For Stress #######################################################
plot_list <- list()
for (i in 1:5) {
  for (j in 1:30){
    param_name <- paste0("stress", i, ".", j)
    p=gf_dhistogram(~get(param_name), data = posterior(x), bins = 50) %>% 
      gf_dens(~get(param_name), linewidth  = 1, alpha = 0.8)+xlab(param_name)
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