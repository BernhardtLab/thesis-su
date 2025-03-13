#fixing model

#packages
library(growthTools)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(gridExtra)
library(grid)
library(png)
library(cowplot)

#data
 allrfu <- read_csv("data/tpc_processed_all_rfus.csv") %>% 
  mutate(rfu = as.numeric(RFU)) %>%
  mutate(log_rfu = log10(RFU)) #changed to LOG not LN
# 
# ggplot(allrfu,aes(x=days,y=log_rfu))+
#   geom_point(aes(colour=treatment))+theme_bw()+
#   facet_wrap(~temp)

#tester
sdat2<-allrfu[allrfu$unique_well =='1_E11_14',]
#res<-get.growth.rate(sdat2$days,sdat2$log_rfu,plot.best.Q = T,id = '1_E11_14')
res <- get.gr.sat(sdat2$days, sdat2$log_rfu, plotQ = T, id = '1_E11_14') #looks good (& diff from get growth rate plot)

#summary
gdat <- allrfu %>% group_by(unique_well, treatment) %>%
  do(grs=get.growth.rate(x=.$days,y=.$log_rfu,
                         id=.$unique_well,plot.best.Q=T,fpath="figures/gdat-summaries")) 

gdat <- allrfu %>% 
  group_by(unique_well, treatment) %>%
  do(grs = get.gr.sat(x = .$days, y = .$log_rfu,
                      id = .$unique_well, plotQ = T, fpath = "figures/gsat-summaries")) #get.gr.sat(x, y, plotQ = F, fpath = NA, id = "")
#doesnt give pdfs of plots!!!
#testing get gr sat
test_df <- allrfu %>% filter(unique_well == "1_E11_14")
get.gr.sat(x = test_df$days, y = test_df$log_rfu, id = test_df$unique_well, plotQ = T, fpath = "figures/gsat-summaries")
#assigning path
fpath <- "figures/gsat-summaries"
get.gr.sat(x = test_df$days, y = test_df$log_rfu, id = test_df$unique_well, plotQ = TRUE, fpath = fpath)
print(paste("Function received fpath:", fpath))

#chat gpt debugging
gdat <- allrfu %>% 
  group_by(unique_well, treatment) %>%
  do(grs = get.gr.sat(x = .$days, y = .$log_rfu,
                      id = .$unique_well, plotQ = T, fpath = "figures/gsat-summaries"))

dir.create("figures/gsat-summaries", recursive = TRUE, showWarnings = FALSE)
  #Modify get.gr.sat() to always generate a test plot
get.gr.sat_mod <- function(x, y, id, plotQ = TRUE, fpath = NULL) {
  if (plotQ) {
    print(paste("Saving to:", file.path(fpath, paste0(id, ".pdf"))))
    
    pdf(file = file.path(fpath, paste0(id, ".pdf")))
    plot(x, y, main = paste("Growth Saturation for", id))
    dev.off()
  }
}
test_df <- allrfu %>% filter(unique_well == "1_E11_14")
get.gr.sat_mod(x = test_df$days, y = test_df$log_rfu, id = test_df$unique_well, plotQ = T, fpath = "figures/gsat-summaries")

get.gr.sat <- function(x, y, id, plotQ = TRUE, fpath = NULL) {
  print(paste("Function received fpath:", fpath))
  
  if (plotQ) {
    print(paste("Attempting to save plot for ID:", id))
    
    pdf_path <- file.path(fpath, paste0(id, ".pdf"))
    print(paste("Saving PDF to:", pdf_path))
    
    pdf(file = pdf_path)
    plot(x, y, main = paste("Growth Saturation for", id))
    dev.off()
    
    print(paste("PDF saved successfully:", pdf_path))
  }
}
get.gr.sat(x = test_df$days, y = test_df$log_rfu, id = "1_E11_14", plotQ = TRUE, fpath = "figures/gsat-summaries")






sumgdat <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
                              best.se=grs$best.se)
# View(sumgdat)
# 
# gdat3 <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
#                             best.se=grs$best.se,best.R2=grs$best.model.rsqr,
#                             nobs.exp=grs$best.model.slope.n)

#write_csv(gdat3, "data/growthtool-gdat-sum.csv")
gdat3 <- read.csv("data/growthtool-gdat-sum.csv")
View(gdat3) #ije code 06, line 77

gdat4 <- gdat3 %>% 
  filter(treatment != "blank_blank") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp"), sep = "_") %>% 
  unite(plate, well, col = "unique_well", sep = "_")

#tpc nls fitting
library(tidyverse)
library(broom)
library(lubridate)
library(minpack.lm)
library(rootSolve)
library(forcats)
library(dplyr)
library(rTPC)
library(nls.multstart)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(MuMIn)

#chosing lactin model (ije's choice)
mod = 'lactin2_1995'

#setting up data to match ije's
a.gt<-gdat4

a.gt$well.ID <- as.factor(gdat4$unique_well)
a.gt$Temp <- as.numeric(a.gt$tpctemp)
a.gt$r.gt <- as.numeric(a.gt$mu)
a.gt$treatment <- as.factor(a.gt$treatment)
treatment_list <- unique(a.gt$treatment)

output_dir2 <- "lactin2-tpc-su" 
if (!dir.exists(output_dir2)) dir.create(output_dir2)



for (treatment in treatment_list) {
  a.i <- a.gt %>%
    filter(treatment == treatment) # need to change this like joey
  
  if (nrow(a.i) == 0) next  # Skip if no data for the combination
  
  # Initialize AICc storage and plot list
  model.AICc <- data.frame(model = character(), AICc = numeric(), stringsAsFactors = FALSE)
  nls.plot.list <- list()
  
  # Function to fit models
  fit_model <- function(model_name, formula, iter, param_count) {
    start_vals <- get_start_vals(a.i$Temp, a.i$r.gt, model_name = model_name) #change a/i/ to a/gt
    
    nls_fit <- try(nls_multstart(
      formula,
      data = a.i,
      iter = rep(4, param_count),
      start_lower = start_vals - 10,
      start_upper = start_vals + 10,
      lower = get_lower_lims(a.i$Temp, a.i$r.gt, model_name = model_name),
      upper = get_upper_lims(a.i$Temp, a.i$r.gt, model_name = model_name),
      supp_errors = 'Y',
      convergence_count = FALSE
    ), silent = TRUE)
    
    if (inherits(nls_fit, "try-error")) return(NULL)
    
    # Store AICc
    model.AICc <<- rbind(model.AICc, data.frame(model = model_name, AICc = AICc(nls_fit)))
    
    # Generate predictions
    preds <- data.frame(Temp = seq(min(a.gt$Temp - 2), max(a.gt$Temp + 2), length.out = 100))
    preds <- broom::augment(nls_fit, newdata = preds)
    
    # Plot
    plot <- ggplot(preds) + 
      geom_point(aes(Temp, r.gt), data = a.i, size = 5) +  
      geom_line(aes(Temp, .fitted), col = 'darkslateblue', linewidth = 6) + 
      theme_classic(base_size = 20) + 
      theme(
        axis.title = element_text(size = 24),  
        axis.text = element_text(size = 20) 
      ) +
      ggtitle(model_name) + 
      ylim(-3, 5)
    
    
    return(plot)
  }
  
  # Fit each model
  nls.plot.list[['Lactin2']] <- fit_model("lactin2_1995", r.gt ~ lactin2_1995(temp = Temp, a, b, tmax, delta_t), 4, 4)
  
  # Save plots
  mod_grid <- plot_grid(plotlist = nls.plot.list)
  plot_filename <- paste0(output_dir1, "/TPC_plots_", treatment, ".pdf")
  ggsave(plot_filename, plot = mod_grid, width = 24, height = 16)
}
}