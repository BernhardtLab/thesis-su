#using growth tools to get growth rate estimates with Ije
## ----install packages --------------------------------------------------------
remotes::install_github("ctkremer/mleTools")
remotes::install_github("ctkremer/growthTools")

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
library(purrr)

#data
allrfu <- read_csv("data/tpc_processed_all_rfus.csv") %>% 
  mutate(rfu = as.numeric(RFU)) %>% 
  mutate(log_rfu = log(RFU))

ggplot(allrfu,aes(x=days,y=log_rfu))+
  geom_point(aes(colour=treatment))+theme_bw()+
  facet_wrap(~temp)

#subset data
sdat2<-allrfu[allrfu$unique_well =='1_E11_14',]

res<-get.growth.rate(sdat2$days,sdat2$log_rfu,plot.best.Q = T,id = '1_E11_14')
res$best.model
res$best.slope
res$best.model.rsqr
res$best.se

#summary
gdat <- allrfu %>% group_by(unique_well, treatment) %>% 
  do(grs=get.growth.rate(x=.$days,y=.$log_rfu,
                         id=.$unique_well,plot.best.Q=F))  

sumgdat <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
                   best.se=grs$best.se)
View(sumgdat)

gdat42 <- sumgdat %>% 
  filter(grepl("42", unique_well)) %>% 
  filter(treatment != "blank_blank")

write_csv(gdat42, "data/gdat42.csv")


unique(sumgdat$best.model)



gdat2 <- allrfu %>% 
  filter(treatment != "blank_blank") %>% 
  group_by(unique_well, treatment) %>% 
  do(grs=get.growth.rate(x=.$days, y=.$log_rfu,id=.$unique_well,plot.best.Q=F,
                         methods=c('sat'))) %>% 
  summarise(trt,mu=grs$best.slope,best.model=grs$best.model)

# View results
print(results)

sumdat2 <- sumgdat %>% 
  filter(treatment != "blank_blank")

# trying to fit the growth rates with a for loop --------------------------
allrfu2 <- allrfu %>% 
  filter(treatment != "blank_blank")


unique_well_list <- unique(allrfu2$unique_well)



for (i in 1:length(unique_well_list)) {
  a.i <- allrfu2 %>%
    filter(unique_well == unique_well_list[i]) # Remove any unnecessary columns if applicable
  
  growth_results <- get.growth.rate(x= a.i$days, y=a.i$log_rfu,id= a.i$unique_well,plot.best.Q=F,
                  methods=c('sat'))
  
  results <- data.frame(mu = growth_results$best.slope, se = growth_results$best.se, unique_well = unique(a.i$unique_well))

output <- bind_rows(results, results[i])

return(output)

}




# trying for loop with chat gpt help --------------------------------------
# Initialize an empty data frame to store results

allrfu_no42 <- allrfu2 %>% 
  filter(!grepl("_42", unique_well))
 
unique_well_list <- unique(allrfu_no42$unique_well) 

output <- data.frame(mu = numeric(), se = numeric(), unique_well = character())

for (i in seq_along(unique_well_list)) { 
  tryCatch({
    a.i <- allrfu2 %>%
      filter(unique_well == unique_well_list[i])  # Filter for the specific well
    
    # Compute growth rate
    growth_results <- get.growth.rate(x = a.i$days, 
                                      y = a.i$log_rfu, 
                                      id = a.i$unique_well, 
                                      plot.best.Q = FALSE, 
                                      methods = c('sat'))
    
    # Store results in a data frame
    results <- data.frame(mu = growth_results$best.slope, 
                          se = growth_results$best.se, 
                          unique_well = unique(a.i$unique_well))
    
    # Append results to output
    output <- bind_rows(output, results)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", conditionMessage(e)))
    # Continue to the next iteration without stopping the loop
  })
}

# Return the final output after the loop completes
output

write_csv(output, "data/output-only-42.csv")
setdiff(unique_well_list, output$unique_well)




# now for the 42 degrees --------------------------------------------------


# trying for loop with chat gpt help --------------------------------------
# Initialize an empty data frame to store results

allrfu_only42 <- allrfu2 %>% 
  filter(grepl("_42", unique_well))

unique_well_list <- unique(allrfu_only42$unique_well) 

output_42 <- data.frame(mu = numeric(), se = numeric(), unique_well = character())

for (i in seq_along(unique_well_list)) { 
  tryCatch({
    a.i <- allrfu_only42 %>%
      filter(unique_well == unique_well_list[i])  # Filter for the specific well
    
    # Compute growth rate
    growth_results <- get.growth.rate(x = a.i$days, 
                                      y = a.i$log_rfu, 
                                      id = a.i$unique_well, 
                                      plot.best.Q = FALSE, 
                                      methods = c('gr'))
    
    # Store results in a data frame
    results <- data.frame(mu = growth_results$best.slope, 
                          se = growth_results$best.se, 
                          unique_well = unique(a.i$unique_well))
    
    # Append results to output
    output_42 <- bind_rows(output_42, results)
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", conditionMessage(e)))
    # Continue to the next iteration without stopping the loop
  })
}

# Return the final output after the loop completes
output_42





######
gdat3 <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
                            best.se=grs$best.se,best.R2=grs$best.model.rsqr,
                            nobs.exp=grs$best.model.slope.n)

write_csv(gdat3, "data/growthtool-gdat-sum.csv")
View(gdat3) #ije code 06, line 77

gdat3 <- read_csv("data/growthtool-gdat-sum.csv")


gdat4 <- gdat3 %>% 
  filter(treatment != "blank_blank") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp"), sep = "_") %>% 
  unite(plate, well, col = "unique_well", sep = "_")

#  separate(col = treatment, into = c("acclimation_trt", "replicate_flask"), sep = "_")

# gdat4 <- gdat4 %>% 
#   unite(plate, well, tpctemp, col = "unique_well", sep = "_") %>% 
#   unite(acclimation_trt, replicate_flask, col = "treatment", sep = "_")

################################################################################
#ije's code 07 tpc nls fitting
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

#ije line 38
# choose model - redoing this with lactin2_1995
mod = 'lactin2_1995'

tester <- gdat4 %>% 
  filter (unique_well == "1_C10")
str(tester)
tester$tpctemp <- as.numeric(tester$tpctemp)

# get start vals
start_vals <- get_start_vals(tester$tpctemp, tester$mu, model_name = 'lactin2_1995')
#>         a          b       tmax    delta_t 
#0.1194843 -0.2540080 42.0000000  6.0000000

#wliune 66 remove ed
# get limits
low_lims <- get_lower_lims(tester$tpctemp, tester$mu, model_name = 'lactin2_1995')
low_lims
#>  a       b    tmax delta_t 
#>     0     -10      12       0 

upper_lims <- get_upper_lims(tester$tpctemp, tester$mu, model_name = 'lactin2_1995')
upper_lims
#>     a       b    tmax delta_t 
#>       1       1     420    3840 

start_lower <- pmax(start_vals - 10, low_lims)
start_upper <- pmin(start_vals + 10, upper_lims)

lactinfit <- nls_multstart(mu ~ lactin2_1995(temp = tpctemp, a, b, tmax, delta_t),
                           data = tester,
                           iter = c(4,4,4,4),
                           start_lower = get_start_vals(tester$tpctemp, tester$mu, model_name = 'lactin2_1995') - 10,
                           start_upper = get_start_vals(tester$tpctemp, tester$mu, model_name = 'lactin2_1995') + 10,
                           lower = low_lims,
                           upper = upper_lims,
                           supp_errors = 'Y', 
                           convergence_count = FALSE)
lactinfit

calc_params(lactinfit) %>%
  mutate_all(round, 2)

# rmax  topt ctmin ctmax    e   eh q10 thermal_safety_margin thermal_tolerance breadth skewness
#1 2.71 31.79 -2.76 41.49 0.36 3.19  NA                   9.7             44.25   13.07    -2.83

#ije line  359-----------------
#lactin2 is the bets model for all the data - now loop through all treatments with just lactin2 and plot the graphs by the species/phos treatment
a.gt<-gdat4

a.gt$well.ID <- as.factor(gdat4$unique_well)
a.gt$Temp <- as.numeric(a.gt$tpctemp)
a.gt$r.gt <- as.numeric(a.gt$mu)
a.gt$treatment <- as.factor(a.gt$treatment)
treatment_list <- unique(a.gt$treatment)


treatment_list[1]
i<- 2
# Create directory to store outputs
output_dir1 <- "lactin2-tpc" 
if (!dir.exists(output_dir1)) dir.create(output_dir1)

for (i in 1:length(treatment_list)) {
    a.i <- a.gt %>%
      filter(treatment == treatment_list[i]) # Remove any unnecessary columns if applicable

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
      preds <- data.frame(Temp = seq(min(a.i$Temp - 2), max(a.i$Temp + 2), length.out = 100))
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
    plot_filename <- paste0(output_dir1, "/TPC_plots_", treatment_list[i], ".pdf")
    ggsave(plot_filename, plot = mod_grid, width = 24, height = 16)
}
}
#save as csv with all params, send code to Ije