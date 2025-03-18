#march 17, after meeting with joey
#combining outputs to make a "good enough" tpc
#everything but 42 is gr sat and good
#for 42, need to do gr, but currently not working

allrfu2 <- allrfu %>% 
  filter(treatment != "blank_blank")

#everything but 42-------------------------------
allrfu_no42 <- allrfu2 %>% 
  filter(!grepl("_42", unique_well))

unique_well_list <- unique(allrfu_no42$unique_well) 

output <- data.frame(mu = numeric(), se = numeric(), unique_well = character()) #adding treatment = character() creates a column with NA values

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

write_csv(output, "data/output-no42.csv")
output_no42 <- read.csv("data/output-no42.csv")

#good enough 42 data---------------------------
gdat42 <- sumgdat %>% 
  filter(grepl("42", unique_well)) %>% 
  filter(treatment != "blank_blank")

write_csv(gdat42, "data/gdat42.csv")
output_only42 <- read.csv("data/gdat42.csv")

#binding---------------------
col_no42 <- data.frame(allrfu_no42$unique_well, allrfu_no42$treatment) %>% 
  rename("unique_well" = "allrfu_no42.unique_well") %>% 
  rename("treatment" = "allrfu_no42.treatment")

output_no42 <- left_join(output_no42, col_no42)

output_only42 <- output_only42 %>% 
  rename ("se" = "best.se") %>% 
  select(-"best.model")

mock_ouput <- bind_rows(output_no42, output_only42)
View(mock_ouput)

mock_ouput <- mock_ouput %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp"), sep = "_") %>% 
  unite(plate, well, col = "unique_well", sep = "_")

#making tpc-------------------------
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

#using lactin2
a.gt<-mock_ouput

a.gt$well.ID <- as.factor(mock_ouput$unique_well)
a.gt$Temp <- as.numeric(mock_ouput$tpctemp) 
a.gt$r.gt <- as.numeric(mock_ouput$mu)
a.gt$treatment <- as.factor(mock_ouput$treatment)
treatment_list <- unique(mock_ouput$treatment)

# Create directory to store outputs
output_dir1 <- "lactin2-tpc" 
if (!dir.exists(output_dir1)) dir.create(output_dir1)

for (treatment in treatment_list) {
  a.i <- a.gt %>%
    filter(treatment == treatment) # Remove any unnecessary columns if applicable
  
  if (nrow(a.i) == 0) next  # Skip if no data for the combination
  
  # Initialize AICc storage and plot list
  model.AICc <- data.frame(model = character(), AICc = numeric(), stringsAsFactors = FALSE)
  nls.plot.list <- list()
  
  # Function to fit models
  fit_model <- function(model_name, formula, iter, param_count) {
    start_vals <- get_start_vals(a.gt$Temp, a.gt$r.gt, model_name = model_name) #change a/i/ to a/gt
    
    nls_fit <- try(nls_multstart(
      formula,
      data = a.gt,
      iter = rep(4, param_count),
      start_lower = start_vals - 10,
      start_upper = start_vals + 10,
      lower = get_lower_lims(a.gt$Temp, a.gt$r.gt, model_name = model_name),
      upper = get_upper_lims(a.gt$Temp, a.gt$r.gt, model_name = model_name),
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
      geom_point(aes(Temp, r.gt), data = a.gt, size = 5) +  
      geom_line(aes(Temp, .fitted), col = 'violet', linewidth = 6) + 
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