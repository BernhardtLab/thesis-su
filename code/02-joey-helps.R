#fixing with joey
a.gt<-gdat4

a.gt$well.ID <- as.factor(gdat4$unique_well)
a.gt$Temp <- as.numeric(a.gt$tpctemp)
a.gt$r.gt <- as.numeric(a.gt$mu)
a.gt$treatment <- as.factor(a.gt$treatment)
treatment_list <- unique(a.gt$treatment)

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
    start_vals <- get_start_vals(a.i$Temp, a.i$r.gt, model_name = model_name) 
    
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
  plot_filename <- paste0(output_dir1, "/TPC_plots_", treatment, ".pdf")
  ggsave(plot_filename, plot = mod_grid, width = 24, height = 16)
}
}