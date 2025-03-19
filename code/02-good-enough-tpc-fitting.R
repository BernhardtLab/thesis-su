#march 17, after meeting with joey
#combining outputs to make a "good enough" tpc
#everything but 42 is gr sat and good
#for 42, need to do gr, but currently not working

allrfu <- read_csv("data/tpc_processed_all_rfus.csv") %>% 
  mutate(rfu = as.numeric(RFU)) %>%
  mutate(log_rfu = log(RFU)) 


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


m2 <- mock_ouput %>% 
  distinct()



# Joey fitting tpcs -------------------------------------------------------

get_topt <- function(df){
  grfunc<-function(x){
    -nbcurve(x, z = df$z[[1]],w = df$w[[1]],a = df$a[[1]],b = df$b[[1]])
  }
  optinfo<-optim(c(x=df$z[[1]]),grfunc)
  opt <-c(optinfo$par[[1]])
  maxgrowth <- c(-optinfo$value)
  results <- data.frame(topt = opt, rmax = maxgrowth)
  return(results)
}

get_tmax <- function(df){
  uniroot.all(function(x) nbcurve(x, z = df$z[[1]],w = df$w[[1]],a = df$a[[1]], b = df$b[[1]]),c(30,150))
}

get_tmin <- function(df){
  uniroot.all(function(x) nbcurve(x, z = df$z[[1]],w = df$w[[1]],a = df$a[[1]], b = df$b[[1]]),c(-40,25))
}


nbcurve<-function(temp,z,w,a,b){
  res<-a*exp(b*temp)*(1-((temp-z)/(w/2))^2)
  res
}

prediction_function <- function(df) {
  tpc <-function(x){
    res<-(df$a[[1]]*exp(df$b[[1]]*x)*(1-((x-df$z[[1]])/(df$w[[1]]/2))^2))
    res
  }
  
  pred <- function(x) {
    y <- tpc(x)
  }
  
  x <- seq(0, 50, by = 0.1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    dplyr::rename(temperature = x, 
                  growth = preds)
}


m2 %>% 
  mutate(temperature = as.numeric(tpctemp)) %>% 
  ggplot(aes(x = temperature, y = mu, color = treatment)) + geom_point() +
  facet_wrap(~ treatment)


m3 <- m2 %>% 
  mutate(temp = as.numeric(tpctemp)) %>% 
  filter(treatment == "6F_1")

m3 %>% 
  ggplot(aes(x = temp, y = mu)) + geom_point()



fit1 <- nlsLM(mu ~ a*exp(b*temp)*(1-((temp-z)/(w/2))^2),
        data = m3,
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = -20, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 120,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))


out_1 <- tidy(fit1) %>% 
  select(estimate, term) %>%  
  spread(key = term, value = estimate)

# get_topt(out_1) #36.6
# get_tmin(out_1) #19.1
# get_tmax(out_1) #42.9

preds_ind10 <- prediction_function(out_1)


m3 %>% 
  ggplot(aes(x = temp, y = mu)) + geom_point() +
  geom_line(aes(x = temperature, y = growth), data = preds_ind10) +
  ylim(-1, 1.5)





fitting_function <- function(df) {
  
  fit1 <- nlsLM(mu ~ a*exp(b*temp)*(1-((temp-z)/(w/2))^2),
                data = df,
                start= list(z= 25,w= 25,a= 0.2, b= 0.1),
                lower = c(z = -20, w= 0, a = -0.2, b = 0),
                upper = c(z = 40, w= 120,a =  2, b = 2),
                control = nls.control(maxiter=1024, minFactor=1/204800000))
  
  out_1 <- tidy(fit1) %>% 
    select(estimate, term) %>%  
    spread(key = term, value = estimate)
  
  preds_ind10 <- prediction_function(out_1)
  
  topt <- get_topt(out_1)
  tmin <- get_tmin(out_1)
  tmax <- get_tmax(out_1)
  
  output <- bind_cols(temp = preds_ind10$temperature, predicted_growth = preds_ind10$growth, topt = topt, tmax = tmax)
  return(output)
  
}


df_split <- m2 %>% 
  mutate(temp = as.numeric(tpctemp)) %>% 
  split(.$treatment)


output_all <- df_split %>% 
  map_df(fitting_function, .id = "treatment")

write_csv(output_all, "data/output-norberg.csv")

output_all %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>%
  ggplot(aes(x = temp, y = predicted_growth, color = incubator)) + geom_line() +
  ylim(-1, 2) 
ggsave("figures/predicted_growth.png", width = 10, height = 8)





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

for (i in seq_along(treatment_list)) { 
  tryCatch({
    a.i <- a.gt %>%
      filter(treatment == treatment_list[i])  
    
    if (nrow(a.i) == 0) next
    
    model.AICc <- data.frame(model = character(), AICc = numeric(), stringsAsFactors = FALSE)
    nls.plot.list <- list()
    
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
      
      # Generate predictions (Fix: Use a.i instead of a.gt)
      preds <- data.frame(Temp = seq(min(a.i$Temp) - 2, max(a.i$Temp) + 2, length.out = 100))
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
    
    # Save plots (Fix: Use treatment_list[i] for filename)
    mod_grid <- plot_grid(plotlist = nls.plot.list)
    plot_filename <- paste0(output_dir1, "/TPC_plots_", treatment_list[i], ".pdf")
    ggsave(plot_filename, plot = mod_grid, width = 24, height = 16)
    
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", conditionMessage(e)))
  })
}
