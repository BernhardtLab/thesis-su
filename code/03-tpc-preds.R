#Sveta Uzhova
#acclimation W25
#fitting TPC models

#data---------------------------------------------------------------------------
#gr <- read_csv("data/gr-estimate.csv")
gr <- results
# View(gr)
# View(a7)

res <- nlsLM(RFU ~  N0 * exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
             data= a7,  #changed data = gr to data = a7
             start= list(z= 25,w= 25,a= 0.2, b= 0.1),
             lower = c(z = 0, w= 0, a = -0.2, b = 0),
             upper = c(z = 40, w= 80,a =  2, b = 2),
             control = nls.control(maxiter=1024, minFactor=1/204800000))
summary(res)

out1 <- tidy(res) %>%  
  select(estimate, term) %>%  
  spread(key = term, value = estimate)

out2 <- glance(res)

res2 <- tidy(res)

df <- out1

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

#fitting a model for each flask-------------------------------------------------
##Separating my treatments by indivdual flask 
treatments <- unique(gr$treatment)  
# Initialize a list to store outputs
outputs <- list()

# Loop over each treatment
for (treatment in treatments) {
  # Filter the data for the current treatment
  data_subset <- subset(gr, treatment == treatment)
  
  # Fit the model
  model <- nlsLM(estimate ~ a * exp(b * temp) * (1 - ((temp - z) / (w / 2))^2),
                 data = data_subset,
                 start = list(z = 25, w = 25, a = 0.2, b = 0.1),
                 lower = c(z = 0, w = 0, a = -0.2, b = 0),
                 upper = c(z = 40, w = 80, a = 2, b = 2),
                 control = nls.control(maxiter = 1024, minFactor = 1 / 204800000))
  
  # Get the summary of the model
  model_summary <- summary(model)
  
  # Tidy the model output and extract parameters
  out_ind <- tidy(model) %>%
    select(estimate, term) %>%
    spread(key = term, value = estimate)
  
  # Calculate Topt, Tmin, and Tmax (assuming you have these functions available)
  topt <- get_topt(out_ind)
  tmin <- get_tmin(out_ind)
  tmax <- get_tmax(out_ind)
  
  # Generate predictions using the model parameters
  preds_ind <- prediction_function(out_ind)
  
  # Getting 95% confidence intervals for the current treatment
  param_estimates <- coef(model)
  param_cov <- vcov(model)
  
  set.seed(123) 
  n_sim <- 2000
  simulated_params <- MASS::mvrnorm(n = n_sim, mu = param_estimates, Sigma = param_cov)
  
  # Define the prediction function for growth
  predict_growth <- function(temp, params) {
    nbcurve(temp, z = params["z"], w = params["w"], a = params["a"], b = params["b"])
  }
  
  # Adjust the temperature range for predictions
  temperature_range <- preds_ind$temp
  
  # Simulate predictions using the parameter estimates
  simulated_predictions <- sapply(temperature_range, function(temp) {
    apply(simulated_params, 1, function(params) predict_growth(temp, params))
  })
  
  # Compute 95% confidence intervals (lower and upper)
  ci_growth <- apply(simulated_predictions, 2, quantile, probs = c(0.025, 0.975))
  
  # Combine the predicted values with confidence intervals
  preds_with_ci <- preds_ind %>%
    mutate(
      lower = ci_growth[1, ],
      upper = ci_growth[2, ]
    )
  
  # Store the results 
  outputs[[treatment]] <- list(
    treatment = treatment,
    model_summary = model_summary,
    out_ind = out_ind,
    topt = topt,
    tmin = tmin,
    tmax = tmax,
    preds_with_ci = preds_with_ci  
  )
}

#Make single data frame
combined_preds_with_ci <- bind_rows(
  lapply(outputs, function(treatment_data) {
    # Add treatment column and return the data
    treatment_data$preds_with_ci %>%
      mutate(treatment = treatment_data$treatment)
  })
)
#Bring back temp treatment
combined_preds_with_ci <- combined_preds_with_ci %>% 
  separate(treatment, into = c("temp_treatment", "replicate"), sep = "_", remove = FALSE)


# Plot using ggplot2 with facet_wrap - WEIRD, ALL TRTS SAME?
combined_preds_with_ci %>%
  filter(temp_treatment == "6F") %>% 
  ggplot(aes(temperature, growth)) + 
  geom_line(colour = "blue") + 
  facet_wrap(~ treatment, scales = "free_y") +  
  theme_minimal()
