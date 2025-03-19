#fitting gr to 42 only data
#this is in tandem with 02-good-enough and 02-growth-tools-joey

allrfu_only42 <- allrfu2 %>% 
  filter(grepl("_42", unique_well))

unique_well_list <- unique(allrfu_only42$unique_well) 

output <- data.frame(mu = numeric(), se = numeric(), unique_well = character())

for (i in seq_along(unique_well_list)) { 
  tryCatch({
    a.i <- allrfu_only42 %>%
      filter(unique_well == unique_well_list[i])  # Filter for the specific well
    
    # Compute growth rate
    growth_results <- get.gr(x = a.i$days, 
                             y = a.i$log_rfu, 
                             id = a.i$unique_well)
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
