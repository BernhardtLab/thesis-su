#fitting gr to 42 only data
#this is in tandem with 02-good-enough and 02-growth-tools-joey

allrfu_only42 <- allrfu2 %>% 
  filter(grepl("_42", unique_well))

unique_well_list <- unique(allrfu_only42$unique_well) 

output <- data.frame(mu = numeric(), se = numeric(), unique_well = character(), stringsAsFactors = FALSE)

for (i in seq_along(unique_well_list)) { 
  tryCatch({
    a.i <- allrfu_only42 %>%
      filter(unique_well == unique_well_list[i])  # Filter for the specific well
    
    # Compute growth rate
    growth_results <- get.gr(x = a.i$days, 
                             y = a.i$log_rfu, 
                             id = a.i$unique_well)
    if (!is.null(growth_results$best.slope) && !is.null(growth_results$best.se)) {
      results <- data.frame(mu = growth_results$best.slope, 
                            se = growth_results$best.se, 
                            unique_well = unique(a.i$unique_well))
      output <- bind_rows(output, results)
    } else {
      message(paste("Skipping iteration", i, "due to missing growth rate results"))
    }
    
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

#Skipping iteration 96 due to missing growth rate results
#Error in iteration 96 : arguments imply differing number of rows: 0, 1
print(str(growth_results))
