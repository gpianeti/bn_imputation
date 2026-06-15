message("run data_loading_cleaning.R, data_imputation.R and models_fitting.R before")

# defining list
scenario_name <- c("0" = "Main analysis",
                   "1" = "Complete cases",
                   "2" = "Mean/mode imputation",
                   "3" = "Partial data",
                   "4" = "Slope data")[[as.character(scenario)]]
fit.results <- list(
  gen.acc = c(res.rf$gen.acc, res.ba$gen.acc, res.bn$gen.acc, res.nb$gen.acc, res.tan$gen.acc),
  byloc.acc = cbind(res.rf$byloc$location, 
                    "Random Forest" = res.rf$byloc$bal.acc,
                    "BART" = res.ba$byloc$bal.acc,
                    "BN" = res.bn$byloc$bal.acc,
                    "Naive Bayes" = res.nb$byloc$bal.acc,
                    "TAN" = res.tan$byloc$bal.acc)
)
scenario_results <- list(
  scenario       = scenario,
  scenario.name  = scenario_name,
  imputation_dag = if (exists("finaldag")) finaldag else NULL,
  bn_dag         = res.bn$dag,
  fit.results    = fit.results
)

# save scenario results
saveRDS(scenario_results,
        file = here(paste0("results/scenario_", scenario, ".rds")))


cat("Saved scenario", scenario, "-", scenario_name, "\n")