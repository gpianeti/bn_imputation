message("Run data_loading_cleaning.R before")

#####################################################
### TRAIN-TEST SPLIT ################################
#####################################################

if (scenario != 1) {
  n.train <- 500
} else {
  n.train <- 160
}
set.seed(42)
training <- sample(1:nrow(hd), n.train, replace = F)
train <- hd[training, ] %>% select(-location)
test <- hd[-training, ] %>% select(-location)
train.complete <- na.omit(train)

num_vars_names <- names(train %>% select(where(is.numeric)))
cat_vars_names <- names(train %>% select(where(~ !is.numeric(.))))

##################################################### 
### CROSS VALIDATION ################################ 
##################################################### 
if (scenario %in% c(0,3,4)) {
  
  # cross validation for BN inputation
  n <- nrow(train.complete)
  p <- ncol(train.complete)
  set.seed(42)
  index <- split(sample(1:n, n), ceiling(1:n / (n/10)))
  param <- expand.grid(k = seq(from = 0.5, to = 10, by = 0.5), maxp = 1:5)
  score_k <- rep(NA, nrow(param))
  
  # function for ordinal or mean/mode imputing to handle computational issues (zero weights)
  impute_topological <- function(fit, data, n_samples = 1000) {
    dag        <- bn.net(fit)
    node_order <- node.ordering(dag)
    imputed    <- data
    
    for (node in node_order) {
      na_rows <- which(is.na(imputed[[node]]))
      if (length(na_rows) == 0) next
      
      parents <- bnlearn::parents(dag, node)
      is_cat  <- is.factor(imputed[[node]])
      
      for (row in na_rows) {
        
        evidence <- if (length(parents) == 0) {
          TRUE
        } else {
          parent_vals <- imputed[row, parents, drop = FALSE]
          
          if (any(is.na(parent_vals))) {
            message(paste("NA in parents: row", row, "node", node))
            next
          }
          
          ev <- lapply(seq_along(parents), function(k) {
            val <- imputed[row, parents[k]]
            if (is.factor(val)) as.character(val) else as.numeric(val)
          })
          names(ev) <- parents
          ev
        }
        
        # try with evidence, fallback to marginal if zero weights
        samples <- tryCatch({
          cpdist(fit,
                 nodes    = node,
                 evidence = evidence,
                 method   = "lw",
                 n        = n_samples)[[1]]
        }, error = function(e) {
          message(paste("Zero weights for node", node, "row", row, 
                        "- falling back to marginal"))
          cpdist(fit,
                 nodes    = node,
                 evidence = TRUE, 
                 method   = "lw",
                 n        = n_samples)[[1]]
        })
        
        imputed[row, node] <- if (is_cat) {
          lv <- levels(imputed[[node]])
          factor(lv[which.max(tabulate(match(samples, lv)))], levels = lv)
        } else {
          mean(samples, na.rm = TRUE)
        }
      }
    }
    return(imputed)
  }
  
  
  ### cross-validation loop
  set.seed(42)
  for (idx in 1:nrow(param)) {
    
    fold_mean <- rep(NA, 10) 
    
    for (i in 1:10 ) {
      
      # creating datasets
      train.val <- train.complete[-index[[i]], ] # complete train set
      test.val <- train.complete[index[[i]], ]   # complete test set
      test.na <- train.complete[index[[i]], ]    # missing values test set:
      
      # introducing NAs
      fold_size <- nrow(test.val)
      nna <- floor(fold_size*0.2)
      na.index <- replicate(p, sample(1:fold_size, nna), simplify = F)
      iwalk(na.index, ~ {test.na[.x, .y] <<- NA})
      
      # fitting
      dag <- tabu(train.val, score = "bic-cg", k = param$k[idx], maxp = param$maxp[idx])
      fit <- bn.fit(dag, train.val)
      
      # checking if probabilities computation is feasible and imputing data
      imputed.data <- tryCatch({
        m <- impute(fit, test.na, method = "bayes-lw", n = 10000)
        message("Bayes")
        m},
        error = function(e) tryCatch({
          m <- impute(fit, test.na, method = "parents")
          message("Parents")
          m},
          error = function(e) {
            m <- impute_topological(fit, test.na, n_samples = 10000)
            message("Topological")
            m
          }
        )
      )
      
      # measuring performance
      gof_score <- rep(NA, p)
      variable <- names(imputed.data)
      for (j in 1:p) {
        true_vals <- test.val[na.index[[j]], j]
        imputed_vals <- imputed.data[na.index[[j]], j]
        
        if (variable[j] %in% num_vars_names) {
          mse <- (1/nna)*sum((true_vals - imputed_vals)^2)
          rmse <- sqrt(mse)
          nrmse <- rmse / sd(train.complete[, j])
          nrmse_scaled <- nrmse / (1 + nrmse)
          gof_score[j] <- nrmse_scaled
          
        } else if (variable[j] %in% cat_vars_names) {
          err <- (1/nna)*sum(true_vals != imputed_vals)
          gof_score[j] <- err
          
        } else (message(paste(j, "-th variable to compute performance not found")))
        
      }
      fold_mean[i] <- mean(gof_score)
    }
    score_k[idx] <- mean(fold_mean)
    print(c(idx, score_k[idx]))
  }
  
  # finding the best parameters
  k.sl <- param[which.min(score_k), 1]
  maxp.sl <- param[which.min(score_k), 2]
  
}


#####################################################
### DATA IMPUTATION ################################# 
#####################################################

if (scenario %in% c(0,3,4)) {
  
  # fitting dag for imputation
  finaldag <- tabu(train.complete, score = "bic-cg", k = k.sl, maxp = maxp.sl)
  finalfit <- bn.fit(finaldag, train.complete)
  
  # imputing training values
  set.seed(42)
  train.imputed <- tryCatch(
    impute(finalfit, train, method = "bayes-lw", n = 100000),
    error = function(e) tryCatch(
      impute(finalfit, train, method = "parents"),
      error = function(e) tryCatch(
        impute_topological(finalfit, train, n_samples = 100000),
        error = function(e) message("Training values imputation failed")
      )
    )
  )
  
  # imputing test values
  test.imputed <- test
  test.imputed$dis <- factor(NA, levels = dimnames(finalfit$dis$prob)[[1]]) 
  set.seed(42)
  
  test.imputed <- tryCatch(
    impute(finalfit, test.imputed, method = "bayes-lw", n = 100000),
    error = function(e) tryCatch(
      impute(finalfit, test.imputed, method = "parents"),
      error = function(e) tryCatch(
        impute_topological(finalfit, test.imputed, n_samples = 100000),
        error = function(e) message("Test values imputation failed")
      )
    )
  )
  
} else if (scenario == 2) {
  means <- train %>%
    summarise(across(all_of(num_vars_names), ~ mean(., na.rm = TRUE)))
  modes <- train %>%
    summarise(across(all_of(cat_vars_names), ~ names(which.max(table(.)))))
  mm_impute <- function(df, means, modes) {
    df %>%
      mutate(
        across(all_of(num_vars_names), ~ coalesce(., means[[cur_column()]])),
        across(all_of(cat_vars_names), ~ coalesce(., modes[[cur_column()]]))
      )
  }
  train.imputed <- mm_impute(train, means, modes)
  test.imputed  <- mm_impute(test,  means, modes)
  
  coerce_types <- function(df, num_vars, cat_vars) {
    df %>%
      mutate(across(all_of(num_vars), as.numeric),
             across(all_of(cat_vars), as.factor))
  }
  train.imputed <- coerce_types(train.imputed, num_vars_names, cat_vars_names)
  test.imputed  <- coerce_types(test.imputed,  num_vars_names, cat_vars_names)
  
} else if (scenario == 1) {
  train.imputed <- train
  test.imputed <- test
}


