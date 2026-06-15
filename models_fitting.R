message("Run data_loading_cleaning.R and data_imputation.R before")

#####################################################
#####################################################
### ANALYSIS PREPARATION ############################
#####################################################
#####################################################


#####################################################
### RF AND BART CROSS VALIDATION ####################
#####################################################


### CV PARAMETERS
n <- nrow(train.imputed)
p <- ncol(train.imputed) - 1
set.seed(42)
index <- split(sample(1:n, n), ceiling(1:n / (n/10)))
grid.rf <- expand.grid(
  trees = as.integer(seq(from = 50, to = 300, by = 25)), 
  nodesize = as.integer(1:10)
)
grid.ba <- expand.grid(
  ntree = c(50, 100, 200),
  k     = c(1, 2, 3),
  power = c(1, 2),
  base  = c(0.75, 0.95)
)
bal.accuracy.rf <- rep(NA, nrow(grid.rf))


### BALANCED ACCURACY FUNCTION
bal.acc.fun <- function(val, predval) {
  prec.matrix <- table(val, predval)
  sens <- prec.matrix[2,2]/sum(prec.matrix[2, ])
  spec <- prec.matrix[1,1]/sum(prec.matrix[1, ])
  bal.acc <- (sens+spec)/2
  bal.acc
}


### RANDOM FOREST CROSS VALIDATION
set.seed(42)
for (idx in 1:nrow(grid.rf)) {
  
  fold.acc <- rep(NA, 10)
  
  for (k in 1:10) {
    
    # defining fold dataframes
    train.val <- train.imputed[-index[[k]], ]
    test.val <- train.imputed[index[[k]], ] %>% select(-dis)
    Y <- train.imputed[index[[k]], ]$dis
    
    # fitting
    rf.fit <- randomForest(dis ~ ., data = train.val, 
                           mtry = floor(sqrt(p)), 
                           ntree = grid.rf$trees[idx], 
                           nodesize = grid.rf$nodesize[idx])
    
    # preditcion and computing balanced accuracy
    predval.rf <- factor(predict(rf.fit, test.val), levels = c("0", "1"))
    fold.acc[k] <- bal.acc.fun(Y, predval.rf)
  }
  
  # storing results
  bal.accuracy.rf[idx] <- mean(fold.acc)
}

# setting tuned parameters
ntree.t <- grid.rf[which.max(bal.accuracy.rf), 1]     
nodesize.t <- grid.rf[which.max(bal.accuracy.rf), 2]



### BART CROSS VALIDATION (executed in parallel)
n_cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(n_cores)
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, 42)

bal.accuracy.ba <- foreach(idx = 1:nrow(grid.ba),
                           .combine = c, 
                           .packages = c("BART", "tidyverse")
) %dopar% {
  
  fold.acc <- rep(NA, 10)
  
  for (k in 1:10) {
    
    # creating fold dataset
    train.val <- train.imputed[-index[[k]], ] %>% select(-dis)
    train.Y <- as.integer(train.imputed[-index[[k]], ]$dis) - 1
    test.val <- train.imputed[index[[k]], ] %>% select(-dis)
    Y <- train.imputed[index[[k]], ]$dis
    
    # fitting, prediction and encoding
    response.ba <- pbart(x.train = train.val, 
                         y.train = train.Y, 
                         x.test = test.val,
                         ntree = grid.ba$ntree[idx],
                         k = grid.ba$k[idx],
                         power = grid.ba$power[idx],
                         base = grid.ba$base[idx],
                         nskip = 100,
                         ndpost = 200)$prob.test.mean
    
    # preditcion and computing balanced accuracy
    predval.ba <- factor(ifelse(response.ba > 0.5, "1", "0"), levels = c("0", "1"))
    fold.acc[k] <- bal.acc.fun(Y, predval.ba)
  }
  
  # storing results
  mean(fold.acc)
}
parallel::stopCluster(cl)

# setting tuned parameters
ntree.t = grid.ba[which.max(bal.accuracy.ba), 1]
k.t     = grid.ba[which.max(bal.accuracy.ba), 2]
power.t = grid.ba[which.max(bal.accuracy.ba), 3]
base.t  = grid.ba[which.max(bal.accuracy.ba), 4]



#####################################################
### BN-BASED MODELS #################################
#####################################################


### DISCRETIZATION FUNCTION
# discretize train set and apply same cutpoints to test set
discretize_train_test <- function(train, test, breaks = 4, ibreaks = 10) {
  
  # learn discretization on training set only
  train.disc <- discretize(train,
                           method  = "hartemink",
                           breaks  = breaks,
                           ibreaks = ibreaks,
                           idisc   = "interval")
  bins <- lapply(attr(train.disc, "cutpoints"), function(x) {
    if (length(x) > 1) { x[1] <- -Inf; x[length(x)] <- Inf }
    x
  })
  
  # apply cutpoints to test set
  test.disc <- test
  for (col in names(bins)) {
    if (!is.null(bins[[col]])) {
      test.disc[[col]] <- cut(test[[col]],
                              breaks          = bins[[col]],
                              include.lowest  = TRUE)
    }
  }
  
  # align factor levels between train and test
  for (col in names(test.disc)) {
    if (is.factor(test.disc[[col]]) &&
        !identical(levels(test.disc[[col]]), levels(train.disc[[col]]))) {
      levels(test.disc[[col]]) <- levels(train.disc[[col]])
    }
  }
  
  # diagnostics
  all_match <- all(sapply(names(train.disc), function(col)
    identical(levels(train.disc[[col]]), levels(test.disc[[col]]))))
  if (!all_match) warning("Level mismatch between train and test after discretization")
  if (any(is.na(train.disc))) warning(paste(sum(is.na(train.disc)), "NAs in discretized train"))
  if (any(is.na(test.disc)))  warning(paste(sum(is.na(test.disc)),  "NAs in discretized test"))
  
  list(train = train.disc, test = test.disc)
}


### FITTING-EVALUATING FUNCTION
# fit and evaluate a BN-based classifier (BN, NB, TAN)
fit_bn_classifier <- function(model,           # "bn", "nb", "tan"
                              train,           # training set (discretized if needed)
                              test,            # test set    (discretized if needed)
                              test.original,   # original test set (for correct target labels)
                              location,        # factor vector of test locations
                              blacklist = NULL,
                              breaks = 4) {
  
  # structure learning
  dag <- if (model == "bn") {
    tabu(train, blacklist = blacklist, score = "bic-cg")
  } else if (model == "nb") {
    naive.bayes(train, training = "dis")
  } else if (model == "tan") {
    tree.bayes(train, training = "dis")
  } else {message("model in fit_bn_classifier misspecified")}

  # parameter learning
  fit <- bn.fit(dag, train)
  
  # prediction
  pred <- predict(fit, node = "dis", data = test, 
                  method = "bayes-lw", n = 100000,
                  prob = FALSE)
  pred <- factor(pred, levels = levels(test.original$dis))
  
  # global balanced accuracy
  gen.acc <- bal.acc.fun(test.original$dis, pred)
  
  # balanced accuracy by location
  byloc.acc <- tibble(dis = test.original$dis, 
                      predval = pred, 
                      location = location) %>%
    group_by(location) %>%
    summarise(bal.acc = bal.acc.fun(dis, predval), .groups = "drop")
  
  list(dag     = dag,
       fit     = fit,
       pred    = pred,
       gen.acc = gen.acc,
       byloc   = byloc.acc)
}


### BLACKLIST CONSTRUCTION
# blacklist arcs pointing to demographic root nodes
make_blacklist <- function(train) {
  cols <- colnames(train)
  data.frame(
    from = rep(cols, 2),
    to   = c(rep("age", length(cols)), rep("sex", length(cols)))
  )
}


### SETTING FUNCTION INPUTS
# retrieve test location for byloc summaries (location was removed from train/test)
test.location <- hd$location[-training]

# number of discretization breaks: complete cases only (scenario 1) has fewer obs
breaks <- if (scenario == 1) 3 else 4
ibreaks <- if (scenario == 1) 5 else 10

# discretize train and test for NB and TAN
# BN works directly on mixed data, no discretization needed
set.seed(42)
disc <- discretize_train_test(train.imputed, test.imputed, breaks = breaks, ibreaks = ibreaks)
train.disc <- disc$train
test.disc  <- disc$test

# blacklists
bl      <- make_blacklist(train.imputed)
bl.disc <- make_blacklist(train.disc)




#####################################################
#####################################################
### MODELS FITTING ##################################
#####################################################
#####################################################


### RANDOM FORESTS FIT
set.seed(42)
rf.finalfit <- randomForest(dis ~ ., data = train.imputed, 
                            mtry = floor(sqrt(p)), 
                            ntree = ntree.t, 
                            nodesize = nodesize.t)
final.predval.rf <- factor(predict(rf.finalfit, 
                                   select(test.imputed, -dis)), 
                           levels = c("0", "1"))
gen.acc.rf <- bal.acc.fun(test$dis, final.predval.rf)
byloc.data.rf <- test %>%
  mutate(predval   = final.predval.rf,
         location  = hd$location[-training])
byloc.acc.rf <- byloc.data.rf %>% 
  group_by(location) %>% 
  summarise(bal.acc = bal.acc.fun(dis, predval))

res.rf <- list(fit = rf.finalfit,
               pred = final.predval.rf,
               gen.acc = gen.acc.rf,
               byloc = byloc.acc.rf)

cat("RF - global balanced accuracy:", res.rf$gen.acc, "\n")
print(res.rf$byloc)


### BART FIT
set.seed(42)
response.ba <- pbart(x.train = select(train.imputed, -dis), 
                     y.train = as.integer(train.imputed$dis) -1, 
                     x.test = select(test.imputed, -dis),
                     ntree = ntree.t,
                     k = k.t,
                     power = power.t,
                     base = base.t,
                     nskip = 200,
                     ndpost = 1000)$prob.test.mean
final.predval.ba <- factor(ifelse(response.ba > 0.5, "1", "0"), levels = c("0", "1"))
gen.acc.ba <- bal.acc.fun(test$dis, final.predval.ba)
byloc.data.ba <- test %>%
  mutate(predval   = final.predval.ba,
         location  = hd$location[-training])
byloc.acc.ba <- byloc.data.ba %>% 
  group_by(location) %>% 
  summarise(bal.acc = bal.acc.fun(dis, predval))

res.ba <- list(pred = final.predval.ba,
               gen.acc = gen.acc.ba,
               byloc = byloc.acc.ba)
cat("BART - global balanced accuracy:", res.ba$gen.acc, "\n")
print(res.ba$byloc)


### BAYESIAN NETWORK FIT
set.seed(42)
res.bn <- fit_bn_classifier(
  model         = "bn",
  train         = train.imputed,
  test          = test.imputed,
  test.original = test,
  location      = test.location,
  blacklist     = bl
)

graphviz.plot(res.bn$dag)
cat("BN - global balanced accuracy:", res.bn$gen.acc, "\n")
print(res.bn$byloc)


### NAIVE BAYES FIT
set.seed(42)
res.nb <- fit_bn_classifier(
  model         = "nb",
  train         = train.disc,
  test          = test.disc,
  test.original = test,
  location      = test.location
)

graphviz.plot(res.nb$dag, layout = "circo")
cat("NB - global balanced accuracy:", res.nb$gen.acc, "\n")
print(res.nb$byloc)


### TREE-AUGMENTED NAIVE BAYES FIT
set.seed(42)
res.tan <- fit_bn_classifier(
  model         = "tan",
  train         = train.disc,
  test          = test.disc,
  test.original = test,
  location      = test.location
)

graphviz.plot(res.tan$dag, layout = "circo")
cat("TAN - global balanced accuracy:", res.tan$gen.acc, "\n")
print(res.tan$byloc)
