
#######################################################################################################################-
# Initialize ----
#######################################################################################################################-

load("1_explore.rdata")
source("./code/0_init.R")


## Initialize parallel processing
Sys.getenv("NUMBER_OF_PROCESSORS") 
cl = makeCluster(4)
registerDoParallel(cl) 
# stopCluster(cl) #stop cluster


l.boot = NULL



#######################################################################################################################-
# Performance Plots ----
#######################################################################################################################-

## Data for interpretation
df.interpret = as.data.frame(df.samp)
df.interpret$INT = 1 #Add intercept variable (needed for partial dependence plots)

# Define prior base probabilities (needed to correctly switch probabilities of undersampled data)
b_all = mean(df$target_num)
b_sample = mean(df.interpret$target_num)



## Create predictions for holdout data (several simulation runs)
nsim = 3
yhat_holdout = c()
y_holdout = c()
for (sim in 1:nsim) {
  
  # Hold out a k*100% set
  set.seed(sim*999)
  k = 0.2  
  i.holdout = sample(1:nrow(df.interpret), floor(k*nrow(df.interpret)))
  df.holdout = df.interpret[i.holdout,]
  df.train = df.interpret[-i.holdout,]    
  
  # Model and predict holdout
  ctrl = trainControl(method = "none",  #Do not tune! 
                      summaryFunction = twoClassSummary, classProbs = T, returnResamp = "final")
  fit = train( df.train[predictors], df.train$target, trControl = ctrl, metric = "ROC",
               method = "gbm", 
               tuneGrid = expand.grid(n.trees = 700, interaction.depth = 6, 
                                      shrinkage = 0.01, n.minobsinnode = 10), 
               verbose = FALSE )
  yhat_holdout = c(yhat_holdout, predict(fit, df.holdout[predictors], type = "prob", na.action = na.pass)[,2]) 
  y_holdout = c(y_holdout, as.character(df.holdout$target))
  print(performance( prediction(yhat_holdout, y_holdout), "auc" )@y.values[[1]])
}


## Plot performance
plot_performance("./output/performance.pdf", yhat_holdout, y_holdout)



#######################################################################################################################-
# Importance + Partial Depedence plot ----
#######################################################################################################################-

## Get model for all data
ctrl = trainControl(method = "none",  #Do not tune! 
                    summaryFunction = twoClassSummary, classProbs = T, returnResamp = "final")

fit.gbm = train( df.interpret[c("INT",predictors)], df.interpret$target, trControl = ctrl, metric = "ROC",
             method = "gbm", 
             tuneGrid = expand.grid(n.trees = 700, interaction.depth = 6, 
                                    shrinkage = 0.01, n.minobsinnode = 10), 
             verbose = FALSE )



## Get model for bootstrapped data and collect all models into one list
nboot = 20
l.boot = foreach(i = 1:nboot, .combine = c, .packages = c("caret")) %dopar% { 
  
  # Bootstrap
  set.seed(i*9999)
  i.boot = sample(1:nrow(df.interpret), replace = TRUE)
  df.boot = df.interpret[i.boot,]
  
  # Model
  df.boot$INT = 1
  fit.gbm = train( df.boot[c("INT",predictors)], df.boot$target, trControl = ctrl, metric = "ROC",
               method = "gbm", 
               tuneGrid = expand.grid(n.trees = 700, interaction.depth = 6, 
                                      shrinkage = 0.01, n.minobsinnode = 10), 
               verbose = FALSE )
  return(list(fit.gbm))
}



## Variable Importance (works for all caret models)
plot(varImp(fit.gbm)) #Default plot

# Variable importance only for topn important variables 
topn = 10
(topn_varimp = rownames(varImp(fit.gbm)$importance)[order(varImp(fit.gbm)$importance, decreasing = TRUE)][1:topn])
plot_variableimportance("./output/variableimportance_gbm.pdf", vars = topn_varimp, l.boot = l.boot, 
                        ncols = 5, nrows = 2, w = 18, h = 12)



## Partial dependence (only for gbm)
# Default plots
plot(fit.gbm$finalModel, i.var = "age", type = "link") #-> shows 1-P(target="Y") !!!
plot(fit.gbm$finalModel, i.var = 7, type = "response") 

# Partial dependence only for topn important variables 
topn = 10
(topn_varimp = rownames(varImp(fit.gbm)$importance)[order(varImp(fit.gbm)$importance, decreasing = TRUE)][1:topn])
plot_partialdependence("./output/partialdependance_gbm.pdf", vars = topn_varimp, l.boot = l.boot, 
                       ylim = c(0,0.3), ncols = 5, nrows = 2, w = 18, h = 12)



#######################################################################################################################-
# Interactions
#######################################################################################################################-


## Test for interaction (only for gbm)
(intervars = rownames(varImp(fit.gbm)$importance)[order(varImp(fit.gbm)$importance, decreasing = TRUE)][1:10])
plot_interactiontest("./output/interactiontest_gbm.pdf", vars = intervars, l.boot = l.boot)


## -> Relvant interactions
inter1 = c("TT4","TSH_LOG_")
inter2 = c("sex","referral_source_OTHER_") 
inter3 = c("TT4","referral_source_OTHER_")
plot_inter("./output/inter1.pdf", inter1, ylim = c(0,.4))
plot_inter("./output/inter2.pdf", inter2, ylim = c(0,.4))
plot_inter("./output/inter3.pdf", inter3, ylim = c(0,.4))

plot_inter_active("./output/anim", vars = inter1, duration = 3)
