### setup ----
library(data.table)
library(magrittr)
library(ggplot2)
library(xgboost)

# one-hot encoding function
one_hot_encode = function(dt, cols, drop_levels = TRUE, sep = "=") {
  setDT(dt)
  newDT = copy(dt)
  newDT[, OHEID := .I]
  setkey(newDT, OHEID)
  
  if(drop_levels) {
    tempDT = newDT[, c("OHEID", cols), with = FALSE]
    
    # unpivot
    melted = melt(tempDT, "OHEID", value.factor = TRUE)
    
    # cleaning level names
    ## transliteration to ASCII from local encoding + replacing whitespaces with "_"
    new_levels_names = levels(melted$value) %>%
      iconv(to="ASCII//TRANSLIT") %>%
      gsub("\\s+", "_", .)
    ## changing levels of variable
    setattr(melted$value, "levels", new_levels_names)
    
    # pivot
    res = dcast(melted, OHEID ~ variable+value, fun.aggregate = length, sep = sep, drop = drop_levels)
    
    # joining newly one-hot encoded columns and the rest of the table
    res = merge(newDT[, !cols, with=FALSE], res)
    out = res[, !"OHEID"]
  } else {
    lll = lapply(cols,
                 function(col) {
                   tempDT = newDT[, c("OHEID", col), with = FALSE]
                   # unpivot
                   melted = melt(tempDT, "OHEID", value.factor = TRUE)
                   # cleaning level names
                   ## transliteration to ASCII from local encoding + replacing whitespaces with "_"
                   new_levels_names = levels(melted$value) %>%
                     iconv(to="ASCII//TRANSLIT") %>%
                     gsub("\\s+", "_", .)
                   ## changing levels of variable
                   setattr(melted$value, "levels", new_levels_names)
                   # pivot
                   dcast(melted, OHEID ~ variable+value, fun.aggregate = length, sep = sep, drop = drop_levels)
                 })
    res = Reduce(merge, lll)
    res = merge(newDT[, !cols, with=FALSE], res)
    out = res[, !"OHEID"]
  }
  
  out
}

### data ----

dt_loan = fread("loan.csv")

## issue date
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

dt_loan[, issue_Date := as.Date(paste0("1-", issue_d), format = "%d-%b-%Y")]
dt_loan[, issue_yyyymm := 100*year(issue_Date) + month(issue_Date)]

Sys.setlocale("LC_TIME", lct)

# defaulted
dt_loan[, defaulted := NA_integer_]
dt_loan[loan_status %in% c("Late (31-120 days)", "Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off"), 
        defaulted := 1L]
dt_loan[loan_status %in% c("Fully Paid", "Does not meet the credit policy. Status:Fully Paid"), 
        defaulted := 0L]
target = "defaulted"

# removing columns

cols_to_do_away = c("collection_recovery_fee", "last_pymnt_amnt", "last_pymnt_d", "next_pymnt_d", "loan_status", "out_prncp",
                    "out_prncp_inv", "pymnt_plan", "recoveries", "total_pymnt", "total_pymnt_inv", "total_rec_int", "total_rec_late_fee",
                    "total_rec_prncp", "hardship_flag", "hardship_type", "hardship_reason", "hardship_status", "deferral_term",
                    "hardship_amount", "hardship_start_date", "hardship_end_date", "payment_plan_start_date", "hardship_length",
                    "hardship_dpd", "hardship_loan_status", "orig_projected_additional_accrued_interest", "hardship_payoff_balance_amount",
                    "hardship_last_payment_amount", "disbursement_method", "debt_settlement_flag", "debt_settlement_flag_date", 
                    "settlement_status", "settlement_date", "settlement_amount", "settlement_percentage", "settlement_term")

cols_to_do_away = union(cols_to_do_away, c("issue_Date", "issue_d"))

## removing variables with too many missing values
# computing share of non-missing values for each column, then melting (unpivoting) the result
dt_fill_prc = dt_loan[, lapply(.SD, function(x) mean(!is.na(x)))] %>% melt(variable.name = "column", variable.factor = F, value.name = "non_missing_share")

cols_to_do_away = union(cols_to_do_away,
                        dt_fill_prc[non_missing_share < 0.9 & column != target, column]
)

set(dt_loan, j = cols_to_do_away, value = NULL)


## removing character variables with too many levels

char_cols = setdiff(colnames(dt_loan)[sapply(dt_loan, is.character)], cols_to_do_away)


# reduce number of levels to few most frequent for: purpose and addr_state
dt_loan[, `:=`(purpose_v2 = ifelse(purpose %in% dt_loan[, .N, purpose][order(-N)][1:6, purpose], purpose, "other"),
               addr_state_v2 = ifelse(addr_state %in% dt_loan[, .N, addr_state][order(-N)][1:10, addr_state], addr_state, "other"))]

cols_onehot = c("term", "grade", "emp_length", "home_ownership", "verification_status", "purpose_v2", "addr_state_v2",
                "initial_list_status", "application_type")

cols_to_do_away = setdiff(char_cols, cols_onehot)

# dt_loan = dt_loan[, !cols_to_do_away, with = F]
set(dt_loan, j = cols_to_do_away, value = NULL)

gc()

# filtering rows for modeling - removing non-terminated loans
dt_loan = dt_loan[!is.na(defaulted) & issue_yyyymm >= 201603 & issue_yyyymm <= 201806]

# one-hot encoding

dt_loan = one_hot_encode(dt_loan, cols = cols_onehot)


### modeling ----

dt_train = dt_loan[issue_yyyymm < 201701]
dt_val = dt_loan[issue_yyyymm >= 201701]

rm(dt_loan)

gc()

# xgboost ----
xgb_dtrain = xgb.DMatrix(data.matrix(dt_train[, !target, with = F]), label = dt_train[[target]])

xgb_dval = xgb.DMatrix(data.matrix(dt_val[, !target, with = F]), label = dt_val[[target]])

# cv

params_cv = expand.grid(eta = c(0.05, 0.1), max.depth = c(4,6), subsample = c(0.8, 1), colsample_bytree = c(0.8, 1)) %>% 
  split(., seq(nrow(.)))

xgb_cv_wrapp = function(par) {
  xgb_cv_it = xgb.cv(par, data = xgb_dtrain, nrounds = 1000, nfold = 5, objective = "binary:logistic",
                     print_every_n = 10, early_stopping_rounds = 20, metrics = "auc", nthread = 3, stringsAsFactors = FALSE)
  out = cbind(xgb_cv_it$evaluation_log[xgb_cv_it$best_iteration], par)
  out %>% write.table(file = "test.txt", append = T)
  out
}

results = lapply(split(params_cv, seq(nrow(params_cv))), xgb_cv_wrapp)
# dt_results = rbindlist(results)
# dt_results = fread("test.txt"); dt_results[, V1 := NULL]
# fwrite(dt_results, file = "xgb_cv_result.csv")

xgb_cv_result = fread("xgb_cv_result.csv")
xgb_cv_result[order(-test_auc_mean)]

params = list(eta = 0.05, max.depth = 4, subsample = 0.8, colsample_bytree = 0.8)
xgb_1 = xgb.train(params, data = xgb_dtrain, nrounds = 750, objective = "binary:logistic", nthread = 3,
                  watchlist = list(train = xgb_dtrain), eval_metric = "auc", print_every_n = 10)

pred_val = predict(xgb_1, xgb_dval)
Metrics::auc(actual = dt_val[[target]], predicted = pred_val)

xgb_pred_val = pred_val
# fwrite(xgb_pred_val, file = "xgb_pred_val.csv")

var_imp = xgb.importance(colnames(xgb_dtrain), xgb_1)
xgb.plot.importance(var_imp, 30)

# glmnet ----

pred_cols = setdiff(colnames(dt_train)[sapply(dt_train, function(x) !any(is.na(x)))], target)

library(glmnet)

glm_f1 = glmnet(x = data.matrix(dt_train[, pred_cols, with = F]), y = dt_train[[target]], family = "binomial", alpha = 1)

glm_cv1 = cv.glmnet(x = data.matrix(dt_train[, pred_cols, with = F]), 
                    y = dt_train[[target]], 
                    family = "binomial", alpha = 1, nfolds = 5)

coef(glm_cv1)
coef(glm_cv1, s = )
plot(glm_cv1)

glm_pred_val = predict.cv.glmnet(glm_cv1, newx = data.matrix(dt_val[, pred_cols, with = F]), type="response", s = "lambda.1se")

glm_pred_train = predict.cv.glmnet(glm_cv1, newx = data.matrix(dt_train[, pred_cols, with = F]), type="response", s = "lambda.1se")
# eksport obiektów ----

save(list = c("xgb_cv_result", "xgb_pred_val", "var_imp", "glm_pred_val", "glm_cv1"), file = "prepared_objects.RData")

load("prepared_objects.RData")

### AUC

library(pROC)

roc(dt_val[[target]], pred_val)
plot.roc(dt_val[[target]], pred_val, print.auc = TRUE)


### Ewaluacja

# creeping rate
dt_evaluation[order(xgboost_prediction), .(xgboost_prediction, xgboost_prediction_qtile, defaulted, .I, cumsum(defaulted), rate =cumsum(defaulted)/.I)][
  , .(xgboost_prediction = mean(xgboost_prediction), rate = rate[1], rate2 = tail(rate, 1)), xgboost_prediction_qtile]


#### render ----

rmarkdown::render("report.Rmd", 
                  # params = list(path_to_data_dir = "./"), 
                  envir = new.env())

rmarkdown::render("report.Rmd", 
                  params = list(path_to_data_dir = "./test_dir/"), 
                  envir = new.env())
