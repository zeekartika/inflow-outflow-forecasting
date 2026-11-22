# =====================================================================
# Forecasting Indonesia's currency inflow and outflow
# =====================================================================

library(forecast)
library(xgboost)
set.seed(42)

# =====================================================================
# Data and dummy variables
# =====================================================================

setwd("directory")
data <- read.csv("data")
nrow(data)   # should be 177 (January 2012 - September 2026)

# Eid al-Fitr: month and day of the first day, 2012-2026
eid_period <- c("2012-08", "2013-08", "2014-07", "2015-07", "2016-07",
                "2017-06", "2018-06", "2019-06", "2020-05", "2021-05",
                "2022-05", "2023-04", "2024-04", "2025-03", "2026-03")
eid_day    <- c(19, 8, 28, 17, 6,
                25, 15, 5, 24, 13,
                2, 22, 10, 31, 21)

# Row number of each Eid month in the data
eid_row <- match(eid_period, data$Periode)

# Group the Eid months by week
# Week I = days 1-7, II = 8-15, III = 16-23, IV = 24 to the end
week1 <- eid_row[eid_day <= 7]
week2 <- eid_row[eid_day >= 8  & eid_day <= 15]
week3 <- eid_row[eid_day >= 16 & eid_day <= 23]
week4 <- eid_row[eid_day >= 24]

# Week-based dummies (12): 0 everywhere, 1 in the month before (-1),
# the Eid month, and the month after (+1)
data$V1_sebelum <- 0;  data$V1_sebelum[week1 - 1] <- 1
data$V1_saat    <- 0;  data$V1_saat[week1]        <- 1
data$V1_sesudah <- 0;  data$V1_sesudah[week1 + 1] <- 1

data$V2_sebelum <- 0;  data$V2_sebelum[week2 - 1] <- 1
data$V2_saat    <- 0;  data$V2_saat[week2]        <- 1
data$V2_sesudah <- 0;  data$V2_sesudah[week2 + 1] <- 1

data$V3_sebelum <- 0;  data$V3_sebelum[week3 - 1] <- 1
data$V3_saat    <- 0;  data$V3_saat[week3]        <- 1
data$V3_sesudah <- 0;  data$V3_sesudah[week3 + 1] <- 1

data$V4_sebelum <- 0;  data$V4_sebelum[week4 - 1] <- 1
data$V4_saat    <- 0;  data$V4_saat[week4]        <- 1
data$V4_sesudah <- 0;  data$V4_sesudah[week4 + 1] <- 1

# Simple Eid dummies (3), without the week split
data$Eid_before <- 0;  data$Eid_before[eid_row - 1] <- 1
data$Eid_during <- 0;  data$Eid_during[eid_row]     <- 1
data$Eid_after  <- 0;  data$Eid_after[eid_row + 1]  <- 1

# Covid dummy: April 2020 - December 2021
covid_start <- which(data$Periode == "2020-04")
covid_end   <- which(data$Periode == "2021-12")
data$covid <- 0;  data$covid[covid_start:covid_end] <- 1

# Outlier dummy for May 2022 (used only in the full-data outflow model)
data$outlier_2022_05 <- 0;  data$outlier_2022_05[data$Periode == "2022-05"] <- 1

week_dummies <- c("V1_sebelum", "V1_saat", "V1_sesudah",
                  "V2_sebelum", "V2_saat", "V2_sesudah",
                  "V3_sebelum", "V3_saat", "V3_sesudah",
                  "V4_sebelum", "V4_saat", "V4_sesudah")

View(data)

# =====================================================================
# Figure 2: monthly inflow and outflow, Eid months marked
# =====================================================================

dir.create("results", showWarnings = FALSE)

time   <- data$Tahun + (data$Bulan - 1) / 12   # e.g. March 2025 = 2025.17
shape  <- c(16, 15, 17, 18)                    # weeks I, II, III, IV
colour <- c("#2a78d6", "#eb6834", "#1a9e77", "#6a4fc7")
size   <- c(1.5, 1.4, 1.5, 2)

png("results/Figure2_Time_Series.png", width = 2850, height = 2100, res = 300)
layout(matrix(c(1, 2, 3), ncol = 1), heights = c(0.7, 3, 3.3))

par(mar = c(0, 0, 0, 0), cex = 1)
plot.new()
legend("center", c("Week I", "Week II", "Week III", "Week IV"),
       pch = shape, col = colour, pt.cex = size, horiz = TRUE, bty = "n",
       title = "Eid al-Fitr month, by week")

par(mar = c(1.5, 4.5, 0.5, 1), las = 1, bty = "l", cex = 1)
y <- data$Inflow / 1000
plot(time, y, type = "l", lwd = 2, xaxt = "n", xlab = "",
     ylab = "Inflow (IDR trillion)", ylim = c(0, max(y) * 1.05))
axis(1, at = seq(2012, 2026, 2), labels = FALSE)
points(time[week1], y[week1], pch = shape[1], col = colour[1], cex = size[1])
points(time[week2], y[week2], pch = shape[2], col = colour[2], cex = size[2])
points(time[week3], y[week3], pch = shape[3], col = colour[3], cex = size[3])
points(time[week4], y[week4], pch = shape[4], col = colour[4], cex = size[4])

par(mar = c(4, 4.5, 0.5, 1), las = 1, bty = "l", cex = 1)
y <- data$Outflow / 1000
plot(time, y, type = "l", lwd = 2, xaxt = "n", xlab = "",
     ylab = "Outflow (IDR trillion)", ylim = c(0, max(y) * 1.05))
axis(1, at = seq(2012, 2026, 2))
mtext("Year", side = 1, line = 2.5)
points(time[week1], y[week1], pch = shape[1], col = colour[1], cex = size[1])
points(time[week2], y[week2], pch = shape[2], col = colour[2], cex = size[2])
points(time[week3], y[week3], pch = shape[3], col = colour[3], cex = size[3])
points(time[week4], y[week4], pch = shape[4], col = colour[4], cex = size[4])

dev.off()

# =====================================================================
# Seasonal naive: forecast = value 12 months earlier
# =====================================================================

row_dec2021 <- which(data$Periode == "2021-12")
row_last    <- which(data$Periode == "2026-09")
origins     <- which(data$Periode %in% c("2021-12", "2022-12", "2023-12",
                                         "2024-12", "2025-12"))

# Test rows: January 2022 - September 2026 (57 months)
test <- (row_dec2021 + 1):row_last

# Table of test months; the forecasts of every model are added here
result <- data.frame(
  Periode = data$Periode[test],
  Round   = rep(1:5, times = c(12, 12, 12, 12, 9)),   # rolling-origin round
  Inflow  = data$Inflow[test],
  Outflow = data$Outflow[test]
)

result$SN_Inflow  <- data$Inflow[test - 12]
result$SN_Outflow <- data$Outflow[test - 12]

# =====================================================================
# Models for one series
# Run with series <- "Inflow", then change to "Outflow" and run again
# =====================================================================

series <- "Outflow"
y      <- data[[series]]

# ---------------------------------------------------------------------
# Identification: differencing and ACF (training data, log scale)
# ---------------------------------------------------------------------
y_train <- ts(log(y[1:row_dec2021]), start = c(2012, 1), frequency = 12)
ndiffs(y_train)                                    
BoxCox.lambda(ts(y[1:row_dec2021], frequency = 12)) 
Acf(diff(y_train), lag.max = 48, main = series)    # spike at lag 12

# ---------------------------------------------------------------------
# SARIMA and ARIMAX models (log scale, d = D = 1)
# ---------------------------------------------------------------------
models_arima <- list(
  SARIMA           = NULL,                                      # no dummies
  SARIMA_Covid     = "covid",                                   # Covid only
  ARIMAX           = c(week_dummies, "covid"),                  # main model
  ARIMAX_EidOnly   = week_dummies,                              # Eid only
  ARIMAX_Simple    = c("Eid_before", "Eid_during", "Eid_after", "covid"),
  ARIMAX_No2020_21 = week_dummies                               # 2020-2021 removed
)

orders <- data.frame()

for (m in names(models_arima)) {
  
  y_log <- log(y)
  if (m == "ARIMAX_No2020_21") y_log[data$Tahun %in% c(2020, 2021)] <- NA
  
  if (is.null(models_arima[[m]])) {
    X <- NULL
  } else {
    X <- as.matrix(data[, models_arima[[m]], drop = FALSE])
  }
  
  # 1. Order selection by AICc
  y_train <- ts(y_log[1:row_dec2021], start = c(2012, 1), frequency = 12)
  if (is.null(X)) X_train <- NULL else X_train <- X[1:row_dec2021, , drop = FALSE]
  
  best_aicc <- Inf
  for (p in 0:2) for (q in 0:2) for (P in 0:1) for (Q in 0:1) {
    fit <- try(Arima(y_train, order = c(p, 1, q), seasonal = c(P, 1, Q),
                     xreg = X_train, method = "ML"), silent = TRUE)
    if (!inherits(fit, "try-error") && fit$aicc < best_aicc) {
      best_aicc <- fit$aicc
      best <- c(p, q, P, Q)
    }
  }
  orders <- rbind(orders, data.frame(Series = series, Model = m,
                                     Order = paste0("(", best[1], ",1,", best[2], ")(", best[3], ",1,", best[4], ")[12]")))
  
  # 2. Rolling origin
  forecast_all <- c()
  for (end in origins) {
    h     <- min(12, row_last - end)
    y_fit <- ts(y_log[1:end], start = c(2012, 1), frequency = 12)
    if (is.null(X)) {
      fit <- Arima(y_fit, order = c(best[1], 1, best[2]),
                   seasonal = c(best[3], 1, best[4]), method = "ML")
      fc  <- forecast(fit, h = h)
    } else {
      fit <- Arima(y_fit, order = c(best[1], 1, best[2]),
                   seasonal = c(best[3], 1, best[4]),
                   xreg = X[1:end, , drop = FALSE], method = "ML")
      fc  <- forecast(fit, xreg = X[(end + 1):(end + h), , drop = FALSE])
    }
    forecast_all <- c(forecast_all, exp(fc$mean))
  }
  result[[paste0(m, "_", series)]] <- forecast_all
  
  # Keep the selected order of the main ARIMAX for Part B4
  if (m == "ARIMAX") best_arimax <- best
}

print(orders)

# ---------------------------------------------------------------------
# XGBoost without and with Eid dummies
# ---------------------------------------------------------------------
n     <- nrow(data)
log_y <- log(y)
lag12 <- c(rep(NA, 12), log_y[1:(n - 12)])
lag24 <- c(rep(NA, 24), log_y[1:(n - 24)])
level <- as.numeric(stats::filter(lag12, rep(1 / 12, 12), sides = 1))  # mean of lags 12-23
level_lag12 <- c(rep(NA, 12), level[1:(n - 12)])

features <- data.frame(
  lag12_rel = lag12 - level,
  lag24_rel = lag24 - level,
  growth    = level - level_lag12,
  month     = data$Bulan,
  covid     = data$covid
)
target <- log_y - level
usable <- complete.cases(features)         

# Hyperparameter grid (extended)
grid <- expand.grid(max_depth = 1:4, eta = c(0.01, 0.03, 0.06, 0.1),
                    nrounds = c(100, 200, 400, 800),
                    min_child_weight = c(1, 3), lambda = c(1, 5))

xgb_settings <- data.frame()

for (m in c("XGB", "XGB_Eid")) {
  
  if (m == "XGB") X <- as.matrix(features)
  if (m == "XGB_Eid") X <- as.matrix(cbind(features, data[, week_dummies]))
  
  # 1. Choose settings: average MAPE in validation years 2019, 2020, 2021
  grid_mape <- rep(NA, nrow(grid))
  for (g in 1:nrow(grid)) {
    params <- list(objective = "reg:squarederror", max_depth = grid$max_depth[g],
                   eta = grid$eta[g], min_child_weight = grid$min_child_weight[g],
                   lambda = grid$lambda[g], subsample = 0.8,
                   colsample_bytree = 0.8, seed = 42, nthread = 2)
    mape_val <- c()
    for (year in 2018:2020) {
      end   <- which(data$Periode == paste0(year, "-12"))
      train <- which(usable & 1:n <= end)
      valid <- (end + 1):(end + 12)
      set.seed(42)
      model <- xgb.train(params = params,
                         data = xgb.DMatrix(X[train, ], label = target[train]),
                         nrounds = grid$nrounds[g], verbose = 0)
      pred  <- exp(predict(model, xgb.DMatrix(X[valid, , drop = FALSE])) + level[valid])
      mape_val <- c(mape_val, mean(abs((y[valid] - pred) / y[valid])) * 100)
    }
    grid_mape[g] <- mean(mape_val)
  }
  b <- which.min(grid_mape)
  xgb_settings <- rbind(xgb_settings, data.frame(Series = series, Model = m, grid[b, ]))
  
  # 2. Rolling origin with the chosen settings
  params <- list(objective = "reg:squarederror", max_depth = grid$max_depth[b],
                 eta = grid$eta[b], min_child_weight = grid$min_child_weight[b],
                 lambda = grid$lambda[b], subsample = 0.8,
                 colsample_bytree = 0.8, seed = 42, nthread = 2)
  forecast_all <- c()
  for (end in origins) {
    h     <- min(12, row_last - end)
    train <- which(usable & 1:n <= end)
    new   <- (end + 1):(end + h)
    set.seed(42)
    model <- xgb.train(params = params,
                       data = xgb.DMatrix(X[train, ], label = target[train]),
                       nrounds = grid$nrounds[b], verbose = 0)
    forecast_all <- c(forecast_all,
                      exp(predict(model, xgb.DMatrix(X[new, , drop = FALSE])) + level[new]))
  }
  result[[paste0(m, "_", series)]] <- forecast_all
}

print(xgb_settings)

# ---------------------------------------------------------------------
# Accuracy (57 test months), MAPE per round, and Diebold-Mariano test
#     DM compares each model with ARIMAX (p < 0.05: accuracy differs)
# ---------------------------------------------------------------------
all_models <- c("SN", "SARIMA", "SARIMA_Covid", "ARIMAX", "ARIMAX_EidOnly",
                "ARIMAX_Simple", "ARIMAX_No2020_21", "XGB", "XGB_Eid")

train_sn <- 13:row_dec2021
scale    <- mean(abs(y[train_sn] - y[train_sn - 12]))   # MASE denominator

actual       <- result[[series]]
error_arimax <- actual - result[[paste0("ARIMAX_", series)]]

accuracy  <- data.frame()
per_round <- data.frame()

for (m in all_models) {
  error <- actual - result[[paste0(m, "_", series)]]
  ape   <- abs(error / actual) * 100
  
  if (m == "ARIMAX") {
    dm_sq <- NA; dm_abs <- NA
  } else {
    dm_sq  <- dm.test(error_arimax, error, h = 1, power = 2)$p.value
    dm_abs <- dm.test(error_arimax, error, h = 1, power = 1)$p.value
  }
  
  accuracy <- rbind(accuracy, data.frame(
    Series = series, Model = m,
    MAPE   = mean(ape),
    MdAPE  = median(ape),
    RMSE   = sqrt(mean(error^2)) / 1000,      # IDR trillion
    MAE    = mean(abs(error)) / 1000,         # IDR trillion
    MASE   = mean(abs(error)) / scale,
    DM_p_squared  = dm_sq,
    DM_p_absolute = dm_abs
  ))
  
  per_round <- rbind(per_round, data.frame(
    Series = series, Model = m, t(tapply(ape, result$Round, mean))))
}

print(accuracy, digits = 4)
print(per_round, digits = 4)   # columns X1-X5 = 2022, 2023, 2024, 2025, 2026

# ---------------------------------------------------------------------
# Main ARIMAX on all data: multipliers with 95% CI (Table 5)
#     and residual diagnostics
# ---------------------------------------------------------------------
y_all <- ts(log(y[1:row_last]), start = c(2012, 1), frequency = 12)
X_all <- as.matrix(data[1:row_last, c(week_dummies, "covid")])

fit_all <- Arima(y_all, order = c(best_arimax[1], 1, best_arimax[2]),
                 seasonal = c(best_arimax[3], 1, best_arimax[4]),
                 xreg = X_all, method = "ML")
fit_all                                   # ARMA coefficients for Eq. 13/14

b  <- coef(fit_all)[c(week_dummies, "covid")]
se <- sqrt(diag(fit_all$var.coef))[c(week_dummies, "covid")]
table5 <- data.frame(
  Dummy      = names(b),
  Multiplier = exp(b),
  CI_lower   = exp(b - 1.96 * se),
  CI_upper   = exp(b + 1.96 * se),
  Significant = abs(b / se) > 1.96
)
print(table5, digits = 3)

# Number of Eid occurrences in each week (all data)
c(week1 = length(week1), week2 = length(week2),
  week3 = length(week3), week4 = length(week4))

# Residual diagnostics: Ljung-Box at several lags, normality, ACF
n_arma <- best_arimax[1] + best_arimax[2] + best_arimax[3] + best_arimax[4]
res    <- residuals(fit_all)
Box.test(res, lag = 12, type = "Ljung-Box", fitdf = n_arma)
Box.test(res, lag = 24, type = "Ljung-Box", fitdf = n_arma)
Box.test(res, lag = 36, type = "Ljung-Box", fitdf = n_arma)
shapiro.test(res)
Acf(res, lag.max = 36, main = paste("Residuals,", series))

# Outflow only: the same model with an outlier dummy for May 2022
if (series == "Outflow") {
  X_out   <- cbind(X_all, outlier_2022_05 = data$outlier_2022_05[1:row_last])
  fit_out <- Arima(y_all, order = c(best_arimax[1], 1, best_arimax[2]),
                   seasonal = c(best_arimax[3], 1, best_arimax[4]),
                   xreg = X_out, method = "ML")
  print(fit_out)
  res_out <- residuals(fit_out)
  print(Box.test(res_out, lag = 12, type = "Ljung-Box", fitdf = n_arma))
  print(Box.test(res_out, lag = 24, type = "Ljung-Box", fitdf = n_arma))
  print(Box.test(res_out, lag = 36, type = "Ljung-Box", fitdf = n_arma))
  print(shapiro.test(res_out))
}

# =====================================================================
# PART C. Figure 3 (after Part B has been run for Inflow and Outflow)
# =====================================================================

png("results/Figure3_Actual_vs_Forecast.png", width = 2850, height = 2100, res = 300)
par(mfrow = c(2, 1), mar = c(3, 4.5, 2.5, 1), las = 1, bty = "l")
x       <- 1:nrow(result)
january <- grepl("-01$", result$Periode)

for (s in c("Inflow", "Outflow")) {
  actual_s <- result[[s]] / 1000
  arimax_s <- result[[paste0("ARIMAX_", s)]] / 1000
  xgb_s    <- result[[paste0("XGB_Eid_", s)]] / 1000
  plot(x, actual_s, type = "o", pch = 16, lwd = 2, xaxt = "n", xlab = "",
       ylab = "IDR trillion", main = paste("Currency", tolower(s)),
       ylim = c(0, 1.2 * max(actual_s, arimax_s, xgb_s)))
  lines(x, arimax_s, type = "o", pch = 15, lwd = 2, lty = 2, col = "#eb6834")
  lines(x, xgb_s,    type = "o", pch = 17, lwd = 2, col = "#2a78d6")
  axis(1, at = x[january], labels = substr(result$Periode[january], 1, 4))
  legend("topright", c("Actual", "ARIMAX", "XGBoost with Eid dummies"),
         bty = "n", horiz = TRUE, col = c("black", "#eb6834", "#2a78d6"),
         lty = c(1, 2, 1), pch = c(16, 15, 17), lwd = 2)
}
dev.off()

