# =============================================================================
# ShopMatch: Regression + Collaborative Filtering for Real-Time Product
# Suggestions using E-Commerce Purchase History
# Group 12 - Yuvraj, Sagar Raut
#
# HOW TO RUN (RStudio):
#   1. Put this file in an empty folder, open it in RStudio.
#   2. (Recommended) Put the real dataset in the SAME folder as ONE of:
#        data.csv           -> Kaggle "E-Commerce Data" (carrie1/ecommerce-data)
#        Online Retail.xlsx -> UCI "Online Retail" (same data, Excel format)
#      If neither file is found the script tries to download the UCI file.
#      If that also fails it falls back to SIMULATED data and says so loudly.
#   3. Session > Set Working Directory > To Source File Location
#   4. Click "Source" (Ctrl+Shift+S). Figures + tables go to ./outputs
#   5. Open ShopMatch_Report.Rmd and click Knit to build the PDF report.
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "ggplot2", "readr", "readxl", "scales")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(readr); library(readxl); library(scales)
})
options(dplyr.summarise.inform = FALSE)
set.seed(42)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))
save_fig <- function(p, name, w = 7, h = 4.5)
  ggsave(file.path("outputs/figures", name), p, width = w, height = h, dpi = 150)

res <- list()   # everything the PDF report needs is collected here

# ---- 1. DATA COLLECTION & SOURCING -----------------------------------------
simulate_data <- function(n_cust = 1500, n_prod = 80) {
  prod_id   <- sprintf("P%03d", seq_len(n_prod))
  price     <- round(runif(n_prod, 0.5, 15), 2)
  pop       <- rexp(n_prod) + 0.2
  prod_grp  <- sample(1:4, n_prod, TRUE)
  cust_grp  <- sample(1:4, n_cust, TRUE)
  countries <- c("United Kingdom", "Germany", "France", "Spain", "Netherlands")
  rows <- vector("list", n_cust); inv <- 10000
  for (i in seq_len(n_cust)) {
    w <- ifelse(prod_grp == cust_grp[i], 5, 1) * pop
    ctry <- sample(countries, 1, prob = c(.9, .03, .03, .02, .02))
    n_inv <- 1 + rpois(1, 3)
    parts <- vector("list", n_inv)
    for (j in seq_len(n_inv)) {
      inv <- inv + 1
      k <- min(n_prod, 1 + rpois(1, 5))
      it <- sample(n_prod, k, prob = w)
      parts[[j]] <- data.frame(
        InvoiceNo = as.character(inv), StockCode = prod_id[it],
        Description = paste("Product", it), Quantity = 1 + rpois(k, 3),
        InvoiceDate = as.POSIXct("2010-12-01", tz = "UTC") +
          runif(1, 0, 373) * 86400,
        UnitPrice = price[it], CustomerID = 12000 + i, Country = ctry,
        stringsAsFactors = FALSE)
    }
    rows[[i]] <- do.call(rbind, parts)
  }
  do.call(rbind, rows)
}

load_data <- function() {
  if (file.exists("data.csv")) {
    cat("Loading local Kaggle file data.csv\n")
    return(list(df = read_csv("data.csv", locale = locale(encoding = "latin1"),
                              show_col_types = FALSE),
                src = "Kaggle E-Commerce Data (data.csv; UCI Online Retail)"))
  }
  xl <- list.files(".", pattern = "online.?retail.*\\.xlsx$", ignore.case = TRUE)
  xl <- xl[!grepl("^~\\$", xl)]
  if (length(xl) > 0) {
    cat("Loading local Excel file:", xl[1], "(this takes about a minute)\n")
    return(list(df = read_excel(xl[1]),
                src = "UCI Online Retail (local Excel file)"))
  }
  cat("No local file found - trying UCI download...\n")
  dl <- tryCatch({
    zf <- tempfile(fileext = ".zip")
    download.file("https://archive.ics.uci.edu/static/public/352/online+retail.zip",
                  zf, mode = "wb", quiet = TRUE)
    ex <- file.path(tempdir(), "uci_retail"); dir.create(ex, showWarnings = FALSE)
    unzip(zf, exdir = ex)
    f <- list.files(ex, pattern = "\\.xlsx$", full.names = TRUE)[1]
    list(df = read_excel(f), src = "UCI Online Retail (downloaded)")
  }, error = function(e) NULL)
  if (!is.null(dl)) return(dl)
  cat("\n*** WARNING: real data unavailable. Using SIMULATED data. ***\n",
      "*** Put data.csv from Kaggle next to this script for the real results. ***\n\n")
  list(df = simulate_data(), src = "SIMULATED data (real dataset not found)")
}

ld  <- load_data()
raw <- ld$df
res$data_source <- ld$src
cat("Data source:", res$data_source, "\n")
stopifnot(all(c("InvoiceNo", "StockCode", "Description", "Quantity",
                "InvoiceDate", "UnitPrice", "CustomerID", "Country") %in% names(raw)))
cat("Raw dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
res$raw_dim <- dim(raw)

# ---- 2. DATA PRE-PROCESSING & WRANGLING ------------------------------------
step_log <- data.frame(step = "Raw data", rows = nrow(raw), stringsAsFactors = FALSE)
log_step <- function(d, label) {
  step_log <<- rbind(step_log, data.frame(step = label, rows = nrow(d),
                                          stringsAsFactors = FALSE))
  d
}

res$missing <- data.frame(column = names(raw),
                          missing = sapply(raw, function(x) sum(is.na(x))),
                          row.names = NULL)

df <- raw
if (!inherits(df$InvoiceDate, "POSIXt")) {
  df$InvoiceDate <- as.POSIXct(as.character(df$InvoiceDate),
                               format = "%m/%d/%Y %H:%M", tz = "UTC")
}
df <- df %>%
  mutate(InvoiceNo = as.character(InvoiceNo), StockCode = as.character(StockCode),
         CustomerID = as.character(CustomerID)) %>%
  filter(!is.na(CustomerID), CustomerID != "NA") %>% log_step("Drop rows with no CustomerID") %>%
  filter(!grepl("^C", InvoiceNo))                  %>% log_step("Drop cancelled invoices (start with C)") %>%
  filter(Quantity > 0, UnitPrice > 0)              %>% log_step("Drop non-positive quantity / price") %>%
  filter(!grepl("^(POST|DOT|M|BANK CHARGES|PADS|AMAZONFEE|CRUK|B|D|S)$", StockCode)) %>%
  log_step("Drop non-product codes (postage, fees, ...)") %>%
  distinct(InvoiceNo, StockCode, Quantity, UnitPrice, CustomerID, .keep_all = TRUE) %>%
  log_step("Drop duplicate lines") %>%
  mutate(Revenue = Quantity * UnitPrice)
res$cleaning <- step_log
print(step_log)

# product descriptions (most frequent description per code)
prod_desc <- df %>% filter(!is.na(Description)) %>%
  count(StockCode, Description) %>% arrange(StockCode, desc(n)) %>%
  group_by(StockCode) %>% slice(1) %>% ungroup() %>%
  select(StockCode, Description)

# keep the 100 most widely bought products and customers with >= 3 of them
TOP_N <- 100
top_prod <- df %>% group_by(StockCode) %>%
  summarise(customers = n_distinct(CustomerID)) %>%
  arrange(desc(customers)) %>% slice_head(n = TOP_N)
dfm <- df %>% filter(StockCode %in% top_prod$StockCode)
keep_cust <- dfm %>% group_by(CustomerID) %>%
  summarise(k = n_distinct(StockCode)) %>% filter(k >= 3) %>% pull(CustomerID)
dfm <- dfm %>% filter(CustomerID %in% keep_cust)

res$dims <- list(rows_clean = nrow(df), customers = n_distinct(df$CustomerID),
                 products = n_distinct(df$StockCode), invoices = n_distinct(df$InvoiceNo),
                 date_min = format(min(df$InvoiceDate), "%Y-%m-%d"),
                 date_max = format(max(df$InvoiceDate), "%Y-%m-%d"),
                 model_customers = length(keep_cust), model_products = TOP_N,
                 sparsity = NA_real_)

# customer-level features (whole cleaned data)
make_features <- function(d, ref_date) {
  d %>% group_by(CustomerID) %>%
    summarise(n_invoices = n_distinct(InvoiceNo), n_items = sum(Quantity),
              total_spend = sum(Revenue), n_products = n_distinct(StockCode),
              last_buy = max(InvoiceDate), country = first(Country)) %>%
    mutate(avg_basket = total_spend / n_invoices,
           recency_days = as.numeric(difftime(ref_date, last_buy, units = "days")),
           is_uk = as.integer(country == "United Kingdom")) %>%
    select(-last_buy)
}
ref_date <- max(df$InvoiceDate)
cust <- make_features(df, ref_date)
res$summary_stats <- cust %>%
  summarise(across(c(n_invoices, n_items, total_spend, avg_basket, n_products, recency_days),
                   list(mean = mean, median = median, sd = sd, max = max))) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"),
               names_pattern = "(.*)_(mean|median|sd|max)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(res$summary_stats)

# ---- 3. EXPLORATORY & INFERENTIAL ANALYSIS ---------------------------------
# 3a. Exploratory plots
monthly <- df %>% mutate(month = as.Date(format(InvoiceDate, "%Y-%m-01"))) %>%
  group_by(month) %>% summarise(revenue = sum(Revenue), orders = n_distinct(InvoiceNo))
p1 <- ggplot(monthly, aes(month, revenue)) + geom_line(linewidth = 1, colour = "#2C5F8A") +
  geom_point(colour = "#2C5F8A") + scale_y_continuous(labels = comma) +
  labs(title = "Monthly revenue", x = NULL, y = "Revenue")
save_fig(p1, "01_monthly_revenue.png")

top10 <- df %>% group_by(StockCode) %>% summarise(units = sum(Quantity)) %>%
  arrange(desc(units)) %>% slice_head(n = 10) %>% left_join(prod_desc, by = "StockCode") %>%
  mutate(label = substr(ifelse(is.na(Description), StockCode, Description), 1, 30))
p2 <- ggplot(top10, aes(reorder(label, units), units)) + geom_col(fill = "#2C5F8A") +
  coord_flip() + scale_y_continuous(labels = comma) +
  labs(title = "Top 10 products by units sold", x = NULL, y = "Units")
save_fig(p2, "02_top_products.png", h = 4.5)

p3 <- ggplot(cust, aes(total_spend)) + geom_histogram(bins = 40, fill = "#2C5F8A", colour = "white") +
  scale_x_log10(labels = comma) + labs(title = "Customer lifetime spend (log scale)",
                                       x = "Total spend", y = "Customers")
save_fig(p3, "03_spend_distribution.png")

basket <- df %>% group_by(InvoiceNo) %>% summarise(items = n_distinct(StockCode))
p4 <- ggplot(basket, aes(items)) + geom_histogram(binwidth = 2, fill = "#2C5F8A", colour = "white") +
  coord_cartesian(xlim = c(0, 80)) + labs(title = "Distinct products per basket", x = "Products in basket", y = "Invoices")
save_fig(p4, "04_basket_size.png")

ctry <- df %>% group_by(Country) %>% summarise(revenue = sum(Revenue)) %>%
  arrange(desc(revenue)) %>% slice_head(n = 6)
p5 <- ggplot(ctry, aes(reorder(Country, revenue), revenue)) + geom_col(fill = "#2C5F8A") +
  coord_flip() + scale_y_continuous(labels = comma) + labs(title = "Revenue by country (top 6)", x = NULL, y = "Revenue")
save_fig(p5, "05_country_revenue.png")

# 3b. Inferential statistics
ct1 <- cor.test(cust$n_invoices, cust$total_spend, method = "spearman", exact = FALSE)
wt  <- wilcox.test(total_spend ~ is_uk, data = cust)
kt  <- kruskal.test(total_spend ~ cut(n_products, breaks = quantile(n_products, 0:4 / 4),
                                      include.lowest = TRUE), data = cust)
res$tests <- data.frame(
  test = c("Spearman correlation: number of orders vs total spend",
           "Wilcoxon rank-sum: spend, UK vs non-UK customers",
           "Kruskal-Wallis: spend across product-variety quartiles"),
  statistic = c(unname(ct1$estimate), unname(wt$statistic), unname(kt$statistic)),
  p_value = c(ct1$p.value, wt$p.value, kt$p.value),
  stringsAsFactors = FALSE)
print(res$tests)

# ---- 4. MODEL TRAINING -----------------------------------------------------
# customer x product binary purchase matrix
cust_ids <- sort(unique(dfm$CustomerID)); prod_ids <- top_prod$StockCode
M <- matrix(0, length(cust_ids), length(prod_ids), dimnames = list(cust_ids, prod_ids))
pairs <- unique(dfm[, c("CustomerID", "StockCode")])
M[cbind(match(pairs$CustomerID, cust_ids), match(pairs$StockCode, prod_ids))] <- 1
res$dims$sparsity <- round(1 - mean(M), 4)
cat("Purchase matrix:", nrow(M), "customers x", ncol(M), "products; sparsity",
    res$dims$sparsity, "\n")

# 4a. Logistic regression: will a customer buy the best-selling product?
target <- prod_ids[which.max(colSums(M))]
target_name <- prod_desc$Description[match(target, prod_desc$StockCode)]
res$target <- list(code = target, name = target_name, prevalence = round(mean(M[, target]), 4))
f_nt <- make_features(df %>% filter(CustomerID %in% cust_ids, StockCode != target), ref_date)
lg <- f_nt %>% mutate(y = M[CustomerID, target]) %>% as.data.frame()
strat_split <- function(y, frac = 0.8) {
  unlist(lapply(split(seq_along(y), y), function(i) sample(i, floor(frac * length(i)))))
}
tr_idx <- strat_split(lg$y)
train <- lg[tr_idx, ]; test <- lg[-tr_idx, ]
form <- y ~ log1p(n_invoices) + log1p(n_items) + log1p(avg_basket) +
  log1p(n_products) + recency_days + is_uk
logit <- glm(form, data = train, family = binomial)
cs <- summary(logit)$coefficients
res$logit_coef <- data.frame(term = rownames(cs), estimate = cs[, 1],
                             odds_ratio = exp(cs[, 1]), p_value = cs[, 4], row.names = NULL)
print(res$logit_coef)

# 4b. Multiple linear regression: customer total spend
cust2 <- cust %>% as.data.frame()
tr2 <- sample(nrow(cust2), floor(0.8 * nrow(cust2)))
lm_train <- cust2[tr2, ]; lm_test <- cust2[-tr2, ]
lmod <- lm(log(total_spend) ~ log1p(n_invoices) + log1p(n_products) +
             recency_days + is_uk, data = lm_train)
ls <- summary(lmod)$coefficients
res$lm_coef <- data.frame(term = rownames(ls), estimate = ls[, 1], std_error = ls[, 2],
                          p_value = ls[, 4], row.names = NULL)
print(res$lm_coef)

# 4c. Item-based collaborative filtering (cosine similarity)
cosine_sim <- function(X) {
  nrm <- sqrt(colSums(X^2)); nrm[nrm == 0] <- 1
  S <- crossprod(X) / outer(nrm, nrm); diag(S) <- 0; S
}

# ---- 5. MODEL TESTING & EVALUATION -----------------------------------------
# 5a. Logistic regression metrics
auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
cls_metrics <- function(y, p, th) {
  pred <- as.integer(p >= th)
  tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1); tn <- sum(pred == 0 & y == 0)
  prec <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  rec  <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1   <- ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
  list(acc = (tp + tn) / length(y), prec = prec, rec = rec, f1 = f1,
       cm = matrix(c(tn, fp, fn, tp), 2, dimnames = list(Predicted = c("0", "1"), Actual = c("0", "1"))))
}
p_tr <- predict(logit, train, type = "response")
p_te <- predict(logit, test,  type = "response")
ths  <- seq(0.05, 0.95, 0.01)
th   <- ths[which.max(sapply(ths, function(t) cls_metrics(train$y, p_tr, t)$f1))]
m_te <- cls_metrics(test$y, p_te, th)
base_acc <- max(mean(test$y), 1 - mean(test$y))
res$logit_eval <- list(threshold = th, auc = auc(test$y, p_te), accuracy = m_te$acc,
                       precision = m_te$prec, recall = m_te$rec, f1 = m_te$f1,
                       majority_baseline = base_acc, n_train = nrow(train), n_test = nrow(test),
                       confusion = m_te$cm)
cat("\nLogistic regression (test set): AUC =", round(res$logit_eval$auc, 3),
    "| Accuracy =", round(m_te$acc, 3), "| Recall =", round(m_te$rec, 3), "\n")
print(m_te$cm)

roc <- do.call(rbind, lapply(seq(0, 1, 0.01), function(t) {
  pr <- as.integer(p_te >= t)
  data.frame(fpr = sum(pr == 1 & test$y == 0) / sum(test$y == 0),
             tpr = sum(pr == 1 & test$y == 1) / sum(test$y == 1))
}))
p6 <- ggplot(roc, aes(fpr, tpr)) + geom_line(linewidth = 1, colour = "#2C5F8A") +
  geom_abline(linetype = "dashed", colour = "grey50") +
  labs(title = sprintf("ROC curve (AUC = %.3f)", res$logit_eval$auc),
       x = "False positive rate", y = "True positive rate")
save_fig(p6, "06_roc_curve.png", h = 4.5)

cm_df <- as.data.frame(as.table(m_te$cm))
p7 <- ggplot(cm_df, aes(Actual, Predicted, fill = Freq)) + geom_tile() +
  geom_text(aes(label = Freq), size = 6, colour = "white") +
  scale_fill_gradient(low = "#9DB8D2", high = "#1B3B5A") + labs(title = "Confusion matrix (test set)") +
  theme(legend.position = "none")
save_fig(p7, "07_confusion_matrix.png", w = 4.5, h = 4)

# 5b. Linear regression metrics
pred_lm <- predict(lmod, lm_test)
act_lm  <- log(lm_test$total_spend)
res$lm_eval <- list(r2_train = summary(lmod)$r.squared,
                    adj_r2 = summary(lmod)$adj.r.squared,
                    r2_test = 1 - sum((act_lm - pred_lm)^2) / sum((act_lm - mean(act_lm))^2),
                    rmse_log = sqrt(mean((act_lm - pred_lm)^2)),
                    mae_log = mean(abs(act_lm - pred_lm)),
                    n_train = nrow(lm_train), n_test = nrow(lm_test))
cat("Linear regression (test): R2 =", round(res$lm_eval$r2_test, 3),
    "| RMSE(log) =", round(res$lm_eval$rmse_log, 3), "\n")
p8 <- ggplot(data.frame(pred = pred_lm, act = act_lm), aes(pred, act)) +
  geom_point(alpha = .35, colour = "#2C5F8A") + geom_abline(colour = "firebrick", linetype = "dashed") +
  labs(title = "Predicted vs actual log(total spend), test set", x = "Predicted", y = "Actual")
save_fig(p8, "08_lm_pred_vs_actual.png")

# 5c. Collaborative filtering: leave-one-out hit rate
hold <- apply(M, 1, function(r) sample(which(r == 1), 1))
Mtr  <- M; Mtr[cbind(seq_len(nrow(M)), hold)] <- 0
S_tr <- cosine_sim(Mtr)
sc_cf  <- Mtr %*% S_tr;                    sc_cf[Mtr == 1]  <- -Inf
pop_v  <- colSums(Mtr)
sc_pop <- matrix(pop_v, nrow(M), ncol(M), byrow = TRUE); sc_pop[Mtr == 1] <- -Inf
sc_rnd <- matrix(runif(length(M)), nrow(M));  sc_rnd[Mtr == 1] <- -Inf
rank_of_held <- function(sc) {
  hs <- sc[cbind(seq_len(nrow(sc)), hold)]
  rowSums(sc >= hs)                       # pessimistic rank (ties count against us)
}
eval_rank <- function(rk, name) data.frame(
  model = name, HitRate_at_5 = mean(rk <= 5), HitRate_at_10 = mean(rk <= 10),
  HitRate_at_20 = mean(rk <= 20), MRR = mean(1 / rk))
res$cf_eval <- rbind(eval_rank(rank_of_held(sc_cf),  "Item-based CF (cosine)"),
                     eval_rank(rank_of_held(sc_pop), "Popularity baseline"),
                     eval_rank(rank_of_held(sc_rnd), "Random baseline"))
res$cf_eval[, -1] <- round(res$cf_eval[, -1], 4)
print(res$cf_eval)

cf_long <- res$cf_eval %>% select(model, HitRate_at_5, HitRate_at_10, HitRate_at_20) %>%
  pivot_longer(-model, names_to = "k", values_to = "hit") %>%
  mutate(k = factor(sub("HitRate_at_", "Top-", k), levels = c("Top-5", "Top-10", "Top-20")))
p9 <- ggplot(cf_long, aes(k, hit, fill = model)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c("#2C5F8A", "#8FB3D1", "#C9C9C9")) +
  scale_y_continuous(labels = percent) + labs(title = "Recommendation hit rate (leave-one-out)",
                                              x = NULL, y = "Hit rate", fill = NULL) +
  theme(legend.position = "bottom")
save_fig(p9, "09_cf_hit_rate.png")

# item-item similarity heatmap (top 15 products, final model on all data)
S_all <- cosine_sim(M)
top15 <- order(colSums(M), decreasing = TRUE)[1:15]
hm <- as.data.frame(as.table(S_all[top15, top15]))
names(hm) <- c("A", "B", "sim")
p10 <- ggplot(hm, aes(A, B, fill = sim)) + geom_tile() +
  scale_fill_gradient(low = "white", high = "#1B3B5A") +
  labs(title = "Item-item cosine similarity (top 15 products)", x = NULL, y = NULL, fill = "Sim") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
save_fig(p10, "10_similarity_heatmap.png", h = 5)

# ---- 6. REAL-TIME RECOMMENDER DEMO ------------------------------------------
recommend <- function(customer_id, k = 5) {
  customer_id <- as.character(customer_id)
  if (!customer_id %in% rownames(M)) stop("Unknown customer id")
  s <- as.numeric(M[customer_id, ] %*% S_all); s[M[customer_id, ] == 1] <- -Inf
  top <- order(s, decreasing = TRUE)[1:k]
  data.frame(rank = seq_len(k), StockCode = prod_ids[top],
             Description = prod_desc$Description[match(prod_ids[top], prod_desc$StockCode)],
             score = round(s[top], 3), row.names = NULL)
}
demo_ids <- sample(rownames(M), 3)
res$examples <- do.call(rbind, lapply(demo_ids, function(id)
  cbind(customer = id, recommend(id, 5))))
cat("\nExample real-time recommendations:\n"); print(res$examples)

# ---- 7. SAVE EVERYTHING FOR THE REPORT --------------------------------------
saveRDS(res, "outputs/results.rds")
cat("\nDone. Figures and results saved in ./outputs. Now knit ShopMatch_Report.Rmd to PDF.\n")