# ---------------------------------------------------------------------------
# Packages — install any that are missing, then load
# ---------------------------------------------------------------------------
pkgs <- c("readxl", "dplyr", "stringr", "lubridate", "arules", "arulesViz")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs)) install.packages(new_pkgs)


# 01_load_clean.R

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

artifact_dir <- "data"
if (!dir.exists(artifact_dir)) dir.create(artifact_dir, recursive = TRUE)

# --- Input ---
df <- read_excel("online_retail_II.xlsx")
head(df, 20)


df_clean <- df %>%
  filter(
    !is.na(Invoice),
    !is.na(Description),
    Quantity > 0,
    !str_detect(Invoice, "^C")   # credit notes / returns
  ) %>%
  mutate(
    Invoice      = as.character(Invoice),
    StockCode    = as.character(StockCode),
    Description  = str_squish(str_to_upper(Description)),
    `Customer ID` = as.integer(`Customer ID`),
    Country      = as.character(Country)
  ) %>%
  select(Invoice, StockCode, Description, Quantity, InvoiceDate, Price, `Customer ID`, Country)

# --- Lightweight profiling (used later to set minsup/minconf) ---
cat("Rows after cleaning:", nrow(df_clean), "\n")
cat("Distinct invoices:", n_distinct(df_clean$Invoice), "\n")
cat("Distinct items (Description):", n_distinct(df_clean$Description), "\n")

# Top items by line frequency (not transaction-level support yet)
df_clean %>%
  count(Description, sort = TRUE) %>%
  slice_head(n = 20) %>%
  print(n = 20)

# Basket size at invoice-line level (will be deduplicated to unique items per invoice in step 2)
df_clean %>%
  count(Invoice, name = "n_lines") %>%
  summarise(
    mean_lines   = mean(n_lines),
    median_lines = median(n_lines),
    p90_lines    = quantile(n_lines, 0.90),
    p99_lines    = quantile(n_lines, 0.99)
  ) %>%
  print()

saveRDS(df_clean, file.path(artifact_dir, "online_retail_clean.rds"))

# --- Temporal profiling (lubridate) ---
# Useful for understanding seasonality before picking support thresholds
df_clean %>%
  mutate(ym = format(InvoiceDate, "%Y-%m")) %>%
  distinct(Invoice, ym) %>%
  count(ym, name = "n_invoices") %>%
  arrange(ym) %>%
  print(n = 30)

df_clean %>%
  mutate(DoW = wday(InvoiceDate, label = TRUE, week_start = 1)) %>%
  distinct(Invoice, DoW) %>%
  count(DoW, name = "n_invoices") %>%
  print()


# 02_build_transactions.R
# Goal: Convert the cleaned invoice-line table into market-basket format
# (one basket per Invoice) and create an 'arules' transactions object.

library(dplyr)
library(arules)

df_clean <- readRDS(file.path(artifact_dir, "online_retail_clean.rds"))

# Build StockCode → canonical Description lookup (most frequent description per code).
# Using StockCode as the basket item avoids spurious item splits from minor description typos.
item_labels <- df_clean %>%
  count(StockCode, Description, sort = TRUE) %>%
  distinct(StockCode, .keep_all = TRUE) %>%
  select(StockCode, Description)

# Only substitute description when it maps to exactly one StockCode;
# shared descriptions (e.g. product variants) keep the StockCode as label.
unique_descs <- item_labels %>%
  count(Description) %>%
  filter(n == 1) %>%
  pull(Description)

item_labels <- item_labels %>%
  mutate(label = if_else(Description %in% unique_descs, Description, StockCode))

# 1) Build baskets: unique StockCodes per invoice
baskets <- df_clean %>%
  distinct(Invoice, StockCode) %>%
  group_by(Invoice) %>%
  summarise(items = list(StockCode), .groups = "drop")

# 2) Convert to transactions and swap StockCode labels for readable descriptions
trans <- as(baskets$items, "transactions")
label_map         <- setNames(item_labels$label, item_labels$StockCode)
current_labels    <- itemLabels(trans)
itemLabels(trans) <- label_map[current_labels]

# 3) Drop single-item baskets — cannot form rules and slightly deflate support estimates
trans <- trans[size(trans) >= 2]
summary(trans)

# 4) Quick checks for downstream parameter tuning
itemFrequencyPlot(trans, topN = 20, type = "absolute")

basket_sizes <- size(trans)
summary(basket_sizes)

saveRDS(trans, file.path(artifact_dir, "online_retail_transactions.rds"))


# 03_itemsets_rules.R

# --- Frequent itemsets (Apriori) ---

fis_apriori <- apriori(
  trans,
  parameter = list(
    target = "frequent itemsets",
    supp   = 0.01,   
    maxlen = 3       
  )
)

cat("Frequent itemsets (apriori):", length(fis_apriori), "\n")
inspect(head(sort(fis_apriori, by = "support"), 20))

# --- Frequent itemsets (Eclat) ---
fis_eclat <- eclat(
  trans,
  parameter = list(
    supp   = 0.01,
    maxlen = 3
  )
)

cat("Frequent itemsets (eclat):", length(fis_eclat), "\n")
inspect(head(sort(fis_eclat, by = "support"), 20))

# Sanity check: both algorithms must agree on itemset count
stopifnot(length(fis_apriori) == length(fis_eclat))

# --- Association rules (Apriori) ---
# Basic configuration: short LHS, single-item RHS tends to be the easiest to interpret.
rules_raw <- apriori(
  trans,
  parameter = list(
    supp   = 0.005,  # lower than itemsets to discover cross-sell patterns
    conf   = 0.20,
    minlen = 2,
    maxlen = 4
  )
)

cat("Rules (raw):", length(rules_raw), "\n")

# Filter for non-trivial positive association (lift >= 1.5 avoids noise from near-random co-occurrences)
rules <- subset(rules_raw, subset = lift >= 1.5)

# Remove redundant rules (same signal, longer antecedent)
rules <- rules[!is.redundant(rules)]

cat("Rules (filtered & non-redundant):", length(rules), "\n")

# Inspect strongest rules by lift (good for "interestingness")
inspect(head(sort(rules, by = "lift"), 20))

# Inspect strongest rules by confidence (good for "predictability")
inspect(head(sort(rules, by = "confidence"), 20))

# Persist outputs for reporting / visualization
saveRDS(fis_apriori, file.path(artifact_dir, "fis_apriori.rds"))
saveRDS(fis_eclat,   file.path(artifact_dir, "fis_eclat.rds"))
saveRDS(rules,       file.path(artifact_dir, "rules_final.rds"))


# 04_rules_curation.R

rules_raw <- readRDS(file.path(artifact_dir, "rules_final.rds"))  # already non-redundant (filtered in section 03)

cat("Rules loaded:", length(rules_raw), "\n")

# --- 1) Focus on interpretable structure: single-item RHS (cleanest narrative) ---
rules_rhs1 <- subset(rules_raw, subset = size(rhs) == 1)
cat("Rules with |RHS|=1:", length(rules_rhs1), "\n")

# --- 3) Create report-oriented "views" ---
# A) High-lift bundles (strong co-occurrence)
rules_bundle <- head(sort(rules_rhs1, by = "lift"), 30)

# B) High-confidence "if X then Y" (predictability)
rules_conf <- head(sort(rules_rhs1, by = "confidence"), 30)

# C) More "businessy" cross-sell:
# Require decent support (not ultra-rare), moderate-to-high lift
rules_xsell <- rules_rhs1 %>%
  subset(subset = support >= 0.01 & lift >= 2) %>%
  sort(by = "lift") %>%
  head(30)

# --- 4) Convert to data frames for export ---
df_bundle <- as(rules_bundle, "data.frame")
df_conf   <- as(rules_conf,   "data.frame")
df_xsell  <- as(rules_xsell,  "data.frame")

write.csv(df_bundle, file.path(artifact_dir, "rules_top_lift.csv"), row.names = FALSE)
write.csv(df_conf,   file.path(artifact_dir, "rules_top_confidence.csv"), row.names = FALSE)
write.csv(df_xsell,  file.path(artifact_dir, "rules_cross_sell.csv"), row.names = FALSE)

# --- 5) Minimal console output (for quick validation) ---
cat("\nTop 10 by lift:\n")
inspect(head(sort(rules_bundle, by = "lift"), 10))

cat("\nTop 10 by confidence:\n")
inspect(head(sort(rules_conf, by = "confidence"), 10))

cat("\nTop 10 cross-sell (support>=0.01, lift>=2):\n")
inspect(head(rules_xsell, 10))


# 05_actionable_rules.R

library(stringr)

rules <- readRDS(file.path(artifact_dir, "rules_final.rds"))  # already non-redundant (filtered in section 03)

# 1) Enforce interpretable shape
rules <- subset(rules, subset = size(lhs) <= 2 & size(rhs) == 1)

# 2) Drop operational artifacts (shipping/fees) that create fake associations
drop_terms <- c("POSTAGE", "DOTCOM", "CARRIAGE")
rules <- rules[!(lhs(rules) %pin% drop_terms)]
rules <- rules[!(rhs(rules) %pin% drop_terms)]

# 3) Prefer higher support and avoid extreme-lift niche bundles
# (extreme lift tends to be set/variant completion rather than broad cross-sell)
rules_actionable <- subset(
  rules,
  subset = support >= 0.015 & confidence >= 0.30 & lift >= 1.5 & lift <= 15
)

cat("Actionable candidate rules:", length(rules_actionable), "\n")

# 4) Rank by lift first (interestingness) and export top-N
rules_top <- head(sort(rules_actionable, by = "lift"), 50)
inspect(head(rules_top, 20))

write.csv(as(rules_top, "data.frame"), file.path(artifact_dir, "rules_actionable_top50.csv"), row.names = FALSE)
saveRDS(rules_top, file.path(artifact_dir, "rules_actionable_top50.rds"))

# 06_visualize_rules.R
library(arulesViz)

rules <- readRDS(file.path(artifact_dir, "rules_actionable_top50.rds"))

# Scatter plot: support vs confidence, colored by lift
plot(
  rules_top,
  measure = c("support", "confidence"),
  shading = "lift"
)

# Network-style visualization (top 20 for readability)
# set.seed ensures reproducible force-directed layout
set.seed(42)
rules_net <- head(sort(rules, by = "lift"), 20)
plot(
  rules_net,
  method = "graph",
  engine = "htmlwidget"
)
