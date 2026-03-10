# ============================================================
#  00_data_check.R  (v7 - fixed csranks API)
#
#  Newer csranks requires a covariance MATRIX, not an SE vector.
#  Fix: diag(SE^2) converts SE vector to diagonal variance matrix.
#
#  Saves to output/ folder in replication package root:
#    table1_journals.csv
#    table2_universities.csv
#    figure1_journals_marg.pdf   (mirrors paper Figure 1)
#    figure2_universities_marg.pdf (mirrors paper Figure 2)
#
#  INSTRUCTIONS:
#  Place in ROOT of replication package (same folder as master.R)
#  Session > Set Working Directory > To Source File Location
#  Then run the whole script.
# ============================================================


# ============================================================
#  PART 0: Packages
# ============================================================

cat("\n========================================\n")
cat("  REPLICATION PACKAGE DATA CHECK v7\n")
cat("========================================\n\n")

cat("--- PART 0: Packages ---\n\n")

cran_packages <- c("ggplot2", "dplyr", "readxl", "devtools")
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

if (!requireNamespace("csranks", quietly = TRUE)) {
  cat("Installing csranks from GitHub...\n")
  devtools::install_github("danielwilhelm/R-CS-ranks")
}
library(csranks)
cat("All packages loaded.\n\n")

# Create output folder
if (!dir.exists("output")) dir.create("output")
cat("Output folder: ", file.path(getwd(), "output"), "\n\n")


# ============================================================
#  PART 1: Journal data
# ============================================================

cat("--- PART 1: Journal Impact Factor Data ---\n\n")

ifdata_path <- file.path("journal IF", "IFdata.RData")
if (!file.exists(ifdata_path)) {
  cat("IFdata.RData not found - running convertdata.R...\n")
  source(file.path("journal IF", "convertdata.R"))
}
load(ifdata_path)

# Exact KMS (2003) top-30 list from authors' journalranking.R
top30names <- c("JEL", "QJE", "JEP", "JFE", "JPE", "Econometrica",
                "AER", "REStud", "RESTAT", "JLE", "JEEM", "JHumRes",
                "J INT ECON", "EconJ", "JMonEcon", "JEconometrics",
                "RAND J ECON", "J BUS ECON STAT", "JPubEcon",
                "JAppliedEconmet", "EUR ECON REV", "IER", "JET",
                "OXFORD B ECON STAT", "GAME ECON BEHAV",
                "J ECON DYN CONTROL", "SCAND J ECON",
                "ECONOMET THEOR", "Econ Theory", "EL")

jIF <- journalIF %>%
  filter(!is.na(IF), IF > 0, journal %in% top30names) %>%
  arrange(desc(IF)) %>%
  mutate(rank = row_number())

cat("KMS top-30 journals matched:", nrow(jIF), "of 30\n\n")

# --- Summary stats ---
jall <- journalIF %>% filter(!is.na(IF), IF > 0)
cat("--- Summary Statistics: All Valid Journals (n =", nrow(jall), ") ---\n")
cat("Mean IF       :", round(mean(jall$IF),   4), "\n")
cat("Median IF     :", round(median(jall$IF), 4), "\n")
cat("SD of IF      :", round(sd(jall$IF),     4), "\n")
cat("Mean SE       :", round(mean(jall$SE),   4), "\n")
cat("Mean N arts   :", round(mean(jall$n),    1), "\n")
cat("IF range      : [", round(min(jall$IF),  4), ",",
                         round(max(jall$IF),  4), "]\n\n")

cat("--- Summary Statistics: KMS Top 30 ---\n")
cat("Mean IF       :", round(mean(jIF$IF),   4), "\n")
cat("Median IF     :", round(median(jIF$IF), 4), "\n")
cat("SD of IF      :", round(sd(jIF$IF),     4), "\n")
cat("Mean SE       :", round(mean(jIF$SE),   4), "\n")
cat("IF range      : [", round(min(jIF$IF),  4), ",",
                         round(max(jIF$IF),  4), "]\n\n")

# --- Table 1 ---
table1 <- jIF %>%
  select(Rank = rank, Journal = journal, IF, SE, N_articles = n) %>%
  mutate(IF = round(IF, 4), SE = round(SE, 4))

cat("=== Table 1: KMS Top 30 Journals Ranked by Impact Factor ===\n\n")
print(as.data.frame(table1), row.names = FALSE)
write.csv(table1, file.path("output", "table1_journals.csv"), row.names = FALSE)
cat("\nSaved: output/table1_journals.csv\n\n")

# --- Compute marginal confidence sets ---
# KEY FIX: csranks now requires a covariance matrix, not an SE vector
# diag(SE^2) builds a diagonal matrix where variances are on the diagonal
cat("Computing marginal confidence sets for journal ranks...\n")
cat("(bootstrap R=1000, ~1-2 minutes)\n\n")

Sigma_journals <- diag(jIF$SE^2)   # p x p diagonal covariance matrix

CS_marg <- csranks::csranks(jIF$IF,
                             Sigma    = Sigma_journals,
                             coverage = 0.95,
                             stepdown = TRUE,
                             simul    = FALSE,
                             R        = 1000,
                             seed     = 101)
jIF$margCSL <- CS_marg$L
jIF$margCSU <- CS_marg$U
cat("Done.\n\n")

# --- Figure 1 (mirrors paper Figure 1) ---
# x-axis = rank, y-axis = journal names ordered rank 1 at bottom
fig1 <- ggplot(jIF, aes(x = rank, y = reorder(journal, rank))) +
  theme_bw() +
  geom_point(size = 1.5) +
  geom_errorbar(aes(xmin = margCSL, xmax = margCSU),
                width = 0.4) +
  scale_x_continuous(
    name   = "Rank",
    limits = c(1, 30),
    breaks = c(1, 5, 10, 15, 20, 25, 30)
  ) +
  ylab(NULL) +
  labs(
    title    = "Ranking of 30 Journals by 2011 Impact Factor",
    subtitle = "(with 95% marginal confidence sets)"
  )

ggsave(file.path("output", "figure1_journals_marg.pdf"),
       plot = fig1, width = 7, height = 7)
cat("Saved: output/figure1_journals_marg.pdf\n\n")


# ============================================================
#  PART 2: University data
# ============================================================

cat("--- PART 2: University RePEC Data ---\n\n")

top100_path <- file.path("Repec", "top100unis.csv")

if (!file.exists(top100_path)) {
  cat("top100unis.csv not found. Run master.R once to generate it.\n\n")
} else {
  top100 <- read.csv(top100_path)

  unis <- top100 %>%
    filter(!is.na(IFmean)) %>%
    arrange(desc(IFmean)) %>%
    mutate(rank = row_number())

  # --- Summary stats ---
  cat("--- Summary Statistics: 100 Universities ---\n")
  cat("Mean IFmean   :", round(mean(unis$IFmean),   4), "\n")
  cat("Median IFmean :", round(median(unis$IFmean), 4), "\n")
  cat("SD of IFmean  :", round(sd(unis$IFmean),     4), "\n")
  cat("Mean IFse     :", round(mean(unis$IFse),     4), "\n")
  cat("Total authors :", sum(unis$n), "\n")
  cat("IF range      : [", round(min(unis$IFmean),  4), ",",
                           round(max(unis$IFmean),  4), "]\n\n")

  # --- Table 2 ---
  table2 <- unis %>%
    slice(1:20) %>%
    select(Rank = rank, University = name, IFmean, IFse, N_authors = n) %>%
    mutate(IFmean = round(IFmean, 4), IFse = round(IFse, 4))

  cat("=== Table 2: Top 20 Universities by Impact Factor ===\n\n")
  print(as.data.frame(table2), row.names = FALSE)
  write.csv(table2, file.path("output", "table2_universities.csv"), row.names = FALSE)
  cat("\nSaved: output/table2_universities.csv\n\n")

  # --- Compute marginal confidence sets ---
  cat("Computing marginal confidence sets for university ranks...\n")
  cat("(bootstrap R=1000, ~1-2 minutes)\n\n")

  Sigma_unis <- diag(unis$IFse^2)   # 100 x 100 diagonal covariance matrix

  CS_unis <- csranks::csranks(unis$IFmean,
                               Sigma    = Sigma_unis,
                               coverage = 0.95,
                               stepdown = TRUE,
                               simul    = FALSE,
                               R        = 1000,
                               seed     = 101)
  unis$margCSL <- CS_unis$L
  unis$margCSU <- CS_unis$U
  cat("Done.\n\n")

  # --- Figure 2 (mirrors paper Figure 2) ---
  fig2 <- ggplot(unis, aes(x = rank, y = reorder(name, rank))) +
    theme_bw() +
    geom_point(size = 0.6) +
    geom_errorbar(aes(xmin = margCSL, xmax = margCSU),
                  width = 0.5) +
    scale_x_continuous(
      name   = "Rank",
      limits = c(1, 100),
      breaks = c(1, seq(10, 100, by = 10))
    ) +
    ylab(NULL) +
    labs(
      title    = "Ranking of All Universities by Impact Factors",
      subtitle = "(with 95% marginal confidence sets)"
    ) +
    theme(axis.text.y = element_text(size = 5))

  ggsave(file.path("output", "figure2_universities_marg.pdf"),
         plot = fig2, width = 8, height = 14)
  cat("Saved: output/figure2_universities_marg.pdf\n\n")
}


# ============================================================
#  FINAL STATUS
# ============================================================

cat("\n========================================\n")
cat("  STATUS SUMMARY\n")
cat("========================================\n")
cat("[", ifelse(requireNamespace("csranks", quietly=TRUE),                       "OK  ", "FAIL"), "] csranks available\n")
cat("[", ifelse(file.exists(file.path("output","table1_journals.csv")),           "OK  ", "FAIL"), "] table1_journals.csv\n")
cat("[", ifelse(file.exists(file.path("output","table2_universities.csv")),       "OK  ", "FAIL"), "] table2_universities.csv\n")
cat("[", ifelse(file.exists(file.path("output","figure1_journals_marg.pdf")),     "OK  ", "FAIL"), "] figure1_journals_marg.pdf\n")
cat("[", ifelse(file.exists(file.path("output","figure2_universities_marg.pdf")), "OK  ", "FAIL"), "] figure2_universities_marg.pdf\n")
cat("========================================\n")
cat("All OK = pipeline confirmed. Files in: output/\n\n")
