################################################################################
# Econ 687: Replication Project
# Paper: Mogstad, Romano, Shaikh & Wilhelm (2022)
#        "Statistical Uncertainty in the Ranking of Journals and Universities"
#        AEA Papers and Proceedings, Vol. 112, pp. 626-631
# Authors: Jason Jankoski, Alan Lamb, Linna Wang
# University of Maryland, Spring 2026
################################################################################

################################# Setup ########################################
packages <- c("ggplot2", "dplyr", "readxl", "devtools")
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(lapply(packages, library, character.only = TRUE))

# install csranks from GitHub if not already present
# this is the authors' own R package implementing the confidence set method
if (!requireNamespace("csranks", quietly = TRUE)) {
  devtools::install_github("danielwilhelm/R-CS-ranks")
}
library(csranks)

# set working directory to the folder containing this script (code/)
# then set data and output paths relative to the project root
working.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
root.dir    <- dirname(working.dir)
data.dir    <- file.path(root.dir, "data")
output.dir  <- file.path(root.dir, "output")

# create output folder if it doesn't exist
if (!dir.exists(output.dir)) dir.create(output.dir)
################################################################################


################################################################################
# Section 1: Journal Impact Factor Data
# Source: Stern (2013), via journal IF/IFdata.RData
# The raw data is journal IF/JEL_Data.xlsx - one sheet per journal with
# article-level citation counts by year. The authors' convertdata.R processes
# this into a clean data frame and saves it as IFdata.RData.
################################################################################

# load the processed journal IF data
load(file.path(data.dir, "journal IF", "IFdata.RData"))

# journalIF is a 232-row data frame with columns:
#   journal = journal abbreviation
#   IF      = estimated 2011 impact factor (mean citations per article)
#   SE      = standard error of the impact factor estimate
#   sigma   = estimated standard deviation of citations across articles
#   n       = number of articles in the journal

# the paper focuses on the 30 journals pre-selected by
# Kalaitzidakis, Mamuneas & Stengos (2003) - an earlier study identifying
# leading outlets in economics based on citation data from 1994-1998
top30names <- c("JEL", "QJE", "JEP", "JFE", "JPE", "Econometrica",
                "AER", "REStud", "RESTAT", "JLE", "JEEM", "JHumRes",
                "J INT ECON", "EconJ", "JMonEcon", "JEconometrics",
                "RAND J ECON", "J BUS ECON STAT", "JPubEcon",
                "JAppliedEconmet", "EUR ECON REV", "IER", "JET",
                "OXFORD B ECON STAT", "GAME ECON BEHAV",
                "J ECON DYN CONTROL", "SCAND J ECON",
                "ECONOMET THEOR", "Econ Theory", "EL")

# filter to KMS top 30, remove missing/zero IFs, and assign point ranks
# arrange(desc(IF)) sorts highest impact factor first
# mutate(rank = row_number()) assigns rank 1 to the highest IF journal
jIF <- journalIF %>%
  filter(!is.na(IF), IF > 0, journal %in% top30names) %>%
  arrange(desc(IF)) %>%
  mutate(rank = row_number())

cat("KMS top-30 journals matched:", nrow(jIF), "of 30\n")

# descriptive statistics - all valid journals
jall <- journalIF %>% filter(!is.na(IF), IF > 0)
cat("\n--- Descriptive Statistics: All Valid Journals (n =", nrow(jall), ") ---\n")
cat("Mean IF:   ", round(mean(jall$IF), 4), "\n")
cat("Median IF: ", round(median(jall$IF), 4), "\n")
cat("SD of IF:  ", round(sd(jall$IF), 4), "\n")
cat("Mean SE:   ", round(mean(jall$SE), 4), "\n")
cat("IF range:  [", round(min(jall$IF), 4), ",", round(max(jall$IF), 4), "]\n")

# descriptive statistics - KMS top 30 only
cat("\n--- Descriptive Statistics: KMS Top 30 ---\n")
cat("Mean IF:   ", round(mean(jIF$IF), 4), "\n")
cat("Median IF: ", round(median(jIF$IF), 4), "\n")
cat("SD of IF:  ", round(sd(jIF$IF), 4), "\n")
cat("Mean SE:   ", round(mean(jIF$SE), 4), "\n")
cat("IF range:  [", round(min(jIF$IF), 4), ",", round(max(jIF$IF), 4), "]\n")

# save descriptive table to output
table1 <- jIF %>%
  select(Rank = rank, Journal = journal, IF, SE, N_articles = n) %>%
  mutate(IF = round(IF, 4), SE = round(SE, 4))
write.csv(table1, file.path(output.dir, "table1_journals.csv"), row.names = FALSE)
cat("\nSaved: output/table1_journals.csv\n")
################################################################################


################################################################################
# Section 2: Journal Confidence Sets (Figure 1)
# The core statistical step. csranks() takes the point estimates and their
# uncertainty and returns confidence sets for ranks via bootstrap simulation.
################################################################################

# step 1: build the covariance matrix
# csranks() requires a matrix, not a vector of SEs
# diag(SE^2) creates a p x p diagonal matrix with variances on the diagonal
# and zeros off the diagonal - this assumes estimation errors are independent
# across journals, which is reasonable since each journal's IF is estimated
# from its own set of articles
Sigma.journals <- diag(jIF$SE^2)

# step 2: compute marginal confidence sets for ranks
# marginal = each journal's set covers its true rank individually at 95%
# stepdown = iterative procedure that gives tighter sets than single-step
# R = 1000 bootstrap draws (same as authors' code)
# seed = 101 (same as authors' code, ensures reproducibility)
cat("\nComputing marginal confidence sets for journal ranks...\n")
cat("(bootstrap R=1000, approx 1-2 minutes)\n")

CS.journals.marg <- csranks::csranks(jIF$IF,
                                      Sigma    = Sigma.journals,
                                      coverage = 0.95,
                                      stepdown = TRUE,
                                      simul    = FALSE,
                                      R        = 1000,
                                      seed     = 101)

# attach confidence set bounds to the data frame
# CS$L = lower bound of rank confidence set (best possible rank)
# CS$U = upper bound of rank confidence set (worst possible rank)
jIF$margCSL <- CS.journals.marg$L
jIF$margCSU <- CS.journals.marg$U

# step 3: compute simultaneous confidence sets for ranks
# simultaneous = all journals' sets jointly cover true ranks at 95%
# these are wider than marginal sets because the guarantee is stronger
cat("Computing simultaneous confidence sets for journal ranks...\n")

CS.journals.simul <- csranks::csranks(jIF$IF,
                                       Sigma    = Sigma.journals,
                                       coverage = 0.95,
                                       stepdown = TRUE,
                                       simul    = TRUE,
                                       R        = 1000,
                                       seed     = 101)

jIF$simulCSL <- CS.journals.simul$L
jIF$simulCSU <- CS.journals.simul$U

cat("Done.\n")

# Figure 1: marginal confidence sets (mirrors paper Figure 1)
# x-axis = rank (1 = highest IF, 30 = lowest IF)
# y-axis = journal names ordered so rank 1 appears at the bottom
# dots = point rank estimates, bars = 95% marginal confidence sets
fig1 <- ggplot(jIF, aes(x = rank, y = reorder(journal, rank))) +
  theme_bw() +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = margCSL, xmax = margCSU), width = 0.4) +
  scale_x_continuous(name   = "Rank",
                     limits = c(1, 32),
                     breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  ylab(NULL) +
  labs(title    = "Ranking of 30 Journals by 2011 Impact Factor",
       subtitle = "(with 95% marginal confidence sets)")

ggsave(file.path(output.dir, "figure1_journals_marg.pdf"),
       plot = fig1, width = 8, height = 7)
cat("Saved: output/figure1_journals_marg.pdf\n")

# Appendix Figure A6: simultaneous confidence sets
fig1.simul <- ggplot(jIF, aes(x = rank, y = reorder(journal, rank))) +
  theme_bw() +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = simulCSL, xmax = simulCSU), width = 0.4) +
  scale_x_continuous(name   = "Rank",
                     limits = c(1, 32),
                     breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  ylab(NULL) +
  labs(title    = "Ranking of 30 Journals by 2011 Impact Factor",
       subtitle = "(with 95% simultaneous confidence sets)")

ggsave(file.path(output.dir, "figureA6_journals_simul.pdf"),
       plot = fig1.simul, width = 8, height = 7)
cat("Saved: output/figureA6_journals_simul.pdf\n")
################################################################################


################################################################################
# Section 3: University Data
# Source: Zimmermann (2013), via Repec/top100unis.csv
# The raw data is Repec/originaldata/repec_raw.txt - 662,604 articles by
# 40,496 authors from RePEC as of July 2021. The authors' cleandata.R and
# keeptop100.R scripts process this into average impact factors per university.
# By the time we load top100unis.csv the data is clean and ready to use.
################################################################################

unis <- read.csv(file.path(data.dir, "Repec", "top100unis.csv"))

# columns in top100unis.csv:
#   name   = university name
#   n      = number of affiliated authors in RePEC
#   IFmean = average impact factor of the university's publications
#   IFse   = standard error of that average
#   CTmean = average citation count
#   CTse   = standard error of the citation mean

# sort by impact factor and assign point ranks
unis <- unis %>%
  filter(!is.na(IFmean)) %>%
  arrange(desc(IFmean)) %>%
  mutate(rank = row_number())

# descriptive statistics
cat("\n--- Descriptive Statistics: 100 Universities ---\n")
cat("Mean IFmean:   ", round(mean(unis$IFmean), 4), "\n")
cat("Median IFmean: ", round(median(unis$IFmean), 4), "\n")
cat("SD of IFmean:  ", round(sd(unis$IFmean), 4), "\n")
cat("Mean IFse:     ", round(mean(unis$IFse), 4), "\n")
cat("Total authors: ", sum(unis$n), "\n")
cat("IF range:      [", round(min(unis$IFmean), 4), ",",
                        round(max(unis$IFmean), 4), "]\n")

# save top 20 table to output
table2 <- unis %>%
  slice(1:20) %>%
  select(Rank = rank, University = name, IFmean, IFse, N_authors = n) %>%
  mutate(IFmean = round(IFmean, 4), IFse = round(IFse, 4))
write.csv(table2, file.path(output.dir, "table2_universities.csv"), row.names = FALSE)
cat("\nSaved: output/table2_universities.csv\n")
################################################################################


################################################################################
# Section 4: University Confidence Sets (Figure 2)
# Same approach as journals but applied to 100 universities.
################################################################################

# build the covariance matrix from university-level standard errors
Sigma.unis <- diag(unis$IFse^2)

# compute marginal confidence sets
cat("\nComputing marginal confidence sets for university ranks...\n")
cat("(bootstrap R=1000, approx 1-2 minutes)\n")

CS.unis.marg <- csranks::csranks(unis$IFmean,
                                  Sigma    = Sigma.unis,
                                  coverage = 0.95,
                                  stepdown = TRUE,
                                  simul    = FALSE,
                                  R        = 1000,
                                  seed     = 101)

unis$margCSL <- CS.unis.marg$L
unis$margCSU <- CS.unis.marg$U
cat("Done.\n")

# Figure 2: marginal confidence sets for all 100 universities
fig2 <- ggplot(unis, aes(x = rank, y = reorder(name, rank))) +
  theme_bw() +
  geom_point(size = 0.7) +
  geom_errorbar(aes(xmin = margCSL, xmax = margCSU), width = 0.4) +
  scale_x_continuous(name   = "Rank",
                     limits = c(1, 100),
                     breaks = c(1, seq(10, 100, by = 10))) +
  ylab(NULL) +
  labs(title    = "Ranking of All Universities by Impact Factors",
       subtitle = "(with 95% marginal confidence sets)") +
  theme(axis.text.y = element_text(size = 5))

ggsave(file.path(output.dir, "figure2_universities_marg.pdf"),
       plot = fig2, width = 8, height = 14)
cat("Saved: output/figure2_universities_marg.pdf\n")
################################################################################


################################################################################
# Status summary
################################################################################
cat("\n--- Output Files ---\n")
cat("table1_journals.csv           :", file.exists(file.path(output.dir, "table1_journals.csv")), "\n")
cat("table2_universities.csv       :", file.exists(file.path(output.dir, "table2_universities.csv")), "\n")
cat("figure1_journals_marg.pdf     :", file.exists(file.path(output.dir, "figure1_journals_marg.pdf")), "\n")
cat("figureA6_journals_simul.pdf   :", file.exists(file.path(output.dir, "figureA6_journals_simul.pdf")), "\n")
cat("figure2_universities_marg.pdf :", file.exists(file.path(output.dir, "figure2_universities_marg.pdf")), "\n")
################################################################################
