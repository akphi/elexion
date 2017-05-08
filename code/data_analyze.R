############################################################
##                      DESCRIPTION                       ##
############################################################

# analyze the dataset quality, create statistical quality
# report on dataset and plot out the attributes

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
source("misc.R")
install_load(c("corrplot",
               "ggplot2",
               "entropy"))

############################################################
##                      INITIALIZE                        ##
############################################################

log.write("start data_analyze script")

############################################################
##                          LOAD                          ##
############################################################

data_orig <-
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "data.csv"), stringsAsFactors = FALSE)
meta_orig <-
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "meta.csv"), stringsAsFactors = FALSE)
# set MAIN
data_seed <- data_orig
meta_seed <- meta_orig

############################################################
##                        MODIFY                          ##
############################################################

# +----------------------------------+
# |               STATE              |
# +----------------------------------+
# convert state FIPS code to factor/nominal
data_seed$statecode_prev <- as.factor(data_seed$statecode_prev)
data_seed$fips <- as.factor(data_seed$fips)

############################################################
##                         REPORT                         ##
############################################################

data_structure <- str.table(data_seed)
data_report <- report.table(data_seed)


############################################################
##                     CORRELATION                        ##
############################################################

data_cor <- getrod(data_seed, c("fips", "statecode_prev", "name_16"))
data_cor$rep16_win <- as.numeric(data_cor$rep16_win)
data_cor$rep12_win <- as.numeric(data_cor$rep12_win)
data_cor$rep08_win <- as.numeric(data_cor$rep08_win)
# sort the correlation table in descending order
data_cor <-
  compute.correlation.pair_wise(data_cor, sort = TRUE, decreasing = TRUE)
data_cor <- data.frame(
  feature.1 = dispv(data_cor$x, meta_seed),
  feature.2 = dispv(data_cor$y, meta_seed),
  correlation = data_cor$cor,
  p_value = data_cor$p
)

############################################################
##                     GENERAL PLOTS                      ##
############################################################

# +----------------------------------+
# |             HISTOGRAM            |
# +----------------------------------+
# plot histograms for all features
plot.histogram(as.data.frame(sapply(data_seed[, c(-2)], as.numeric)),
               meta_seed,
               RES_ANALYZED_HISTORGRAM_DIR)

# +----------------------------------+
# |           SCATTER PLOTS          |
# +----------------------------------+
# list each pairs by a vector to plot the correlation scatter plot of them
to_plot <- c(
  c(
    "Poverty.Rate.below.federal.poverty.threshold",
    "Child.Poverty.living.in.families.below.the.poverty.line"
  ),
  c("At.Least.Bachelor.s.Degree",
    "Graduate.Degree")
)
plot.pairwise_correlation_scatter(data_seed, meta_seed, to_plot, RES_ANALYZED_PLOT_DIR)

############################################################
##                   CASE STUDY PLOTS                     ##
############################################################

# +----------------------------------+
# |    ELECTION WINNER 2008-2016     |
# +----------------------------------+
data_election_compare <- data.frame(
  Party = rep(c("Rep.", "Dem."), each = 3),
  Year = rep(c("2008", "2012", "2016"), 2),
  County = c(
    sum(data_seed$rep08_win),
    sum(data_seed$rep12_win),
    sum(data_seed$rep16_win),
    sum(!data_seed$rep08_win),
    sum(!data_seed$rep12_win),
    sum(!data_seed$rep16_win)
  )
)
ggplot(data = data_election_compare, aes(x = Year, y = County, fill = Party)) +
  ggtitle("Election Result 2008-2016") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(COLOR["Rep. Red"], COLOR["Dem. Blue"])) +
  theme_bw() +
  theme(
    plot.title = element_text(
      size = 22,
      hjust = 0.5,
      margin = margin(b = 1, unit = "cm")
    ),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 0.75, unit = "cm")),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
  )
ggsave(
  mp(RES_ANALYZED_PLOT_DIR,
     "election_result_08-16.png"),
  width = 20,
  height = 20,
  unit = "cm",
  limitsize = TRUE
)

# +----------------------------------+
# |  ELECTION FEATURES CORRELATION   |
# +----------------------------------+
png(
  mp(RES_ANALYZED_PLOT_DIR, "election_features_correlation.png"),
  width = XL_LENGTH,
  height = XL_LENGTH
)
corrplot.mixed(
  cor(dispd(data_seed[, c(
    "Total.Population",
    "votes16_trumpd",
    "rep12",
    "rep08",
    "votes16_clintonh",
    "dem12",
    "dem08",
    "votes16_others",
    "other12",
    "other08",
    "total16",
    "total12",
    "total08",
    "rep16_frac",
    "rep12_frac",
    "rep08_frac",
    "dem16_frac",
    "dem12_frac",
    "dem08_frac",
    "other16_frac",
    "other12_frac",
    "other08_frac",
    "rep16_win",
    "rep12_win",
    "rep08_win"
  )], meta_seed)),
  lower = "number",
  upper = "circle",
  # label
  tl.cex = 1,
  tl.offset = 1,
  tl.pos = "lt",
  tl.col = "black",
  # color bar
  cl.cex = 1,
  cl.ratio = 0.2,
  # coefficient
  number.cex = 0.9
)
dev.off()

############################################################
##                      MATRIX PLOTS                      ##
############################################################

# +----------------------------------+
# |     GIANT CORRELATION MATRIX     |
# +----------------------------------+
png(
  mp(RES_ANALYZED_PLOT_DIR, "all_features_correlation.png"),
  width = 4000,
  height = 4000
)
corrplot.mixed(
  cor(dispd(data_seed[, c(-1:-3)], meta_seed), use = "pairwise.complete.obs"),
  lower = "number",
  upper = "circle",
  # label
  tl.cex = 2,
  tl.offset = 1,
  tl.pos = "lt",
  tl.col = "black",
  # color bar
  cl.cex = 2,
  cl.ratio = 0.1,
  # coefficient
  number.cex = 1
)
dev.off()

# +----------------------------------+
# |     GIANT SCATTER PLOT MATRIX    |
# +----------------------------------+
panel.cor <- function(x,
                      y,
                      digits = 2,
                      cex.cor = 2,
                      ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # correlation coefficient
  r <- cor(x, y, use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  # txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt, cex = cex.cor * r * 3)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if (p < 0.01)
    txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2, cex = cex.cor * r * 1.2)
}

panel.diag <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE, breaks = 20)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}

png(
  mp(RES_ANALYZED_PLOT_DIR, "all_features_scatter.png"),
  width = 9000,
  height = 9000
)
par(bg = BACKGROUND_COLOR) 
plot(
  dispd(data_seed[, c(-1:-3)], meta_seed),
  xaxt = 'n',
  yaxt = 'n',
  cex = 0.3,
  cex.labels = 2
)
pairs(
  dispd(
    data_seed[, c(-1:-3)],
    data.frame(name = meta_seed$name, display_name = paste("Feature", c(1:nrow(
      meta_seed
    ))))
  ),
  upper.panel = panel.cor,
  xaxt = 'n',
  yaxt = 'n',
  cex.labels = NULL,
  diag.panel = panel.diag
)
dev.off()

# Output a lower resolution verion
panel.cor <- function(x,
                      y,
                      digits = 2,
                      cex.cor = 2,
                      ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # correlation coefficient
  r <- abs(cor(x, y, use = "pairwise.complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  # txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt, cex = cex.cor * r * 0.7)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if (p < 0.01)
    txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2, cex = cex.cor * r * 0.3)
}
panel.diag <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE, breaks = 20)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}
png(
  mp(RES_ANALYZED_PLOT_DIR, "all_features_scatter.low.png"),
  width = 3000,
  height = 3000
)
par(bg = BACKGROUND_COLOR) 
plot(
  dispd(data_seed[, c(-1:-3)], meta_seed),
  xaxt = 'n',
  yaxt = 'n',
  cex = 0.3,
  cex.labels = 1
)
pairs(
  dispd(
    data_seed[, c(-1:-3)],
    data.frame(name = meta_seed$name, display_name = paste("Feature", c(1:nrow(
      meta_seed
    ))))
  ),
  upper.panel = panel.cor,
  xaxt = 'n',
  yaxt = 'n',
  cex.labels = NULL,
  diag.panel = panel.diag
)
dev.off()

############################################################
##                        SUMMARY                         ##
############################################################

data_summary <- summarize(data_seed)

############################################################
##                        EXPORT                          ##
############################################################

write.csv(
  data_summary$continuous,
  file = mp(RES_ANALYZED_DIR, "summary_continuous.csv"),
  row.names = FALSE
)
write.csv(
  data_summary$categorical,
  file = mp(RES_ANALYZED_DIR, "summary_categorical.csv"),
  row.names = FALSE
)
write.csv(data_report,
          file = mp(RES_ANALYZED_DIR, "report.csv"),
          row.names = FALSE)
write.csv(
  data_structure,
  file = mp(RES_ANALYZED_DIR, "structure.csv"),
  row.names = FALSE
)
write.csv(
  data_cor,
  file = mp(RES_ANALYZED_DIR, "correlation.csv"),
  row.names = FALSE
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end data_analyze script")
reset.work_space()