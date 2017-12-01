library("sp")
library("maptools")
income <- read.csv("https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.csv")
BMP <- read.csv("https://opendata.arcgis.com/datasets/a973c2c7b7c14a918859f3e38bdffdd2_42.csv")
tract.raw <- getKMLcoordinates(kmlfile="https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.kml", ignoreAltitude = T)
tract.names <- as.character(read.csv("https://github.com/k5cents/dc-bmp/raw/master/tract_names.csv", header = F)$V1)

# Formatting tract polygons and apply to BMP data set
tract.shapes <- as.list(rep(NA, length(tract.raw)))
for(i in 1:length(tract.raw)){
  tract.shapes[[i]] <- Polygon(tract.raw[[i]])}
tract.vec <- rep("tract", length(tract.raw))
tract.nos <- tract.names <- as.character(read.csv("https://github.com/k5cents/dc-bmp/raw/master/tract_names.csv", header = F)$V1)
tract.no <-paste(tract.vec, tract.nos, sep = "_")
names(tract.shapes) <- tract.no
for(i in 1:length(tract.raw)){
  assign(names(tract.shapes)[i], tract.shapes[[i]])}
for(i in 1:length(tract.no)){
  BMP[tract.no[i]] <- point.in.polygon(
    BMP$LONGITUDE,
    BMP$LATITUDE,
    (get(tract.no[i]))@coords[,1],  
    (get(tract.no[i]))@coords[,2])}
BMP$tract <- NA
for(i in 1:length(tract.no)){
  BMP$tract[BMP[tract.no[i]] == 1] <- tract.no[i]}
BMP$tract <- gsub("tract_", "", BMP$tract)

# Make a vector of the number of BMPs per tract:
tract.bmp <- rep(NA, nrow(income))
for(i in 1:nrow(income)){tract.bmp[i] <- nrow(subset(BMP, tract == tract.names[i]))}

# Make vector indicating higher or lower income tract:
higher.income <- subset(income, income$FAGI_MEDIAN_2005 > median(na.omit(income$FAGI_MEDIAN_2005)))
tract.income <- ifelse(as.numeric(tract.nos) %in% unique(higher.income$FEDTRACTNO), 1, 0)

tract.median <- rep(NA, nrow(income))
for(i in 1:nrow(income)){
  tract.median[i] <- income$FAGI_MEDIAN_2005[income$FEDTRACTNO == tract.names[i]]}

# Combine the two vectors
final_test <- as.data.frame(cbind(tract.bmp, tract.income))
names(final_test) <- c("installations", "income")

# Run some tests!
final.outlier <- final_test[-which.max(final_test$installations),]
test.bmp <- t.test(
  formula = installations ~ income,
  data = final_test,
  conf.level = .95)
test.bmp$p.value
test.outlier <- t.test(
  formula = installations ~ income,
  data = final.outlier,
  conf.level = .95)
test.outlier$p.value

# Make some plots!
final_plot <- as.data.frame(cbind(tract.median, tract.bmp))
names(final_plot) <- c("income", "installations")
plot(final_plot, main = "with outlier")
lm_with <- lm(installations ~ income, data = final_plot)
abline(lm_with, col = "red")
abline(v = median(na.omit(income$FAGI_MEDIAN_2005)), col = "blue")
text(2.5e+04, 250, test.bmp$p.value)

# Remove tract 77
final_plot_out <- final_plot[-which.max(final_plot$installations),]
plot(final_plot_out, main = "without outlier")
lm_outlier <- lm(installations ~ income, data = final_plot_out)
abline(lm_outlier, col = "red")
abline(v = median(na.omit(income$FAGI_MEDIAN_2005)), col = "blue")
text(2.5e+04, 250, test.outlier$p.value)
abline(v = mean(na.omit(final_plot_out$income[final_test$income == 0])))
abline(v = mean(na.omit(final_plot_out$income[final_test$income == 1])))

# For log graphs
final_log <- final_plot_out
final_log$income <- log(final_plot_out$income)
plot(final_log, main = "log income")
lm_outlier <- lm(installations ~ income, data = final_log)
abline(lm_outlier, col = "red")
abline(v = median(log(na.omit(income$FAGI_MEDIAN_2005))), col = "black")
text(2.5e+04, 250, test.outlier$p.value)
abline(v = mean(na.omit(final_log$income[final_test$income == 0])), col = "green")
abline(v = mean(na.omit(final_log$income[final_test$income == 1])), col = "blue")
final_log_out <- final_log[-which.min(final_log$income),]
final_log_out2 <- final_log_out[-which.min(final_log_out$income),]
final_log_out3 <- final_log_out2[-which.min(final_log_out2$income),]
plot(final_log_out3, main = "log income no outs")
lm_log_out <- lm(installations ~ income, data = final_log_out3)
abline(lm_outlier, col = "red")
abline(v = median(log(na.omit(income$FAGI_MEDIAN_2005))), col = "black")
abline(v = mean(na.omit(final_log$income[final_test$income == 0])), col = "green")
abline(v = mean(na.omit(final_log$income[final_test$income == 1])), col = "blue")
lm_log_out
summary(lm_log_out)