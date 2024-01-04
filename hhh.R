df <- read.csv("yb_channels_data.csv",header=TRUE, sep = ",")

views <- df$total_views
out <- boxplot(views, plot=FALSE)$out
data <- views[-which(views %in% out)]
write.csv(views, "myFile.csv", row.names = FALSE)
data
boxplot(data)

plot(df$sub_count,df$total_views)


Q1 <- quantile(views, 0.25)
Q3 <- quantile(views, 0.75)
IQR_value <- Q3 - Q1

# Set a threshold for outliers (e.g., 1.5 times the IQR)
threshold <- 1.5

# Identify and remove outliers
views_no <- views[!(views < Q1 - threshold * IQR_value | views > Q3 + threshold * IQR_value)]

boxplot(views_no)

png("bp.png")
boxplot(views, outline = FALSE, main = "Box Plot without Outliers", ylab = "Your Y-axis Label")
dev.off()
