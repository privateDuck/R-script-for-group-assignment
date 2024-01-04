df <- read.csv("yb_channels_data.csv", header = TRUE, sep = ",")
summary(df)

svv <- df[, c("sub_count", "total_views")]

svv <- na.omit(svv)
summary(svv)

write.csv(svv, "relavant_data_frame.csv", row.names = FALSE)

linear_model <- lm(total_views ~ sub_count, data = svv)
summary(linear_model)

# Print a summary of the linear model
summary(linear_model)


library(ggplot2)
png("log_scaled.png", width = 1200, height = 1200, res = 200)


ggplot(svv, aes(x = sub_count, y = total_views)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, col = "red") +
  labs(title = "Regression", x = "Sub Count", y = "Total Views") +
  scale_x_log10() +
  scale_y_log10()

dev.off()

anova_results <- aov(total_views ~ sub_count, data = svv2)

# count of observations
nrow(svv)

# descriptive statistics
summary(svv)

# total_views variance
var(svv$total_views)

# sub_count variance
var(svv$sub_count)

# pearson correlation
cor(svv, method = "pearson")

# data frames
sub_count_df <- data.frame(x = svv$sub_count)
total_views_df <- data.frame(x = svv$total_views)

# sub_count KDE
ggplot(sub_count_df, aes(x)) +
  geom_density(alpha = 0.35, fill = "#0099f8", color = "black") +
  labs(title = "Distribution of Subcribers Count", x = "Subscribers Count") +
  scale_x_log10()

# total_views KDE
ggplot(total_views_df, aes(x)) +
  geom_density(alpha = 0.35, fill = "#2ecc71", color = "black") +
  labs(title = "Distribution of Total Number of Views", x = "Total Views") +
  scale_x_log10()

# subscribers boxplot
sub_bplt <- ggplot(sub_count_df, aes(x = x)) +
              geom_boxplot(outlier.shape = 16, outlier.colour = "black", color = "black",fill="#5ad45a", alpha = 0.5) +
              labs(title = "Distribution of Subcribers Count", x = "Subscribers Count") +
              scale_x_log10()

# total views boxplot
view_bplt <- ggplot(total_views_df, aes(x = x)) +
              geom_boxplot(outlier.shape = 16, outlier.colour = "black", color = "black",fill="#5ad45a", alpha = 0.5) +
              labs(title = "Distribution of Total Number of Views", x = "Total Views") +
              scale_x_log10()


anova_table <- anova(linear_model)
anova_table

# ANOVA CUSTOM
SSR <- sum((fitted(linear_model) - mean(svv$total_views))^2)
SSE <- sum((fitted(linear_model) - svv$total_views)^2)
SST <- SSE + SSR

df_residual <- 38548
df_regression <- 1

# Calculate mean squares
MSE <- SSE / df_residual
MSR <- SSR / df_regression
MSR
MSE
MSR/MSE
library(ggExtra)

x_breaks <- c(100,1e5,1e8)
y_breaks <- c(100,1e5,1e8,1e11)
x_labels <- c("100","100K","100M")
y_labels <- c("100", "100K", "100M", "100B")

svv <- svv[svv > 0,]

p <- ggplot(svv, aes(x = sub_count, y = total_views))+
geom_point(alpha = 0.4, color="#ff0000") +
geom_smooth(method = "lm", se = T, col = "#ffffff") +
theme(
  plot.title = element_text(colour = "#ffffff", face = "bold"),
  plot.background = element_rect(fill = "#111111"),
  panel.background = element_rect(fill = "#000000"), 
  panel.grid.major = element_line(size = 0.5, linetype= 'solid', colour="#2c2c2c"),
  panel.grid.minor = element_line(size = 0.5, linetype= 'solid', colour="#2c2c2c"),
  axis.line = element_line(size = 1, linetype='solid', colour = "#333333"),
  axis.text.x = element_text(colour = "#ffffff"),
  axis.text.y = element_text(colour = "#ffffff"),
  axis.title.x = element_text(colour = "#ffffff", face = "bold"),
  axis.title.y = element_text(colour = "#ffffff", face = "bold")
  )+
labs(title = "Regression Result", x = "Subscribers Count", y = "Total Views") +
scale_x_log10(labels = x_labels, breaks = x_breaks)+
scale_y_log10(labels = y_labels, breaks = y_breaks)
p

ggMarginal(p, type="boxplot", color="#ffffff", fill="#ff0000", alpha = 0.6)

png("regression.png", width = 2048, height = 2048, res = 350)
p
dev.off()

linear_model
plot(linear_model)

png("residuals_x_log.png", width = 2048, height = 2048, res = 350)


ggplot(linear_model, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4, color="#ff0000") +
  geom_hline(yintercept = 0)+
  theme(
  plot.title = element_text(colour = "#ffffff", face = "bold"),
  plot.background = element_rect(fill = "#111111"),
  panel.background = element_rect(fill = "#000000"), 
  panel.grid.major = element_line(size = 0.5, linetype= 'solid', colour="#2c2c2c"),
  panel.grid.minor = element_line(size = 0.5, linetype= 'solid', colour="#2c2c2c"),
  axis.line = element_line(size = 1, linetype='solid', colour = "#333333"),
  axis.text.x = element_text(colour = "#ffffff"),
  axis.text.y = element_text(colour = "#ffffff"),
  axis.title.x = element_text(colour = "#ffffff", face = "bold"),
  axis.title.y = element_text(colour = "#ffffff", face = "bold")
  )+
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values (Logarithmic Scale)", y = "Residuals (in Billions)") +
  scale_x_log10(labels=y_labels, breaks=y_breaks)+
  scale_y_continuous(labels=c("-100", "-50", "0", "50"), breaks=c(-1.0e11,-5e10,0,5e10))


dev.off()
  