library("arm")
library("rstanarm")
library("cmdstanr")
setwd("/Users/anjanarao/Desktop/Courses/Spring/BayesianMultiLevelAnalysis/")
indicators <- read.csv("indicators.csv")
livwell <- read.csv("livwell_lin_interpolated.csv")



#Speaking points
#Interested in women's wellbeing 
#Goal: How women's wellbeing is relfective of their reproductive health choices 

#How does education influence the method of contraception? 

#Data Set: LivWell is a global longitudinal database which provides a range of key indicators related to women’s socioeconomic status, health and well-being, access to basic services, and demographic outcomes.

#2010 data from livwell data 
#44 countries 
#398 regions 
indicator_df <- data.frame(indicators)
livwell_df <- data.frame(livwell)
years <- livwell_df$year
table(years)

livwell_2010 <- livwell_df[livwell_df$year == 2010, ]
print(livwell_2010)
nrow(livwell_2010)

table(livwell_2010$country_name)

#Complete Pooling, All regions 
avg_educ <- livwell_2010$ED_educ_years_mean
perc_cont <- livwell_2010$RH_contr_modern_p
perc_know <- livwell_2010$RH_contr_modern_know_p
plot(avg_educ, perc_cont,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", col = "blue", pch=19, main = "Across all Regions")


livwell_2006 <- livwell_df[livwell_df$year == 2006, ]
print(livwell_2006)
nrow(livwell_2006)

table(livwell_2006$country_name)
table(livwell_2010$country_name)


#No Pooling: "Turkey", "Honduras", "Cambodia", "Bangladesh"  

livwell_turkey <- livwell_2010[livwell_2010$country_name == "Turkey", ]
livwell_honduras <- livwell_2010[livwell_2010$country_name == "Honduras", ]
livwell_cambodia <- livwell_2010[livwell_2010$country_name == "Cambodia", ]
livwell_bangladesh <- livwell_2010[livwell_2010$country_name == "Bangladesh", ]


# Find the overall range of x and y values
x_range <- range(c(0, 12))
y_range <- range(c(10, 50))
plot(livwell_turkey$ED_educ_years_mean, livwell_turkey$RH_contr_modern_p,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Average Education vs. Contraceptive Usage (No Pooling)", col = "blue", pch=19)
points(livwell_honduras$ED_educ_years_mean, livwell_honduras$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "red", pch=19)
points(livwell_cambodia$ED_educ_years_mean, livwell_cambodia$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Cambodia", col = "green", pch=19)
points(livwell_bangladesh$ED_educ_years_mean, livwell_bangladesh$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Bangladesh", col = "purple", pch=19)
legend("topright", legend = c("Turkey", "Honduras", "Cambodia", "Bangladesh" ), col = c("blue", "red", "green", "purple"), pch = 16, cex = .8)

data <- livwell_2010[, c("country_name", "ED_educ_years_mean", "RH_contr_modern_p")]
#linear regression 

lr_fit <- lm(RH_contr_modern_p ~ ED_educ_years_mean, data=data )
coeffs <- lr_fit$coefficients
plot(avg_educ, perc_cont,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", col = "blue", pch=19)
abline(a = coeffs[1], b = coeffs[2], col = "red")
#Multilevel Modeling by country

fit <- lmer(RH_contr_modern_p ~ (1 | country_name) + ED_educ_years_mean, data=data)
summary_model <- summary(fit)

# Extract fixed effects coefficients
fixed_effects <- summary_model$coefficients
intercept <- fixed_effects[1,1]
slope <- fixed_effects[1,2]
fit1 <- stan_glmer(RH_contr_modern_p ~ (1 | country_name) + ED_educ_years_mean, data=data, refresh=0)
#Irrespective of years of education, 11.25% of women use modern contraceptives across all the regions in our dataset 
#if you compare across two regions that differ by one year in average education, there would be a increase in difference of the amount of women that use modern contraceptives by 2.11% 


#Here we can see that the fit from the multilevel model better adjusts for variations across regions within a country. 
plot(avg_educ, perc_cont,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", col = "blue", pch=19)
abline(a = coeffs[1], b = coeffs[2], col = "red")
abline(a = intercept, b = slope, col = "green")

library(ggplot2)
scatter_plot <- ggplot(data, aes(x = ED_educ_years_mean, y = RH_contr_modern_p, color = country_name)) +
  geom_point() +
  labs(x = "Average Education in Years", y = "Percentage that Use Modern Contraceptives", title = "Average Education in Years for Women vs. Percentage of Women that Use Modern Contraceptives") +
  theme_minimal()+
  theme(legend.position = "bottom") +  # Move legend to the bottom
  scale_x_continuous(expand = c(0, 0)) +  # Fix x-axis margins
  scale_y_continuous(expand = c(0, 0))    # Fix y-axis margins


scatter_plot +
  geom_abline(intercept = coeffs[1], slope = coeffs[2], linetype = "solid", color = "red") + 
  geom_abline(intercept =  intercept, slope = slope, linetype = "solid", color = "blue")



livwell_2 <- data <- livwell[, c("year", "region_name_harmonized", "country_name", "ED_educ_years_mean", "RH_contr_modern_p")]

na_indices_year <- which(is.na(livwell_2$year))
na_indices_avg_mean <- which(is.na(livwell_2$ED_educ_years_mean))
na_indices_pct_contra <- which(is.na(livwell_2$RH_contr_modern_p))
na_indices_country <- which(is.na(livwell_2$country_name))
na_indices_region <- which(is.na(livwell_2$region_name_harmonized))

# Print the row indices with NA values in each column
print("NA indices in 'year' column:")
print(na_indices_year)

print("NA indices in 'avg_mean' column:")
print(na_indices_avg_mean)

print("NA indices in 'pct_contra' column:")
print(na_indices_pct_contra)

print("NA indices in 'country' column:")
print(na_indices_country)

print("NA indices in 'region' column:")
print(na_indices_region)

#delete rows for missing na indicies 
livwell_2 <- livwell_2[-na_indices_pct_contra, ]
#multilevel model alternating by year and group
fit3 <- stan_glmer(data = livwell_2, RH_contr_modern_p ~ ED_educ_years_mean  + (1|country_name )+(1|year), refresh = 0)

posterior <- posterior_predict(fit3, newdata = livwell_2)

average_preds <- colMeans(posterior)
livwell_2$predictions <- average_preds


plot_cambodia <- ggplot(data = livwell_2[livwell_2$country_name == "Cambodia", ], 
                        aes(x = ED_educ_years_mean, y = RH_contr_modern_p)) + 
  geom_point(data =  livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2013",  ], aes(x = ED_educ_years_mean, y = predictions, color = "blue"), shape = 24, fill = "blue") +
  geom_point(data =  livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2013",  ], aes(x = ED_educ_years_mean, y = RH_contr_modern_p), color = "blue") +
  geom_point(data =  livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2007",  ], aes(x = ED_educ_years_mean, y = predictions, color = "green"),  shape = 24, fill = "green") +  # Plot the predicted values
  geom_point(data =  livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2007",  ], aes(x = ED_educ_years_mean, y = RH_contr_modern_p, color = "green"), color = "green") +  # Plot the predicted values
  geom_point(data =  livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2010",  ], aes(x = ED_educ_years_mean, y = predictions, color = "purple"),  shape = 24, fill = "purple") +  # Plot the predicted values
  geom_point(data =  livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2010",  ], aes(x = ED_educ_years_mean, y = RH_contr_modern_p,color = "purple")) +  # Plot the predicted values
  labs(x = "Education", y = "Contraceptive", title = paste("Country:", "Cambodia"))

plot_cambodia <- plot_cambodia + 
  geom_smooth(data = livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2013",  ], aes(x = ED_educ_years_mean, y = predictions, color = "blue"), se = FALSE)+
  geom_smooth(data = livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2007",  ], aes(x = ED_educ_years_mean, y = predictions,  color = "green"), se = FALSE)+
  geom_smooth(data = livwell_2[livwell_2$country_name == "Cambodia" & livwell_2$year == "2010",  ], aes(x = ED_educ_years_mean, y = predictions, color = "purple"), se = FALSE)

plot_cambodia + 
  theme_minimal() +
  theme(legend.text = element_text(size = 5))+
  scale_color_manual(values = c("blue","green", "purple"), 
                     labels = c("2013", "2007", "2010"),
                     name = "Year") 





data2 <-livwell_2[livwell_2$country_name == "Bangladesh" & livwell_2$year == "2007",  ]
data3 <-livwell_2[livwell_2$country_name == "Bangladesh" & livwell_2$year == "2010",  ]
data1 <-livwell_2[livwell_2$country_name == "Bangladesh" & livwell_2$year == "2013",  ]
data4 <-livwell_2[livwell_2$country_name == "Bangladesh" & livwell_2$year == "2016",  ]

plot(data1$ED_educ_years_mean, data1$RH_contr_modern_p,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives ", main = "Average Education vs. Contraceptive Usage In Bangladesh", col = "blue", pch=19)
points(data2$ED_educ_years_mean,data2$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "red", pch=19)
points(data3$ED_educ_years_mean,data3$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "green", pch=19)
points(data4$ED_educ_years_mean,data4$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "purple", pch=19)
abline(a = (11.1 + 3.7), b= 1.9, linetype = "solid", col = "blue")
abline(a = (11.1 + 1.3), b= 1.9, linetype = "solid", col = "red")
abline(a = (11.1 +  2.3), b= 1.9, linetype = "solid", col = "green")
abline(a = (11.1 +  2.2), b= 1.9, linetype = "solid", col = "purple")
legend("topright", legend = c("2007","2010" , "2013", "2016"), col = c("red","blue", "green", "purple"), pch = 16, cex = .8)


data2 <-livwell_2[livwell_2$country_name == "Turkey" & livwell_2$year == "2007",  ]
data3 <-livwell_2[livwell_2$country_name == "Turkey" & livwell_2$year == "2010",  ]
data1 <-livwell_2[livwell_2$country_name == "Turkey" & livwell_2$year == "2013",  ]
data4 <-livwell_2[livwell_2$country_name == "Turkey" & livwell_2$year == "2016",  ]

plot(data1$ED_educ_years_mean, data1$RH_contr_modern_p,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives ", main = "Average Education vs. Contraceptive Usage: Turkey", col = "blue", pch=19)
points(data2$ED_educ_years_mean,data2$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "red", pch=19)
points(data3$ED_educ_years_mean,data3$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "green", pch=19)
points(data4$ED_educ_years_mean,data4$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "purple", pch=19)
abline(a = (11.1 + 3.7), b= 1.9, linetype = "solid", col = "blue")
abline(a = (11.1 + 1.3), b= 1.9, linetype = "solid", col = "red")
abline(a = (11.1 +  2.3), b= 1.9, linetype = "solid", col = "green")
abline(a = (11.1 +  2.2), b= 1.9, linetype = "solid", col = "purple")
legend("topright", legend = c("2007","2010" , "2013", "2016"), col = c("red","blue", "green", "purple"), pch = 16, cex = .8)

data2 <-livwell_2[livwell_2$country_name == "Honduras" & livwell_2$year == "2007",  ]
data3 <-livwell_2[livwell_2$country_name == "Honduras" & livwell_2$year == "2010",  ]
data1 <-livwell_2[livwell_2$country_name == "Honduras" & livwell_2$year == "2013",  ]
data4 <-livwell_2[livwell_2$country_name == "Honduras" & livwell_2$year == "2016",  ]

plot(data1$ED_educ_years_mean, data1$RH_contr_modern_p,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Average Education vs. Contraceptive Usage: Honduras", col = "blue", pch=19)
points(data2$ED_educ_years_mean,data2$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "red", pch=19)
points(data3$ED_educ_years_mean,data3$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "green", pch=19)
points(data4$ED_educ_years_mean,data4$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives", main = "Honduras", col = "purple", pch=19)

abline(a = (11.1 + 1.3), b= 1.9, linetype = "solid", col = "red")
abline(a = (11.1 +  2.3), b= 1.9, linetype = "solid", col = "green")
legend("topright", legend = c("2007" , "2010"), col = c("red", "green"), pch = 16, cex = .8)

plot(livwell_2$ED_educ_years_mean, livwell_2$predictions,  xlab = "Average Education in Years", ylab = "Predictions", main = "Average Education vs. Predictions of Contraceptive Usage", col = "blue", pch = 20, cex = 0.5,)

plot(livwell_2$ED_educ_years_mean, livwell_2$RH_contr_modern_p,  xlab = "Average Education in Years", ylab = "Predictions", main = "Average Education vs. Actual Contraceptive Usage", col = "red", pch = 20, cex = 0.5,)

#average education 
par(mfrow = c(4, 5))
all_country <- unique(livwell_df$country_name)
for (i in 1:20) {
  country_data <- livwell_df[livwell_df$country_name == all_country[i],]
  region_name_harmonized <- unique(country_data$region_name_harmonized)
  # Generate colors dynamically for each region
  num_regions <- length(region_name_harmonized)
  all_region_colors <- c("red", "blue", "green", "yellow", "purple", "pink", "orange", "black", "lightblue", "lightgreen")
  region_colors <- all_region_colors[1:num_regions]
  
  # Assign colors based on regions
  color_mapping <- setNames(region_colors, unique(region_name_harmonized))
  region_colors <- sapply(region_name_harmonized, function(r) color_mapping[r])
  print(region_colors)
  plot(country_data$year, country_data$ED_educ_years_mean,col = region_colors,  xlab = "Year", ylab = "Average Education", main =paste("Country: " , all_country[i]))
}

#graph countries avg education and contraceptive usage 

x_range <- range(c(0, 12))
y_range <- range(c(0, 60))
all_country <- unique(livwell_2$country_name)
years <- c("1990", "1995", "2000", "2005", "2010")

par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data_1990 <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data_1990 <- country_data_1990[country_data_1990$year == "1990",]
  if (nrow(country_data) > 0){
    plot(country_data_1990$ED_educ_years_mean, country_data_1990$RH_contr_modern_p,  xlim = x_range, ylim = y_range, xlab = "Average Education", ylab = "Contraceptive Usage", main =paste("Country in year 1990: " , all_country[i]), col = "blue")
    points(country_data_1990$ED_educ_years_mean, country_data_1990$predictions, col = "red")
    legend("topright", legend = c("Actual" , "Prediction"), col = c("blue", "red"), pch = 16, cex = .8)
  }
}

par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data <- country_data[country_data$year == "1995",]
  if (nrow(country_data) > 0){
    plot(country_data$ED_educ_years_mean, country_data$RH_contr_modern_p,  xlim = x_range, ylim = y_range, xlab = "Average Education", ylab = "Contraceptive Usage", main =paste("Country in year 1995: " , all_country[i]), col = "blue")
    points(country_data$ED_educ_years_mean, country_data$predictions, col = "red")
    #legend("topright", legend = c("Actual" , "Prediction"), col = c("blue", "red"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data <- country_data[country_data$year == "2000",]
  if (nrow(country_data) > 0){
    plot(country_data$ED_educ_years_mean, country_data$RH_contr_modern_p,  xlim = x_range, ylim = y_range, xlab = "Average Education", ylab = "Contraceptive Usage", main =paste("Country in year 2000: " , all_country[i]), col = "blue")
    points(country_data$ED_educ_years_mean, country_data$predictions, col = "red")
    #legend("topright", legend = c("Actual" , "Prediction"), col = c("blue", "red"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data <- country_data[country_data$year == "2005",]
  if (nrow(country_data) > 0){
    plot(country_data$ED_educ_years_mean, country_data$RH_contr_modern_p,  xlim = x_range, ylim = y_range, xlab = "Average Education", ylab = "Contraceptive Usage", main =paste("Country in year 2005: " , all_country[i]), col = "blue")
    points(country_data$ED_educ_years_mean, country_data$predictions, col = "red")
    #legend("topright", legend = c("Actual" , "Prediction"), col = c("blue", "red"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data <- country_data[country_data$year == "2010",]
  if (nrow(country_data) > 0){
    plot(country_data$ED_educ_years_mean, country_data$RH_contr_modern_p,  xlim = x_range, ylim = y_range, xlab = "Average Education", ylab = "Contraceptive Usage", main =paste("Country in year 2010: " , all_country[i]), col = "blue")
    points(country_data$ED_educ_years_mean, country_data$predictions, col = "red")
    #legend("topright", legend = c("Actual" , "Prediction"), col = c("blue", "red"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data <- country_data[country_data$year == "2015",]
  if (nrow(country_data) > 0){
    plot(country_data$ED_educ_years_mean, country_data$RH_contr_modern_p,  xlim = x_range, ylim = y_range, xlab = "Average Education", ylab = "Contraceptive Usage", main =paste("Country in year 2015: " , all_country[i]), col = "blue")
    points(country_data$ED_educ_years_mean, country_data$predictions, col = "red")
    #legend("topright", legend = c("Actual" , "Prediction"), col = c("blue", "red"), pch = 16, cex = .8)
  }
}

livwell_3 <- livwell[, c("year", "region_name_harmonized", "country_name", "ED_educ_years_mean", "RH_contr_modern_p", "WL_wealth_gini")]

na_indices_year <- which(is.na(livwell_3$year))
na_indices_avg_mean <- which(is.na(livwell_3$ED_educ_years_mean))
na_indices_pct_contra <- which(is.na(livwell_3$RH_contr_modern_p))
na_indices_country <- which(is.na(livwell_3$country_name))
na_indices_region <- which(is.na(livwell_3$region_name_harmonized))
na_indices_gini <- which(is.na(livwell_3$WL_wealth_gini))
# Print the row indices with NA values in each column
print("NA indices in 'year' column:")
print(na_indices_year)

print("NA indices in 'avg_mean' column:")
print(na_indices_avg_mean)

print("NA indices in 'pct_contra' column:")
print(na_indices_pct_contra)

print("NA indices in 'country' column:")
print(na_indices_country)

print("NA indices in 'region' column:")
print(na_indices_region)

print("NA indices in 'gini' column:")
print(na_indices_gini)

#delete rows for missing na indicies 
livwell_3 <- livwell_3[-na_indices_pct_contra, ]
livwell_3 <-  livwell_3[-na_indices_avg_mean, ]


colnames(livwell_3) <- c("year", "region", "country", "avg_edu", "pct_contra", "gini")

print(livwell_3)
n = nrow(livwell_3)
livwell_3$country_code <- as.numeric(factor(livwell_3$country, levels = unique(livwell_3$country)))
# Convert 'year' column to a factor with numeric codes
livwell_3$year_indicator <- as.numeric(factor(livwell_3$year, levels = unique(livwell_3$year)))

X = cbind(livwell_3$avg_edu, livwell_3$gini)
data <- list(pct_contra = livwell_3$pct_contra, country = livwell_3$country_code, year = livwell_3$year_indicator,  N = nrow(livwell_3), J_country = length(unique(livwell_3$country)), J_year =length(unique(livwell_3$year)), K = 2, X = X)
model <- cmdstan_model("FinalProject_Gini.stan")
fit_4 <- model$sample(data=data, parallel_chains=4, refresh=0)
print(fit_4)
sims <- as_draws_rvars(fit_4$draws())
sims$country_offset
sims$year_offset
sims$beta
sims$posterior_predictions
livwell_3$predictions <- sims$posterior_predictions
plot(livwell_3$avg_edu, livwell_3$pct_contra,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = "Percentage of Women that Use Modern Contraceptives ", main = "Average Education vs. Contraceptive Usage: Turkey", col = "blue", pch=19)

rvar_object <- sims$posterior_predictions

# Extract mean values manually
mean_values <- as.numeric(gsub(" ± .*", "", sims$posterior_predictions))

livwell_3$predictions <- mean_values

plot(livwell_3$avg_edu, livwell_3$pct_contra,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = "Actual Percentage of Women that Use Modern Contraceptives ", main = "Average Education vs. Contraceptive Usage", col = "blue", pch = 20, cex = 0.5)
plot(livwell_3$avg_edu, livwell_3$predictions,  xlim= x_range, ylim = y_range, xlab = "Average Education in Years", ylab = " Predicted Percentage of Women that Use Modern Contraceptives ", main = "Average Education vs. Predicted Contraceptive Usage", col = "red",  pch = 20, cex = 0.5,)


#gini across year
par(mfrow = c(4, 5))
all_country <- unique(livwell_3$country)
for (i in 1:20) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  region <- unique(country_data$region)
  # Generate colors dynamically for each region
  num_regions <- length(region)
  all_region_colors <- c("red", "blue", "green", "yellow", "purple", "pink", "orange")
  region_colors <- region_colors[1:num_regions]
  # Assign colors based on regions
  color_mapping <- setNames(region_colors, unique(region))
  region_colors <- sapply(region, function(r) color_mapping[r])
  print(region_colors)
  plot(country_data$year, country_data$gini,col = region_colors,  xlab = "Year", ylab = "Gini", main =paste("Country & Gini: " , all_country[i]))
}


#graph countries gini and contraceptive usage 

x_range <- range(c(0, 12))
y_range <- range(c(0, 60))
all_country <- unique(livwell_2$country_name)
years <- c("1990", "1995", "2000", "2005", "2010")

par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "1990",]
  country_data_0 <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data_0 <- country_data_0[country_data_0$year == "1990",]
  if (nrow(country_data) > 0){
    plot(country_data$avg_edu, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Edu", ylab = "Contraceptive Usage", main =paste("Year 1990: " , all_country[i]), col = "blue")
    points(country_data$avg_edu, country_data$predictions, col = "red")
    points(country_data_0$ED_educ_years_mean, country_data_0$predictions, col = "green")
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}

par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "1995",]
  country_data_0 <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data_0 <- country_data_0[country_data_0$year == "1995",]
  if (nrow(country_data) > 0){
    plot(country_data$avg_edu, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Edu", ylab = "Contraceptive Usage", main =paste("Year 1995: " , all_country[i]), col = "blue")
    points(country_data$avg_edu, country_data$predictions, col = "red")
    points(country_data_0$ED_educ_years_mean, country_data_0$predictions, col = "green")
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2000",]
  country_data_0 <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data_0 <- country_data_0[country_data_0$year == "2000",]
  if (nrow(country_data) > 0){
    plot(country_data$avg_edu, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Edu", ylab = "Contraceptive Usage", main =paste("Year 2000: " , all_country[i]), col = "blue")
    points(country_data$avg_edu, country_data$predictions, col = "red")
    points(country_data_0$ED_educ_years_mean, country_data_0$predictions, col = "green")
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2005",]
  country_data_0 <- livwell_3[livwell_3$country_name == all_country[i],]
  country_data_0 <- country_data_0[country_data_0$year == "2005",]
  if (nrow(country_data) > 0){
    plot(country_data$avg_edu, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Edu", ylab = "Contraceptive Usage", main =paste("Year 2005:" , all_country[i]), col = "blue")
    points(country_data$avg_edu, country_data$predictions, col = "red")
    points(country_data_0$ED_educ_years_mean, country_data_0$predictions, col = "green")
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2010",]
  country_data_0 <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data_0 <- country_data_0[country_data_0$year == "2010",]
  if (nrow(country_data) > 0){
    plot(country_data$avg_edu, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Edu", ylab = "Contraceptive Usage", main =paste("Year 2010: " , all_country[i]), col = "blue")
    points(country_data$avg_edu, country_data$predictions, col = "red")
    points(country_data_0$ED_educ_years_mean, country_data_0$predictions, col = "green")
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2015",]
  country_data_0 <- livwell_2[livwell_2$country_name == all_country[i],]
  country_data_0 <- country_data_0[country_data_0$year == "2015",]
  if (nrow(country_data) > 0){
    plot(country_data$avg_edu, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Edu", ylab = "Contraceptive Usage", main =paste("Year 1995: " , all_country[i]), col = "blue")
    points(country_data$avg_edu, country_data$predictions, col = "red")
    points(country_data_0$ED_educ_years_mean, country_data_0$predictions, col = "green")
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}



### gini 
x_range <- range(c(0, 1))
y_range <- range(c(0, 60))
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "1995",]
  
  if (nrow(country_data) > 0){
    plot(country_data$gini, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Gini", ylab = "Contraceptive Usage", main =paste("Year w/ Gini 2000: " , all_country[i]), col = "blue")
    points(country_data$gini, country_data$predictions, col = "green")
    
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}
x_range <- range(c(0, 1))
y_range <- range(c(0, 60))
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2000",]
  
  if (nrow(country_data) > 0){
    plot(country_data$gini, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Gini", ylab = "Contraceptive Usage", main =paste("Year w/ Gini 2005: " , all_country[i]), col = "blue")
    points(country_data$gini, country_data$predictions, col = "green")
    
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}
x_range <- range(c(0, 1))
y_range <- range(c(0, 60))
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2010",]
  
  if (nrow(country_data) > 0){
    plot(country_data$gini, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Gini", ylab = "Contraceptive Usage", main =paste("Year w/ Gini 2010: " , all_country[i]), col = "blue")
    points(country_data$gini, country_data$predictions, col = "green")
    
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}


x_range <- range(c(0, 1))
y_range <- range(c(0, 60))
par(mfrow = c(2, 5))
for (i in 1:10) {
  country_data <- livwell_3[livwell_3$country == all_country[i],]
  country_data <- country_data[country_data$year == "2015",]
 
  if (nrow(country_data) > 0){
    plot(country_data$gini, country_data$pct_contra,  xlim = x_range, ylim = y_range, xlab = "Gini", ylab = "Contraceptive Usage", main =paste("Year w/ Gini 2015: " , all_country[i]), col = "blue")
    points(country_data$gini, country_data$predictions, col = "green")
  
    #legend("topright", legend = c("Actual" , "Prediction with Gini", "Prediction with Gini"), col = c("blue", "red", "green"), pch = 16, cex = .8)
  }
}


