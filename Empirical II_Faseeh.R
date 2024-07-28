#set up

#clear the workspace
rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console
#Install and load haven package
if (!require(haven)) install.packages("haven"); library(haven)
summary(atlas)

# focusing on my county
## subset observations to Wisconsin
ny <- subset(atlas,state == 36)
## subset observations to Milwaukee County
kings <- subset(atlas,state == 36 & county == 047)

# corelation between absolute mobility and proportion of black people
#Report correlation coefficients
cor(kings$kfr_pooled_pooled_p25, kings$share_black2010, use="pairwise.complete.obs")

# plot between absolute mobility and proportion of black people
# Install and load ggplot2 package
install.packages("ggplot2")
library(ggplot2)
# Draw scatter plot with linear fit line
ggplot(data = kings) + geom_point(aes(x = kings$share_black2010, y = kings$kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = kings$share_black2010, y = kings$kfr_pooled_pooled_p25), method = "lm", se = F)
#Save graph as .png
ggsave("kings.png")


# scatter plot between absolute mobility and proportion of black people
# Install statar
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_black2010, y = kings$kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# scatter plot between absolute mobility and proportion of white people
# Install statar
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_white2010, y = kings$kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")


# scatter plot between proportion who are college graduates and proportion of black people
# Install statar
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_black2010, y = kings$frac_coll_plus2010)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# scatter plot between proportion who are college graduates and proportion of white people
# Install statar
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_white2010, y = kings$frac_coll_plus2010)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# scatter plot between proportion who are foreigners and proportion of black people
# Install statar
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_black2010, y = kings$foreign_share2010)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# scatter plot between proportion who are foreigners and proportion of white people
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_white2010, y = kings$foreign_share2010)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# scatter plot between job growth and proportion of white people
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$share_white2000, y = kings$ann_avg_job_growth_2004_2013)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")



# histogram for incarceration rates

library(ggplot2)


# Assuming 'kings' is your dataframe and it contains columns for each rate
# Extracting rates for each community and the overall rate from the 'kings' dataframe
jail_pooled_pooled_p25 <- mean(kings$jail_pooled_pooled_p25, na.rm = TRUE)  # Mean overall rate
jail_asian_pooled_p25 <- mean(kings$jail_asian_pooled_p25, na.rm = TRUE)    # Mean Asian rate
jail_black_pooled_p25 <- mean(kings$jail_black_pooled_p25, na.rm = TRUE)    # Mean Black rate
jail_white_pooled_p25 <- mean(kings$jail_white_pooled_p25, na.rm = TRUE)    # Mean White rate

# Create a dataframe
incarceration_data <- data.frame(
  Community = c("Overall", "Asian", "Black", "White"),
  Rate = c(100*jail_pooled_pooled_p25, 100*jail_asian_pooled_p25, 100*jail_black_pooled_p25, 100*jail_white_pooled_p25)
)

# Plotting
ggplot(incarceration_data, aes(x = Community, y = Rate, fill = Community)) +
  geom_bar(stat = "identity", width = 0.5, fill = "darkblue") + # 'identity' to use actual y values
  theme_minimal() + # Minimal theme
  labs(title = "Incarceration Rates by community in Kings County",
       x = "Community",
       y = "Incarceration Rate")


# scatter plot between absolute mobility at 25thp and proportion of white people jailed
install.packages("statar")
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$jail_white_pooled_p25, y = kings$kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# correlation between absolute mobility at 25thp and proportion of white people jailed
cor(kings$kfr_pooled_pooled_p25, kings$jail_white_pooled_p25, use="pairwise.complete.obs")

# scatter plot between absolute mobility at 25thp and proportion of black people jailed
library(statar)
#Draw binscatter plot with linear best fit line
ggplot(kings, aes(x = kings$jail_black_pooled_p25, y = kings$kfr_pooled_pooled_p25)) +
  stat_binmean(n = 20) +
  stat_smooth(method = "lm", se = FALSE)
ggsave("milwaukee_binscatter.png")

# correlation between absolute mobility at 25thp and proportion of black people jailed

cor(kings$kfr_pooled_pooled_p25, kings$jail_black_pooled_p25, use="pairwise.complete.obs")




