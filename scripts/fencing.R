library(nlme)
#install.packages("lme4")
library(lme4)
library(dplyr)
library(stringr)
library(broom)
#install.packages("jtools")
library(jtools)
#install.packages("sjPlot")
library(sjPlot)
library(magrittr)
sv<- readRDS("./data/MGsurvey.RDS")

# filter out those who want to fence:
# Filter rows where the 'howChngNext5Y' column contains the substring "fenc"
#result <- sv %>%
 # filter(str_detect(as.character(howChngNext5Y), "Fenc*"))


# Fencing DV --------------------------------------------------------------
# Create Fencing DV for fencing to use as a response param:
fv <- mutate(sv, FencYN = if_else(str_detect(howChngNext5Y, "Fenc"), 1, 0)) %>%
  mutate(FencYN = if_else(is.na(FencYN), 0, 1)) 
#why is last row empty? need to trace that back.
fv_clean <- fv[1:187,]

# DO WE NEED THIS?
# center the predictor bc the range is never zero
fv_clean$cov23median <- as.numeric(scale(fv_clean$cov23median, scale = FALSE))  # Center without scaling

plot(FencYN~cov23median, data = fv_clean)


# FenceXVeg model ---------------------------------------------------------

# Fit a logistic regression model
f.mod <- glm(FencYN ~ cov23median, data = fv_clean, family = binomial(link = "logit"))
summary(f.mod)
# Fit the null (intercept-only) model
model_null <- glm(FencYN ~ 1, data = fv_clean, family = binomial(link = "logit"))
summary(model_null)


# Add in social dynamics--- 



# check for whether need to account for zero inflation:
install.packages("DHARMa")
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = f.mod)
plot(simulationOutput)
  ## INDICATES NO ISSUE

# so we don't need to do this instead:
# ADJUSTING for zero-inflation
# install.packages("logistf")
# library(logistf)
# f.mod <- logistf(FencYN ~ cov23median, data = fv_clean)
# summary(f.mod)

# Compute medians for each factor level
medians <- fv_clean %>%
  group_by(FencYN) %>%
  summarize(median_value = median(cov23median, na.rm = TRUE))

ks<- ks.test(fv_clean$cov23median[fv_clean$FencYN == "0"], 
        fv_clean$cov23median[fv_clean$FencYN == "1"])
# signif

medians




# mixed effectws model, grouped by Soum
  # (1 | Soum) allows a random intercept for each level of soum.
    # NOT WORKING ---
f.mm <- glmer(FencYN ~ cov23median + (1 | Soum.x), data = fv_clean, family = binomial(link = "logit"))

# get singularity, so just add as fixed-effect?
f.me <- glm(FencYN ~ cov23median + factor(Soum.x), data = fv_clean, family = binomial(link = "logit"))
summary(f.me)
# Now nothing is significant.

# Compare the models using a likelihood ratio test
anova(f.mod, f.me,  test = "Chisq")

# Get a tidy summary
tidy_summary <- tidy(f.mod)

# Print the tidy summary
print(tidy_summary)

# summary from jtools:
# Get a polished summary
summ(f.mod, exp = TRUE)

# from sjPlot (my favorite way)
# Create a table
tab_model(f.mod, f.me, show.se = TRUE,  show.aic = TRUE)



# Generate a dataset for predictions
  # Creates a sequence of cov23median values for which to predict probabilities:
new_data <- data.frame(
  cov23median = seq(
    min(fv_clean$cov23median, na.rm = TRUE),
    max(fv_clean$cov23median, na.rm = TRUE),
    length.out = 100
  )
) 
# creates a sequence of values beyond those sampled:
new_data2 <- data.frame(cov23median = seq(55, 80, length.out = 30))

  # Calculates predicted probabilities for the new data:
new_data$predicted_prob <- predict(f.mod, newdata = new_data, type = "response")

# Plot the predicted probabilities
ggplot() +
  geom_point(data = fv_clean, aes(x = cov23median, y = FencYN), color = "blue") +
  geom_line(data = new_data, aes(x = cov23median, y = predicted_prob), color = "red") +
  labs(
    x = "Median veg cover in surrounding district in 2023",
    y = "Predicted probability of planned fencing"
  ) +
  theme_minimal()

#For multiple predictors: You can fix one predictor at a constant value (e.g., median or mean) and vary the other to create a similar plot.
#For categorical predictors: Facet the plot by the categorical variable using facet_wrap(~ predictor2).


# Fence x Lsk model -------------------------------------------------------

# pull in sfu change from 2c_lsk_changes
# filter out sfu totals and change and Ref #
lsk<- sfu %>% select(c(1,20:22))
lsk <- lsk[1:187,]
# remove outliers
# This isn't matching right yet
View(lsk) #187 rows

lsk_fen <- left_join(lsk, fv, by = "Ref") %>% # still ends up with a few NAs, something with matching?
  filter(sfuChange > -2000)  # now 183

# Fit a logistic regression model
l.mod <- glm(FencYN ~ sfuChange, data = lsk_fen, family = binomial(link = "logit"))
l.mod <- glm(FencYN ~ sfu23, data = lsk_fen, family = binomial(link = "logit"))
l.mod <- glm(FencYN ~ sfu5Y, data = lsk_fen, family = binomial(link = "logit"))

summary(l.mod)

# Compute medians for each factor level
medians <- lsk_fen %>%
  group_by(FencYN) %>%
  summarize(median_value = median(sfuChange, na.rm = TRUE))
medians
ks<- ks.test(lsk_fen$sfuChange[lsk_fen$FencYN == "0"], 
             lsk_fen$sfuChange[lsk_fen$FencYN == "1"])
#not signif diff#


# F x Lsk+ Labor ----------------------------------------------------------
l.mod <- glm(FencYN ~ sfu23 + impacted_labor, data = lsk_fen, family = binomial(link = "logit"))

ll.mod <- glm(FencYN ~ impacted_labor, data = lsk_fen, family = binomial(link = "logit"))
summary(ll.mod)

ctab<- table(lsk_fen$impacted_labor, lsk_fen$FencYN, useNA = "no")
# Chi-square test
chisq.test(ctab)
