##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.3_H3_GLMM
# This script contains code to test Hypothesis 3: Urban and near-urban polygons
# will have more SOR than othe planned development types
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load polygon all data
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Filter out Ports & Marinas from planned development category
model_data <- polygon_all_data |>
  filter(english_categories != "Ports") |>
  # log-transform area
  mutate(log_area = log(area_m2_numeric),
        # convert development category and kommune to factors
        development_category = as.factor(english_categories),
        kommune_factor = as.factor(kommune))

# Check if there are any NAs in key variables
cat("n_occurrences:", sum(is.na(model_data$n_occurrences)), "\n")
cat("log_area:", sum(is.na(model_data$log_area)), "\n")
cat("development_category:", sum(is.na(model_data$development_category)), "\n")
cat("kommune_factor:", sum(is.na(model_data$kommune_factor)), "\n")

# Remove rows with NAs in model variables
model_data <- model_data |>
  filter(!is.na(n_occurrences),
         !is.na(log_area),
         !is.na(development_category),
         !is.na(kommune_factor))

# Check sample size for model
cat("Number of municipalities:", n_distinct(model_data$kommune_factor), "\n")
cat("Development categories:", paste(levels(model_data$development_category), collapse = ", "), "\n")

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. Zero inflated + interaction --------------------------------------------

# Fit model
h3_model1_zinb_interaction <- glmmTMB(n_occurrences ~ log_area * development_category + (1|kommune_factor),
                                      data = model_data,
                                      family = nbinom2,
                                      ziformula = ~log_area + development_category)

# Save model
save(h3_model1_zinb_interaction, file = here::here("data", "models",
                                                   "h3_model1_zinb_interaction.RData"))

## 3.2. Zero infalted no interaction -------------------------------------------

# Fit model
h3_model2_zinb_no_interaction <- glmmTMB(n_occurrences ~ log_area + development_category + (1|kommune_factor),
                                      data = model_data,
                                      family = nbinom2,
                                      ziformula = ~log_area + development_category)

# Save model
save(h3_model2_zinb_no_interaction, file = here::here("data", "models",
                                                   "h3_model2_zinb_no_interaction.RData"))

# Compare models
AICtab(h3_model1_zinb_interaction, h3_model2_zinb_no_interaction, base = TRUE)

# 4. MODEL SUMMARY AND DIAGNOSTICS ---------------------------------------------

## 4.1. Model summary ----------------------------------------------------------

# Get the model summary
print(summary(h3_model1_zinb_interaction))

## 4.2. Check for convergence --------------------------------------------------

if(h3_model1_zinb_interaction$sdr$pdHess) {
  cat("\nModel converged successfully\n")
} else {
  cat("\nWarning: Model may not have converged properly\n")
} # Model converged successfully

## 4.3. Model diagnostics with DHARMa ------------------------------------------

sim_residuals <- simulateResiduals(fittedModel = h3_model1_zinb_interaction, 
                                   n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H3_model_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals)
dev.off()

# Test for dispersion
dispersion_test <- testDispersion(sim_residuals)
cat("\nDispersion test p-value:", dispersion_test$p.value, "\n")

# Test for zero-inflation
zeroinflation_test <- testZeroInflation(sim_residuals)
cat("Zero-inflation test p-value:", zeroinflation_test$p.value, "\n")

## 4.4. Extract random effects -------------------------------------------------

# Get random effect variance
random_effects <- VarCorr(h3_model1_zinb_interaction)
cat("\nRandom effect variance (kommune):\n")
print(random_effects)

# Calculate ICC (intraclass correlation coefficient)
# Proportion of variance explained by municipality
re_var <- as.numeric(random_effects$cond$kommune_factor[1])
cat("\nRandom effect variance:", round(re_var, 4), "\n")

# 5. HYPOTHESIS TESTING --------------------------------------------------------

cat("\n=== HYPOTHESIS 3 TESTING ===\n")
cat("H3: The relationship between area and SOR differs across development categories\n\n")

lrt_result <- anova(h3_model2_zinb_no_interaction, h3_model1_zinb_interaction)
cat("Likelihood Ratio Test for interaction:\n")
print(lrt_result)

if(lrt_result$`Pr(>Chisq)`[2] < 0.05) {
  cat("\nSignificant interaction: The relationship between area and SOR DOES differ across development categories (p < 0.05)\n")
} else {
  cat("\nNon-significant interaction: The relationship between area and SOR does NOT significantly differ across development categories (p >= 0.05)\n")
}

# 6. EXTRACT AND PLOT PREDICTIONS ----------------------------------------------

## 6.1. Calculate predicted values ---------------------------------------------

# Generate predictions across the range of observed areas for each development category
predictions <- ggpredict(h3_model1_zinb_interaction, 
                         terms = c("log_area [all]", "development_category"),
                         type = "fixed")

# Convert predictions to data frame for plotting
pred_df <- as.data.frame(predictions)

## 6.2. Create main effects plot -----------------------------------------------

# Create colour palette for development categories
category_colours <- viridis(n_distinct(model_data$development_category), 
                            option = "turbo")
names(category_colours) <- levels(model_data$development_category)

# Plot predicted SOR vs log(Area) for each development category
fig_h3_predictions <- ggplot() +
  # add raw data points (semi-transparent)
  geom_point(data = model_data,
             aes(x = log_area, y = n_occurrences, color = development_category),
             alpha = 0.2, size = 0.8) +
  # sdd model predictions with confidence intervals
  geom_line(data = pred_df,
            aes(x = x, y = predicted, color = group),
            linewidth = 1.2) +
  geom_ribbon(data = pred_df,
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2) +
  scale_color_manual(values = category_colours,
                     name = "Development category") +
  scale_fill_manual(values = category_colours,
                    name = "Development category") +
  labs(x = expression(paste("Log(Polygon area (m"^2, "))")),
       y = "Predicted number of species occurrence records") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11))

# Save figure
ggsave(filename = here("figures", "Figure_H3_area_SOR_by_category.png"),
       plot = fig_h3_predictions,
       width = 14,
       height = 8,
       dpi = 600)

## 6.3. Extract slopes for each category ---------------------------------------

# Calculate the slope of log_area effect for each development category
slopes_emtrends <- emtrends(h3_model1_zinb_interaction, 
                            specs = "development_category",
                            var = "log_area",
                            type = "response")

cat("\n=== SLOPES (effect of log area on SOR) by development category ===\n")
print(summary(slopes_emtrends))

# Save slopes to CSV
slopes_df <- as.data.frame(summary(slopes_emtrends))
write.csv(slopes_df,
          here("figures", "Table_H3_slopes_by_category.csv"),
          row.names = FALSE)

ggsave(filename = here("figures", "Figure_H3_area_SOR_by_category.pdf"),
       plot = fig_h3_predictions,
       width = 14,
       height = 8,
       dpi = 600)

# 7. POST-HOC PAIRWISE COMPARISONS (if interaction is significant) ------------

if(lrt_result$`Pr(>Chisq)`[2] < 0.05) {
  cat("\n=== PAIRWISE COMPARISONS OF SLOPES ===\n")
  
  # Compare slopes between development categories
  slope_contrasts <- contrast(slopes_emtrends, method = "pairwise", adjust = "bonferroni")
  
  print(summary(slope_contrasts))
  
  # Save contrasts to CSV
  contrasts_df <- as.data.frame(summary(slope_contrasts))
  write.csv(contrasts_df,
            here("figures", "Table_H3_pairwise_slope_comparisons.csv"),
            row.names = FALSE)
  
  cat("\nPairwise comparisons saved to figures/Table_H3_pairwise_slope_comparisons.csv\n")
}

# END OF SCRIPT ----------------------------------------------------------------