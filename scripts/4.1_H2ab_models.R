##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.1_H2ab_models
# This script contains code to test Hypothesis 2a: Area plan polygons have a 
# greater number of SOR than areas not planned for development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
source(here("scripts", "0_setup.R"))

# Load polygons data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Remove orphaned buffers
dev_ids <- model_data |> filter(polygon_type == "Development") |> pull(polygon_id)
model_data <- model_data |> filter(polygon_id %in% dev_ids)

# Create binary presence/absence variable 
model_data_complete <- model_data |>
  mutate(polygon_type    = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor  = factor(kommune),
         pair_id_factor  = factor(pair_id),
         log_area        = log(area_m2_numeric),
         presence        = as.integer(n_occurrences > 0))

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. Full three-way interactions --------------------------------------------

# Set up model
h2a_binomial1_full <- glmer(presence ~ log_area * polygon_type * land_cover_name + 
                              (1|kommune_factor/pair_id_factor),
                            data = model_data_complete,
                            family = binomial(link = "logit"))

# Save model output
saveRDS(h2a_binomial1_full, here("data", "models", "h2a_binomial1_full.rds"))

## 3.2. Two-way interactions ---------------------------------------------------

# Set up model
h2a_binomial2_twoway <- glmer(presence ~ log_area * polygon_type + log_area * land_cover_name + 
                                polygon_type * land_cover_name + (1|kommune_factor/pair_id_factor),
                              data = model_data_complete,
                              family = binomial(link = "logit"))

# Save model output
saveRDS(h2a_binomial2_twoway, here("data", "models", "h2a_binomial2_twoway.rds"))

## 3.3. Additive effects (H2b) -------------------------------------------------

# Set up model
h2a_binomial3_additive <- glmer(presence ~ log_area + polygon_type + land_cover_name + 
                                  (1|kommune_factor/pair_id_factor),
                                data = model_data_complete,
                                family = binomial(link = "logit"))

# Save model output
saveRDS(h2a_binomial3_additive, here("data", "models", "h2a_binomial3_additive.rds"))

# Compare models
AICtab(h2a_binomial1_full, h2a_binomial2_twoway, base = TRUE)

# Get the best model
best_model <- h2a_binomial1_full

## 3.4. Model diagnostics ------------------------------------------------------

# Binomial residual plots
residuals_dev <- residuals(best_model, type = "deviance")
fitted_probs <- fitted(best_model)

# Save figure
png(filename = here("figures", "Figure_H2a_binomial_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 3))

# Binned residual plot
bins <- cut(fitted_probs, breaks = 20)
bin_means <- tapply(residuals_dev, bins, mean)
bin_fitted <- tapply(fitted_probs, bins, mean)

plot(bin_fitted, bin_means,
     main = "Binned Residuals",
     xlab = "Fitted probability",
     ylab = "Average residual",
     pch = 16, cex = 1.5)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Residuals vs fitted
plot(fitted_probs, residuals_dev,
     main = "Deviance Residuals vs Fitted",
     xlab = "Fitted probability",
     ylab = "Deviance residuals",
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.3))
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Observed vs Predicted rates by bins
obs_rate <- tapply(model_data_complete$presence, bins, mean)
pred_rate <- tapply(fitted_probs, bins, mean)

plot(pred_rate, obs_rate,
     main = "Observed vs Predicted Rates",
     xlab = "Predicted probability",
     ylab = "Observed rate",
     pch = 16, cex = 1.5,
     xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lwd = 2, lty = 2)

# Distribution of fitted probabilities
hist(fitted_probs,
     main = "Distribution of Fitted Probabilities",
     xlab = "Fitted probability",
     breaks = 30,
     col = "lightblue")

# Residuals by polygon type
boxplot(residuals_dev ~ model_data_complete$polygon_type,
        main = "Residuals by Polygon Type",
        xlab = "Polygon type",
        ylab = "Deviance residuals",
        col = c("#2ca02c", "#d62728"))
abline(h = 0, col = "blue", lty = 2)

# Residuals by land cover
boxplot(residuals_dev ~ model_data_complete$land_cover_name,
        main = "Residuals by Land Cover",
        xlab = "",
        ylab = "Deviance residuals",
        las = 2, cex.axis = 0.7)
abline(h = 0, col = "blue", lty = 2)

par(mfrow = c(1, 1))
dev.off()

## 3.5. Get model summary ------------------------------------------------------

# Get summary
print(summary(best_model))

# Extract coefficients
coefs <- summary(best_model)$coefficients
polygon_coef <- coefs["polygon_typeDevelopment", ]

# Convert to odds ratio
odds_ratio <- exp(polygon_coef["Estimate"])
cat("Odds Ratio:", round(odds_ratio, 3), "\n")

## 3.6. Extract predictions ----------------------------------------------------

# Get predictions
predictions <- ggpredict(best_model, 
                         terms = c("log_area [all]", "polygon_type", "land_cover_name"),
                         type = "fixed")

# Convert predictions to df
pred_df <- as.data.frame(predictions) |>
  rename(log_area = x, polygon_type = group, land_cover_name = facet)

# Create prediction plot
fig_h2a_binomial <- ggplot(pred_df, aes(x = log_area, y = predicted,
                                        color = polygon_type, fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("Buffer" = "#2ca02c", "Development" = "#d62728"),
                     name = "Area type") +
  scale_fill_manual(values = c("Buffer" = "#2ca02c", "Development" = "#d62728"),
                    name = "Area type") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Predicted Probability of Species Presence",
       x = expression(paste("Log(Area (m"^2, "))")),
       y = "Probability of species presence") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"))

# Save predictions figure
ggsave(filename = here("figures", "Figure_H2a_binomial_predictions.png"),
       plot = fig_h2a_binomial, width = 12, height = 8, dpi = 600)

ggsave(filename = here("figures", "Figure_H2a_binomial_predictions.pdf"),
       plot = fig_h2a_binomial, width = 12, height = 8, dpi = 600)
