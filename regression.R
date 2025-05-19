library(olsrr)
library(RColorBrewer)

# Applying transformations to reduce the skewness of the data
diab_seifa_df$log_IRSD <- log(max(diab_seifa_df$IRSD+10) - diab_seifa_df$IRSD)
diab_seifa_df$log_IER <- log(max(diab_seifa_df$IER+10) - diab_seifa_df$IER)
diab_seifa_df$log_IEO <- log(diab_seifa_df$IEO)
diab_seifa_df$log_Crude_rate <- log(diab_seifa_df$Crude_rate)

write.csv(diab_seifa_df, "processed_data/diab_seifa_df.csv", row.names = F)

# Calculating a regression model with all available parameters, then using 
# olsrr::ols_step_all_possible to find the optimal model
# Found using material from ANU course STAT2008 (Regression Modelling)
full_lm <- lm(data = diab_seifa_df, log_Crude_rate ~ IRSAD + log_IER + log_IEO + log_IRSD)
subs <- ols_step_all_possible(full_lm)

# The result of subs shows us that this model below has the highest adj R2 value
optm_lm <- lm(data = diab_seifa_df, log_Crude_rate ~ IRSAD + log_IER + log_IRSD)

# Figure 5: Regression slope for IRSAD vs log(Crude rate)
png("figs/irsad_lcr_plot.png", 1000, 600)
ggplot(diab_seifa_df, aes(x = IRSAD, y = log_Crude_rate)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "IRSAD", y = "log(Crude rate)", title = "IRSAD and Crude Diabetes Mortality Rates for LGAs") +
  theme(plot.title = element_text(face = "bold", size = 28, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.8),
        axis.title = element_text(face = "bold", size = 21),
        axis.text = element_text(size = 18),
        plot.margin = margin(10, 30, 10, 10))
dev.off()

# Using the regression model to impute missing data
diab_seifa_filled_df <- diab_seifa_df |>
  mutate(
    status = if_else(is.na(Crude_rate), "Estimated", "Actual"),
    predicted_log_Crude_rate = predict(optm_lm, newdata = cur_data()),
    predicted_Crude_rate = if_else(is.na(Crude_rate), exp(predicted_log_Crude_rate), NA_real_),
    Crude_rate = if_else(is.na(Crude_rate), predicted_Crude_rate, Crude_rate),
    log_Crude_rate = if_else(is.na(log_Crude_rate), predicted_log_Crude_rate, log_Crude_rate)
  ) |>
  select(-predicted_log_Crude_rate, -predicted_Crude_rate) |>
  inner_join(seifa_df, by = c("LGA", "IRSD", "IRSAD", "IER", "IEO"))
write.csv(diab_seifa_filled_df, "processed_data/diab_seifa_filled_df.csv", row.names = F)

# Figure 6: Regression slope for IRSAD vs log(Crude rate) with missing values imputed
png("figs/predicted_irsad_lcr_plot.png", 1000, 600)
ggplot(diab_seifa_filled_df, aes(x = IRSAD, y = log_Crude_rate)) +
  geom_point(aes(colour = status), size = 2) + 
  geom_smooth(method = "lm", alpha = 0.3) +
  scale_colour_manual(values = brewer.pal(3, "Dark2")[1:2]) +
  labs(x = "IRSAD", y = "log(Crude rate)", 
       title = "IRSAD and Crude Diabetes Mortality Rates for LGAs (missing values imputed)",
       colour = "") +
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.8),
        axis.title = element_text(face = "bold", size = 21),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 21))
dev.off()

# Figure 6 alt (for poster)
png("figs/predicted_irsad_lcr_plot_ALT.png", 1600, 1000)
ggplot(diab_seifa_filled_df, aes(x = IRSAD, y = log_Crude_rate)) +
  geom_point(aes(colour = status), size = 4) + 
  geom_smooth(method = "lm", alpha = 0.3) +
  scale_colour_manual(values = brewer.pal(3, "Dark2")[1:2]) +
  labs(x = "IRSAD", y = "log(Crude rate)", 
       title = "IRSAD and Crude Diabetes Mortality Rates for LGAs (missing values imputed)",
       colour = "") +
  theme(plot.title = element_text(face = "bold", size = 42, hjust = 0.2), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.8),
        axis.title = element_text(face = "bold", size = 36),
        axis.text = element_text(size = 32),
        legend.text = element_text(size = 36))
dev.off()
