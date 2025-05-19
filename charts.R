library(gt)

# Figure 1: 10 most common causes of death Australia-wide
png("figs/top_causes_chart.png", 1000, 600)
ggplot(top_causes_df[1:10, ]) +
  geom_col(mapping = aes(x = Cause_of_death, y = Deaths), 
           fill = "cornflowerblue") +
  labs(title = "Top 10 Causes of Death in Australia for 2017-2021",
       x = "Cause",
       y = "Number of Deaths") +
  theme(plot.title = element_text(face = "bold", size = 28, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.8),
        axis.title = element_text(face = "bold", size = 21),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
        plot.margin = margin(10, 10, 10, 50))
dev.off()

# Figure 3: Correlation between Cause and IRSAD
top_ten <- top_causes_df$Cause_of_death[1:10]
correlation_results <- comb_allc_df |>
  filter(Cause_of_death %in% top_ten) |>                
  filter(!is.na(Crude_rate) & !is.na(IRSAD)) |>         
  group_by(Cause_of_death) |>
  filter(n() > 1) |>                                    
  summarise(Correlation = cor(Crude_rate, IRSAD)) |>
  arrange(desc(abs(Correlation))) |>
  rename("Cause of Death" = "Cause_of_death", "Correlation with IRSAD" = "Correlation") |>
  gt() |>
  tab_header(title = md("**Correlation Between Crude Death Rate and IRSAD**")) |>
  tab_style(style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
gtsave(correlation_results, filename = "figs/correlation_table.png")
