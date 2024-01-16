
# Total Incomplete Waiters ------------------------------------------------

rtt_total_chart_df <- rtt_total %>%
  group_by(Effective_Snapshot_Date) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE))

rtt_total_chart <- 
  ggplot(data = rtt_total_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways)) +
  geom_line(col = "#40C1AC", linewidth = 0.8) +
  scale_x_date(date_breaks = c("6 months"), date_labels = "%b - %y") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  annotate(geom = "label",
           x = as.Date("2020-01-01"),
           y = 7000000,
           label = "Start of pandemic",
           hjust = 1) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_chart

rtt_total_region_chart_df <- rtt_total %>%
  filter(!Region_Name %in% c("NULL", "UNKNOWN")) %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE))

rtt_total_region_chart_facet <- 
  ggplot(data = rtt_total_region_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways)) +
  geom_line(col = "#40C1AC") +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  facet_wrap(~Region_Name, nrow = 2, scales = "free") +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_region_chart <- 
  ggplot(data = rtt_total_region_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways, col = Region_Name)) +
  geom_line() +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  scale_y_continuous(labels = comma, breaks = c(seq(0, 1500000, by = 250000))) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  annotate(geom = "label",
           x = as.Date("2020-01-01"),
           y = 1200000,
           label = "Start of pandemic",
           hjust = 1) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_region_chart



# Backlog by Week ---------------------------------------------------------

rtt_weeks <- ggplot(rtt_total_quantiles_summary, aes(x = as.Date(Effective_Snapshot_Date))) +
  geom_ribbon(aes(ymin = Percentile_10, ymax = Percentile_90), fill = "#407EC9", col = "#ffffff", alpha = 0.5, show.legend = TRUE)+
  geom_ribbon(aes(ymin = Percentile_25, ymax = Percentile_75), fill = "#68D2DF", col = "#ffffff", alpha = 1, show.legend = TRUE)+
  geom_line(aes(y = Percentile_50, col = "Median Waiting Time"), linewidth = 0.8, show.legend = TRUE) + 
  scale_color_manual("", values = "black")

rtt_weeks <- ggplot(rtt_total_quantiles_summary, aes(x = as.Date(Effective_Snapshot_Date))) +
  geom_ribbon(aes(ymin = Percentile_10, ymax = Percentile_90, fill = "10th - 90th Percentile Range"), col = "#ffffff", alpha = 0.5)+
  geom_ribbon(aes(ymin = Percentile_25, ymax = Percentile_75, fill = "Interquartile Range"), col = "#ffffff", alpha = 0.5)+
  geom_line(aes(y = Percentile_50, col = "Median Waiting Time"), linewidth = 0.8) + 
  scale_color_manual("", values = "black") +
  scale_fill_manual("", values = c("#68D2DF", "#407EC9")) +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  geom_hline(yintercept = 18, linetype = "dashed") +
  annotate(geom = "label",
           x = as.Date("2012-01-01"),
           y = 20,
           label = "18 Weeks",
           hjust = 1) + 
  labs(x = "Month Ending",
       y = "Weeks Waiting",
       caption = "Source: Monthly RTT Published Data",
       title = "Summary of Weeks Waiting at Month End - Incomplete RTT Pathways",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_weeks
