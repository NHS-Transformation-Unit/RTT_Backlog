
# Total Incomplete Waiters ------------------------------------------------

rtt_total_chart_df <- rtt_total %>%
  group_by(Effective_Snapshot_Date) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE))

rtt_total_max_date <- max(rtt_total_chart_df$Effective_Snapshot_Date)

rtt_total_chart_df_latest <- rtt_total_chart_df %>%
  filter(Effective_Snapshot_Date == rtt_total_max_date)

rtt_total_chart <- 
  ggplot(data = rtt_total_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways)) +
  geom_line(col = palette_tu[4], linewidth = 1.2) +
  geom_point(data = rtt_total_chart_df_latest, col = palette_tu[4], size = 2) +
  geom_label_repel(data = rtt_total_chart_df_latest, aes(label = comma(Incomplete_Pathways)), nudge_x = 300) +
  scale_x_date(date_breaks = c("6 months"), date_labels = "%b - %y") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  annotate(geom = "label",
           x = as.Date("2020-01-01"),
           y = 6500000,
           label = "Start of pandemic",
           hjust = 1) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways",
       subtitle = "All England") +
  selected_theme(hex_col = palette_tu[1])

rtt_total_chart

rtt_total_region_chart_df <- rtt_total %>%
  filter(!Region_Name %in% c("NULL", "UNKNOWN")) %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE))

rtt_total_region_chart_df_latest <- rtt_total_region_chart_df %>%
  filter(Effective_Snapshot_Date == rtt_total_max_date)

rtt_total_region_chart <- 
  ggplot(data = rtt_total_region_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways, col = Region_Name)) +
  geom_line() +
  scale_color_manual(values = palette_region, name = "Region") +
  geom_point(data = rtt_total_region_chart_df_latest, size = 2, show.legend = FALSE) +
  geom_label_repel(data = rtt_total_region_chart_df_latest, aes(label = comma(Incomplete_Pathways)), nudge_x = 365, show.legend = FALSE) +
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
       subtitle = "NHS England Regions") +
  selected_theme(hex_col = palette_tu[1])

rtt_total_region_chart

# Backlog by Week ---------------------------------------------------------

rtt_total_quantiles_summary_latest <- rtt_total_quantiles_summary %>%
  filter(Effective_Snapshot_Date == rtt_total_max_date)

rtt_total_weeks_chart <- ggplot(rtt_total_quantiles_summary, aes(x = as.Date(Effective_Snapshot_Date))) +
  geom_ribbon(aes(ymin = Percentile_10, ymax = Percentile_90, fill = "10th - 90th Percentile Range"), col = "#ffffff", alpha = 0.5)+
  geom_ribbon(aes(ymin = Percentile_25, ymax = Percentile_75, fill = "Interquartile Range"), col = "#ffffff", alpha = 0.5)+
  geom_line(aes(y = Percentile_50, col = "Median Waiting Time"), linewidth = 1.2) + 
  scale_color_manual("", values = "black") +
  scale_fill_manual("", values = c(palette_wong_regions[2], palette_wong_regions[5])) +
  geom_point(data = rtt_total_quantiles_summary_latest, aes(y = Percentile_50), size = 2, show.legend = FALSE) +
  geom_label_repel(data = rtt_total_quantiles_summary_latest, aes(label = Percentile_50, y = Percentile_50), nudge_x = 200, show.legend = FALSE, col = "black") +
  geom_label_repel(data = rtt_total_quantiles_summary_latest, aes(label = Percentile_10, y = Percentile_10), nudge_x = 200, show.legend = FALSE, col = palette_wong_regions[2]) +
  geom_label_repel(data = rtt_total_quantiles_summary_latest, aes(label = Percentile_25, y = Percentile_25), nudge_x = 200, show.legend = FALSE, col = palette_wong_regions[5]) +
  geom_label_repel(data = rtt_total_quantiles_summary_latest, aes(label = Percentile_75, y = Percentile_75), nudge_x = 200, show.legend = FALSE, col = palette_wong_regions[5]) +
  geom_label_repel(data = rtt_total_quantiles_summary_latest, aes(label = Percentile_90, y = Percentile_90), nudge_x = 200, show.legend = FALSE, col = palette_wong_regions[2]) +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  scale_y_continuous(expand = c(0,0)) + 
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
  selected_theme(hex_col = palette_tu[1])

rtt_total_weeks_chart


# Waiting List Shape ------------------------------------------------------

rtt_waiting_list_shape_chart <- ggplot(rtt_wls, aes(x = weeks_int, y = Incomplete_Pathways, fill = factor(Effective_Snapshot_Date))) +
  geom_area(stat = "identity", position = "identity", col = "white") +
  facet_wrap(~(format(Effective_Snapshot_Date, "%B %Y"))) +
  scale_fill_manual(values = c(palette_wong_regions[1], palette_wong_regions[5]), name = "Month Ending") +
  scale_y_continuous(label = comma, expand = c(0,0)) +
  geom_vline(xintercept = 18, linetype = "dashed") +
  annotate(geom = "text",
           x = 20,
           y = 0.8 * max(rtt_wls$Incomplete_Pathways),
           label = "18 Weeks",
           hjust = -0.1) +
  labs(x = "Weeks Waiting",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Shape of RTT Incomplete Waiting List at Month End - Pre-Pandemic and Latest Month",
       subtitle = "All England") +
  selected_theme(hex_col = palette_tu[1])

rtt_waiting_list_shape_chart

rtt_waiting_list_shape_prop_chart <- ggplot(rtt_wls, aes(x = weeks_int, y = Incomplete_Pathways_Prop, fill = factor(Effective_Snapshot_Date))) +
  geom_area(stat = "identity", position = "identity", col = "white") +
  facet_wrap(~(format(Effective_Snapshot_Date, "%B %Y"))) +
  scale_fill_manual(values = c(palette_wong_regions[1], palette_wong_regions[5]), name = "Month Ending") +
  scale_y_continuous(label = percent, expand = c(0,0)) +
  geom_vline(xintercept = 18, linetype = "dashed") +
  annotate(geom = "text",
           x = 20,
           y = 0.8 * max(rtt_wls$Incomplete_Pathways_Prop),
           label = "18 Weeks",
           hjust = -0.1) +
  labs(x = "Weeks Waiting",
       y = "Percentage of Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Shape of RTT Incomplete Waiting List at Month End - Pre-Pandemic and Latest Month",
       subtitle = "All England") +
  selected_theme(hex_col = palette_tu[1])

rtt_waiting_list_shape_prop_chart

rtt_waiting_list_shape_prop_overlap_chart <- ggplot(rtt_wls, aes(x = weeks_int, y = Incomplete_Pathways_Prop, fill = factor(Effective_Snapshot_Date))) +
  geom_area(stat = "identity", position = "identity", col = "white", alpha = 0.5) +
  scale_fill_manual(values = c(palette_wong_regions[1], palette_wong_regions[5]), name = "Month Ending") +
  scale_y_continuous(label = percent, expand = c(0,0)) +
  geom_vline(xintercept = 18, linetype = "dashed") +
  annotate(geom = "text",
           x = 20,
           y = 0.8 * max(rtt_wls$Incomplete_Pathways_Prop),
           label = "18 Weeks",
           hjust = -0.1) +
  labs(x = "Weeks Waiting",
       y = "Percentage of Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Shape of RTT Incomplete Waiting List at Month End - Pre-Pandemic and Latest Month",
       subtitle = "All England") +
  selected_theme(hex_col = palette_tu[1])

rtt_waiting_list_shape_prop_overlap_chart
