
# TFC Total Backlog -------------------------------------------------------

rtt_tfc_total_chart_df <- rtt_tfc %>%
  group_by(Effective_Snapshot_Date, Treatment_Function_Desc) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Treatment_Function_Desc = case_when(Treatment_Function_Desc == "NULL" ~ "Other",
                                             TRUE ~ Treatment_Function_Desc))

rtt_tfc_total_chart <- 
  ggplot(data = rtt_tfc_total_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways)) +
  geom_line(col = "#40C1AC", linewidth = 0.8) +
  scale_x_date(breaks = seq(as.Date("2011-04-01"), as.Date("2024-04-01"), by = "3 year"), date_labels = "%b - %y", expand = c(0,0)) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  facet_wrap(~Treatment_Function_Desc, scales = "free_y", ncol = 4) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways by Treatment Function",
       subtitle = "All England") +
  theme_tu_white_mf(hex_col = "#40C1AC")

rtt_tfc_total_chart

# TFC Total Backlog for Selected Specialties -------------------------------------------------------

rtt_tfc_total_chart_df_select <- rtt_tfc %>%
  filter(Treatment_Function_Code %in% c("502", "160", "120", "330", "340", "150") | substring(Treatment_Function_Code, 1, 1) == "X") %>%
  group_by(Effective_Snapshot_Date, Treatment_Function_Desc) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Treatment_Function_Desc = case_when(Treatment_Function_Desc == "NULL" ~ "Other",
                                             TRUE ~ Treatment_Function_Desc))

rtt_tfc_total_chart_select <- 
  ggplot(data = rtt_tfc_total_chart_df_select, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways)) +
  geom_line(col = "#40C1AC", linewidth = 0.8) +
  scale_x_date(breaks = seq(as.Date("2011-04-01"), as.Date("2024-04-01"), by = "3 year"), date_labels = "%b - %y", expand = c(0,0)) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  facet_wrap(~Treatment_Function_Desc, scales = "free_y") +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways by Treatment Function",
       subtitle = "All England - Selected Treatment Functions") +
  selected_theme(hex_col = "#40C1AC")

rtt_tfc_total_chart_select

# TFC Backlog Change ------------------------------------------------------

max_date <- max(rtt_tfc_total_chart_df$Effective_Snapshot_Date)

rtt_tfc_total_comp <- rtt_tfc_total_chart_df %>%
  filter(Effective_Snapshot_Date == "2020-02-29" | Effective_Snapshot_Date == max_date) %>%
  spread(Effective_Snapshot_Date, Incomplete_Pathways) %>%
  rename("Pre-COVID" = 2, "Latest" = 3) %>%
  mutate(Change = (Latest/`Pre-COVID`) - 1) %>%
  arrange(desc(Change))

rtt_tfc_total_change_chart <- ggplot(rtt_tfc_total_comp, aes(y = reorder(Treatment_Function_Desc, - Change), x = Change)) +
  geom_bar(stat = "identity", fill = "#40C1AC") +
  geom_text(aes(label = round(Change * 100, 1)), hjust = 1.1, col = "#000000", size = 3) +
  scale_x_continuous(label = percent) +
  labs(y = "Treatment Function",
       x = "Percentage increase (%)",
       caption = "Source: Monthly RTT Published Data",
       title = "Percentage Change in Incomplete Pathways",
       subtitle = "All England - Change since pre-pandemic") +
  selected_theme(hex_col = "#40C1AC")

rtt_tfc_total_change_chart


# TFC Waiting Time Summary ------------------------------------------------

rtt_total_quantiles_tfc_summary_latest <- rtt_total_quantiles_tfc_summary %>%
  filter(Effective_Snapshot_Date == rtt_total_max_date)

rtt_total_weeks_chart_tfc <- ggplot(rtt_total_quantiles_tfc_summary, aes(x = as.Date(Effective_Snapshot_Date))) +
  geom_ribbon(aes(ymin = Percentile_10, ymax = Percentile_90, fill = "10th - 90th Percentile Range"), col = "#ffffff", alpha = 0.5, linewidth = 0.2)+
  geom_ribbon(aes(ymin = Percentile_25, ymax = Percentile_75, fill = "Interquartile Range"), col = "#ffffff", alpha = 0.5, linewidth = 0.2)+
  geom_line(aes(y = Percentile_50, col = "Median Waiting Time"), linewidth = 0.7) + 
  scale_color_manual("", values = "black") +
  scale_fill_manual("", values = c(palette_wong_regions[2], palette_wong_regions[5])) +
  scale_x_date(breaks = seq(as.Date("2011-04-01"), as.Date("2024-04-01"), by = "2 year"), date_labels = "%b - %y", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  geom_hline(yintercept = 18, linetype = "dashed") +
  labs(x = "Month Ending",
       y = "Weeks Waiting",
       caption = "Source: Monthly RTT Published Data",
       title = "Summary of Weeks Waiting at Month End",
       subtitle = "NHS England Region") +
  facet_wrap(~Treatment_Function_Desc, scales = "free_x") +
  theme_tu_white_mf(hex_col = palette_tu[1])

rtt_total_weeks_chart_tfc

rtt_total_quantiles_tfc_summary_selected <- rtt_total_quantiles_tfc_summary %>%
  filter(Treatment_Function_Desc %in% c("120: Ear Nose and Throat Service",
                                        "140: Oral Surgery Service",
                                        "150: Neurosurgical Service",
                                        "502: Gynaecology Service",
                                        "100: General Surgery Service",
                                        "110: Trauma and Orthopaedic Service"))

rtt_total_quantiles_tfc_summary_latest_selected <- rtt_total_quantiles_tfc_summary_selected %>%
  filter(Effective_Snapshot_Date == rtt_total_max_date)

rtt_total_weeks_chart_tfc_selected <- ggplot(rtt_total_quantiles_tfc_summary_selected, aes(x = as.Date(Effective_Snapshot_Date))) +
  geom_ribbon(aes(ymin = Percentile_10, ymax = Percentile_90, fill = "10th - 90th Percentile Range"), col = "#ffffff", alpha = 0.5, linewidth = 0.2)+
  geom_ribbon(aes(ymin = Percentile_25, ymax = Percentile_75, fill = "Interquartile Range"), col = "#ffffff", alpha = 0.5, linewidth = 0.2)+
  geom_line(aes(y = Percentile_50, col = "Median Waiting Time"), linewidth = 0.7) + 
  scale_color_manual("", values = "black") +
  scale_fill_manual("", values = c(palette_wong_regions[2], palette_wong_regions[5])) +
  scale_x_date(breaks = seq(as.Date("2011-04-01"), as.Date("2024-04-01"), by = "2 year"), date_labels = "%b - %y", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  geom_hline(yintercept = 18, linetype = "dashed") +
  labs(x = "Month Ending",
       y = "Weeks Waiting",
       caption = "Source: Monthly RTT Published Data",
       title = "Summary of Weeks Waiting at Month End",
       subtitle = "All England - Selected Specialties") +
  facet_wrap(~Treatment_Function_Desc, scales = "free_x") +
  theme_tu_white_mf(hex_col = palette_tu[1])

rtt_total_weeks_chart_tfc_selected
