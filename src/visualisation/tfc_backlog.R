
# TFC Total Backlog -------------------------------------------------------

rtt_tfc_total_chart_df <- rtt_tfc %>%
  group_by(Effective_Snapshot_Date, Treatment_Function_Desc) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Treatment_Function_Desc = case_when(Treatment_Function_Desc == "NULL" ~ "Other",
                                             TRUE ~ Treatment_Function_Desc))

rtt_tfc_total_chart <- 
  ggplot(data = rtt_tfc_total_chart_df, aes(x = as.Date(Effective_Snapshot_Date), y = Incomplete_Pathways)) +
  geom_line(col = "#40C1AC", linewidth = 0.8) +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  facet_wrap(~Treatment_Function_Desc, scales = "free", ncol = 4) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways by Treatment Function",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_tfc_total_chart


# TFC Backlog Change ------------------------------------------------------

max_date <- max(rtt_tfc_total_chart_df$Effective_Snapshot_Date)

rtt_tfc_total_comp <- rtt_tfc_total_chart_df %>%
  filter(Effective_Snapshot_Date == "2020-02-29" | Effective_Snapshot_Date == max_date) %>%
  spread(Effective_Snapshot_Date, Incomplete_Pathways) %>%
  rename("Pre-COVID" = 2, "Latest" = 3) %>%
  mutate(Change = (Latest/`Pre-COVID`) - 1) %>%
  arrange(desc(Change))

ggplot(rtt_tfc_total_comp, aes(x = reorder(Treatment_Function_Desc, - Change), y = Change)) +
  geom_bar(stat = "identity", fill = "#40C1AC") +
  geom_text(aes(label = round(Change * 100, 1)), hjust = 1.1, col = "#000000") +
  scale_y_continuous(label = percent) +
  labs(x = "Treatment Function",
       y = "Percentage increase (%)",
       caption = "Source: Monthly RTT Published Data",
       title = "Percentage Increase in Incomplete Pathways since Pre-Pandemic",
       subtitle = "All England") +
  coord_flip() +
  selected_theme(hex_col = "#40C1AC")
