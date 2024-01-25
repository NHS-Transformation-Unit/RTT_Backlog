
# Long Waiters - National -------------------------------------------------

rtt_total_long_waits_chart <- ggplot(rtt_total_long_waiters, aes(x = as.Date(Effective_Snapshot_Date), y = Count, fill = Group)) +
  geom_area(stat = "identity") +
  facet_wrap(~Group, scale = "free") +
  scale_fill_manual(values = c("#afc9e9", "#6093d2", "#2d609f", "#163050"), name = "Long Wait Group") +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  scale_y_continuous(label = comma) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  annotate(geom = "label",
           x = as.Date("2020-01-01"),
           y = 0,
           label = "Start of pandemic",
           hjust = 1,
           vjust = -1) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways over 52 weeks",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_long_waits_chart

rtt_total_long_waiters_post <- rtt_total_long_waiters %>%
  filter(Effective_Snapshot_Date > '2021-05-31')

rtt_total_long_waiters_post_recent <- rtt_total_long_waiters_post %>%
  filter(Effective_Snapshot_Date == max(Effective_Snapshot_Date))

rtt_total_long_waits_chart_post <- ggplot(rtt_total_long_waiters_post, aes(x = as.Date(Effective_Snapshot_Date), y = Count, fill = Group)) +
  geom_area(stat = "identity", alpha = 0.7) +
  geom_label_repel(data = rtt_total_long_waiters_post_recent, aes(label = Count)) + 
  facet_wrap(~Group, scale = "free") +
  scale_fill_manual(values = c("#afc9e9", "#6093d2", "#2d609f", "#163050"), name = "Long Wait Group") +
  scale_x_date(date_breaks = c("4 months"), date_labels = "%b - %y") +
  scale_y_continuous(label = comma) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways over 52 weeks",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_long_waits_chart_post


rtt_total_long_waits_chart_post_line <- ggplot(rtt_total_long_waiters_post, aes(x = as.Date(Effective_Snapshot_Date), y = Count)) +
  geom_line(col = palette_tu[4], linewidth = 1.2) +
  geom_point(data = rtt_total_long_waiters_post_recent, col = palette_tu[4], size = 2) +
  geom_text_repel(data = rtt_total_long_waiters_post_recent, aes(label = comma(Count)), nudge_x = 90, size = 3) + 
  facet_wrap(~Group, scale = "free") +
  scale_x_date(date_breaks = c("4 months"), date_labels = "%b - %y") +
  scale_y_continuous(label = comma) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways over 52 weeks",
       subtitle = "All England - By Waiting Time") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_long_waits_chart_post_line


rtt_total_long_waits_chart_prop <- ggplot(rtt_total_long_waiters, aes(x = as.Date(Effective_Snapshot_Date), y = Prop, col = Group)) +
  geom_line(stat = "identity") +
  facet_wrap(~Group, scale = "free") +
  scale_color_manual(values = c("#afc9e9", "#6093d2", "#2d609f", "#163050"), name = "Long Wait Group") +
  scale_x_date(date_breaks = c("12 months"), date_labels = "%b - %y") +
  scale_y_continuous(label = percent) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  annotate(geom = "label",
           x = as.Date("2020-01-01"),
           y = 0,
           label = "Start of pandemic",
           hjust = 1,
           vjust = -1) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways over 52 weeks",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_long_waits_chart_prop

rtt_total_long_waits_chart_prop_post <- ggplot(rtt_total_long_waiters_post, aes(x = as.Date(Effective_Snapshot_Date), y = Prop, col = Group)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Group, scale = "free") +
  scale_colour_manual(values = c("#afc9e9", "#6093d2", "#2d609f", "#163050"), name = "Long Wait Group") +
  scale_x_date(date_breaks = c("4 months"), date_labels = "%b - %y") +
  scale_y_continuous(label = percent) +
  labs(x = "Month Ending",
       y = "Incomplete Pathways over target weeks (%)",
       caption = "Source: Monthly RTT Published Data",
       title = "Total Incomplete Pathways over 52 weeks",
       subtitle = "All England") +
  selected_theme(hex_col = "#40C1AC")

rtt_total_long_waits_chart_prop_post
