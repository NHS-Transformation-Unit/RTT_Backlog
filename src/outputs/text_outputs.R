
# Recent Date -------------------------------------------------------------

recent_date <- format(max(rtt_total_chart_df$Effective_Snapshot_Date), "%B %Y")
text_min_date <- min(rtt_total$Effective_Snapshot_Date)
text_max_date <- max(rtt_total$Effective_Snapshot_Date)

# RTT Total Backlog -------------------------------------------------------

rtt_total_text <- rtt_total_chart_df %>%
  filter(Effective_Snapshot_Date == min(Effective_Snapshot_Date)| Effective_Snapshot_Date == "2020-02-29" | Effective_Snapshot_Date == max(Effective_Snapshot_Date)) %>%
  mutate(Incomplete_Pathways_Char = format(Incomplete_Pathways, big.mark = ",")) %>%
  mutate(Incomplete_Pathways_Mil = round(Incomplete_Pathways/1000000, 1))


# TFC Total Backlog -------------------------------------------------------

rtt_total_text_tfc <- rtt_tfc_total_chart_df %>%
  mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date)) %>%
  filter(Effective_Snapshot_Date == text_min_date| Effective_Snapshot_Date == "2020-02-29" | Effective_Snapshot_Date == text_min_date) %>%
  mutate(Incomplete_Pathways_Char = format(Incomplete_Pathways, big.mark = ",")) %>%
  mutate(Incomplete_Pathways_Mil = round(Incomplete_Pathways/1000000, 1))

tfc_text_perc_1 <- paste0(round(rtt_tfc_total_comp[[1,4]]*100,1), "%")
tfc_text_perc_2 <- paste0(round(rtt_tfc_total_comp[[2,4]]*100,1), "%")
tfc_text_1 <- substring(paste0(rtt_tfc_total_comp[[1,1]]), 6, nchar(paste0(rtt_tfc_total_comp[[1,1]])))
tfc_text_2 <- substring(paste0(rtt_tfc_total_comp[[2,1]]), 6, nchar(paste0(rtt_tfc_total_comp[[2,1]])))


# Long Waits Groups -------------------------------------------------------

rtt_long_waiters_text <- rtt_total_long_waiters_post %>%
  filter(Effective_Snapshot_Date == as.Date("2021-06-30") | Effective_Snapshot_Date == text_max_date) %>%
  mutate(Incomplete_Pathways_Char = format(Count, big.mark = ","))

max_waiters_text <- rtt_total_long_waiters_post %>%
  group_by(Group) %>%
  summarise(max = max(Count)) %>%
  mutate(Incomplete_Pathways_Char = format(max, big.mark = ","))

max_52_waiters_text <- rtt_total_long_waiters_post %>%
  filter(Group == "52+") %>%
  filter(Count == max(Count)) %>%
  mutate(max_date = format(Effective_Snapshot_Date, "%B %Y"))
