
# RTT Total Backlog -------------------------------------------------------

rtt_total_text <- rtt_total_chart_df %>%
  filter(Effective_Snapshot_Date == min(Effective_Snapshot_Date)| Effective_Snapshot_Date == "2020-02-29" | Effective_Snapshot_Date == max(Effective_Snapshot_Date)) %>%
  mutate(Incomplete_Pathways_Char = format(Incomplete_Pathways, big.mark = ",")) %>%
  mutate(Incomplete_Pathways_Mil = round(Incomplete_Pathways/1000000, 1))
