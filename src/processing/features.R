
# Create Weeks ------------------------------------------------------------

rtt_total_weeks <- rtt_total_weeks %>%
  mutate(weeks_int = case_when(substr(Number_Of_Weeks_Since_Referral,
                                  nchar(Number_Of_Weeks_Since_Referral),
                                  nchar(Number_Of_Weeks_Since_Referral)) == "+" ~ as.integer(substr(Number_Of_Weeks_Since_Referral,
                                                                                                   1,
                                                                                                   nchar(Number_Of_Weeks_Since_Referral) - 1)),
                           TRUE ~ as.integer(substr(Number_Of_Weeks_Since_Referral,
                                                    2,
                                                    str_locate(Number_Of_Weeks_Since_Referral,"-") - 1)
                           ))) %>%
  mutate(weeks_chr = case_when(substr(Number_Of_Weeks_Since_Referral,
                                      nchar(Number_Of_Weeks_Since_Referral),
                                      nchar(Number_Of_Weeks_Since_Referral)) == "+" ~ Number_Of_Weeks_Since_Referral,
                               weeks_int < 10 ~ paste0("00",weeks_int),
                               weeks_int < 100 ~ paste0("0", weeks_int),
                               TRUE ~ as.character(weeks_int)))



# National Quantiles ------------------------------------------------------

rtt_total_month <- rtt_total_weeks %>%
  group_by(Effective_Snapshot_Date) %>%
  summarise(Total_Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Incomplete_Pathways_P10_Point = round(Total_Incomplete_Pathways * 0.10, 0),
         Incomplete_Pathways_P25_Point = round(Total_Incomplete_Pathways * 0.25, 0),
         Incomplete_Pathways_P50_Point = round(Total_Incomplete_Pathways * 0.50, 0),
         Incomplete_Pathways_P75_Point = round(Total_Incomplete_Pathways * 0.75, 0),
         Incomplete_Pathways_P90_Point = round(Total_Incomplete_Pathways * 0.90, 0)
  )

rtt_total_quantiles <- rtt_total_weeks %>%
  group_by(Effective_Snapshot_Date, weeks_int) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Incomplete_Pathways_cumsum = cumsum(Incomplete_Pathways)) %>%
  left_join(rtt_total_month, by = c("Effective_Snapshot_Date")) %>%
  mutate(Incomplete_Pathways_P10 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P10_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P25 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P25_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P50 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P50_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P75 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P75_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P90 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P90_Point ~ 1,
                                             TRUE ~ 0)
  )

rtt_total_quantiles_P10 <- rtt_total_quantiles %>%
  group_by(Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P10 == 1)[1]) %>%
  select(c(1:2)) %>%
  rename(Percentile_10 = 2)

rtt_total_quantiles_P25 <- rtt_total_quantiles %>%
  group_by(Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P25 == 1)[1]) %>%
  select(c(1:2)) %>%
  rename(Percentile_25 = 2)

rtt_total_quantiles_P50 <- rtt_total_quantiles %>%
  group_by(Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P50 == 1)[1]) %>%
  select(c(1:2)) %>%
  rename(Percentile_50 = 2)

rtt_total_quantiles_P75 <- rtt_total_quantiles %>%
  group_by(Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P75 == 1)[1]) %>%
  select(c(1:2)) %>%
  rename(Percentile_75 = 2)

rtt_total_quantiles_P90 <- rtt_total_quantiles %>%
  group_by(Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P90 == 1)[1]) %>%
  select(c(1:2)) %>%
  rename(Percentile_90 = 2)

rtt_total_quantiles_summary <- rtt_total_quantiles_P10 %>%
  left_join(rtt_total_quantiles_P25, by = c("Effective_Snapshot_Date")) %>%
  left_join(rtt_total_quantiles_P50, by = c("Effective_Snapshot_Date")) %>%
  left_join(rtt_total_quantiles_P75, by = c("Effective_Snapshot_Date")) %>%
  left_join(rtt_total_quantiles_P90, by = c("Effective_Snapshot_Date"))


rtt_total_quantiles_summary_long <- rtt_total_quantiles_summary %>%
  gather(Metric, Week, -c(1))



# Regional Quantiles ------------------------------------------------------

rtt_total_month_region <- rtt_total_weeks %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  summarise(Total_Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Incomplete_Pathways_P10_Point = round(Total_Incomplete_Pathways * 0.10, 0),
         Incomplete_Pathways_P25_Point = round(Total_Incomplete_Pathways * 0.25, 0),
         Incomplete_Pathways_P50_Point = round(Total_Incomplete_Pathways * 0.50, 0),
         Incomplete_Pathways_P75_Point = round(Total_Incomplete_Pathways * 0.75, 0),
         Incomplete_Pathways_P90_Point = round(Total_Incomplete_Pathways * 0.90, 0)
  )

rtt_total_quantiles_region <- rtt_total_weeks %>%
  group_by(Region_Name, Effective_Snapshot_Date, weeks_int) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Incomplete_Pathways_cumsum = cumsum(Incomplete_Pathways)) %>%
  left_join(rtt_total_month_region, by = c("Effective_Snapshot_Date", "Region_Name")) %>%
  mutate(Incomplete_Pathways_P10 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P10_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P25 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P25_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P50 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P50_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P75 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P75_Point ~ 1,
                                             TRUE ~ 0),
         Incomplete_Pathways_P90 = case_when(Incomplete_Pathways_cumsum > Incomplete_Pathways_P90_Point ~ 1,
                                             TRUE ~ 0)
  )

rtt_total_quantiles_region_P10 <- rtt_total_quantiles_region %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P10 == 1)[1]) %>%
  select(c(1:3)) %>%
  rename(Percentile_10 = 3)

rtt_total_quantiles_region_P25 <- rtt_total_quantiles_region %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P25 == 1)[1]) %>%
  select(c(1:3)) %>%
  rename(Percentile_25 = 3)

rtt_total_quantiles_region_P50 <- rtt_total_quantiles_region %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P50 == 1)[1]) %>%
  select(c(1:3)) %>%
  rename(Percentile_50 = 3)

rtt_total_quantiles_region_P75 <- rtt_total_quantiles_region %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P75 == 1)[1]) %>%
  select(c(1:3)) %>%
  rename(Percentile_75 = 3)

rtt_total_quantiles_region_P90 <- rtt_total_quantiles_region %>%
  group_by(Region_Name, Effective_Snapshot_Date) %>%
  slice(which(Incomplete_Pathways_P90 == 1)[1]) %>%
  select(c(1:3)) %>%
  rename(Percentile_90 = 3)


rtt_total_quantiles_region_summary <- rtt_total_quantiles_region_P10 %>%
  left_join(rtt_total_quantiles_region_P25, by = c("Effective_Snapshot_Date", "Region_Name")) %>%
  left_join(rtt_total_quantiles_region_P50, by = c("Effective_Snapshot_Date", "Region_Name")) %>%
  left_join(rtt_total_quantiles_region_P75, by = c("Effective_Snapshot_Date", "Region_Name")) %>%
  left_join(rtt_total_quantiles_region_P90, by = c("Effective_Snapshot_Date", "Region_Name"))

rtt_total_quantiles_region_summary_long <- rtt_total_quantiles_region_summary %>%
  gather(Metric, Week, -c(1:2))


# Long Waiters Percentages ------------------------------------------------

rtt_total_long_waiters <- rtt_total_weeks %>%
  mutate(Flag_52 = case_when(weeks_int >= 52 ~ Incomplete_Pathways,
                             TRUE ~ 0),
         Flag_65 = case_when(weeks_int >= 65 ~ Incomplete_Pathways,
                             TRUE ~0),
         Flag_78 = case_when(weeks_int >= 78 ~ Incomplete_Pathways,
                             TRUE ~ 0),
         Flag_104 = case_when(weeks_int >= 104 ~ Incomplete_Pathways,
                              TRUE ~ 0)
  ) %>%
  group_by(Effective_Snapshot_Date) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE),
            Flag_52 = sum(Flag_52, na.rm = TRUE),
            Flag_65 = sum(Flag_65, na.rm = TRUE),
            Flag_78 = sum(Flag_78, na.rm = TRUE),
            Flag_104 = sum(Flag_104, na.rm = TRUE)) %>%
  mutate(Flag_52_Prop = Flag_52/Incomplete_Pathways,
         Flag_65_Prop = Flag_65/Incomplete_Pathways,
         Flag_78_Prop = Flag_78/Incomplete_Pathways,
         Flag_104_Prop = Flag_104/Incomplete_Pathways) %>%
  gather(Metric, Value, -c(1)) %>%
  filter(Metric != "Incomplete_Pathways") %>%
  mutate(Type = case_when(str_detect(Metric, "Prop") ~ "Prop",
                          TRUE ~ "Count")) %>%
  mutate(Group = case_when(str_detect(Metric, "52") ~ "52+",
                           str_detect(Metric, "65") ~ "65+",
                           str_detect(Metric, "78") ~ "78+",
                           str_detect(Metric, "104") ~ "104+")) %>%
  select(-c(Metric)) %>%
  spread(Type, Value) %>%
  mutate(Group = factor(Group, levels = c("52+", "65+", "78+", "104+"))) %>%
  arrange(Effective_Snapshot_Date, Group)

# Waiting List Shape ------------------------------------------------------

rtt_wls <- rtt_total_weeks %>%
  filter(Effective_Snapshot_Date == "2020-02-29" | Effective_Snapshot_Date == max(Effective_Snapshot_Date)) %>%
  group_by(Effective_Snapshot_Date, weeks_int) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Incomplete_Pathways_Prop = Incomplete_Pathways/sum(Incomplete_Pathways))

rtt_wls_animate <- rtt_total_weeks %>%
  filter(Effective_Snapshot_Date >= "2020-02-29") %>%
  group_by(Effective_Snapshot_Date, weeks_int) %>%
  summarise(Incomplete_Pathways = sum(Incomplete_Pathways, na.rm = TRUE)) %>%
  mutate(Incomplete_Pathways_Prop = Incomplete_Pathways/sum(Incomplete_Pathways))
