
# Transform - annual, national coverage estimates -------------------------

tran_wuenic <- raw_wuenic %>%
  select(
    COUNTRY,
    YEAR,
    COVERAGE_CATEGORY,
    REVISION_YEAR,
    VACCINECODE,
    PERCENTAGE
  ) %>%
  rename(
    iso = COUNTRY,
    year = YEAR,
    cat_cov = COVERAGE_CATEGORY,
    year_revis = REVISION_YEAR,
    code_vaccine = VACCINECODE,
    value = PERCENTAGE
  ) %>%
  filter(cat_cov == "WUENIC",
         year > 2017,
         code_vaccine == "DTPCV1",
         year_revis == 2022)

# df_1 <- left_join(tran_wuenic, tran_entity_details, by = "iso") %>%
#   filter(region_who == "AFR")


# Transform - monthly, national administrative data -----------------------

tran_monthly <- raw_monthly %>%
  select(
    COUNTRY,
    YEAR,
    MONTH,
    COVERAGE_CODE,
    POP_TYPE,
    AGE_GROUP,
    DOSES
  ) %>%
  rename(
    iso = COUNTRY,
    year = YEAR,
    month = MONTH,
    code_cov = COVERAGE_CODE,
    type_pop = POP_TYPE,
    group_age = AGE_GROUP,
    doses = DOSES
  ) %>%
  filter(code_cov == "DTPCV1" | code_cov == "DTPCV3") %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


# Visualize - map, world, risk level ---------------------------------------
map_risk <- left_join(vis_map_risk, raw_map, by = "iso")

map_zd_dtp1_world <- ggplot(map_risk, aes(x = long, y = lat, group = group, fill = risk_level2)) +
  geom_polygon(colour = "black") +
  scale_fill_manual(breaks = c("Active outbreak",
                               "Highest",
                               "High",
                               "Moderate",
                               "Lower"),
                    labels = c("Active outbreak",
                               "Highest",
                               "High",
                               "Moderate",
                               "Lower"),
                    values = c("darkred",
                               "red2",
                               "orange2",
                               "yellow2",
                               "chartreuse3")) +
  theme_void() +
  labs(title = "Assessed risk of diphtheria outbreak across WHO Member States",
       subtitle = "Assessed by estimating under-15 vaccination coverage and geographic proximity to active outbreaks",
       caption = "<b>Data source:</b> WHO-UNICEF Joint Reprting Form; WHO Event Management System<br>
       <b>Figure production:</b> WHO Immunization Analysis & Insights Unit") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3))

map_zd_dtp1_world

ggsave(
  path = path_save,
  filename = paste0(format(Sys.Date(), "%y%m%d"), "map_risk", ".jpg"),
  plot = map_zd_dtp1_world,
  width = 9,
  height = 6.5,
  dpi = 500
)


