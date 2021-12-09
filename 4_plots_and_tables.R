# This script produces the figures and tabular output of numbers featured in the paper

## Install (uncomment) and load additional packages for the plots
# install.packages("ggpubr")
# install.packages("ggrepel")
library(tidyverse)
library(sf)
library(ggpubr)
library(ggrepel)


## Save the table with deaths and standardized rates by period of 2015-2019/2020 and SES groups only, for reference
write.csv(JH.data.w.rates, "Output tables/RESULT_JH_covid_deaths_and_standardized_rates_for_SES_groups.csv", row.names = F, quote = F)


## Calculate the share (in %) of COVID-19 deaths in each SES quintile and time period of 2020, using 2015-2019 average by month as a backdrop
## (no county-level all-cause deaths by week or month are publicly available at this time from CDC)
share.covid.by.SES.and.period <- lt.covid.excess.SES.comp %>% 
  group_by(Range, SES.Quintile) %>% 
  summarise(COVID.Deaths = sum(scaled.covid.deaths),
            all.cause.deaths = sum(pred.15.19.deaths + scaled.covid.deaths)) %>% 
  ungroup() %>% 
  mutate(share.COVID = round(100 * COVID.Deaths/all.cause.deaths, 1),
         Range.Order = rep(1:3, each = 5)) %>% 
  {. ->> share.covid.tmp} %>% 
  group_by(SES.Quintile) %>% 
  summarise(Range.Order = 4,
            COVID.Deaths = sum(COVID.Deaths),
            all.cause.deaths = sum(all.cause.deaths)) %>% 
  ungroup() %>%
  mutate(share.COVID = round(100 * COVID.Deaths/all.cause.deaths, 1)) %>% 
  bind_rows(share.covid.tmp) %>% 
  mutate(Range = case_when(Range.Order == 1 ~ "Period 1: 2020-02-29 to 2020-05-31",
                           Range.Order == 2 ~ "Period 2: 2020-06-01 to 2020-08-31",
                           Range.Order == 3 ~ "Period 3: 2020-09-01 to 2020-12-31",
                           Range.Order == 4 ~ "Whole year: 2020-01-01 to 2020-12-31"),
         share.COVID = paste0(share.COVID, "%")) %>% 
  arrange(Range.Order, SES.Quintile) %>% 
  select(Range, Range.Order, SES.Quintile, COVID.Deaths, all.cause.deaths, share.COVID)

rm(share.covid.tmp)


## Figure 1. Age-adjusted mortality plot
mx.quintile.plot <- ggplot(age.standardized.mx.covid %>% left_join(share.covid.by.SES.and.period, by = c("Range", "SES.Quintile")), 
                           aes(x = SES.Quintile, y = covid.mx.age.std * 1000)) + 
  geom_bar(stat = "identity", alpha = 0.7, fill = "grey40") + 
  coord_flip() +
  geom_errorbar(stat = "identity", aes(ymin = mx.age.std.CI.lower * 1000, ymax = mx.age.std.CI.upper * 1000), 
                size = 0.4, width = 0.2, 
                position = position_dodge(width = 0.9)) +
  theme_minimal() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, max(age.standardized.mx.covid$mx.age.std.CI.upper, na.rm = T) * 1000 + 0.25, 0.25)) +
  ylab("COVID-19 mortality rate (per 1000)") +
  xlab("County SES quintile") +
  facet_wrap(~Range, labeller = labeller(Range = label_wrap_gen(15)), ncol = 4) +
  theme(
    axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 4),
    axis.text.y = element_text(size = 12, hjust = 0.3),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -4),
    axis.text.x = element_text(size = 12, vjust = 1.05, hjust = 1, angle = 45),
    plot.margin = unit(c(1, 0.25 , 0.5, 0.5), "cm"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.spacing = unit(2, "lines"))

mx.quintile.plot


mx.quintile.plot.15.19 <- ggplot(age.standardized.mx.15.19, aes(x = factor(SES.Quintile), y = mx.15.19.age.std*1000)) + 
  geom_bar(stat = "identity", alpha = 0.7, fill = "grey40") + 
  coord_flip() +
  theme_minimal() +
  geom_errorbar(stat = "identity", aes(ymin = mx.15.19.age.std.CI.lower*1000, ymax = mx.15.19.age.std.CI.upper*1000), 
                size = 0.4, width = 0.2, 
                position = position_dodge(width = 0.9)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, max(age.standardized.mx.15.19$mx.15.19.age.std.CI.upper*1000, na.rm = T) + 0.25, 1), 
                     limits = c(0,max(age.standardized.mx.15.19$mx.15.19.age.std.CI.upper*1000, na.rm = T) + 0.25)) +
  ylab("All-cause mortality rate (per 1000)") +
  xlab("County SES quintile") +
  labs(title = "US 2015-2019 (average) all-cause mortality rates") +
  theme(
    axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 4),
    axis.text.y = element_text(size = 12, hjust = 0.3),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -4),
    axis.text.x = element_text(size = 12, vjust = 1.05, hjust = 1, angle = 45),
    plot.margin = unit(c(1, 0.5 , 1.5, 0.5), "cm"),
    strip.text = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line())

mx.quintile.plot.15.19


fig.out.1 <- ggarrange(mx.quintile.plot, 
                       ggarrange(ggplot() + theme_void(), mx.quintile.plot.15.19, ggplot() + theme_void(), widths = c(2,4,2), ncol = 3), nrow = 2)
fig.out.1

ggsave(plot = fig.out.1, filename = "Plots/Figure_1_Mx_comparison_by_SES_and_time_period.jpg", 
       width = 300, height = 200, units = "mm", dpi = 600, scale = 1)


## Export age adjusted mortality rates as tables
write.csv(age.standardized.mx.covid, "Output tables/RESULT_age_standardized_mx_2020_covid.csv", quote = F, row.names = F)
write.csv(age.standardized.mx.15.19, "Output tables/RESULT_age_standardized_mx_2015_2019_average_all_causes.csv", quote = F, row.names = F)



## View overall age-adjusted mortality rates by period in 2020 and for the whole year
## as well as the overall 2015-2019 average all-cause rate

# Total age-standardized COVID-19 mortality rate by period of 2020
mx.age.std.period %>% 
  group_by(Range, Range.Order, Age.Group.Order) %>% 
  summarise(scaled.covid.deaths = sum(scaled.covid.deaths),
            Population.2017 = sum(Population.2017)) %>% 
  ungroup() %>%
  mutate(mx.covid = scaled.covid.deaths / if_else(Range.Order == 1 | Range.Order == 2, Population.2017/4,
                                                  if_else(Range.Order == 3, Population.2017/3, Population.2017)),
         pop.std = rep(pop.by.age.2017.total, nrow(.)/11),
         expected.covid.deaths = mx.covid * if_else(Range.Order == 1 | Range.Order == 2, pop.std/4,
                                                    if_else(Range.Order == 3, pop.std/3, pop.std))) %>% 
  group_by(Range, Range.Order) %>% 
  summarise(expected.covid.dx = sum(expected.covid.deaths)) %>% 
  ungroup() %>% 
  mutate(covid.mx.age.std = expected.covid.dx / if_else(Range.Order == 1 | Range.Order == 2, sum(pop.by.age.2017.total)/4,
                                                        if_else(Range.Order == 3, sum(pop.by.age.2017.total/3), sum(pop.by.age.2017.total))),
         covid.mx.age.std.SE = covid.mx.age.std/sqrt(expected.covid.dx),
         mx.age.std.CI.upper = covid.mx.age.std + 1.96*covid.mx.age.std.SE,
         mx.age.std.CI.lower = covid.mx.age.std - 1.96*covid.mx.age.std.SE) %>% 
  arrange(Range.Order) 


# Total age-standardized mortality rate for all-cause 2015-2019 mortality
mx.15.19.overall %>%
  group_by(Age.Group.Order) %>% 
  summarise(pred.15.19.deaths = sum(pred.15.19.deaths),
            Population.2017 = sum(Population.2017)) %>% 
  ungroup() %>%
  mutate(mx.15.19 = pred.15.19.deaths / Population.2017,
         pop.std = rep(pop.by.age.2017.total, nrow(.)/11),
         expected.15.19.deaths = mx.15.19 * pop.std) %>%
  summarise(expected.15.19.dx = sum(expected.15.19.deaths)) %>% 
  mutate(mx.15.19.age.std = expected.15.19.dx / sum(pop.by.age.2017.total),
         mx.15.19.age.std.SE = mx.15.19.age.std/sqrt(expected.15.19.dx),
         mx.15.19.age.std.CI.upper = mx.15.19.age.std + 1.96*mx.15.19.age.std.SE,
         mx.15.19.age.std.CI.lower = mx.15.19.age.std - 1.96*mx.15.19.age.std.SE)


## Organize and export the summary comparison tables, comparing the life expectancies before and during the COVID-19 epidemic
## for each lifetable type generated during the main analysis
out.lt <- lt.covid.excess.SES.comp %>% 
  select(-chiang.col7.quantity, -chiang.col8.quantity, -chiang.P.var.15.19, -chiang.P.var.2020, -chiang.P.se.15.19, -chiang.P.se.2020) %>% 
  rename(mx.15.19 = mx, ax.15.19 = ax) %>% 
  mutate(ax.2020 = if_else(Age.Group == "0", 0.07 + 1.7*mx.2020, 
                           if_else(Age.Group == "85+", 1/mx.2020, ax.15.19))) %>%
  select("Range", "SES.Quintile", "Age.Group.Order", "Age.Group", "scaled.covid.deaths", 
         "pred.2020.deaths", "pred.15.19.deaths", "Population.2017", "n", "mx.15.19", "ax.15.19", 
         "qx.15.19", "lx.15.19", "dx.15.19", "Lx.15.19", "Tx.15.19", "ex.15.19", "ex.var.15.19", "ex.se.15.19", 
         "mx.2020",  "ax.2020", "qx.2020", "lx.2020", "dx.2020", "Lx.2020", "Tx.2020", "ex.2020", "ex.var.2020", "ex.se.2020", 
         "ex.diff", "ex.diff.se", "ex.diff.CI.upper", "ex.diff.CI.lower", "YLL", "YLL.CI.upper", "YLL.CI.lower") %>% 
  bind_rows(lt.covid.excess.SES.overall.comp %>% 
              select(-chiang.P.var.15.19, -chiang.P.var.2020, -chiang.P.se.15.19, -chiang.P.se.2020) %>% 
              rename(mx.15.19 = mx, ax.15.19 = ax) %>% 
              mutate(ax.2020 = if_else(Age.Group == "0", 0.07 + 1.7*mx.2020, 
                                       if_else(Age.Group == "85+", 1/mx.2020, ax.15.19)),
                     Range = "Whole year") %>% 
              select("Range", "SES.Quintile", "Age.Group.Order", "Age.Group", "scaled.covid.deaths", 
                     "pred.2020.deaths", "pred.15.19.deaths", "Population.2017", "n", "mx.15.19", "ax.15.19", 
                     "qx.15.19", "lx.15.19", "dx.15.19", "Lx.15.19", "Tx.15.19", "ex.15.19", "ex.var.15.19", "ex.se.15.19", 
                     "mx.2020",  "ax.2020", "qx.2020", "lx.2020", "dx.2020", "Lx.2020", "Tx.2020", "ex.2020", "ex.var.2020", "ex.se.2020", 
                     "ex.diff", "ex.diff.se", "ex.diff.CI.upper", "ex.diff.CI.lower", "YLL", "YLL.CI.upper", "YLL.CI.lower"))

write.csv(out.lt,
          file = "Output tables/RESULT_Table_1_part_1_covid19_estimated_life_tables_quintiles.csv",
          quote = F,
          row.names = F)


out.lt.total <- lt.covid.excess.age.comp %>% 
  select(-chiang.P.var.15.19, -chiang.P.var.2020, -chiang.P.se.15.19, -chiang.P.se.2020) %>% 
  rename(mx.15.19 = mx, ax.15.19 = ax) %>% 
  mutate(ax.2020 = if_else(Age.Group == "0", 0.07 + 1.7*mx.2020, 
                           if_else(Age.Group == "85+", 1/mx.2020, ax.15.19))) %>%
  select("Range", "Age.Group.Order", "Age.Group", "scaled.covid.deaths", 
         "pred.2020.deaths", "pred.15.19.deaths", "Population.2017", "n", "mx.15.19", "ax.15.19", 
         "qx.15.19", "lx.15.19", "dx.15.19", "Lx.15.19", "Tx.15.19", "ex.15.19", "ex.var.15.19", "ex.se.15.19", 
         "mx.2020",  "ax.2020", "qx.2020", "lx.2020", "dx.2020", "Lx.2020", "Tx.2020", "ex.2020", "ex.var.2020", "ex.se.2020", 
         "ex.diff", "ex.diff.se", "ex.diff.CI.upper", "ex.diff.CI.lower", "YLL", "YLL.CI.upper", "YLL.CI.lower") %>% 
  bind_rows(lt.covid.excess.total.comp %>% 
              select(-chiang.P.var.15.19, -chiang.P.var.2020, -chiang.P.se.15.19, -chiang.P.se.2020) %>% 
              rename(mx.15.19 = mx, ax.15.19 = ax) %>% 
              mutate(ax.2020 = if_else(Age.Group == "0", 0.07 + 1.7*mx.2020, 
                                       if_else(Age.Group == "85+", 1/mx.2020, ax.15.19)),
                     Range = "Whole year") %>% 
              select("Range", "Age.Group.Order", "Age.Group", "scaled.covid.deaths", 
                     "pred.2020.deaths", "pred.15.19.deaths", "Population.2017", "n", "mx.15.19", "ax.15.19", 
                     "qx.15.19", "lx.15.19", "dx.15.19", "Lx.15.19", "Tx.15.19", "ex.15.19", "ex.var.15.19", "ex.se.15.19", 
                     "mx.2020",  "ax.2020", "qx.2020", "lx.2020", "dx.2020", "Lx.2020", "Tx.2020", "ex.2020", "ex.var.2020", "ex.se.2020", 
                     "ex.diff", "ex.diff.se", "ex.diff.CI.upper", "ex.diff.CI.lower", "YLL", "YLL.CI.upper", "YLL.CI.lower"))

write.csv(out.lt.total,
          file = "Output tables/RESULT_Table_1_part_2_covid19_estimated_life_tables_total_based_on_quintiles.csv",
          quote = F,
          row.names = F)



## Extract YLL totals by SES Quintile and period of 2020 from the simulated distribution of deaths and life expectancies
YLL.totals.by.SES <- matrix(NA, 20, 3)
totals.by.SES.index <- list(c(1:11, 56:66, 111:121), c(12:22, 67:77, 122:132), c(23:33, 78:88, 133:143), c(34:44, 89:99, 144:154), c(45:55, 100:110, 155:165))

for (k in seq(1,165,11)) {
      YLL.totals.by.SES[floor(k/11) + 1,] <- 
        t(c(
        round(mean(apply(sim.covid.lt.by.SES.2020$d_sim_mat[k:(k+10),] * sim.normal.lt.by.SES.15.19$ex_sim_mat[k:(k+10),], MARGIN =  2, sum)), 0),
        round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[k:(k+10),] * sim.normal.lt.by.SES.15.19$ex_sim_mat[k:(k+10),], MARGIN =  2, sum), c(0.025, 0.975))[1], 0),
        round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[k:(k+10),] * sim.normal.lt.by.SES.15.19$ex_sim_mat[k:(k+10),], MARGIN =  2, sum), c(0.025, 0.975))[2], 0))
      )
}
   
for (k in 1:5) {
      YLL.totals.by.SES[15 + k,] <- 
        t(c(
        round(mean(apply(sim.covid.lt.by.SES.2020$d_sim_mat[totals.by.SES.index[[k]],] * sim.normal.lt.by.SES.15.19$ex_sim_mat[totals.by.SES.index[[k]],], MARGIN =  2, sum)), 0),
        round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[totals.by.SES.index[[k]],] * sim.normal.lt.by.SES.15.19$ex_sim_mat[totals.by.SES.index[[k]],], MARGIN =  2, sum), c(0.025, 0.975))[1], 0),
        round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[totals.by.SES.index[[k]],] * sim.normal.lt.by.SES.15.19$ex_sim_mat[totals.by.SES.index[[k]],], MARGIN =  2, sum), c(0.025, 0.975))[2], 0))
      )
}

YLL.totals.by.SES <- as_tibble(cbind(rep(c("March-May", "June-August", "September-December", "Whole year"), each = 5), rep(1:5, 4), YLL.totals.by.SES))
colnames(YLL.totals.by.SES) <- c("Range", "SES.Quintile", "YLL.mean", "YLL.CI.lower", "YLL.CI.upper")


## Total number of YLL per period with 95% CI, irrespective of SES
YLL.totals <- as_tibble(
    cbind(c("March-May", "June-August", "September-December", "Whole year"),
        t(matrix(
          c(
              round(mean(apply(sim.covid.lt.by.SES.2020$d_sim_mat[1:55,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[1:55,], MARGIN =  2, sum)), 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[1:55,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[1:55,], MARGIN =  2, sum), c(0.025, 0.975))[1], 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[1:55,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[1:55,], MARGIN =  2, sum), c(0.025, 0.975))[2], 0),
              round(mean(apply(sim.covid.lt.by.SES.2020$d_sim_mat[56:110,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[56:110,], MARGIN =  2, sum)), 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[56:110,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[56:110,], MARGIN =  2, sum), c(0.025, 0.975))[1], 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[56:110,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[56:110,], MARGIN =  2, sum), c(0.025, 0.975))[2], 0),
              round(mean(apply(sim.covid.lt.by.SES.2020$d_sim_mat[111:165,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[111:165,], MARGIN =  2, sum)), 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[111:165,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[111:165,], MARGIN =  2, sum), c(0.025, 0.975))[1], 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[111:165,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[111:165,], MARGIN =  2, sum), c(0.025, 0.975))[2], 0),
              round(mean(apply(sim.covid.lt.by.SES.2020$d_sim_mat[1:165,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[1:165,], MARGIN =  2, sum)), 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[1:165,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[1:165,], MARGIN =  2, sum), c(0.025, 0.975))[1], 0),
              round(quantile(apply(sim.covid.lt.by.SES.2020$d_sim_mat[1:165,] * sim.normal.lt.by.SES.15.19$ex_sim_mat[1:165,], MARGIN =  2, sum), c(0.025, 0.975))[2], 0)),
              nrow = 3, ncol = 4, byrow = F))))

colnames(YLL.totals) <- c("Range", "YLL.mean", "YLL.CI.lower", "YLL.CI.upper")

## Output summary tables of YLL by SES as well as for each period, irrespective of SES
write.csv(YLL.totals.by.SES, "Output tables/RESULT_YLL_sim_totals_by_period_and_SES.csv", quote = F, row.names = F)
write.csv(YLL.totals, "Output tables/RESULT_YLL_sim_totals_by_period.csv", quote = F, row.names = F)



## Figure 2. Decomposition of mortality by region, metropolitan status, and SES Quintile

# Install and load the package necessary to download the geographic shapes and metropolitan designations
# install.packages("tidycensus")
library(tidycensus)

# You will need to register and acquire your own free Census API key and paste it in the quotes below in order to download the shapes
# A unique Census API key can be obtained here: http://api.census.gov/data/key_signup.html
acs.key <- ""

# Download shapefiles for counties and core-based statistical areas
cnty <- get_acs(geography = "county", geometry = T, year = 2019, key = acs.key, keep_geo_vars = TRUE, variables = "B01001_001E")
cbsa <- get_acs(geography = "cbsa", geometry = T, year = 2019, key = acs.key, keep_geo_vars = TRUE, variables = "B01001_001E")

# Define geometric projection for spatial joining
Albers.project.sting <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# Join the two shapefiles and produce a set of metro/non-metro identifiers for counties
cnty.metro <- cnty %>% 
  st_as_sf() %>% 
  st_transform(Albers.project.sting) %>% 
  st_centroid() %>%
  st_join(., st_transform(st_as_sf(cbsa), Albers.project.sting), join = st_within) %>% 
  mutate(metro = case_when(LSAD.y == "M1" ~ "Metropolitan", TRUE ~ "Non-Metropolitan")) %>% 
  st_set_geometry(NULL) %>% 
  select(GEOID.x, NAME.y.y, metro) 


# Create a data set containing weekly deaths by county, with classifications by SES Quintile, metro/non-metro areas, and geographic regions
deaths.quintiles.by.week <- JH.ranged.df %>% 
  left_join(pop.2017 %>% filter(Age.Group == "All ages") %>% select(FIPS, State.FIPS, County.Population, SES.Quintile), by = "FIPS") %>% 
  mutate(SES.Quintile = case_when(
    (FIPS == "02063" | FIPS == "02066") ~ 3,
    FIPS == "02158" ~ 1,
    FIPS == "15005" ~ 4,
    FIPS == "46102" ~ 1,
    TRUE ~ as.numeric(SES.Quintile))) %>% 
  filter(!is.na(SES.Quintile)) %>% 
  
  # Define geographic divisions for counties as per US Census Bureau classification 2019
  # https://www.census.gov/geographies/reference-files/2019/demo/popest/2019-fips.html
  mutate(geo.division = case_when(State.FIPS %in% c("09", "23", "25", "33", "44", "50", "34", "36", "42") ~ 1,
                                  State.FIPS %in% c("17", "18", "26", "39", "55", "19", "20", "27", "29", "31", "38", "46") ~ 2,
                                  State.FIPS %in% c("10", "11", "12", "13", "24", "37", "45", "51", "54", "01", "21", "28", "47", "05", "22", "40", "48") ~ 3,
                                  State.FIPS %in% c("04", "08", "16", "30", "32", "35", "49", "56", "02", "06", "15", "41", "53") ~ 4)) %>% 
  mutate(geo.division = factor(geo.division, levels = 1:4, labels = c("Northeast", "Midwest", "South", "West"))) %>%
  
  # Merge in the metropolitan status identifiers
  left_join(cnty.metro, by = c("FIPS" = "GEOID.x")) %>% 
  
  mutate(metro = if_else(FIPS %in% c("02063", "02066"), "Non-Metropolitan", metro))


# Calculating age-standardized implied death rates by county SES Quintile
age.std.mx.quintiles.by.week <- deaths.quintiles.by.week %>% 
  select(-number.of.days, -County.Population, -Start.Date, -End.Date) %>% 
  right_join(scaled.covid.deaths.by.county.and.week %>% select(FIPS, week, Age.Group, scaled.covid.deaths), by = c("FIPS", "week")) %>% 
  left_join(pop.2017 %>% 
              rename(Population.2017 = County.Population) %>% 
              select(FIPS, Age.Group, Population.2017) %>% 
              mutate(Age.Group = factor(rep(0:11, nrow(.)/12), levels = 0:11, 
                                        labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))),
            by = c("FIPS", "Age.Group")) %>% 
  left_join(pop.2020 %>% 
              rename(Population.2020 = County.Population) %>% 
              select(FIPS, Age.Group, Population.2020) %>% 
              mutate(Age.Group = factor(rep(0:11, nrow(.)/12), levels = 0:11, 
                                        labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))),
            by = c("FIPS", "Age.Group")) %>%
  
  filter(Age.Group != "All Ages", !is.na(State.FIPS)) %>% 
  
  mutate(Range.Order = if_else(Range == "March-May", 1, 
                               if_else(Range == "June-August", 2,
                                       if_else(Range == "September-December", 3, 4))),
         mx.covid = scaled.covid.deaths / (Population.2020/52),
         pop.std = rep(pop.by.age.2017.total, nrow(.)/11),
         expected.covid.deaths = mx.covid * (pop.std/52)) %>% 
  
  group_by(week, SES.Quintile, metro, geo.division) %>% 
  summarise(expected.covid.dx = sum(expected.covid.deaths)) %>%
  ungroup() %>% 
  mutate(covid.mx.age.std = expected.covid.dx / sum(pop.by.age.2017.total)/52,
         covid.mx.age.std.SE = covid.mx.age.std/sqrt(expected.covid.dx),
         mx.age.std.CI.upper = covid.mx.age.std + 1.96*covid.mx.age.std.SE,
         mx.age.std.CI.lower = covid.mx.age.std - 1.96*covid.mx.age.std.SE)


# Plot absolute deaths
fig.2.a <- 
  ggplot(deaths.quintiles.by.week, aes(fill = as.factor(SES.Quintile))) +
  geom_bar(aes(x = week, y = covid.2020.deaths), stat = "identity", width = 1) +
  facet_grid(metro ~ geo.division) +
  theme_minimal() +
  scale_x_discrete(expand = c(0.05,0.05), breaks = seq(10, 53, 10)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_fill_grey(start = 0, end = 0.85) +
  ylab("COVID-19 deaths") +
  xlab("Week of 2020") +
  labs(fill = "County SES \nQuintile") +
  theme(
    axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 4),
    axis.text.y = element_text(size = 12, hjust = 0.3),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -4),
    axis.text.x = element_text(size = 12, vjust = 1.05, hjust = 1, angle = 45),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 14, face = "bold", vjust = 2),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 12, face = "italic", vjust = -6, hjust = 1.45),
    plot.margin = unit(c(1, 0.25 , 1.5, 0.5), "cm"),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey80"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.position = "bottom",
    legend.box.margin = margin(20, 0, 0, 0),
    panel.spacing = unit(1, "lines")) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf)

fig.2.a

# Plot age-standardized rate
fig.2.b <- 
  ggplot(age.std.mx.quintiles.by.week, aes(fill = as.factor(SES.Quintile))) +
  geom_bar(aes(x = week, y = covid.mx.age.std*1000), stat = "identity", width = 1) +
  facet_grid(metro ~ geo.division) +
  theme_minimal() +
  scale_x_discrete(expand = c(0.05,0.05), breaks = seq(10, 53, 10)) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_fill_grey(start = 0, end = 0.85) +
  ylab("COVID-19 mortality rate (per 1000)") +
  xlab("Week of 2020") +
  labs(fill = "County SES \nQuintile") +
  theme(
    axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 4),
    axis.text.y = element_text(size = 12, hjust = 0.3),
    axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -4),
    axis.text.x = element_text(size = 12, vjust = 1.05, hjust = 1, angle = 45),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 14, face = "bold", vjust = 2),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.caption = element_text(size = 14, face = "italic", vjust = -6, hjust = 1.45),
    plot.margin = unit(c(1, 0.25 , 1.5, 0.5), "cm"),
    strip.text = element_text(size = 16, face = "bold"),
    strip.background = element_rect(fill = "grey80"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.position = "bottom",
    legend.box.margin = margin(20, 0, 0, 0),
    #panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf)

fig.2.b

# Combine the two plots into one figure
fig.out.2 <- ggarrange(fig.2.a, NULL, fig.2.b, widths = c(1, 0.02, 1),  ncol = 3, common.legend = T)

fig.out.2

# Save the plot
ggsave(plot = fig.out.2, filename = "Plots/Figure_2_covid19_deaths_and_age_std_rate_by_SES_quintile_week_and_metro_status.jpg", 
       width = 300, height = 200, units = "mm", dpi = 600, scale = 1)





## Figure 3. Plots of YLL by age and SES quintile over each period of 2020
fig.3.a <- ggplot(lt.covid.excess.SES.comp, aes(x = Age.Group, y = YLL/1000000, group = SES.Quintile, fill = SES.Quintile)) +
  geom_bar(stat = "identity", position = position_stack(), alpha = 1, aes(fill = SES.Quintile)) + 
  scale_fill_grey(start = 0, end = 0.85) +
  facet_wrap(~Range) +
  theme_minimal() +
  scale_x_discrete(expand = c(0.005,0.005)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 2.000000, 0.250000), 
                     limits = c(0, 1.500000)) +
  ylab("Years of life lost (Millions)") +  
  xlab("Age group") + 
  labs(color = "County SES Quintile") +
  theme(axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 4),
        axis.text.y = element_text(size = 12, hjust = 0.3),
        axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -4),
        axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 1, angle = 90),
        axis.ticks.x = element_line(size = 0.5),
        plot.title = element_text(size = 14, face = "bold", vjust = 2),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 13, face = "italic", vjust = -10, hjust = 0),
        plot.margin = unit(c(1, 1 , 0.5, 1.2), "cm"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold", vjust = 2))

fig.3.a 

fig.3.b <- 
  ggplot(lt.covid.excess.SES.overall.comp, aes(x = Age.Group, y = YLL/1000000, group = SES.Quintile)) +
  geom_bar(stat = "identity", position = position_stack(), alpha = 1, aes(fill = SES.Quintile)) + 
  theme_minimal() +
  scale_fill_grey(start = 0, end = 0.85) +
  scale_x_discrete(expand = c(0.005,0.005)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 2.000000, 0.250000), 
                     limits = c(0, 1.500000)) +
  labs(title = "Whole year") +
  ylab("Years of life lost (Millions)") +
  xlab("Age group") + 
  labs(fill = "County SES \nQuintile") +
  theme(axis.title.y = element_text(size = 14, hjust = 0.5, vjust = 4),
        axis.text.y = element_text(size = 12, hjust = 0.3),
        axis.title.x = element_text(size = 14, hjust = 0.5, vjust = -4),
        axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 1, angle = 90),
        axis.ticks.x = element_line(size = 0.5),
        plot.title = element_text(size = 14, face = "bold", vjust = 2, hjust = 0.5),
        plot.margin = unit(c(0.5, 1 , 1.5, 1.2), "cm"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))

fig.3.b


# Combine, show, and save the plots
fig.out.3 <- ggpubr::ggarrange(fig.3.a + theme(legend.position = "none", axis.title.y = element_text(size = 16, hjust = 1300), 
                                               axis.title.x = element_text(size = 0.1, color = "transparent"),
                                               panel.spacing.x = unit(2, "lines")),
                               ggarrange(ggplot() + theme_void(), 
                                         fig.3.b + theme(legend.box.margin = margin(0,0,0,50),
                                                         legend.key.size = unit(0.75, "cm"),
                                                         legend.text = element_text(size = 16),
                                                         axis.title.y = element_blank()), 
                                         ggplot() + theme_void(), widths = c(1.4,4,0), 
                                         ncol = 3),
                               nrow = 2)

fig.out.3

ggsave(plot = fig.out.3, filename = "Plots/Figure_3_YLL_covid19_plot.jpg", 
       width = 200, height = 150, units = "mm", dpi = 600, scale = 1.3)


## Figure S1. Bubble map plot
# Bubble map plots 
# NOTE: This will take a minute or two to run

JH.data.for.plt <- JH.data
levels(JH.data.for.plt$Range) <- c(levels(JH.data.for.plt$Range), "Whole year")

counties.JH.geo <- counties(cb = T, year = 2019) %>%
  filter(STATEFP != "02", STATEFP != "15") %>%                # Not plotting Hawaii and Alaska
  select(GEOID, geometry) %>%   
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ") %>% 
  left_join(JH.data.for.plt %>%
              {. ->> tmp} %>% 
              group_by(FIPS, County, State.Name, State.Code, SES.Quintile) %>% 
              summarise(COVID.Deaths.2020 = sum(COVID.Deaths.2020)) %>% 
              ungroup() %>% 
              mutate(Range = factor(4, levels = 1:4, labels = levels(JH.data.for.plt$Range))) %>% 
              bind_rows(tmp),
            by = c("GEOID" = "FIPS")) %>%
  group_by(GEOID) %>%
  st_centroid() %>%
  ungroup() %>%
  filter(!is.na(SES.Quintile)) %>% 
  mutate(COVID.Deaths.2020 = if_else(COVID.Deaths.2020 < 1, as.numeric(NA), as.numeric(COVID.Deaths.2020)),   # Setting 0 deaths to NA for plotting on log scale
         SES.Quintile.Groups = if_else(SES.Quintile %in% 1:2, 2, 3)) %>% 
  mutate(SES.Quintile.Groups = factor(SES.Quintile.Groups, 
                                      levels = 1:3, 
                                      labels = c("All counties", "County SES quintiles 1-2", "County SES quintiles 3-5"))) %>% 
  arrange(Range, COVID.Deaths.2020) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  st_transform() %>% 
  filter(!is.na(Range)) 

# Grouping all counties together for comparison
counties.JH.geo.overall <- counties.JH.geo %>% 
  group_by(Range, GEOID) %>% 
  summarise(COVID.Deaths.2020 = sum(COVID.Deaths.2020, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(COVID.Deaths.2020 = if_else(COVID.Deaths.2020 < 1, as.numeric(NA), as.numeric(COVID.Deaths.2020)),
         SES.Quintile.Groups = 1,
         SES.Quintile.Groups = factor(SES.Quintile.Groups, 
                                      levels = 1:3, 
                                      labels = c("All counties", "County SES quintiles 1-2", "County SES quintiles 3-5")),
         lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  bind_rows(counties.JH.geo) %>% 
  arrange(Range, COVID.Deaths.2020) %>% 
  left_join(pop.2020 %>% filter(Age.Group == "All ages") %>% select(FIPS, County.Population), by = c("GEOID" = "FIPS")) %>% 
  mutate(SES.Quintile.Groups = case_when(
    (GEOID == "02063" | GEOID == "02066") ~ factor(3, levels = 1:3, labels = c("All counties", "County SES quintiles 1-2", "County SES quintiles 3-5")),
    GEOID == "02158" ~ factor(2, levels = 1:3, labels = c("All counties", "County SES quintiles 1-2", "County SES quintiles 3-5")),
    GEOID == "15005" ~ factor(3, levels = 1:3, labels = c("All counties", "County SES quintiles 1-2", "County SES quintiles 3-5")),
    GEOID == "46102" ~ factor(2, levels = 1:3, labels = c("All counties", "County SES quintiles 1-2", "County SES quintiles 3-5")),
    TRUE ~ SES.Quintile.Groups))

# USA shape cropped to the geographic extent of the counties to be plotted
usa <- nation() %>% 
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
usa <- usa[counties.JH.geo,]

# Background for counties in a group
alt.background <- counties(cb = T, year = 2019) %>% 
  left_join(counties.JH.geo %>% st_set_geometry(NULL), by = "GEOID") %>% 
  mutate(shading.col = "grey60") %>% 
  filter(!is.na(Range)) %>% 
  {. ->> tmp} %>% 
  mutate(SES.Quintile.Groups = "All counties",
         shading.col = "grey60") %>%
  bind_rows(tmp) %>% 
  select(GEOID, Range, SES.Quintile.Groups, shading.col, geometry)


# Plot the multipanel map
fig.out.S1 <- ggplot(counties.JH.geo.overall) +
  geom_sf(data = usa, aes(fill = "USA"), color = "transparent", show.legend = "polygon") +
  geom_sf(data = alt.background, aes(fill = "Counties"), color = "transparent", show.legend = "polygon") +
  geom_point(data = counties.JH.geo.overall %>% filter(!is.na(COVID.Deaths.2020)) %>% arrange(Range, COVID.Deaths.2020),
             aes(x = lon, y = lat, 
                 size = log10(COVID.Deaths.2020),
                 color = log10(COVID.Deaths.2020), 
                 alpha = ifelse(COVID.Deaths.2020 < 200, 0.2, 1)), 
             shape = 16) + 
  coord_sf(xlim = c(-2700000, 2500000),
           ylim = c(-1350000, 1600000),
           expand = FALSE) +
  facet_grid(Range ~ SES.Quintile.Groups, switch = "y", labeller = label_wrap_gen(width = 15)) + 
  scale_color_viridis_c(guide = "colorbar", option = "plasma", na.value = "grey80", direction = 1,
                        labels = c(round(c(10^(seq(0,3,1)))), paste0(max(counties.JH.geo.overall$COVID.Deaths.2020, na.rm = T), " = max")),
                        breaks = c(seq(0,3,1), log10(max(counties.JH.geo.overall$COVID.Deaths.2020, na.rm = T)))) +
  scale_alpha(range = c(0.7, 1), guide = "none") +
  scale_size_continuous(guide = "legend", name = "COVID-19\ndeaths\n(log scale)", 
                        breaks = c(seq(0,3,1), log10(max(counties.JH.geo.overall$COVID.Deaths.2020, na.rm = T))), 
                        labels = c(1, 10, 100, 1000, paste0(max(counties.JH.geo.overall$COVID.Deaths.2020, na.rm = T), " = max")),  
                        limits = c(0,log10(max(counties.JH.geo.overall$COVID.Deaths.2020, na.rm = T)))) +
  scale_fill_manual(values = c("USA" = "grey80", "Counties" = "grey60"), 
                    labels = c("USA" = "Counties not\nin SES group", "Counties"  = "Counties in\nSES group")) +
  theme_minimal() +
  guides(size = guide_legend(reverse = T, override.aes = list(fill = NA, size = rev(c(1,2,4,5,8))), order = 2),
         fill = guide_legend(order = 1)) +
  labs(fill = "",
       color = "") +
  theme(legend.title = element_text(size = 18, angle = 0, vjust = -4),
        legend.title.align = 0,
        legend.text = element_text(size = 16),
        legend.key.size = unit(0.75, "cm"),
        legend.key.height = unit(1.5, "cm"), 
        legend.margin = margin(0, 0, 0, 0.5, "cm"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.5, "cm"),
        legend.box.just = "top",
        legend.justification = c(0, 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y = element_text(vjust = 1),
        plot.title = element_text(size = 16, face = "bold", vjust = 2),
        plot.subtitle = element_text(size = 14, face = "italic"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing.y = unit(2, "lines"))


fig.out.S1

# Save the plot
ggsave(plot = fig.out.S1, filename = "Plots/Figure_S1_covid19_county_map_by_SES_quintile_and_time_period_comparison.jpg", 
       width = 300, height = 210, units = "mm", dpi = 300, scale = 1.25)




## OPTIONAL - As presented in the Supplementary material ##
## Prepare the data set for spatial dependence tests on COVID-19 deaths across counties at 3 time periods of 2020
JH.sp.dep <- counties.JH.geo %>% 
  mutate(COVID.Deaths.2020 = replace_na(COVID.Deaths.2020, 0)) %>% 
  filter(Range != "Whole year") %>% 
  select(GEOID, SES.Quintile, Range, COVID.Deaths.2020) %>% 
  st_set_geometry(NULL) %>% 
  pivot_wider(id_cols = c(GEOID, SES.Quintile), names_from = "Range", values_from = "COVID.Deaths.2020") %>% 
  mutate(Period0 = 0) %>% 
  left_join(pop.2020 %>% filter(Age.Group == "All ages") %>% select(FIPS, County.Population), by = c("GEOID" = "FIPS")) %>% 
  left_join(cnty %>% select(GEOID, geometry), by = "GEOID") %>% 
  st_as_sf() 

colnames(JH.sp.dep) <- c("GEOID", "SES.Quintile", "COVID.Deaths.P1", "COVID.Deaths.P2", "COVID.Deaths.P3", "Zero.COVID", "Population", "geometry")

# Calculating simple crude rates to balance unequal population sizes between counties
JH.sp.dep <- JH.sp.dep %>% 
  mutate(CrudeP1 = COVID.Deaths.P1 / (Population/4),
         CrudeP2 = COVID.Deaths.P2 / (Population/4),
         CrudeP3 = COVID.Deaths.P3 / (Population/3))

# Export as ESRI shapefile set
st_write(JH.sp.dep, dsn = "JH_by_period.shp", driver = "ESRI Shapefile")

## The remainder of the optional chunk has been conducted in GeoDa software with queen-1 contiguity weights
## and bi-variate local Moran/LISA plots (computational routines for inversion of covariance matrices are much more resource efficient in GeoDa)

####### THIS CONCLUDES THE ANALYSIS #######
