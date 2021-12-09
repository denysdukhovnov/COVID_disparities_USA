## This script runs the main analysis estimating the reductions of lifespan during the COVID-19 epidemic in the US


## Create separate datasets for each 3 or 4-month range in 2020 (starting with March 1, 2020)
## by calculating differences between the cumulative deaths by county reported by Johns Hopkins at the 
## beginning and at the end of each successive period

# Range 1: 3/1 - 5/31 (df.list.patched index 94)
# Range 2: 6/1 - 8/31 (df.list.patched index 185)
# Range 3: 9/1 - 12/31 (df.list.patched index = length(df.list.patched))

range1.df <- JH.covid.df.fun(df.list.patched, df.index = 93) %>% 
  left_join(JH.covid.df.fun(df.list.patched, df.index = 1) %>% select(FIPS, JH.deaths, CalendarDate), by = "FIPS") %>% 
  mutate(covid.2020.deaths = JH.deaths.x - JH.deaths.y,
         number.of.days = CalendarDate.x - CalendarDate.y) %>%
  rename(Start.Date = CalendarDate.y,
         End.Date = CalendarDate.x) %>% 
  select(FIPS, County, Start.Date, number.of.days, End.Date, covid.2020.deaths) %>% 
  filter(!is.na(covid.2020.deaths))

range2.df <- JH.covid.df.fun(df.list.patched, df.index = 185) %>% 
  left_join(JH.covid.df.fun(df.list.patched, df.index = 93) %>% select(FIPS, JH.deaths, CalendarDate), by = "FIPS") %>% 
  mutate(covid.2020.deaths = JH.deaths.x - JH.deaths.y,
         number.of.days = CalendarDate.x - CalendarDate.y) %>%
  rename(Start.Date = CalendarDate.y,
         End.Date = CalendarDate.x) %>% 
  select(FIPS, County, Start.Date, number.of.days, End.Date, covid.2020.deaths) %>% 
  filter(!is.na(covid.2020.deaths))

range3.df <- JH.covid.df.fun(df.list.patched, df.index = length(df.list)) %>% 
  left_join(JH.covid.df.fun(df.list.patched, df.index = 185) %>% select(FIPS, JH.deaths, CalendarDate), by = "FIPS") %>% 
  mutate(covid.2020.deaths = JH.deaths.x - JH.deaths.y,
         number.of.days = CalendarDate.x - CalendarDate.y) %>%
  rename(Start.Date = CalendarDate.y,
         End.Date = CalendarDate.x) %>% 
  select(FIPS, County, Start.Date, number.of.days, End.Date, covid.2020.deaths) %>% 
  filter(!is.na(covid.2020.deaths))



## Combine the ranges into a single data frame format range labels and assign SES quintile to each county
JH.data <- bind_rows(range1.df, range2.df, range3.df) %>% 
  left_join(deaths.SES %>% select(FIPS, SES.Quintile), by = "FIPS") %>%
  # Ad-hoc adjustment of county codes for administrative changes occurring between the time of the SES data analysis and the JH dataset
  mutate(SES.Quintile = if_else(FIPS == "02063" | FIPS == "02066", factor(3, levels = 1:5), SES.Quintile),
         SES.Quintile = if_else(FIPS == "02158", factor(1, levels = 1:5), SES.Quintile),
         SES.Quintile = if_else(FIPS == "15005", factor(4, levels = 1:5), SES.Quintile),
         SES.Quintile = if_else(FIPS == "46102", factor(1, levels = 1:5), SES.Quintile)) %>% 
  filter(!is.na(SES.Quintile)) %>% 
  mutate(Range = as.factor(if_else(End.Date <= sort(unique(End.Date))[1], 
                                   paste0("Period 1: ", sort(unique(Start.Date))[1], " to ", sort(unique(End.Date))[1]),
                                   if_else(End.Date <= sort(unique(End.Date))[2] & End.Date > sort(unique(End.Date))[1],
                                           paste0("Period 2: ", sort(unique(Start.Date + 1))[2], " to ", sort(unique(End.Date))[2]),
                                           if_else(End.Date <= max(unique(End.Date), na.rm = T) & End.Date > sort(unique(End.Date))[2], 
                                                   paste0("Period 3: ", sort(unique(Start.Date + 1))[3], " to ", max(unique(End.Date), na.rm = T)), 
                                                   "Whole year: 2020-01-01 to 2020-12-31"))))) %>% 
  rename(COVID.Deaths.2020 = covid.2020.deaths) %>% 
  mutate(State.FIPS = substr(FIPS, 1, 2)) %>% 
  left_join(state.dictionary, by = "State.FIPS") %>% 
  select(FIPS, County, State.Name, State.Code, SES.Quintile, Range, COVID.Deaths.2020) %>% 
  arrange(Range, FIPS)

# Check the total, matching all identified county FIPS in JH data, excluding Puerto Rico and the US territories
sum(JH.data$COVID.Deaths.2020)


## Calculate the weekly age-specific mortality rates in order to find implied COVID-19 deaths
weekly.nmx <- CDC_COVID_national_rates_weekly %>% 
   filter(sex == "All Sex") %>% 
   
   mutate(Range = cut(as.numeric(mmwr_week), breaks = c(0, 10, 23, 37, 54), right = FALSE, 
                      labels = c("January-February", "March-May", "June-August", "September-December")),
          age_group = factor(rep(1:12, nrow(.)/12), levels = 1:12, labels = levels(pop.2020$Age.Group))) %>% 
  left_join(pop.2020 %>% 
               group_by(Age.Group) %>% 
               summarise(Population = sum(County.Population)) %>% 
               ungroup(),
             by = c("age_group" = "Age.Group")) %>% 
   mutate(mx.covid = as.numeric(covid_19_deaths)/Population)


## Create a weekly COVID-19 data frame with John Hopkins data so that it can be matched to the CDC weekly 
## age-specific death rates

# Initiate an index for the elements of the list containing the beginning and ending dates of each week from March 1 to December 31, 2020
index.vec <- seq(1, length(df.list.patched) + ((length(df.list.patched) - 1) %% 7), 7)
if(tail(index.vec, 1) > length(df.list.patched)) index.vec <- c(index.vec[1:(length(index.vec)-1)], length(df.list.patched))

# Calculate the weekly deaths from John Hopkins data and store them in the list
ranged.df.list <- list()
for (i in index.vec) {  
  
  index <- match(i, index.vec)
  next.i <- index.vec[match(i, index.vec) + 1] 
  
  if (!is.na(next.i)) {
    ranged.df.list[[index]] <- JH.covid.df.fun(df = df.list.patched, df.index = next.i) %>% 
      left_join(JH.covid.df.fun(df = df.list.patched, df.index = i) %>% select(FIPS, JH.deaths, CalendarDate), by = "FIPS") %>% 
      mutate(covid.2020.deaths = as.numeric(JH.deaths.x - JH.deaths.y),
             number.of.days = CalendarDate.x - CalendarDate.y) %>%
      rename(Start.Date = CalendarDate.y,
             End.Date = CalendarDate.x) %>% 
      select(FIPS, County, Start.Date, number.of.days, End.Date, covid.2020.deaths) %>% 
      filter(!is.na(covid.2020.deaths))
  }
}

# Flatten the list in to a data frame with properly labeled weeks and 3 or 4 month periods of 2020
JH.ranged.df <- bind_rows(ranged.df.list) %>% 
  mutate(Range = cut(as.numeric(End.Date - as.Date("2020-01-01")), 
                     breaks = c(0, 61, 153, 245, 366), right = FALSE, 
                     labels = c("January-February", "March-May", "June-August", "September-December")),
         week = cut(as.numeric(End.Date - as.Date("2020-01-01")),
                    breaks = c(0, seq(5, 362, 7), 366), right = FALSE,
                    labels = 1:53))

# Check the sum matches the cumulative JH total on the last day of 2020
sum(JH.ranged.df$covid.2020.deaths)



## Combine the ranged data from John Hopkins and CDC weekly rates, as well as 2020 county population
ranged.df <- JH.ranged.df %>% 
  select(FIPS, County, covid.2020.deaths, Range, week) %>% 
  left_join(weekly.nmx %>% select(mmwr_week, age_group, mx.covid) %>% 
              rename(Age.Group = age_group) %>%  
              mutate(Age.Group = factor(rep(0:11, nrow(.)/12), levels = 0:11, 
                                        labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))),
            by = c("week" = "mmwr_week")) %>% 
  left_join(JH.data %>%
              select(FIPS, SES.Quintile) %>%
              group_by(FIPS, SES.Quintile) %>% 
              summarise(FIPS = FIPS[1], SES.Quintile = SES.Quintile[1]),
            by = "FIPS") %>% 
  left_join(pop.2020 %>% 
              select(FIPS, Age.Group, County.Population) %>% 
              mutate(Age.Group = factor(rep(0:11, nrow(.)/12), levels = 0:11, 
                                        labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))),
            by = c("FIPS",  "Age.Group")) %>% 
  
  # Calculate the covid deaths that occurred in a county by age in a given week, based on the county population
  # by age and the weekly NATIONAL age-specific COVID-19 mortality rates computed earlier
  mutate(implied.covid.deaths = County.Population * mx.covid) %>% 
  
  arrange(FIPS, week, Age.Group) %>% 
  filter(Age.Group != "All Ages") %>% 
  
  # Compute the scaling coefficient to ensure the total matches the total for the county and week from John Hopkins data
  group_by(FIPS, week) %>% 
  mutate(total.implied.covid.deaths = sum(implied.covid.deaths)) %>% 
  mutate(scaling.coef = covid.2020.deaths/(total.implied.covid.deaths + 0.000000001)) %>%
  fill(scaling.coef) %>% 
  ungroup() %>% 
  
  # Compute the scaled implied deaths from COVID-19
  mutate(scaled.covid.deaths = implied.covid.deaths * scaling.coef) %>%
  replace_na(list(scaled.covid.deaths = 0)) %>%                               #### --->>> NOTE: IF NO ACTUAL DEATHS REPORTED BY JH, SETTING MISSING VALUES TO 0
  
  {. ->> scaled.covid.deaths.by.county.and.week} %>% 
  
  # Finally, aggregating all the weekly implied COVID-19 deaths to the March-May, June-August, and September-December ranges 
  group_by(SES.Quintile, Age.Group, Range) %>% 
  summarise(scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!is.na(SES.Quintile))

# Check the sum of the scaled deaths. The total ought to match the earlier total from JH.data (some small deviation is possible due to rounding and aggregation)
sum(ranged.df$scaled.covid.deaths)


## Compute the age-specific mortality rates for 2015-2019 mean, 3-to-4-month periods that correspond to the periods of 2020
lt.15.19.quintile <- NCHS.county.deaths %>%
       left_join(pop.2017 %>% 
                   filter(Age.Group != "All ages") %>% 
                   group_by(SES.Quintile, Age.Group) %>% 
                   summarise(Population.2017 = sum(County.Population)) %>% 
                   ungroup(),
                 by = c("SES.Quintile", "Age.Group")) %>%
       
       # Mortality rate (per year, based on deaths in monthly ranges), exposure adjusted, assuming constant July 1, 2017 population
       mutate(mx.15.19 = if_else(Range == "September-December", (Deaths.15.19) / (Population.2017/3), (Deaths.15.19) / (Population.2017/4)),
              SES.Quintile = as.factor(SES.Quintile),
              Age.Group = factor(rep(1:11, nrow(.)/11), levels = 0:11, 
                                        labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))) %>%    
       rename(pred.15.19.deaths = Deaths.15.19) %>% 
       
       # Save the complete version of the data frame with rows for January-February period
       {. ->> lt.15.19.quintile.complete} %>% 
       filter(Range != "January-February")


# Join the 2015-2019 life tables by quintile for the whole year
normal.scenario.pred.deaths.overall <- lt.15.19.quintile.complete %>%
  group_by(SES.Quintile, Age.Group) %>% 
  summarise(pred.15.19.deaths = sum(pred.15.19.deaths),
            Population.2017 = Population.2017[1]) %>% 
  ungroup() 


### CALCULATING EXCESS MORTALITY

## Load and process the population for 2017 and 2020, grouping it by age and SES quintiles
pop.2017.for.lt <- pop.2017 %>%
  filter(Age.Group != "All ages") %>% 
  group_by(SES.Quintile, Age.Group) %>% 
  summarise(Population.2017 = sum(County.Population)) %>% 
  ungroup() %>% 
  mutate(SES.Quintile = as.factor(SES.Quintile),
         Age.Group = factor(rep(1:11, nrow(.)/11), levels = 0:11, 
                            labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")))

pop.2020.for.lt <- pop.2020 %>%
  filter(Age.Group != "All ages") %>% 
  group_by(SES.Quintile, Age.Group) %>% 
  summarise(Population.2020 = sum(County.Population)) %>% 
  ungroup() %>% 
  mutate(SES.Quintile = as.factor(SES.Quintile),
         Age.Group = factor(rep(1:11, nrow(.)/11), levels = 0:11, 
                            labels = c("All Ages", "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")))


## Join the estimated COVID-19 deaths and the pre-pandemic scenario (2015-2019) deaths 
## and calculate COVID-19 excess life tables by SES quintile and time period
lt.covid.excess.SES <- ranged.df %>% 
  
  left_join(lt.15.19.quintile.complete %>% select(-mx.15.19), by = c("SES.Quintile", "Age.Group", "Range")) %>% 
  left_join(pop.2020.for.lt, by = c("SES.Quintile", "Age.Group")) %>% 
  mutate(pred.2020.deaths = scaled.covid.deaths + pred.15.19.deaths) %>% 
  
  mutate(Age.Group.Order = rep(rep(1:11, each = 3), 5)) %>% 
  arrange(Range, SES.Quintile, Age.Group.Order) %>% 
  
  # Compute life tables combining projected normal-scenario deaths + COVID-19 deaths by quintile 
  lt.calc.function(df = ., d = .$pred.2020.deaths, p = .$Population.2020, 
                   exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance: Chiang method with adjustment by Lo et al. (2016)
  group_by(Range, SES.Quintile) %>% 
  mutate(chiang.P.var = (qx^2) * (1 - qx)/pred.2020.deaths,
         chiang.col7.quantity = if_else(Age.Group == "85+", 0, lx^2 * ((1 - ax) * n + lead(ex, 1))^2 * chiang.P.var),
         chiang.col8.quantity = rev(cumsum(rev(chiang.col7.quantity))),
         c.var.ex = (chiang.col8.quantity / lx^2) + (( last(lx)/lx )^2 / ( last(mx)^3 * last(Population.2020) ))) %>% # Chiang variance with adjustment
  ungroup() %>% 
  select(-chiang.col7.quantity, -chiang.col8.quantity) %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.2020.deaths, p = .$Population.2020, n.iter = 10000,
                  exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), 
                  qx.conversion = "keyfitz") %>% 
  
  {. ->> sim.covid.lt.by.SES.2020} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  rename(mx.2020 = mx,
         qx.2020 = qx, 
         lx.2020 = lx,
         dx.2020 = dx,
         Lx.2020 = Lx,
         Tx.2020 = Tx,
         ex.2020 = ex,
         chiang.P.var.2020 = chiang.P.var, 
         ex.var.2020 = ex.var,
         ex.se.2020 = ex.se) %>%
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 


## Create a comparison data frame by additionally calculating the predicted 2015-2019 both-sex "normal/background scenario" life table
lt.covid.excess.SES.comp <- lt.covid.excess.SES %>% 
  
  left_join(pop.2017.for.lt, by = c("SES.Quintile", "Age.Group")) %>% 
  
  # Computing life table for the background scenario (excluding COVID-19)
  lt.calc.function(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, 
                   exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance: Chiang method with adjustment by Lo et al. (2016)
  group_by(Range, SES.Quintile) %>% 
  mutate(chiang.P.var = (qx^2) * (1 - qx)/pred.15.19.deaths,
         chiang.col7.quantity = if_else(Age.Group == "85+", 0, lx^2 * ((1 - ax) * n + lead(ex, 1))^2 * chiang.P.var),
         chiang.col8.quantity = rev(cumsum(rev(chiang.col7.quantity))),
         c.var.ex = (chiang.col8.quantity / lx^2) + (( last(lx)/lx )^2 / ( last(mx)^3 * last(Population.2017) ))) %>% # *Chiang variance with adjustment
  ungroup() %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, n.iter = 10000,
                  exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), 
                  qx.conversion = "keyfitz") %>%
  
  {. ->> sim.normal.lt.by.SES.15.19} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  
  rename(qx.15.19 = qx,
         lx.15.19 = lx,
         dx.15.19 = dx,
         Lx.15.19 = Lx,
         Tx.15.19 = Tx,
         ex.15.19 = ex,
         chiang.P.var.15.19 = chiang.P.var, 
         ex.var.15.19 = ex.var,
         ex.se.15.19 = ex.se) %>%
  
  left_join(lt.covid.excess.SES %>% 
              select(-any_of(c("scaled.covid.deaths", "Deaths.15.19", "Population.2017", "pred.15.19.deaths", 
                               "pred.2020.deaths", "deaths", "pop", "Age.Group.Order", "n", "ax"))), 
            by = c("Range", "SES.Quintile", "Age.Group")) %>% 
  
  mutate(ex.diff = ex.15.19 - ex.2020,
         ex.diff.se = sqrt(ex.var.15.19 + ex.var.2020),
         chiang.P.se.15.19 = sqrt(chiang.P.var.15.19),
         chiang.P.se.2020 = sqrt(chiang.P.var.2020),
         ex.diff.CI.upper = ex.diff + 1.96 * ex.diff.se,
         ex.diff.CI.lower = ex.diff - 1.96 * ex.diff.se,
         YLL = scaled.covid.deaths * ex.15.19) %>% 
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 



## Repeating the same calculation, but for the entire year
## Join the estimated COVID-19 deaths and the whole-year pre-epidemic scenario (2015-2019) deaths 
## and calculate COVID-19 excess life tables by SES quintile over the whole year
lt.covid.excess.SES.overall <- ranged.df %>% 
  
  # For overall (i.e. full year) group the scaled COVID deaths by SES and age group first
  group_by(SES.Quintile, Age.Group) %>% 
  summarise(scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T)) %>% 
  ungroup() %>% 
  
  # For overall (i.e. full year) group the predicted normal deaths by SES and age group first
  left_join(normal.scenario.pred.deaths.overall, by = c("SES.Quintile", "Age.Group")) %>% 
  left_join(pop.2020.for.lt, by = c("SES.Quintile", "Age.Group")) %>% 
  mutate(pred.2020.deaths = scaled.covid.deaths + pred.15.19.deaths) %>% 
  
  mutate(Age.Group.Order = rep(rep(1:11), 5)) %>% 
  arrange(SES.Quintile, Age.Group.Order) %>% 
  
  lt.calc.function(df = ., d = .$pred.2020.deaths, p = .$Population.2020, 
                   exposure.length = 1, group.by.vars = c("SES.Quintile"), 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.2020.deaths, p = .$Population.2020, n.iter = 10000,
                  exposure.length = 1, group.by.vars = c("SES.Quintile"), 
                  qx.conversion = "keyfitz") %>% 
  
  {. ->> sim.covid.lt.by.SES.overall.2020} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  rename(mx.2020 = mx,
         qx.2020 = qx, 
         lx.2020 = lx,
         dx.2020 = dx,
         Lx.2020 = Lx,
         Tx.2020 = Tx,
         ex.2020 = ex,
         chiang.P.var.2020 = chiang.P.var, 
         ex.var.2020 = ex.var,
         ex.se.2020 = ex.se) %>%
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 


## Create a comparison data frame by additionally calculating the predicted 2015-2019 both-sex "normal-scenario" life table
lt.covid.excess.SES.overall.comp <- lt.covid.excess.SES.overall %>% 
  
  left_join(pop.2017.for.lt, by = c("SES.Quintile", "Age.Group")) %>% 
  
  lt.calc.function(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, 
                   exposure.length = 1, group.by.vars = c("SES.Quintile"), 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, n.iter = 10000,
                  exposure.length = 1, group.by.vars = c("SES.Quintile"), 
                  qx.conversion = "keyfitz") %>%
  
  {. ->> sim.normal.lt.by.SES.overall.15.19} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  
  rename(qx.15.19 = qx,
         lx.15.19 = lx,
         dx.15.19 = dx,
         Lx.15.19 = Lx,
         Tx.15.19 = Tx,
         ex.15.19 = ex,
         chiang.P.var.15.19 = chiang.P.var, 
         ex.var.15.19 = ex.var,
         ex.se.15.19 = ex.se) %>%
  
  left_join(lt.covid.excess.SES.overall %>% 
              select(-any_of(c("scaled.covid.deaths", "Deaths.15.19", "Population.2017", "pred.15.19.deaths", 
                               "pred.2020.deaths", "deaths", "pop", "Age.Group.Order", "n", "ax"))), 
            by = c("SES.Quintile", "Age.Group")) %>% 
  
  mutate(ex.diff = ex.15.19 - ex.2020,
         ex.diff.se = sqrt(ex.var.15.19 + ex.var.2020),
         chiang.P.se.15.19 = sqrt(chiang.P.var.15.19),
         chiang.P.se.2020 = sqrt(chiang.P.var.2020),
         YLL = scaled.covid.deaths * ex.15.19) %>% 
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 

## Compute 95% confidence bounds for the difference between life expectancy estimates by computing 2.5th and 97.5th percentiles of the difference distribution
## from the simulated runs, by county SES quintile and period of the year
lt.covid.excess.SES.comp$ex.diff.CI.lower <- apply(sim.normal.lt.by.SES.15.19$ex_sim_mat - sim.covid.lt.by.SES.2020$ex_sim_mat, 
                                                   MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.SES.comp$ex.diff.CI.upper <- apply(sim.normal.lt.by.SES.15.19$ex_sim_mat - sim.covid.lt.by.SES.2020$ex_sim_mat, 
                                                   MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]

## Compute 95% confidence bounds for the difference between life expectancy estimates by computing 2.5th and 97.5th quantiles of the difference distribution
## from the simulated runs, by county SES quintile and the full year
lt.covid.excess.SES.overall.comp$ex.diff.CI.lower <- apply(sim.normal.lt.by.SES.overall.15.19$ex_sim_mat - sim.covid.lt.by.SES.overall.2020$ex_sim_mat, 
                                                   MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.SES.overall.comp$ex.diff.CI.upper <- apply(sim.normal.lt.by.SES.overall.15.19$ex_sim_mat - sim.covid.lt.by.SES.overall.2020$ex_sim_mat, 
                                                   MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]


## Compute YLL mean and 95% confidence bounds for the YLL measure for each set of complete life tables
lt.covid.excess.SES.comp$YLL <- apply(sim.covid.lt.by.SES.2020$d_sim_mat * sim.normal.lt.by.SES.15.19$ex_sim_mat,
                                        MARGIN =  1, FUN = mean)
lt.covid.excess.SES.comp$YLL.CI.lower <- apply(sim.covid.lt.by.SES.2020$d_sim_mat * sim.normal.lt.by.SES.15.19$ex_sim_mat,
                                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.SES.comp$YLL.CI.upper <- apply(sim.covid.lt.by.SES.2020$d_sim_mat * sim.normal.lt.by.SES.15.19$ex_sim_mat,
                                                 MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]

lt.covid.excess.SES.overall.comp$YLL <- apply(sim.covid.lt.by.SES.overall.2020$d_sim_mat * sim.normal.lt.by.SES.overall.15.19$ex_sim_mat,
                                                MARGIN =  1, FUN = mean)
lt.covid.excess.SES.overall.comp$YLL.CI.lower <- apply(sim.covid.lt.by.SES.overall.2020$d_sim_mat * sim.normal.lt.by.SES.overall.15.19$ex_sim_mat,
                                                        MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.SES.overall.comp$YLL.CI.upper <- apply(sim.covid.lt.by.SES.overall.2020$d_sim_mat * sim.normal.lt.by.SES.overall.15.19$ex_sim_mat,
                                                        MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]



## Like above, calculate the life tables and Years of Life Lost (YLL) by period of 2020 and for the whole year, regardless of the SES category
lt.covid.excess.age <- ranged.df %>% 
  
  group_by(Range, Age.Group) %>% 
  summarise(scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T)) %>% 
  ungroup() %>% 
  
  left_join(lt.15.19.quintile %>% 
              group_by(Range, Age.Group) %>% 
              summarise(pred.15.19.deaths = sum(pred.15.19.deaths, na.rm = T)) %>% 
              ungroup(),
            by = c("Range", "Age.Group")) %>% 
  left_join(pop.2020.for.lt %>% group_by(Age.Group) %>% summarise(Population.2020 = sum(Population.2020)) %>% ungroup(), by = "Age.Group") %>% 
  mutate(pred.2020.deaths = scaled.covid.deaths + pred.15.19.deaths) %>% 
  
  mutate(Age.Group.Order = rep(rep(1:11), 3)) %>% 
  arrange(Range, Age.Group.Order) %>% 
  
  lt.calc.function(df = ., d = .$pred.2020.deaths, p = .$Population.2020, 
                   exposure.length = 0, group.by.vars = c("Range"), 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.2020.deaths, p = .$Population.2020, n.iter = 10000,
                  exposure.length = 0, group.by.vars = c("Range"), 
                  qx.conversion = "keyfitz") %>% 
  
  {. ->> sim.covid.lt.by.age.2020} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  rename(mx.2020 = mx,
         qx.2020 = qx, 
         lx.2020 = lx,
         dx.2020 = dx,
         Lx.2020 = Lx,
         Tx.2020 = Tx,
         ex.2020 = ex,
         chiang.P.var.2020 = chiang.P.var, 
         ex.var.2020 = ex.var,
         ex.se.2020 = ex.se) %>%
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 


# Create a comparison data set by additionally calculating the predicted 2015-2019 both-sex "normal/background scenario" life table
lt.covid.excess.age.comp <- lt.covid.excess.age %>% 
  
  left_join(pop.2017.for.lt %>% group_by(Age.Group) %>% summarise(Population.2017 = sum(Population.2017)) %>% ungroup(), by = "Age.Group") %>% 
  
  lt.calc.function(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, 
                   exposure.length = 0, group.by.vars = c("Range"), 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, n.iter = 10000,
                  exposure.length = 0, group.by.vars = c("Range"), 
                  qx.conversion = "keyfitz") %>%
  
  {. ->> sim.normal.lt.by.age.15.19} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  
  rename(qx.15.19 = qx,
         lx.15.19 = lx,
         dx.15.19 = dx,
         Lx.15.19 = Lx,
         Tx.15.19 = Tx,
         ex.15.19 = ex,
         chiang.P.var.15.19 = chiang.P.var, 
         ex.var.15.19 = ex.var,
         ex.se.15.19 = ex.se) %>%
  
  left_join(lt.covid.excess.age %>% 
              select(-any_of(c("scaled.covid.deaths", "Deaths.15.19", "Population.2017", "pred.15.19.deaths", 
                               "pred.2020.deaths", "deaths", "pop", "Age.Group.Order", "n", "ax"))), 
            by = c("Range", "Age.Group")) %>% 
  
  mutate(ex.diff = ex.15.19 - ex.2020,
         ex.diff.se = sqrt(ex.var.15.19 + ex.var.2020),
         chiang.P.se.15.19 = sqrt(chiang.P.var.15.19),
         chiang.P.se.2020 = sqrt(chiang.P.var.2020),
         YLL = scaled.covid.deaths * ex.15.19) %>% 
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 



## And finally, calculate the overall life table and Years of Life Lost (YLL) in 2020, irrespective of county SES or time period of 2020
lt.covid.excess.total <- ranged.df %>% 
  
  # For overall (i.e. full year) group the scaled COVID deaths by SES and age group first
  group_by(SES.Quintile, Age.Group) %>% 
  summarise(scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T)) %>% 
  ungroup() %>% 
  
  # For overall (i.e. full year) group the predicted normal deaths by SES and age group first
  left_join(normal.scenario.pred.deaths.overall, by = c("SES.Quintile", "Age.Group")) %>% 
  left_join(pop.2020.for.lt, by = c("SES.Quintile", "Age.Group")) %>% 
  mutate(pred.2020.deaths = scaled.covid.deaths + pred.15.19.deaths) %>% 
  mutate(Age.Group.Order = rep(rep(1:11), 5)) %>% 
  
  # For overall (i.e. full year) group the scaled COVID deaths by SES and age group first
  group_by(Age.Group, Age.Group.Order) %>% 
  summarise(across(c("scaled.covid.deaths", "Population.2020", "pred.15.19.deaths", "pred.2020.deaths"), sum)) %>% 
  ungroup() %>% 
  
  arrange(Age.Group.Order) %>% 
  
  lt.calc.function(df = ., d = .$pred.2020.deaths, p = .$Population.2020, 
                   exposure.length = 1, group.by.vars = NULL, 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.2020.deaths, p = .$Population.2020, n.iter = 10000,
                  exposure.length = 1, group.by.vars = NULL, 
                  qx.conversion = "keyfitz") %>%
  
  {. ->> sim.covid.lt.total.2020} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>%
  
  rename(mx.2020 = mx,
         qx.2020 = qx, 
         lx.2020 = lx,
         dx.2020 = dx,
         Lx.2020 = Lx,
         Tx.2020 = Tx,
         ex.2020 = ex,
         chiang.P.var.2020 = chiang.P.var, 
         ex.var.2020 = ex.var,
         ex.se.2020 = ex.se) %>%
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 


## Create a comparison data set by additionally calculating the predicted 2015-2019 both-sex "normal/background scenario" life table
lt.covid.excess.total.comp <- lt.covid.excess.total %>% 
  
  left_join(pop.2017.for.lt %>% group_by(Age.Group) %>% summarise(Population.2017 = sum(Population.2017)) %>% ungroup(), by = "Age.Group") %>% 
  
  lt.calc.function(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, 
                   exposure.length = 1, group.by.vars = NULL, 
                   qx.conversion = "keyfitz") %>% 
  
  # Variance using Silcocks et al. method for the last age group
  silcocks.ex.var(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, n.iter = 10000,
                  exposure.length = 1, group.by.vars = NULL, 
                  qx.conversion = "keyfitz") %>%
  
  {. ->> sim.normal.lt.total.15.19} %>% 
  getElement("main") %>% 
  
  mutate(ex.var = if_else(Age.Group != "85+", c.var.ex, s.var.ex),
         ex.se = sqrt(ex.var)) %>% 
  
  rename(qx.15.19 = qx,
         lx.15.19 = lx,
         dx.15.19 = dx,
         Lx.15.19 = Lx,
         Tx.15.19 = Tx,
         ex.15.19 = ex,
         chiang.P.var.15.19 = chiang.P.var, 
         ex.var.15.19 = ex.var,
         ex.se.15.19 = ex.se) %>%
  
  left_join(lt.covid.excess.total %>% 
              select(-any_of(c("scaled.covid.deaths", "Deaths.15.19", "Population.2017", "pred.15.19.deaths", 
                               "pred.2020.deaths", "deaths", "pop", "Age.Group.Order", "n", "ax"))), 
            by = "Age.Group") %>% 
  
  mutate(ex.diff = ex.15.19 - ex.2020,
         ex.diff.se = sqrt(ex.var.15.19 + ex.var.2020),
         chiang.P.se.15.19 = sqrt(chiang.P.var.15.19),
         chiang.P.se.2020 = sqrt(chiang.P.var.2020),
         YLL = scaled.covid.deaths * ex.15.19) %>% 
  select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex"))) 


## Compute 95% confidence bounds for the difference between life expectancy estimates by computing 2.5th and 97.5th quantiles of the difference distribution
## from the simulated runs, by age and period of the year
lt.covid.excess.age.comp$ex.diff.CI.lower <- apply(sim.normal.lt.by.age.15.19$ex_sim_mat - sim.covid.lt.by.age.2020$ex_sim_mat, 
                                                    MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.age.comp$ex.diff.CI.upper <- apply(sim.normal.lt.by.age.15.19$ex_sim_mat - sim.covid.lt.by.age.2020$ex_sim_mat, 
                                                    MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]

## Compute 95% confidence bounds for the difference between life expectancy estimates by computing 2.5th and 97.5th quantiles of the difference distribution
## from the simulated runs, by age and for the full year
lt.covid.excess.total.comp$ex.diff.CI.lower <- apply(sim.normal.lt.total.15.19$ex_sim_mat - sim.covid.lt.total.2020$ex_sim_mat, 
                                                      MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.total.comp$ex.diff.CI.upper <- apply(sim.normal.lt.total.15.19$ex_sim_mat - sim.covid.lt.total.2020$ex_sim_mat, 
                                                      MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]


## Compute YLL and 95% confidence bounds for the YLL measure for each set of complete life tables
lt.covid.excess.age.comp$YLL <- apply(sim.covid.lt.by.age.2020$d_sim_mat * sim.normal.lt.by.age.15.19$ex_sim_mat,
                                       MARGIN =  1, FUN = mean)
lt.covid.excess.age.comp$YLL.CI.lower <- apply(sim.covid.lt.by.age.2020$d_sim_mat * sim.normal.lt.by.age.15.19$ex_sim_mat,
                                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.age.comp$YLL.CI.upper <- apply(sim.covid.lt.by.age.2020$d_sim_mat * sim.normal.lt.by.age.15.19$ex_sim_mat,
                                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]

lt.covid.excess.total.comp$YLL <- apply(sim.covid.lt.total.2020$d_sim_mat * sim.normal.lt.total.15.19$ex_sim_mat,
                                         MARGIN =  1, FUN = mean)
lt.covid.excess.total.comp$YLL.CI.lower <- apply(sim.covid.lt.total.2020$d_sim_mat * sim.normal.lt.total.15.19$ex_sim_mat,
                                                       MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
lt.covid.excess.total.comp$YLL.CI.upper <- apply(sim.covid.lt.total.2020$d_sim_mat * sim.normal.lt.total.15.19$ex_sim_mat,
                                                       MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]



## AGE-STANDARDIZED RATES FOR COVID-19 and normal/background (2015-2019) conditions

# Standard exposure
pop.by.age.2017.total <- lt.covid.excess.total.comp$Population.2017

age.standardized.mx.covid <- lt.covid.excess.SES.comp %>% 
  select(-any_of(c("chiang.col7.quantity", "chiang.col8.quantity"))) %>% 
  bind_rows(lt.covid.excess.SES.overall.comp %>% mutate(Range = "Whole year")) %>%
  select(Range, SES.Quintile, Age.Group, Age.Group.Order, scaled.covid.deaths, Population.2017) %>% 
  mutate(Range.Order = if_else(Range == "March-May", 1, 
                               if_else(Range == "June-August", 2,
                                       if_else(Range == "September-December", 3, 4))),
         Range = if_else(Range.Order == 1, "Period 1: 2020-02-29 to 2020-05-31",
                         if_else(Range.Order == 2, "Period 2: 2020-06-01 to 2020-08-31",
                                 if_else(Range.Order == 3, "Period 3: 2020-09-01 to 2020-12-31", "Whole year: 2020-01-01 to 2020-12-31")))) %>% 
  {. ->> mx.age.std.period} %>%
  mutate(mx.covid = scaled.covid.deaths / if_else(Range.Order == 1 | Range.Order == 2, Population.2017/4,
                                                  if_else(Range.Order == 3, Population.2017/3, Population.2017)),
         pop.std = rep(pop.by.age.2017.total, nrow(.)/11),
         expected.covid.deaths = mx.covid * if_else(Range.Order == 1 | Range.Order == 2, pop.std/4,
                                                    if_else(Range.Order == 3, pop.std/3, pop.std))) %>% 
  group_by(Range, Range.Order, SES.Quintile) %>% 
  summarise(expected.covid.dx = sum(expected.covid.deaths)) %>% 
  ungroup() %>% 
  mutate(covid.mx.age.std = expected.covid.dx / if_else(Range.Order == 1 | Range.Order == 2, sum(pop.by.age.2017.total)/4,
                                                        if_else(Range.Order == 3, sum(pop.by.age.2017.total/3), sum(pop.by.age.2017.total))),
         covid.mx.age.std.SE = covid.mx.age.std/sqrt(expected.covid.dx),
         mx.age.std.CI.upper = covid.mx.age.std + 1.96*covid.mx.age.std.SE,
         mx.age.std.CI.lower = covid.mx.age.std - 1.96*covid.mx.age.std.SE) %>% 
  arrange(Range.Order, SES.Quintile) %>%
  # Z-score differences between adjacent quintiles within the same period
  group_by(Range.Order) %>% 
  mutate(Zscore = round((covid.mx.age.std - lead(covid.mx.age.std))/sqrt(covid.mx.age.std.SE^2 + lead(covid.mx.age.std.SE)^2), 2)) %>% 
  ungroup()

# All-cause 2015-2019 age-standardized rates by SES and time period
age.standardized.mx.15.19.period <- lt.covid.excess.SES.comp %>%
  select(Range, SES.Quintile, Age.Group, Age.Group.Order, pred.15.19.deaths, Population.2017) %>% 
  mutate(Range.Order = if_else(Range == "March-May", 1, 
                               if_else(Range == "June-August", 2,
                                       if_else(Range == "September-December", 3, 4)))) %>% 
  {. ->> mx.15.19.period} %>% 
  mutate(mx.15.19 = pred.15.19.deaths / if_else(Range.Order == 1 | Range.Order == 2, Population.2017/4,
                                                if_else(Range.Order == 3, Population.2017/3, Population.2017)),
         pop.std = rep(pop.by.age.2017.total, nrow(.)/11),
         expected.15.19.deaths = mx.15.19 * if_else(Range.Order == 1 | Range.Order == 2, pop.std/4,
                                                    if_else(Range.Order == 3, pop.std/3, pop.std))) %>%
  group_by(Range, Range.Order, SES.Quintile) %>%
  summarise(expected.15.19.dx = sum(expected.15.19.deaths)) %>% 
  ungroup() %>% 
  mutate(mx.15.19.age.std = expected.15.19.dx / if_else(Range.Order == 1 | Range.Order == 2, sum(pop.by.age.2017.total)/4,
                                                        if_else(Range.Order == 3, sum(pop.by.age.2017.total/3), sum(pop.by.age.2017.total))),
         mx.15.19.age.std.SE = mx.15.19.age.std/sqrt(expected.15.19.dx),
         mx.15.19.age.std.CI.upper = mx.15.19.age.std + 1.96*mx.15.19.age.std.SE,
         mx.15.19.age.std.CI.lower = mx.15.19.age.std - 1.96*mx.15.19.age.std.SE) %>% 
  arrange(Range, SES.Quintile) %>% 
  group_by(Range) %>% 
  # Z-score differences between adjacent quintiles within the same period
  mutate(Zscore = round((mx.15.19.age.std - lead(mx.15.19.age.std))/sqrt(mx.15.19.age.std.SE^2 + lead(mx.15.19.age.std.SE)^2), 2)) %>% 
  ungroup()


# All-cause 2015-2019 age-standardized rates by SES, for the full year
age.standardized.mx.15.19 <- lt.covid.excess.SES.overall.comp %>%
  select(SES.Quintile, Age.Group, Age.Group.Order, pred.15.19.deaths, Population.2017) %>% 
  {. ->> mx.15.19.overall} %>% 
  mutate(mx.15.19 = pred.15.19.deaths / Population.2017,
         pop.std = rep(pop.by.age.2017.total, nrow(.)/11),
         expected.15.19.deaths = mx.15.19 * pop.std) %>%
  group_by(SES.Quintile) %>%
  summarise(expected.15.19.dx = sum(expected.15.19.deaths)) %>% 
  ungroup() %>% 
  mutate(mx.15.19.age.std = expected.15.19.dx / sum(pop.by.age.2017.total),
         mx.15.19.age.std.SE = mx.15.19.age.std/sqrt(expected.15.19.dx),
         mx.15.19.age.std.CI.upper = mx.15.19.age.std + 1.96*mx.15.19.age.std.SE,
         mx.15.19.age.std.CI.lower = mx.15.19.age.std - 1.96*mx.15.19.age.std.SE) %>% 
  arrange(SES.Quintile) %>% 
  # Z-score differences between adjacent quintiles within the same period
  mutate(Zscore = round((mx.15.19.age.std - lead(mx.15.19.age.std))/sqrt(mx.15.19.age.std.SE^2 + lead(mx.15.19.age.std.SE)^2), 2))


# Generate the reporting data set with COVID-19 deaths, as reported by John Hopkins and computed age-standardized rates for SES groups
# NOTE: Reported age-standardized rates and pertinent statistics are for SES Quintile groups at a given time 
#       period of 2020 (or corresponding period of 2015-2019 for all-cause mortality), NOT for individual counties!!!
JH.data.w.rates <- JH.data %>% 
  group_by(FIPS, County, State.Name, State.Code, SES.Quintile) %>% 
  summarise(COVID.Deaths.2020 = sum(COVID.Deaths.2020)) %>% 
  ungroup() %>%
  mutate(Range = "Whole year: 2020-01-01 to 2020-12-31") %>% 
  bind_rows(JH.data) %>% 
  arrange(Range, FIPS) %>% 
  mutate(Range = as.character(Range)) %>% 
  select(FIPS, County, State.Name, State.Code, SES.Quintile, Range, COVID.Deaths.2020) %>% 
  left_join(age.standardized.mx.covid %>% mutate(Range = as.character(Range)) %>% 
              select(Range, SES.Quintile, covid.mx.age.std, covid.mx.age.std.SE, mx.age.std.CI.lower, mx.age.std.CI.upper),
            by = c("Range", "SES.Quintile")) %>% 
  
  rename(covid.age.std.rate.SES.group = covid.mx.age.std, covid.age.std.rate.SES.group.SErr = covid.mx.age.std.SE, 
         covid.age.std.rate.CI.lower = mx.age.std.CI.lower, covid.age.std.rate.CI.upper = mx.age.std.CI.upper) %>% 
  
  left_join(
    
    # Join 2019 all-cause age standardized rates (NOTE: labels are for 2020 dates to ensure row-wise match and comparison)
    bind_rows(age.standardized.mx.15.19 %>% mutate(Range = "Whole year: 2020-01-01 to 2020-12-31"), 
              age.standardized.mx.15.19.period %>% select(-expected.15.19.dx, -Zscore)) %>% 
      mutate(Range = case_when(Range == "March-May" ~ "Period 1: 2020-02-29 to 2020-05-31",
                               Range == "June-August" ~ "Period 2: 2020-06-01 to 2020-08-31",
                               Range == "September-December" ~ "Period 3: 2020-09-01 to 2020-12-31",
                               TRUE ~ "Whole year: 2020-01-01 to 2020-12-31")) %>% 
      select(Range, SES.Quintile, mx.15.19.age.std, mx.15.19.age.std.SE, mx.15.19.age.std.CI.lower, mx.15.19.age.std.CI.upper),
    by = c("Range", "SES.Quintile")) %>%
  
  rename(all.cause.age.std.rate.2015.2019.SES.group = mx.15.19.age.std, all.cause.age.std.rate.2015.2019.SES.group.SErr = mx.15.19.age.std.SE, 
         all.cause.age.std.rate.2015.2019.CI.lower = mx.15.19.age.std.CI.lower, all.cause.age.std.rate.2015.2019.CI.upper = mx.15.19.age.std.CI.upper)
  

####### THIS CONCLUDES THE MAIN DATA ANALYSIS STEP                                                  #######
####### OPTIONALLY RUN 3_sensitivity_analysis.R FOR q(x) CONVERSION METHODS (note: takes long time) #######
####### OR SKIP TO 4_plots_and_tables.R TO PRODUCE FIGURES AND TABLES                               #######