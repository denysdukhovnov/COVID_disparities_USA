## This script will run the analysis testing the sensitivity of life expectancy reductions to the lifetable probability of death estimation methods
## Two methods (Wachter and non-iterative Keyfitz) are tested here, as discussed in the paper
##
## NB: In each instance, 10,000 Monte Carlo simulations for confidence intervals may take a fairly long time (about an hour total) to complete.
##     For users interested in the central estimate with a much quicker run, reduce the n.iter argument below to 100. 

# install.packages("ggpubr")
library(ggpubr)

# Run sensitivity analyses
wachter.lt.covid.excess.SES.comp <- sensitivity.disaggr.lt.fun(n.iter = 10000, exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), qx.conversion = "wachter")
wachter.lt.covid.excess.SES.overall.comp <- sensitivity.overall.lt.fun(n.iter = 10000, exposure.length = 1, group.by.vars = c("SES.Quintile"), qx.conversion = "wachter")

keyfitz.lt.covid.excess.SES.comp <- sensitivity.disaggr.lt.fun(n.iter = 10000, exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), qx.conversion = "keyfitz")
keyfitz.lt.covid.excess.SES.overall.comp <- sensitivity.overall.lt.fun(n.iter = 10000, exposure.length = 1, group.by.vars = c("SES.Quintile"), qx.conversion = "keyfitz")


###
### PLOTS
###

wachter.ex.diff.plot <- interval.plot.fun(wachter.lt.covid.excess.SES.comp)
wachter.ex.diff.overall.plot <- overall.plot.fun(wachter.lt.covid.excess.SES.overall.comp)

keyfitz.ex.diff.plot <- interval.plot.fun(keyfitz.lt.covid.excess.SES.comp)
keyfitz.ex.diff.overall.plot <- overall.plot.fun(keyfitz.lt.covid.excess.SES.overall.comp)

# Combine, show, and save the plots
wachter.fig.out <- ggpubr::ggarrange(wachter.ex.diff.plot + theme(legend.position = "none", axis.title.y = element_text(colour = "transparent"), panel.spacing.x = unit(2, "lines")),
                                     ggarrange(wachter.ex.diff.overall.plot + theme(legend.box.margin = margin(0,0,0,50), axis.title.y = element_text(size = 16, hjust = -2)), 
                                               ggplot() + theme_void(), widths = c(4,1), ncol = 2), nrow = 2)
wachter.fig.out
ggsave(plot = wachter.fig.out, filename = "Plots/SI_sensitivity_analysis_wachter_complete_plot_ex_difference_covid19.png", width = 7, height = 6, units = "in", scale = 1.5)


keyfitz.fig.out <- ggpubr::ggarrange(keyfitz.ex.diff.plot + theme(legend.position = "none", axis.title.y = element_text(colour = "transparent"), panel.spacing.x = unit(2, "lines")),
                                     ggarrange(keyfitz.ex.diff.overall.plot + theme(legend.box.margin = margin(0,0,0,50), axis.title.y = element_text(size = 16, hjust = -2)), 
                                               ggplot() + theme_void(), widths = c(4,1), ncol = 2), nrow = 2)
keyfitz.fig.out
ggsave(plot = keyfitz.fig.out, filename = "Plots/SI_sensitivity_analysis_keyfitz_complete_plot_ex_difference_covid19.png", width = 7, height = 6, units = "in", scale = 1.5)



###
### TABLES AND STATISTICS
###

# Assemble the results into a table
ex.diff.compare.df <- wachter.lt.covid.excess.SES.comp %>%
  mutate(Range = as.character(Range)) %>% 
  bind_rows(wachter.lt.covid.excess.SES.overall.comp %>% 
              mutate(Range = "Whole year")) %>% 
  select(SES.Quintile, Range, Age.Group, ex.15.19, ex.2020, ex.diff, ex.diff.se, YLL) %>% 
  rename(wachter.ex.15.19 = ex.15.19,
         wachter.ex.2020 = ex.2020,
         wachter.ex.diff = ex.diff,
         wachter.ex.diff.se = ex.diff.se,
         wachter.YLL = YLL) %>% 
  left_join(keyfitz.lt.covid.excess.SES.comp %>%
              mutate(Range = as.character(Range)) %>% 
              bind_rows(keyfitz.lt.covid.excess.SES.overall.comp %>% 
                          mutate(Range = "Whole year")) %>% 
              select(SES.Quintile, Range, Age.Group, ex.15.19, ex.2020, ex.diff, ex.diff.se, YLL) %>% 
              rename(keyfitz.ex.15.19 = ex.15.19,
                     keyfitz.ex.2020 = ex.2020,
                     keyfitz.ex.diff = ex.diff,
                     keyfitz.ex.diff.se = ex.diff.se,
                     keyfitz.YLL = YLL),
            by = c("SES.Quintile", "Range", "Age.Group")) %>% 
  mutate(wachter.keyfitz.diff.in.ex.diff = round(wachter.ex.diff - keyfitz.ex.diff, 2),
         wachter.keyfitz.diff.in.ex.diff.Zscore = round(wachter.keyfitz.diff.in.ex.diff/sqrt(wachter.ex.diff.se^2 + keyfitz.ex.diff.se^2), 2),
         wachter.keyfitz.diff.in.ex.diff.CI.upper = round(wachter.keyfitz.diff.in.ex.diff + 1.96 * sqrt(wachter.ex.diff.se^2 + keyfitz.ex.diff.se^2), 2),
         wachter.keyfitz.diff.in.ex.diff.CI.lower = round(wachter.keyfitz.diff.in.ex.diff - 1.96 * sqrt(wachter.ex.diff.se^2 + keyfitz.ex.diff.se^2), 2),
         wachter.keyfitz.diff.in.ex.diff.95pct.signif = if_else(wachter.keyfitz.diff.in.ex.diff.Zscore > 1.96 | wachter.keyfitz.diff.in.ex.diff.Zscore  < -1.96, 1, 0))


table.out <- ex.diff.compare.df %>% 
  group_by(Range, SES.Quintile) %>% 
  mutate(wachter.YLL = sum(wachter.YLL)/1000000,
         keyfitz.YLL = sum(keyfitz.YLL)/1000000) %>% 
  ungroup() %>% 
  mutate(YLL.diff = wachter.YLL - keyfitz.YLL) %>% 
  filter(Age.Group == 0) %>% 
  select(Range, SES.Quintile, 
         wachter.ex.15.19, keyfitz.ex.15.19, 
         wachter.ex.2020, keyfitz.ex.2020, 
         wachter.ex.diff, keyfitz.ex.diff,
         wachter.ex.diff.se, keyfitz.ex.diff.se,
         wachter.keyfitz.diff.in.ex.diff,
         wachter.keyfitz.diff.in.ex.diff.Zscore, 
         wachter.YLL, keyfitz.YLL, YLL.diff) %>% 
  mutate(across(wachter.ex.15.19:YLL.diff, round, 2))

# Export the table with the results
write.csv(table.out, "Output tables/lt_sensitivity_results.csv", row.names = F, quote = F)


####### THIS CONCLUDES THE SENSITIVITY ANALYSIS STEP, PLEASE RUN 4_plots_and_tables.R FOR TO PRODUCE FIGURES AND TABLES #######
