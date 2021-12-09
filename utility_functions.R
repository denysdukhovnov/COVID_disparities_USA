# This auxiliary script contains all the custom functions necessary for the analysis


# Function to extract John Hopkins deaths by day, by indexing the list where processed data has been stored
JH.covid.df.fun <- function(df = NULL, df.index = length(df.list)) {
  bind_rows(df[df.index]) %>% 
    select(FIPS, State, County, Deaths, CalendarDate) %>%  
    arrange(State, County, Deaths) %>% 
    group_by(State, County) %>% 
    summarise(JH.deaths = sum(Deaths),
              FIPS = FIPS,
              CalendarDate = CalendarDate) %>% 
    ungroup() %>% 
    return()
}



# Function to calculate life tables
lt.calc.function <- function(df, d, p, exposure.length = 1, group.by.vars = NULL, qx.conversion = "wachter") {
  require(tidyverse)
  
  p.name <- gsub("\\.[$]", "", deparse(substitute(p))) # Extracting name from population to restore the original name once LT calculation is completed
  
  if(!is.null(group.by.vars)) {
    df.out <- df %>% 
      ungroup() %>% 
      mutate(deaths = d,
             pop = if (exposure.length >= 1) {p * exposure.length
                   } else {p * if_else(exposure.length < 1 & Range == "September-December", 1/3, 
                                       if_else(exposure.length < 1 & Range %in% c("March-May", "June-August"), 1/4,
                                               as.numeric(NA)))}) %>% 
      
      group_by(across({{group.by.vars}}), Age.Group.Order) %>% 
      summarise(Age.Group = Age.Group,
                scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T),
                pred.2020.deaths = sum(pred.2020.deaths, na.rm = T),
                pred.15.19.deaths = sum(pred.15.19.deaths, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                pop = first(pop)) %>% 
      ungroup() %>%
      
      arrange(across({{group.by.vars}}), Age.Group.Order) %>% 
      
      mutate(mx = deaths/pop,
             n = rep(c(1, 4, 10, 10, 10, 10, 10, 10, 10, 10, 100), nrow(.)/11),
             ax = if_else(Age.Group == "0", 0.07 + 1.7 * mx, 
                          if_else(Age.Group == "1-4", 1.5,     
                                  if_else(Age.Group == "85+", 1/mx, n/2))),
             
             # Using Wachter q(x) conversion by default (Keyfitz and Wachter methods as alternate options)
             qx = if (qx.conversion == "wachter") {if_else(Age.Group == "85+", 1, (n * mx) / (1 + (n - ax) * mx))}
                  else if (qx.conversion == "keyfitz") {
                       if_else(Age.Group == "0", 1 - exp(-n * mx - (n / (48 * pop) * (-lead(pop)) * lead(mx))),
                               ifelse(Age.Group != "0" & Age.Group != "85+", 
                                      1 - exp(-n * mx - (n / (48 * pop) * (lag(pop) - lead(pop)) * (lead(mx) - lag(mx)))), 1))}
             
             
      ) %>%
      
      arrange(across({{group.by.vars}}), Age.Group.Order) %>% 
      
      group_by(across(c({{group.by.vars}}))) %>%
      mutate(lx = ifelse(Age.Group != "0", lag(cumprod(1 - qx)) * 100000, 100000),
             dx = if_else(Age.Group == "85+", lx, lx - lead(lx)),
             Lx = if_else(Age.Group == "85+", ax * dx, lead(lx) * n + ax * dx),
             Tx = rev(cumsum(rev(Lx))),
             ex = Tx/lx) %>% 
      
      # Variance: Chiang method with adjustment by Lo et al. (2016)
      mutate(chiang.P.var = (qx^2) * (1 - qx)/deaths,
             chiang.col7.quantity = if_else(Age.Group == "85+", 0, lx^2 * ((1 - ax) * n + lead(ex, 1))^2 * chiang.P.var),
             chiang.col8.quantity = rev(cumsum(rev(chiang.col7.quantity))),
             # *Chiang variance with adjustment
             c.var.ex = (chiang.col8.quantity / lx^2) + (( last(lx)/lx )^2 / ( last(mx)^3 * last(pop)))) %>% 
      ungroup() %>% 
      select(-chiang.col7.quantity, -chiang.col8.quantity)
    
  } else {
    
    df.out <- df %>% 
      ungroup() %>% 
      mutate(deaths = d,
             pop = if (exposure.length >= 1) {p * exposure.length
                   } else {p * if_else(exposure.length < 1 & Range == "September-December", 1/3, 
                                       if_else(exposure.length < 1 & Range %in% c("March-May", "June-August"), 1/4,
                                               as.numeric(NA)))}) %>% 
      
      group_by(Age.Group.Order) %>% 
      summarise(Age.Group = Age.Group,
                scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T),
                pred.2020.deaths = sum(pred.2020.deaths, na.rm = T),
                pred.15.19.deaths = sum(pred.15.19.deaths, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                pop = first(pop)) %>% 
      ungroup() %>%
      
      arrange(Age.Group.Order) %>% 
      
      mutate(mx = deaths/pop,                                                 
             n = rep(c(1, 4, 10, 10, 10, 10, 10, 10, 10, 10, 100), nrow(.)/11),
             ax = if_else(Age.Group == "0", 0.07 + 1.7 * mx,
                          if_else(Age.Group == "1-4", 1.5, 
                                  if_else(Age.Group == "85+", 1/mx, n/2))),
             
             # Using Wachter method for q(x) conversion by default
             qx = if (qx.conversion == "wachter") {if_else(Age.Group == "85+", 1, (n * mx) / (1 + (n - ax) * mx))}
                  else if (qx.conversion == "keyfitz") {
                       if_else(Age.Group == "0", 1 - exp(-n * mx - (n / (48 * pop) * (-lead(pop)) * lead(mx))),
                               ifelse(Age.Group != "0" & Age.Group != "85+", 
                                      1 - exp(-n * mx - (n / (48 * pop) * (lag(pop) - lead(pop)) * (lead(mx) - lag(mx)))), 1))}
      ) %>%
      
      arrange(Age.Group.Order) %>% 
     
      mutate(lx = ifelse(Age.Group != "0", lag(cumprod(1 - qx)) * 100000, 100000),
             dx = if_else(Age.Group == "85+", lx, lx - lead(lx)),
             Lx = if_else(Age.Group == "85+", ax * dx, lead(lx) * n + ax * dx),
             Tx = rev(cumsum(rev(Lx))),
             ex = Tx/lx) %>% 
      
      # Variance: Chiang method with adjustment by Lo et al. (2016)
      mutate(chiang.P.var = (qx^2) * (1 - qx)/deaths,
             chiang.col7.quantity = if_else(Age.Group == "85+", 0, lx^2 * ((1 - ax) * n + lead(ex, 1))^2 * chiang.P.var),
             chiang.col8.quantity = rev(cumsum(rev(chiang.col7.quantity))),
             # *Chiang variance with adjustment
             c.var.ex = (chiang.col8.quantity / lx^2) + (( last(lx)/lx )^2 / ( last(mx)^3 * last(pop)))) %>% 
      select(-chiang.col7.quantity, -chiang.col8.quantity)
    
  }
  
  # Correct the population column
  if (exposure.length < 1) {
    n.age.grps <- length(unique(df.out$Age.Group))
    n.SES.grps <- ifelse("SES.Quintile" %in% colnames(df.out), length(unique(df.out$SES.Quintile)), 1)
    df.out <- df.out %>%
      mutate(pop = rep(12 * pop[1:(n.SES.grps * n.age.grps)]/3, nrow(.)/(n.SES.grps * n.age.grps)))
  }
  
  colnames(df.out)[colnames(df.out) == "pop"] <- p.name  # Renaming population back to the original name (as in function argument)
  
  return(df.out)
}

# Silcocks et al. variance computation function
silcocks.ex.var <- function(df, d, p, n.iter = 10000, exposure.length = 1, group.by.vars = c("Range", "SES.Quintile"), qx.conversion = "wachter") {
  require(tidyverse)
  n.iter <- n.iter
  d.sim.mat <- matrix(NA, nrow = nrow(df), ncol = n.iter)
  mx.sim.mat <- matrix(NA, nrow = nrow(df), ncol = n.iter)
  ex.sim.mat <- matrix(NA, nrow = nrow(df), ncol = n.iter)
  
  for(i in 1:n.iter) {
    
    if(i %in% seq(0, 10000, 500)) cat("Monte Carlo simulation: ", 100*(i/n.iter), "% comlpete\n", sep = "")
    
    df.out <- df %>%
      ungroup() %>% 
      mutate(sim.deaths = rpois(n = nrow(df), lambda = d)) %>% 
      lt.calc.function(df = ., d = .$sim.deaths, p = p, exposure.length = exposure.length, group.by.vars = all_of(group.by.vars), qx.conversion = qx.conversion)
    
    d.sim.mat[,i] <- df.out$scaled.covid.deaths
    mx.sim.mat[,i] <- df.out$mx
    ex.sim.mat[,i] <- df.out$ex
  }
  
  var.ex <- apply(ex.sim.mat, MARGIN = 1, var)

  df.w.var <- df %>% 
    mutate(s.var.ex = var.ex)
  
  out.list <- list("main" = df.w.var, "d_sim_mat" = d.sim.mat, "mx_sim_mat" = mx.sim.mat, "ex_sim_mat" = ex.sim.mat)
  
  return(out.list)
}


sensitivity.disaggr.lt.fun <- function(n.iter = 10000, exposure.length = 0, group.by.vars = c("Range", "SES.Quintile"), qx.conversion = "wachter") {
  
  df.1 <- ranged.df %>% 
    left_join(lt.15.19.quintile.complete, by = c("SES.Quintile", "Age.Group", "Range")) %>% 
    left_join(pop.2020.for.lt %>% group_by(SES.Quintile, Age.Group) %>% summarise(Population.2020 = sum(Population.2020)) %>% ungroup(), 
              by = c("SES.Quintile", "Age.Group")) %>% 
    mutate(pred.2020.deaths = scaled.covid.deaths + pred.15.19.deaths) %>% 
    mutate(Age.Group.Order = rep(rep(1:11, each = 3), 5)) %>% 
    arrange(Range, SES.Quintile, Age.Group.Order) %>% 
    
    # Compute life tables combining projected normal-scenario deaths + COVID-19 deaths by quintile 
    lt.calc.function(df = ., d = .$pred.2020.deaths, p = .$Population.2020, 
                     exposure.length = exposure.length, group.by.vars = group.by.vars, 
                     qx.conversion = qx.conversion) %>% 
    
    # Variance using Silcocks et al. method for the last age group
    silcocks.ex.var(df = ., d = .$pred.2020.deaths, p = .$Population.2020, n.iter = n.iter,
                    exposure.length = exposure.length, group.by.vars = group.by.vars, 
                    qx.conversion = qx.conversion) %>% 
    
    {. ->> sim.df1} %>% 
    getElement("main") %>% 
    
    mutate(ex.var = s.var.ex,
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
  
  # Computing life table for the background scenario (excluding COVID-19)
  df.2 <- df.1 %>% 
    
    left_join(pop.2017.for.lt %>% group_by(SES.Quintile, Age.Group) %>% summarise(Population.2017 = sum(Population.2017)) %>% ungroup(), 
              by = c("SES.Quintile", "Age.Group")) %>% 
    
    lt.calc.function(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, 
                     exposure.length = exposure.length, group.by.vars = group.by.vars, 
                     qx.conversion = qx.conversion) %>% 
    
    # Variance using Silcocks et al. method for the last age group
    silcocks.ex.var(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, n.iter = n.iter,
                    exposure.length = exposure.length, group.by.vars = group.by.vars, 
                    qx.conversion = qx.conversion) %>%
    
    {. ->> sim.df2} %>% 
    getElement("main") %>% 
    
    mutate(ex.var = s.var.ex, 
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
    
    left_join(df.1 %>% 
                select(-any_of(c("scaled.covid.deaths", "Deaths.15.19", "Population.2017", "pred.15.19.deaths", 
                                 "pred.2020.deaths", "deaths", "pop", "Age.Group.Order", "n", "ax"))), 
              by = c("Range", "SES.Quintile", "Age.Group")) %>% 
    
    mutate(ex.diff = ex.15.19 - ex.2020,
           ex.diff.se = sqrt(ex.var.15.19 + ex.var.2020),
           chiang.P.se.15.19 = sqrt(chiang.P.var.15.19),
           chiang.P.se.2020 = sqrt(chiang.P.var.2020),
           YLL = scaled.covid.deaths * ex.15.19) %>% 
    select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex")))  
  
  
  df.2$ex.diff.CI.lower <- apply(sim.df2$ex_sim_mat - sim.df1$ex_sim_mat, 
                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
  df.2$ex.diff.CI.upper <- apply(sim.df2$ex_sim_mat - sim.df1$ex_sim_mat, 
                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]
  
  df.2$YLL.CI.lower <- apply(sim.df1$d_sim_mat * sim.df2$ex_sim_mat,
                             MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
  df.2$YLL.CI.upper <- apply(sim.df1$d_sim_mat * sim.df2$ex_sim_mat,
                             MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]
  
  # Prepare comparison data for plotting
  df.3 <- df.2 %>% 
    select(SES.Quintile, Range, Age.Group, ex.diff, ex.diff.se, ex.diff.CI.lower, ex.diff.CI.upper, ex.15.19,
           ex.2020, chiang.P.se.15.19, chiang.P.se.2020, YLL, YLL.CI.lower, YLL.CI.upper) %>%
    mutate(SES.Quintile = factor(SES.Quintile, levels = 1:5, labels = paste0("SES quintile ", 1:5)),
           Age.Group = factor(rep(1:11, nrow(.)/11), levels = 1:11, labels = c("0", "1", "5", "15", "25", "35", "45", "55", "65", "75", "85")),
           ex.diff.Zscore = (ex.15.19 - ex.2020)/ex.diff.se,
           ex.diff.signif.95pct = if_else(ex.diff.Zscore > 1.96 | ex.diff.Zscore < -1.96, 1, 0))
  
  return(df.3)
}

sensitivity.overall.lt.fun <- function(n.iter = 10000, exposure.length = 1, group.by.vars = c("SES.Quintile"), qx.conversion = "wachter") {
  
  df.1 <- ranged.df %>% 
    # For overall (i.e. full year) group the scaled COVID deaths by SES and age group first
    group_by(SES.Quintile, Age.Group) %>% 
    summarise(scaled.covid.deaths = sum(scaled.covid.deaths, na.rm = T)) %>% 
    ungroup() %>% 
    
    # For overall (i.e. full year) group the predicted normal deaths by SES and age group first
    left_join(normal.scenario.pred.deaths.overall %>% 
                group_by(SES.Quintile, Age.Group) %>% 
                summarise(pred.15.19.deaths = sum(pred.15.19.deaths, na.rm = T)) %>% 
                ungroup(),
              by = c("SES.Quintile", "Age.Group")) %>% 
    left_join(pop.2020.for.lt %>% group_by(SES.Quintile, Age.Group) %>% summarise(Population.2020 = sum(Population.2020)) %>% ungroup(), 
              by = c("SES.Quintile", "Age.Group")) %>%
    mutate(pred.2020.deaths = scaled.covid.deaths + pred.15.19.deaths) %>% 
    mutate(Age.Group.Order = rep(rep(1:11), 5)) %>% 
    arrange(SES.Quintile, Age.Group.Order) %>% 
    
    lt.calc.function(df = ., d = .$pred.2020.deaths, p = .$Population.2020,
                     exposure.length = exposure.length, group.by.vars = group.by.vars, 
                     qx.conversion = qx.conversion) %>% 
    
    # Variance using Silcocks et al. method for the last age group
    silcocks.ex.var(df = ., d = .$pred.2020.deaths, p = .$Population.2020, n.iter = n.iter,
                    exposure.length = exposure.length, group.by.vars = group.by.vars, 
                    qx.conversion = qx.conversion) %>% 
    
    {. ->> sim.df1} %>% 
    getElement("main") %>% 
    
    # For sensitivity analysis, use Silcocks simulation for Var(ex) in all q(x) conversion methods
    mutate(ex.var = s.var.ex, #  if_else(Age.Group != "85+", c.var.ex, s.var.ex),
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
  
  ## Create a comparison data frame by additionally calculating the predicted 2019 both-sex "normal-scenario" life table
  df.2 <- df.1 %>% 
    
    left_join(pop.2017.for.lt %>% group_by(SES.Quintile, Age.Group) %>% summarise(Population.2017 = sum(Population.2017)) %>% ungroup(), 
              by = c("SES.Quintile", "Age.Group")) %>%
    
    lt.calc.function(df = ., d = .$pred.15.19.deaths, p = .$Population.2017,
                     exposure.length = exposure.length, group.by.vars = group.by.vars, 
                     qx.conversion = qx.conversion) %>% 
    
    # Variance using Silcocks et al. method for the last age group
    silcocks.ex.var(df = ., d = .$pred.15.19.deaths, p = .$Population.2017, n.iter = n.iter,
                    exposure.length = exposure.length, group.by.vars = group.by.vars, 
                    qx.conversion = qx.conversion) %>%
    
    {. ->> sim.df2} %>% 
    getElement("main") %>% 
    
    # For sensitivity analysis, use Silcocks simulation for Var(ex) in all q(x) conversion methods
    mutate(ex.var = s.var.ex, 
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
    
    left_join(df.1 %>% 
                select(-any_of(c("scaled.covid.deaths", "Deaths.15.19", "Population.2017", "pred.15.19.deaths", 
                                 "pred.2020.deaths", "deaths", "pop", "Age.Group.Order", "n", "ax"))), 
              by = c("SES.Quintile", "Age.Group")) %>% 
    
    mutate(ex.diff = ex.15.19 - ex.2020,
           ex.diff.se = sqrt(ex.var.15.19 + ex.var.2020),
           chiang.P.se.15.19 = sqrt(chiang.P.var.15.19),
           chiang.P.se.2020 = sqrt(chiang.P.var.2020),
           YLL = scaled.covid.deaths * ex.15.19) %>% 
    select(-any_of(c("deaths", "pop", "c.var.ex", "s.var.ex")))
  
  
  df.2$ex.diff.CI.lower <- apply(sim.df2$ex_sim_mat - sim.df1$ex_sim_mat, 
                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
  df.2$ex.diff.CI.upper <- apply(sim.df2$ex_sim_mat - sim.df1$ex_sim_mat, 
                                MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]
  
  df.2$YLL <- apply(sim.df1$d_sim_mat * sim.df2$ex_sim_mat, MARGIN = 1, mean)
  df.2$YLL.CI.lower <- apply(sim.df1$d_sim_mat * sim.df2$ex_sim_mat,
                                 MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[1,]
  df.2$YLL.CI.upper <- apply(sim.df1$d_sim_mat * sim.df2$ex_sim_mat,
                                 MARGIN =  1, FUN = function(r) quantile(r, c(0.025, 0.975)))[2,]
  
  # Prepare comparison data for plotting and calculate significance
  df.3 <- df.2 %>% 
    select(any_of(c("SES.Quintile", "Range", "Age.Group", "ex.diff", "ex.diff.se", "ex.diff.CI.lower", "ex.diff.CI.upper",
                    "ex.15.19", "ex.2020", "qx.15.19", "qx.2020", "chiang.P.se.15.19", "chiang.P.se.2020", "YLL", "YLL.CI.lower", "YLL.CI.upper"))) %>%
    mutate(SES.Quintile = factor(SES.Quintile, levels = 1:5, labels = paste0("SES quintile ", 1:5)),
           Age.Group = factor(rep(1:11, nrow(.)/11), levels = 1:11, labels = c("0", "1", "5", "15", "25", "35", "45", "55", "65", "75", "85")),
           ex.diff.Zscore = (ex.15.19 - ex.2020)/ex.diff.se,
           ex.diff.signif.95pct = if_else(ex.diff.Zscore > 1.96 | ex.diff.Zscore < -1.96, 1, 0))
  
  return(df.3)
}


## Plot functions for the sensitivity analysis
interval.plot.fun <- function(df) {
  
  plot.out <- 
    ggplot(df, aes(x = Age.Group, y = ex.diff, group = SES.Quintile, color = SES.Quintile)) +
    geom_ribbon(aes(ymin = ex.diff.CI.lower, ymax = ex.diff.CI.upper), linetype = 0, alpha = 0.1) +
    geom_line(size = 1.2) + 
    geom_hline(yintercept = 0, col = "black", size = 1.25, linetype = 2) +
    facet_wrap(~Range) +
    theme_minimal() +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    scale_x_discrete(expand = c(0.005,0.005)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = seq(0, max(df[, "ex.diff"], na.rm = T) + 0.5, 0.5)) +
    ylab("Difference in years of life expectancy") +  
    xlab("Age") + 
    labs(color = "County SES Quintile") +
    theme(axis.title.y = element_text(size = 16, hjust = 0.5, vjust = 4),
          axis.text.y = element_text(size = 14, hjust = 0.3),
          axis.title.x = element_text(size = 16, hjust = 0.5, vjust = -4),
          axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 16, face = "bold", vjust = 2),
          plot.subtitle = element_text(size = 14, face = "italic"),
          plot.caption = element_text(size = 12, face = "italic", vjust = -10, hjust = 0),
          plot.margin = unit(c(1, 1 , 1.5, 1.2), "cm"),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14),
          strip.text = element_text(size = 16, face = "bold", vjust = 2))
  
  return(plot.out)
}

overall.plot.fun <- function(df) {
  
  plot.out <- 
    ggplot(df, aes(x = Age.Group, y = ex.diff, group = SES.Quintile, color = SES.Quintile)) +
    geom_ribbon(aes(ymin = ex.diff.CI.lower, ymax = ex.diff.CI.upper), linetype = 0, alpha = 0.1) +
    geom_line(size = 1.2) + 
    geom_hline(yintercept = 0, col = "black", size = 1.25, linetype = 2) +
    theme_minimal() +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    scale_x_discrete(expand = c(0.005,0.005)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = seq(0, 5, 0.5), 
                       limits = c(0, 3)) +
    labs(title = "Whole year") +
    ylab("Difference in years of life expectancy") +
    xlab("Age") + 
    labs(color = "County SES Quintile") +
    theme(axis.title.y = element_text(size = 16, hjust = 0.5, vjust = 4),
          axis.text.y = element_text(size = 14, hjust = 0.3),
          axis.title.x = element_text(size = 16, hjust = 0.5, vjust = -4),
          axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 16, face = "bold", vjust = 2, hjust = 0.5),
          plot.margin = unit(c(1, 1 , 1.5, 1.2), "cm"),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14))
  
  return(plot.out)
}
