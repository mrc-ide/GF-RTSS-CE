library(tidyverse)
library(ggplot2)
dalyoutput_cost <- readRDS('./03_output/dalyoutput_cost.rds')

# Groups are defined by:
# pfpr, seasonality, ITNuse, resistance, SMC (depends on seasonality), RTSS cost per dose and delivery cost

# scenarios are: ITNboost, ITN=pbo, RTSS (RTSScov)
# baseline is:
# ITNboost=0
# RTSS = none
# RTSS_cov=0 (this is the same as RTSS=none)
# ITN=pyr
# Also group by:
# 3 options for dose cost and delivery cost

# scenarios are:
# ITNboost=1
# RTSS = EPI or SV
# RTSScov = 0.85
# ITN=pbo

# SMC = always 0 in perennial, always 0.85 in highly seasonal
# 0 or 0.85 in seasonal setting

# View(group_by(dalyoutput_cost, pfpr, seasonality, ITNuse, resistance, cost_per_dose, delivery_cost) %>%
#   summarise(n()))
#
# # All have 9 entries, except seasonal has 18 because of SMC
#
# View(filter(dalyoutput_cost, pfpr==0.2, seasonality=="perennial", ITNuse==0, resistance==0,
#             cost_per_dose==2.69, delivery_cost==0.96))
# 3 pyr without boost (baseline), 3 pyr with boost (scenario), 3 pbo without boost (scenario)
# 3 options within each is the RTSS

dalyoutput_cost_group <- as_tibble(dalyoutput_cost) %>%
  group_by(pfpr, seasonality, ITNuse, resistance, SMC, cost_per_dose, delivery_cost) %>%
  mutate(ID=cur_group_id(),
         scenario=ifelse(test=(ITNboost==0 & RTSS == "none" & ITN== "pyr"),yes="baseline",
                          no=paste0("scenario",ITNboost,"_",ITN,"_",RTSS))) %>%
  ungroup()

dalyoutput_cost2 <- dalyoutput_cost_group %>%
  select(ID, scenario, daly, cost_total) %>%
  pivot_wider(names_from = scenario,
              values_from = c(daly, cost_total))

dalyoutput_cost2[,grep("^daly.*",names(dalyoutput_cost2), value=T)] <-
  (dalyoutput_cost2[,grep("^daly.*",names(dalyoutput_cost2), value=T)]-
  dalyoutput_cost2$daly_baseline) * (-1)
dalyoutput_cost2[,grep("^cost.*",names(dalyoutput_cost2), value=T)] <-
  dalyoutput_cost2[,grep("^cost.*",names(dalyoutput_cost2), value=T)]-
     dalyoutput_cost2$cost_total_baseline

# Turn into long format and merge back with scenario information
dalys_averted <- select(dalyoutput_cost2, ID, starts_with("daly")) %>%
  pivot_longer(cols=starts_with("daly"),
               names_to = "scenario",
               values_to = "dalys_averted") %>%
  mutate(scenario = gsub(".*scenario","",scenario),
         scenario =  gsub(".*daly_","",scenario))

cost <- select(dalyoutput_cost2, ID, starts_with("cost")) %>%
  pivot_longer(cols=starts_with("cost"),
               names_to = "scenario",
               values_to = "cost") %>%
  mutate(scenario = gsub(".*scenario","",scenario),
         scenario =  gsub(".*cost_total_","",scenario))


dalyoutput_cost3 <- full_join(cost, dalys_averted, by = c("ID", "scenario")) %>%
    left_join(select(dalyoutput_cost_group, ID, pfpr, seasonality,
                                   ITNuse, resistance, SMC, cost_per_dose, delivery_cost,
                                   ITNboost, ITN, RTSS, RTSScov),
                            by = "ID") %>%
  mutate(costing_group = paste0("dose",cost_per_dose, "+delivery", delivery_cost),
         scenario2 = scenario) %>%
  mutate(scenario = recode(scenario,
         "0_pbo_EPI" = "PBO switch+RTSS EPI",
         "0_pbo_none" = "PBO switch",
         "0_pbo_SV" = "PBO switch+RTSS SV",
         "0_pyr_EPI" = "RTSS EPI",
         "0_pyr_SV" = "RTSS SV",
         "1_pyr_EPI" = "ITN boost+RTSS EPI",
         "1_pyr_none" = "ITN boost",
         "1_pyr_SV" = "ITN boost+RTSS SV"),
         ITNuse_label = paste0("ITN use = ", ITNuse),
         pfpr_label = paste0("PfPR = ", pfpr))

ggplot(filter(dalyoutput_cost3, costing_group=="dose2.69+delivery0.96")) +
  geom_point(aes(x=dalys_averted, y = cost, colour=reorder(scenario, dalys_averted))) +
  scale_colour_brewer(palette="Paired") +
  geom_vline(xintercept=0) +
  facet_wrap(~ITNuse_label) +
  geom_hline(yintercept=0) +
  labs(title="Cost per dose = 2.69 + delivery cost = 0.96",
       x="DALYs averted", y = "Cost (dollars)",
       colour="Scenario") +
  theme_classic()

ggplot(filter(dalyoutput_cost3, costing_group=="dose2.69+delivery0.96")) +
  geom_point(aes(x=dalys_averted, y = cost, colour=reorder(scenario, dalys_averted))) +
  scale_colour_brewer(palette="Paired") +
  geom_vline(xintercept=0) +
  facet_wrap(pfpr_label~ITNuse_label) +
  geom_hline(yintercept=0) +
  labs(title="Cost per dose = 2.69 + delivery cost = 0.96",
       x="DALYs averted", y = "Cost (dollars)",
       colour="Scenario") +
  theme_classic()

