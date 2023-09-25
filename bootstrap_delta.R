# Calculate empirical p-values of deltas between calibration intercepts and slopes

library(tidyverse)

analyze_delta <- function(data1,
                          data2,
                          metric,
                          label1,
                          label2,
                          variable) {
  #plot histograms
  metric1 <- unlist(map(data1, `[`, c(metric)))
  metric2 <- unlist(map(data2, `[`, c(metric)))
  
  plot_df <- tibble(value = c(metric1, metric2),
                    label = c(rep(label1, times = length(metric1)),
                              rep(label2, times = length(metric2))))
  
  hist <- ggplot(plot_df, aes(x = value, fill = label)) + 
    geom_histogram() +
    theme_bw() + 
    theme(panel.grid.major = element_blank()) +
    labs(x = metric,
         y = "Count",
         fill = variable)

  delta = metric2 - metric1
  niters = length(delta)
  n_above0 <- if(mean(metric1) < 0 & mean(metric2) < 0 & mean(metric2) > mean(metric1)) {
    sum(delta < 0)
  } else if(mean(metric1) < 0 & mean(metric2) > 0 & mean(metric2) > mean(metric1)) {
    sum(delta < 0)
  } else if(mean(metric1) < 0 & mean(metric2) < 0 & mean(metric2) < mean(metric1)) {
    sum(delta > 0)
  } else if(mean(metric1) > 0 & mean(metric2) > 0 & mean(metric2) < mean(metric1)) {
    sum(delta > 0)
  } else if(mean(metric1) > 0 & mean(metric2) > 0 & mean(metric2) > mean(metric1)) {
    sum(delta < 0)    
  } else if(mean(metric1) > 0 & mean(metric2) < 0 & mean(metric2) < mean(metric1)) {
    sum(delta > 0)  
  }
  
  # n_above0 <- ifelse(abs(mean(metric2)) > abs(mean(metric1)), sum(delta > 0), sum(delta < 0))
  n_below0 = niters - n_above0
  
  res_df = data.frame(boot_iters = niters, 
                      mean_delta = mean(delta), 
                      median_delta = median(delta), 
                      n_above0 = n_above0, 
                      n_below0 = n_below0,
                      pvalue = n_above0/niters)
  return(list(plot = hist,
              res = res_df))
}
# Male vs female ----
male_m1 <- readRDS("male_m1_bootstats.Rds")
female_m1 <- readRDS("female_m1_bootstats.Rds")

mf_intercept_res <- analyze_delta(male_m1,
                                  female_m1,
                                  "Intercept",
                                  "Male",
                                  "Female",
                                  "Gender")
mf_intercept_res$plot
mf_intercept_res$res

mf_slope_res <- analyze_delta(male_m1,
                              female_m1,
                              "Slope",
                              "Male",
                              "Female",
                              "Gender")
mf_slope_res$plot
mf_slope_res$res

# Black vs white ----
black_m1 <- readRDS("black_m1_bootstats.Rds")
white_m1 <- readRDS("white_m1_bootstats.Rds")

bw_intercept_res <- analyze_delta(black_m1,
                                  white_m1,
                                  "Intercept",
                                  "Black",
                                  "White",
                                  "Race")
bw_intercept_res$plot
bw_intercept_res$res

bw_slope_res <- analyze_delta(black_m1,
                              white_m1,
                              "Slope",
                              "Black",
                              "White",
                              "Race")
bw_slope_res$plot
bw_slope_res$res

# 2021 vs. 2022 ----
m1_2021 <- readRDS("2021_m1_bootstats.Rds")
m1_2022 <- readRDS("2022_m1_bootstats.Rds")

year_intercept_res <- analyze_delta(m1_2021,
                                    m1_2022,
                                    "Intercept",
                                    "2021",
                                    "2022",
                                    "Year")
year_intercept_res$plot
year_intercept_res$res

year_slope_res <- analyze_delta(m1_2021,
                                m1_2022,
                                "Slope",
                                "2021",
                                "2022",
                                "Year")
year_slope_res$plot
year_slope_res$res

# payor type ----
commercial_m1 <- readRDS("commercial_m1_bootstats.Rds")
medicaid_m1 <- readRDS("medicaid_m1_bootstats.Rds")
medicare_m1 <- readRDS("medicare_m1_bootstats.Rds")

commercial_inter <- unlist(map(commercial_m1, `[`, c("Intercept")))
medicaid_inter <- unlist(map(medicaid_m1, `[`, c("Intercept")))
medicare_inter <- unlist(map(medicare_m1, `[`, c("Intercept")))
commercial_slope <- unlist(map(commercial_m1, `[`, c("Slope")))
medicaid_slope <- unlist(map(medicaid_m1, `[`, c("Slope")))
medicare_slope <- unlist(map(medicare_m1, `[`, c("Slope")))

payor_intercept_df <- tibble(value = c(commercial_inter , medicaid_inter, medicare_inter),
                             label = c(rep("Commercial", times = length(commercial_inter)),
                                       rep("Medicaid", times = length(medicaid_inter)),
                                       rep("Medicare", times = length(medicare_inter))))

payor_intercept_hist <- ggplot(payor_intercept_df, aes(x = value, fill = label)) + 
  geom_histogram() +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  labs(x = "Intercept",
       y = "Count",
       fill = "Payor Type")

payor_slope_df <- tibble(value = c(commercial_slope , medicaid_slope, medicare_slope),
                         label = c(rep("Commercial", times = length(commercial_slope)),
                                   rep("Medicaid", times = length(medicaid_slope)),
                                   rep("Medicare", times = length(medicare_slope))))

payor_slope_hist <- ggplot(payor_slope_df, aes(x = value, fill = label)) + 
  geom_histogram() +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  labs(x = "Slope",
       y = "Count",
       fill = "Payor Type")

ccaid_intercept_res <- analyze_delta(commercial_m1,
                                     medicaid_m1,
                                     "Intercept",
                                     "Commercial",
                                     "Medicaid",
                                     "Payor Type")
ccaid_intercept_res$plot
ccaid_intercept_res$res

ccaid_slope_res <- analyze_delta(commercial_m1,
                                 medicaid_m1,
                                 "Slope",
                                 "Commercial",
                                 "Medicaid",
                                 "Payor Type")
ccaid_slope_res$plot
ccaid_slope_res$res

ccare_intercept_res <- analyze_delta(commercial_m1,
                                     medicare_m1,
                                     "Intercept",
                                     "Commercial",
                                     "Medicare",
                                     "Payor Type")
ccare_intercept_res$plot
ccare_intercept_res$res

ccare_slope_res <- analyze_delta(commercial_m1,
                                 medicare_m1,
                                 "Slope",
                                 "Commercial",
                                 "Medicare",
                                 "Payor Type")
ccare_slope_res$plot
ccare_slope_res$res

caidcare_intercept_res <- analyze_delta(medicaid_m1,
                                        medicare_m1,
                                        "Intercept",
                                        "Medicaid",
                                        "Medicare",
                                        "Payor Type")
caidcare_intercept_res$plot
caidcare_intercept_res$res

caidcare_slope_res <- analyze_delta(medicaid_m1,
                                    medicare_m1,
                                    "Slope",
                                    "Medicaid",
                                    "Medicare",
                                    "Payor Type")
caidcare_slope_res$plot
caidcare_slope_res$res

# hospital type ----
community_m1 <- readRDS("community_m1_bootstats.Rds")
tertiary_m1 <- readRDS("tertiary_m1_bootstats.Rds")
quaternary_m1 <- readRDS("quaternary_m1_bootstats.Rds")

community_inter <- unlist(map(community_m1, `[`, c("Intercept")))
tertiary_inter <- unlist(map(tertiary_m1, `[`, c("Intercept")))
quaternary_inter <- unlist(map(quaternary_m1, `[`, c("Intercept")))
community_slope <- unlist(map(community_m1, `[`, c("Slope")))
tertiary_slope <- unlist(map(tertiary_m1, `[`, c("Slope")))
quaternary_slope <- unlist(map(quaternary_m1, `[`, c("Slope")))

hospital_intercept_df <- tibble(value = c(community_inter, tertiary_inter, quaternary_inter),
                                label = c(rep("Community", times = length(community_inter)),
                                          rep("Tertiary", times = length(tertiary_inter)),
                                          rep("Quaternary", times = length(quaternary_inter))))

hospital_intercept_hist <- ggplot(hospital_intercept_df, aes(x = value, fill = label)) + 
  geom_histogram() +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  labs(x = "Intercept",
       y = "Count",
       fill = "Hospital Type")

hospital_slope_df <- tibble(value = c(community_slope, tertiary_slope, quaternary_slope),
                            label = c(rep("Community", times = length(community_slope)),
                                      rep("Tertiary", times = length(tertiary_slope)),
                                      rep("Quaternary", times = length(quaternary_slope))))

hospital_slope_hist <- ggplot(hospital_slope_df, aes(x = value, fill = label)) + 
  geom_histogram() +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  labs(x = "Slope",
       y = "Count",
       fill = "Hospital Type")

ct_intercept_res <- analyze_delta(community_m1,
                                  tertiary_m1,
                                  "Intercept",
                                  "Community",
                                  "Tertiary",
                                  "Hospital Type")
ct_intercept_res$plot
ct_intercept_res$res

ct_slope_res <- analyze_delta(community_m1,
                              tertiary_m1,
                              "Slope",
                              "Community",
                              "Tertiary",
                              "Hospital Type")
ct_slope_res$plot
ct_slope_res$res

cq_intercept_res <- analyze_delta(community_m1,
                                  quaternary_m1,
                                  "Intercept",
                                  "Community",
                                  "Quaternary",
                                  "Hospital Type")
cq_intercept_res$plot
cq_intercept_res$res

cq_slope_res <- analyze_delta(community_m1,
                              quaternary_m1,
                              "Slope",
                              "Community",
                              "Quaternary",
                              "Hospital Type")
cq_slope_res$plot
cq_slope_res$res

tq_intercept_res <- analyze_delta(tertiary_m1,
                                  quaternary_m1,
                                  "Intercept",
                                  "Tertiary",
                                  "Quaternary",
                                  "Hospital Type")
tq_intercept_res$plot
tq_intercept_res$res

tq_slope_res <- analyze_delta(tertiary_m1,
                              quaternary_m1,
                              "Slope",
                              "Tertiary",
                              "Quaternary",
                              "Hospital Type")
tq_slope_res$plot
tq_slope_res$res

