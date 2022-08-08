# Jordan Wingenroth
# August 8, 2022

library(tidyverse)

## Load data

mds <- read_csv("./data/scghg-RFF-sectoral-2020-CO2-n10000/mds_CO2--n10000-total.csv") %>% as.matrix()
cpc <- read_csv("./data/scghg-RFF-sectoral-2020-CO2-n10000/cpc_CO2--n10000-total.csv") %>% as.matrix()

## Calculate rates

g <- cpc
for (i in 1:ncol(cpc)) {
    g[,i] <- log(cpc[,i]/cpc[,1])/(i-1)    
}

r_ce_1.5 <- vector()
r_ce_2.0 <- vector()
r_ce_2.5 <- vector()
r_ce_3.0 <- vector()
r_ra_1.5 <- vector()
r_ra_2.0 <- vector()
r_ra_2.5 <- vector()
r_ra_3.0 <- vector()

for (i in 1:ncol(g)) {
    r_ce_1.5 [i] <- -log(mean(exp(-(i-1)*(exp(9.149606e-05) - 1 + 1.016010   *g[,i]))))/(i-1)
    r_ce_2.0 [i] <- -log(mean(exp(-(i-1)*(exp(0.001972641)  - 1 + 1.244458999*g[,i]))))/(i-1)
    r_ce_2.5 [i] <- -log(mean(exp(-(i-1)*(exp(0.004618784)  - 1 + 1.421158088*g[,i]))))/(i-1)
    r_ce_3.0 [i] <- -log(mean(exp(-(i-1)*(exp(0.007702711)  - 1 + 1.567899391*g[,i]))))/(i-1)
    r_ra_1.5 [i] <- -log(mean(mds[,i]*exp(-(i-1)*(exp(9.149606e-05) - 1 + 1.016010   *g[,i])))/mean(mds[,i]))/(i-1)
    r_ra_2.0 [i] <- -log(mean(mds[,i]*exp(-(i-1)*(exp(0.001972641)  - 1 + 1.244458999*g[,i])))/mean(mds[,i]))/(i-1)
    r_ra_2.5 [i] <- -log(mean(mds[,i]*exp(-(i-1)*(exp(0.004618784)  - 1 + 1.421158088*g[,i])))/mean(mds[,i]))/(i-1)
    r_ra_3.0 [i] <- -log(mean(mds[,i]*exp(-(i-1)*(exp(0.007702711)  - 1 + 1.567899391*g[,i])))/mean(mds[,i]))/(i-1)
}

## Plot rates

data_long <- tibble(r_ce_1.5, r_ce_2.0, r_ce_2.5, r_ce_3.0, r_ra_1.5, r_ra_2.0, r_ra_2.5, r_ra_3.0) %>%
    mutate(year = row_number()+2019) %>%
    pivot_longer(r_ce_1.5:r_ra_3.0) %>%
    mutate(rate = str_extract(name, "\\d*[.]\\d*"),
           type = if_else(str_detect(name, "ce"), "Certainty-equivalent", "Risk-adjusted"))

p1 <- data_long %>%           
    ggplot(aes(x = year, y = value, color = rate, lty = type)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Year", y = "Discount Rate", lty = "", color = "Near-term discount rate") +
    theme_bw()

ggsave("ce_vs_ra.png", p1)

## Export data

data_wide <- data_long %>%
    select(year, name, value) %>%
    pivot_wider(names_from = name) %>%
    filter(year > 2020)

write_csv(data_wide, "ce_vs_ra.csv")