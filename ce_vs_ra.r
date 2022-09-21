# Jordan Wingenroth
# August 8, 2022

library(tidyverse)

## Load data

cpc <- read_csv("./data/scghg-RFF-sectoral-2020-CO2-n10000/cpc_CO2--n10000-total.csv") %>% as.matrix()

mds_files <- list.files("./data/scghg-RFF-sectoral-2020-CO2-n10000", "mds", full.names = TRUE)

mds <- lapply(mds_files, read_csv) %>%
    lapply(as.matrix)

names(mds) <- str_sub(mds_files, 59, -5)

## Calculate rates

g <- cpc
for (i in 1:ncol(cpc)) {
    g[,i] <- log(cpc[,i]/cpc[,1])/(i-1)    
}

r_ce_1.5 <- vector()
r_ce_2.0 <- vector()
r_ce_2.5 <- vector()
r_ce_3.0 <- vector()
r_ra_1.5 <- list(0)
r_ra_2.0 <- list(0)
r_ra_2.5 <- list(0)
r_ra_3.0 <- list(0)

for (i in 1:ncol(g)) {
    r_ce_1.5 [i] <- -log(mean(exp(-(i-1)*(exp(9.149606e-05) - 1 + 1.016010   *g[,i]))))/(i-1)
    r_ce_2.0 [i] <- -log(mean(exp(-(i-1)*(exp(0.001972641)  - 1 + 1.244458999*g[,i]))))/(i-1)
    r_ce_2.5 [i] <- -log(mean(exp(-(i-1)*(exp(0.004618784)  - 1 + 1.421158088*g[,i]))))/(i-1)
    r_ce_3.0 [i] <- -log(mean(exp(-(i-1)*(exp(0.007702711)  - 1 + 1.567899391*g[,i]))))/(i-1)
}

for (j in 1:length(mds)) {

    r_ra_1.5[[j]] <- vector()
    r_ra_2.0[[j]] <- vector()
    r_ra_2.5[[j]] <- vector()
    r_ra_3.0[[j]] <- vector()

    for (i in 1:ncol(g)) {

        r_ra_1.5[[j]][i] <- -log(mean(mds[[j]][,i]*exp(-(i-1)*(exp(9.149606e-05) - 1 + 1.016010   *g[,i])))/mean(mds[[j]][,i]))/(i-1)
        r_ra_2.0[[j]][i] <- -log(mean(mds[[j]][,i]*exp(-(i-1)*(exp(0.001972641)  - 1 + 1.244458999*g[,i])))/mean(mds[[j]][,i]))/(i-1)
        r_ra_2.5[[j]][i] <- -log(mean(mds[[j]][,i]*exp(-(i-1)*(exp(0.004618784)  - 1 + 1.421158088*g[,i])))/mean(mds[[j]][,i]))/(i-1)
        r_ra_3.0[[j]][i] <- -log(mean(mds[[j]][,i]*exp(-(i-1)*(exp(0.007702711)  - 1 + 1.567899391*g[,i])))/mean(mds[[j]][,i]))/(i-1)
       
    }
}

names(r_ra_1.5) <- names(mds)
names(r_ra_2.0) <- names(mds)
names(r_ra_2.5) <- names(mds)
names(r_ra_3.0) <- names(mds)

## Plot rates

data_long <- tibble(r_ce_1.5, r_ce_2.0, r_ce_2.5, r_ce_3.0, r_ra_1.5$total, r_ra_2.0$total, r_ra_2.5$total, r_ra_3.0$total) %>%
    mutate(year = row_number()+2019) %>%
    pivot_longer(r_ce_1.5:`r_ra_3.0$total`) %>%
    mutate(rate = str_extract(name, "\\d*[.]\\d*"),
           type = if_else(str_detect(name, "ce"), "Certainty-equivalent", "Risk-adjusted"))

p1 <- data_long %>%
    mutate(type = factor(type, levels = c("Risk-adjusted","Certainty-equivalent"))) %>%
    filter(rate == "2.0") %>%
    ggplot(aes(x = year, y = value, color = type)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent, limits = c(0, .04), expand = c(0,0)) +
    scale_x_continuous(breaks = c(2020, 2100, 2200, 2300), minor_breaks = NULL) +
    scale_color_manual(values = c("#FF6663","#3298EA")) +
    labs(x = "Year", y = "Discount Rate", color = "") +
    theme_bw()

ggsave("ce_vs_ra.png", p1, width = 6, height = 4, dpi = 900)

## Plot comparing 4 sectors

p2 <- bind_rows(r_ra_2.0) %>%
    mutate(year = 2020:2300) %>%
    pivot_longer(agriculture:total) %>%
    filter(year >= 2030) %>%
    ggplot(aes(x = year, y = value, color = name)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent, limits = c(0, .04), expand = c(0,0)) +
    labs(x = "Year", y = "Discount Rate", color = "") +
    theme_bw()

ggsave("sectors_riskadj.png", p2, width = 6, height = 4, dpi = 900)

## Export data

data_wide <- data_long %>%
    select(year, name, value) %>%
    pivot_wider(names_from = name) %>%
    filter(year > 2020)

write_csv(data_wide, "ce_vs_ra.csv")