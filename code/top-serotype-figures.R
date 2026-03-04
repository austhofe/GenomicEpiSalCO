#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(ggridges)

#### INFO ####
#run the health-data.R script first to get all of the datasets, then run each plot one by one to see the different graphics


# bar plot of top 20 serotypes for seroseq
ggplot(top20_seroseq, aes(x = reorder(`Predicted serotype`, n), y = n)) +
  geom_bar(stat = "identity", fill="turquoise4", color=NA) +
  coord_flip() +
  labs(
    x = "Predicted serotype",
    y = "Count",
    title = "Top 20 predicted serotypes"
  ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

ggplot(top20_sistr, aes(x = reorder(`serovar`, n), y = n)) +
  geom_bar(stat = "identity", fill="turquoise4", color=NA) +
  coord_flip() +
  labs(
    x = "Predicted serotype",
    y = "Count",
    title = "Top 20 predicted serotypes, CO 2018-2024"
  ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())


# Step 5: Stacked bar chart top 20 serotypes by year
ggplot(top20_sistryear,
       aes(x = reorder(sistr_serotype, n, sum),
           y = n,
           fill = factor(year))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3", name = "Year") +
  labs(
    x = "Predicted serotype",
    y = "Count",
    title = "Top 20 predicted serotypes by year, all isolates CO 2018-2024"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Step 5: Stacked bar chart for non outbreak isolates
ggplot(top20_sistryear_nooutbreak,
       aes(x = reorder(sistr_serotype, n, sum),
           y = n,
           fill = factor(year))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3", name = "Year") +
  labs(
    x = "Predicted serotype",
    y = "Count",
    title = "Top 20 predicted serotypes by year, non-outbreak isolates CO 2018-2024"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggplot(top20_sistryear_nooutbreak,
       aes(x=year, 
           y=sistr_serotype,
       )) +
  geom_density_ridges()


# Step 5: Ridgeline graph of counts by month by serotype
ggplot(top10_sistrm_nooutbreak,
       aes(x=month, 
           y=reorder(sistr_serotype, n, sum),
           height = n,
           fill=sistr_serotype)) +
  geom_density_ridges(stat="identity")+
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  coord_cartesian(clip = "off") +
  scale_fill_brewer(palette = "Set3")+
  labs(
    x = "Month",
    y = "Predicted serotype",
    title = "Top 10 serotypes by month, non-outbreak isolates 2018-2024"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    panel.border = element_blank())
)
