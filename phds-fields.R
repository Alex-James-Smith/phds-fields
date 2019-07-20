library(tidyverse)
library(magrittr)

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")


## Number of PhDs by Broad Field and year line plot
phd_field %>% group_by(broad_field, year) %>% 
  summarise(bf_n_phds = sum(n_phds, na.rm = TRUE)) %>% 
  ggplot(aes(year, bf_n_phds, colour = broad_field)) + 
  geom_line() +
  labs(title = "Number of PhDs by Broad Field and year (2008 - 2017)", 
       x = NULL, y = NULL, colour = NULL, 
       caption = "Source: NSF.gov\n#TidyTuesday") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + 
  theme(panel.background =element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"), 
        panel.grid.minor.y = element_line(colour = "grey50"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "#000029"),
        legend.background = element_rect(fill = "#000029"),
        legend.key = element_rect(fill = "#000029", colour = "#000029"),
        text = element_text(colour = "grey80"),
        axis.text = element_text(colour = "grey80"),
        axis.ticks = element_blank())

levels(as_factor(phd_field$major_field))

major_phd_field <- phd_field %>% group_by(broad_field, major_field) %>% 
  mutate(n_phds = sum(n_phds, na.rm = TRUE)) %>% 
  distinct(broad_field, major_field, .keep_all = TRUE) %>% 
  select(broad_field, major_field, n_phds) %>% arrange(broad_field, major_field)

phd_field %>% filter(broad_field == "Life sciences") %>% select(-c(broad_field, year)) %>%
  group_by(major_field, field) %>% mutate(n_phds = sum(n_phds, na.rm = TRUE)) %>% 
  distinct(major_field, field, .keep_all = TRUE) %>% arrange(desc(n_phds))

phd_field %>% filter(broad_field == "Psychology and social sciences") %>% select(-c(broad_field, year)) %>%
  group_by(major_field, field) %>% mutate(n_phds = sum(n_phds, na.rm = TRUE)) %>% 
  distinct(major_field, field, .keep_all = TRUE) %>% arrange(desc(n_phds))

levels(as_factor(phd_field$year))
