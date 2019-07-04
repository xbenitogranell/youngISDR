
library(maps)
library(ggplot2)
library(tidyverse)
library(viridis)


visits <- read.csv("data/visits_blog_country_July2019.csv")
visits$region <- as.character(visits$region) 

world <-map_data("world") %>% filter(!region=="Antarctica")

# Make world map with blog views
visits_map_data <- left_join(world, visits, by = "region")

colnames(visits_map_data)[colnames(visits_map_data)=="views"] <- "Blog_views"

world <- ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "grey")

# num_vals <- unique(visits_map_data$Blog_views) 
# num_vals <- num_vals[!is.na(num_vals)]
# col_pal <- viridis(length(num_vals))

plt_blog_views <- ggplot(visits_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Blog_views)) +
  #scale_fill_viridis_c(option = "D") +
  scale_fill_gradient(low="#1f7f79", high="#DBBD6C") +
  theme_bw() +
  ylab("Latitude") + xlab("Longitude") +
  theme(text= element_text(size=12), legend.position = c(0.1, 0.2))
  
ggsave("figures/blog_views.png", plt_blog_views, height = 8, width = 12, bg="transparent")

# make barplots for DOM stats
dom <- read.csv("data/dom.csv")
names(dom)

dom$Month <- lubridate::dmy(dom$Month)
dplyr::arrange(dom, Month)

dom_tidy <- dom %>% 
  gather(key=group, value=count, -Taxon, -Type, -Month) %>%
  filter(!group=="FB.likes") %>%
  filter(!group=="Twitter.engagements") %>%
  mutate(count_num = as.numeric(count)) %>%
  mutate(Month = as.character(Month))

domplt <- ggplot(dom_tidy, aes(fill=group, y=count_num, x=Month)) + 
  geom_bar( stat="identity", position="fill") +
  theme_bw() +
  #scale_fill_viridis_d()+
  scale_fill_manual(values=c("#340042", "#1f7f79", "#DBBD6C")) +
  labs(fill = "") + ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.4, 0.2))
  


ggsave("figures/DOM_stats.png", domplt, height = 8, width = 12, bg="transparent")
