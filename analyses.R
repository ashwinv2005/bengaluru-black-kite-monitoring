##################################### full data

require(lubridate)
require(tidyverse)
require(sp)
require(rgeos)
require(ggfortify)
require(rgdal)
require(sf)
require(mapview)
require(leaflet)
require(rmapshaper)

cols <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
          "#B69AC9", "#319cc0","#31954E","#493F3D",
          "#EA5599", "#9999CC", "#A13E2B", "#66CC99")

load("maps_sf.RData")
load("kitedata.RData")

data_expanded = data %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>%
  dplyr::select(-COMMON.NAME, -OBSERVATION.COUNT)

data_kite = data %>%
  filter(COMMON.NAME == "Black Kite") %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER,OBSERVATION.COUNT)

data = data_expanded %>%
  left_join(data_kite) %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  mutate(OBSERVATION.COUNT = replace_na(OBSERVATION.COUNT, 0))

sampling_day = data %>%
  group_by(day) %>%
  reframe(lists = n())

## create bins of interest

data = data %>%
  mutate(period = case_when(day <= 23 ~ "Before ban",
                            day %in% c(24:29) ~ "Early ban",
                            day %in% c(30:38) ~ "Mid ban",
                            day %in% c(39:40) ~ "Just before Aero Show",
                            day %in% c(41:43) ~ "Start of Aero Show",
                            day %in% c(44:45) ~ "End of Aero Show",
                            day %in% c(46:52) ~ "Just after Aero Show",
                            TRUE ~ "Well after Aero Show")) %>%
  mutate(period_coarse = case_when(day <= 23 ~ "Start of ban",
                                   day %in% c(24:29) ~ "Start of ban",
                                   day %in% c(30:38) ~ "Before Aero Show",
                                   day %in% c(39:40) ~ "Before Aero Show",
                                   day %in% c(41:43) ~ "During Aero Show",
                                   day %in% c(44:45) ~ "During Aero Show",
                                   day %in% c(46:52) ~ "After Aero Show",
                                   TRUE ~ "After Aero Show")) %>%
  separate(TIME.OBSERVATIONS.STARTED, into = c("hour", "minute", "second"), sep = ":") %>%
  mutate(period_time = case_when(hour < 11 ~ "Morning",
                                 hour < 15 ~ "Afternoon",
                                 TRUE ~ "Evening"))

data$period = factor(data$period, levels = c("Before ban", "Early ban", "Mid ban", "Just before Aero Show",
                                             "Start of Aero Show", "End of Aero Show", "Just after Aero Show",
                                             "Well after Aero Show"))

data$period_coarse = factor(data$period_coarse, levels = c("Start of ban", "Before Aero Show", "During Aero Show", 
                                                           "After Aero Show"))

data$period_time = factor(data$period_time, levels = c("Morning", "Afternoon", "Evening"))


## create the radius circle

# Define the center point (replace with your lat/lon)
lat <- 13.132293038603436
lon <- 77.61538659219599

# Create an sf point object
center_point <- st_sfc(st_point(c(lon, lat)), crs = 4326)

# Transform to a projected CRS that uses meters (UTM zone appropriate for location)
center_utm <- st_transform(center_point, crs = 32643)  # UTM zone 43N for Bengaluru

# Create a buffer of 13 km (13000 meters)
circle_utm <- st_buffer(center_utm, dist = 13000)

# Transform back to geographic coordinates (lat/lon)
circle_wgs84 <- st_transform(circle_utm, crs = 4326) %>%
  st_as_sf(name = "Inside ban radius")


sf_use_s2(FALSE)

temp = data %>%
  distinct(group.id, LONGITUDE, LATITUDE) %>% 
  distinct(group.id, .keep_all = TRUE) |> 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(circle_wgs84)) %>%
  # PAs
  st_join(circle_wgs84) %>%
  st_drop_geometry()

data = data %>% 
  left_join(temp)

data$name[is.na(data$name)] = "Outside ban radius"


## sample sizes

## sampling comparison

sampling = data %>%
  group_by(period,name,period_time) %>%
  reframe(lists = n())

sampling = sampling %>%
  group_by(period,name) %>%
  mutate(min_samp = min(lists)) %>%
  ungroup()


sampling_coarse = data %>%
  group_by(period_coarse,name,period_time) %>%
  reframe(lists_coarse = n())

sampling_coarse = sampling_coarse %>%
  group_by(period_coarse,name) %>%
  mutate(min_samp_coarse = min(lists_coarse)) %>%
  ungroup()


data_std = data %>%
  left_join(sampling) %>%
  left_join(sampling_coarse)



# simple calculation of means

bootst = function(vec) {
  mn = numeric(1000)
  
  for (i in 1:1000)
  {
    temp = sample(vec, replace = T)
    mn[i] = mean(temp)
  }
  
  lci = quantile(mn,0.025)
  med = median(mn)
  rci = quantile(mn,0.975)
  
  res = c(lci,med,rci)
  
  return(res)
}

res_coarse = data_std %>%
  group_by(name,period_coarse,period_time) %>%
  slice(sample(min(min_samp_coarse))) %>%
  group_by(name,period_coarse) %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  reframe(cil = bootst(OBSERVATION.COUNT)[1],
          mean = bootst(OBSERVATION.COUNT)[2],
          ciu = bootst(OBSERVATION.COUNT)[3])

res = data_std %>%
  group_by(name,period,period_time) %>%
  slice(sample(min(min_samp))) %>%
  group_by(name,period) %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  reframe(cil = bootst(OBSERVATION.COUNT)[1],
          mean = bootst(OBSERVATION.COUNT)[2],
          ciu = bootst(OBSERVATION.COUNT)[3])


# mean richness

pd = position_dodge(0.55)

ggp = ggplot(res_coarse, 
             aes(x=period_coarse, y=mean, col = factor(name), fill = factor(name))) + 
  geom_col(width = 0.5, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), linewidth = 0.3, 
                width = 0.1, position = pd, col = "black") +
  xlab("Period") +
  ylab("Mean count")

ggp1 = ggp +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = -15)),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  scale_colour_manual(
    breaks = c("Inside ban radius", "Outside ban radius"),
    values = cols[c(1,2)],
    labels = c("Inside ban radius", "Outside ban radius")
  ) +
  scale_fill_manual(
    breaks = c("Inside ban radius", "Outside ban radius"),
    values = cols[c(1,2)],
    labels = c("Inside ban radius", "Outside ban radius")
  ) +
  scale_x_discrete(
    breaks = c("Start of ban", "Before Aero Show", "During Aero Show", 
               "After Aero Show"),
    labels = c("Start of ban", "Before\nAero Show", "During\nAero Show", 
               "After\nAero Show")
    #labels = c("Before\nban", "Early\nban", "Mid\nban", "Just before\nAero Show",
    #           "Start of\nAero Show", "End of\nAero Show", "Just after\nAero Show",
    #           "Well after\nAero Show")
  ) +
  theme(strip.text.x = element_text(size = 15))+
  guides(fill = guide_legend(nrow = 1)) +
  ggtitle("Kite count per checklist") +
  theme(plot.title = element_text(size = 28))



n1 = paste("mean_kite_count_coarse.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)


ggp = ggplot(res, 
             aes(x=period, y=mean, col = factor(name), fill = factor(name))) + 
  geom_col(width = 0.5, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), linewidth = 0.3, 
                width = 0.1, position = pd, col = "black") +
  xlab("Period") +
  ylab("Mean count")

ggp1 = ggp +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = -15)),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  scale_colour_manual(
    breaks = c("Inside ban radius", "Outside ban radius"),
    values = cols[c(1,2)],
    labels = c("Inside ban radius", "Outside ban radius")
  ) +
  scale_fill_manual(
    breaks = c("Inside ban radius", "Outside ban radius"),
    values = cols[c(1,2)],
    labels = c("Inside ban radius", "Outside ban radius")
  ) +
  scale_x_discrete(
    breaks = c("Before ban", "Early ban", "Mid ban", "Just before Aero Show",
               "Start of Aero Show", "End of Aero Show", "Just after Aero Show",
               "Well after Aero Show"),
    labels = c("Before\nban", "Early\nban", "Mid\nban", "Just before\nAero Show",
               "Start of\nAero Show", "End of\nAero Show", "Just after\nAero Show",
               "Well after\nAero Show")
  ) +
  theme(strip.text.x = element_text(size = 15))+
  guides(fill = guide_legend(nrow = 1)) +
  ggtitle("Kite count per checklist") +
  theme(plot.title = element_text(size = 28))



n1 = paste("mean_kite_count.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)


## plot sampling distribution

blr_urb = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Bangalore","Bengaluru Rural"))


ggp = ggplot(data_std) +
  facet_wrap(~period_coarse) +
  geom_sf(data = blr_urb, colour = cols[3], fill = NA) +  
  geom_sf(data = circle_wgs84, colour = cols[4], fill = NA) +
  geom_point(data = data_std, aes(x=LONGITUDE,y=LATITUDE),size = 1)


ggp1 = ggp +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(strip.text.x = element_text(size = 10))+
  coord_sf()



n1 = paste("Sampling_in_space.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=7, height=7)

write.csv(sampling_coarse, "sample_size_four_periods.csv", row.names = F)
write.csv(sampling, "sample_size_eight_periods.csv", row.names = F)

sampling_daily = data %>%
  group_by(day,name) %>%
  reframe(lists = n())

write.csv(sampling_daily, "sample_size_daily.csv", row.names = F)


## time of day

res_time = data_std %>%
  group_by(name,period_time) %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  reframe(cil = bootst(OBSERVATION.COUNT)[1],
          mean = bootst(OBSERVATION.COUNT)[2],
          ciu = bootst(OBSERVATION.COUNT)[3])

pd = position_dodge(0.55)

ggp = ggplot(res_time, 
             aes(x=period_time, y=mean, col = factor(name), fill = factor(name))) + 
  geom_col(width = 0.5, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), linewidth = 0.3, 
                width = 0.1, position = pd, col = "black") +
  xlab("Time of Day") +
  ylab("Mean count")

ggp1 = ggp +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = -15)),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  scale_colour_manual(
    breaks = c("Inside ban radius", "Outside ban radius"),
    values = cols[c(1,2)],
    labels = c("Inside ban radius", "Outside ban radius")
  ) +
  scale_fill_manual(
    breaks = c("Inside ban radius", "Outside ban radius"),
    values = cols[c(1,2)],
    labels = c("Inside ban radius", "Outside ban radius")
  ) +
  theme(strip.text.x = element_text(size = 15))+
  guides(fill = guide_legend(nrow = 1)) +
  ggtitle("Kite count per checklist") +
  theme(plot.title = element_text(size = 28))



n1 = paste("count_time_of_day.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)
