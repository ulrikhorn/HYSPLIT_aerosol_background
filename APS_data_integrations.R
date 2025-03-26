## READING IN AND WRANGELING THE APS DATA ##

# sVALBARD
read_aps_svalbard <- function(filename){
  aps_svalbard <- read.csv(filename, sep = ';')
  
  # fix comma issue
  aps_svalbard <- data.frame(lapply(aps_svalbard, function(x) {
    gsub(',', '.', x)
  }))
  
  
  ## for svalbard data change
  colnames(aps_svalbard)[c(4, 77, 82)] <- c("time_stamp", "median", "total_conc")
  
  
  # remove unwanted comlums
  aps_svalbard <- aps_svalbard %>% dplyr::select(c(time_stamp, median, total_conc))
  
  # fix datetime format
  aps_svalbard$time_stamp <- aps_svalbard$time_stamp %>% str_replace_all('\\.', '-')
  
  aps_svalbard$time_stamp <- as.POSIXct(aps_svalbard$time_stamp, format = "%d-%m-%Y %H:%M")
  
  aps_svalbard$total_conc <- as.numeric(aps_svalbard$total_conc)
  aps_svalbard$median <- as.numeric(aps_svalbard$median)
  
  aps_svalbard
}

# ELVERUM 
read_aps_elverum <- function(filename){
  aps_svalbard <- read.csv(filename, sep = ';')
  
  # fix comma issue
  aps_svalbard <- data.frame(lapply(aps_svalbard, function(x) {
    gsub(',', '.', x)
  }))
  
  # for elverum data
  aps_svalbard$time <- aps_svalbard$time %>% str_replace("^00", "20")
  colnames(aps_svalbard)[c(22, 27, 31)] <- c("median", "total_conc","time_stamp")
  
  # remove unwanted comlums
  aps_svalbard <- aps_svalbard %>% dplyr::select(c(time_stamp, median, total_conc))
  
  aps_svalbard$time_stamp <- as.POSIXct(aps_svalbard$time_stamp, format = "%Y-%m-%d %H:%M:%S")
  
  aps_svalbard$total_conc <- as.numeric(aps_svalbard$total_conc)
  aps_svalbard$median <- as.numeric(aps_svalbard$median)
  
  aps_svalbard
}


aps_svalbard <- read_aps_svalbard("APS_svalbard_september.csv")
aps_elverum <- read_aps_elverum("UV_APS_DATA.csv")




## Summarise the aps data to be hourly average and combine it with the hysplit data


aps_overland_summarization <- function(aps_data, overland_data){
  
  #split into hours
  aps_svalbard2 <- aps_data %>% mutate(date = as.Date(time_stamp, tz="CET"), hour = hour(time_stamp))
  
  aps_summarised <- aps_svalbard2 %>% group_by(date, hour) %>% summarize(avg_conc = mean(total_conc), avg_median = mean(median))
  
  aps_summarised <- aps_summarised %>% 
    add_column(date_time = as.POSIXct(paste(paste(aps_summarised$date, aps_summarised$hour), rep(":00", times = nrow(aps_summarised)), sep = ""), format = "%Y-%m-%d %H:%M"))
  
  
  
  colnames(overland_data)[1] <- "date_time"
  aps_elevation <- left_join(aps_summarised, overland_data, by = "date_time")
  
  aps_elevation
}

aps_elevation <- aps_overland_summarization(aps_svalbard, overland_elevation)
aps_elevation_elverum <- aps_overland_summarization(aps_elverum, overland_elevation_elverum)

# plot summarized vs not summarized
ggplot(aps_elevation_elverum)+
  geom_line(mapping = aes(date_time, avg_conc))+
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90))+
  scale_x_datetime(date_breaks = "1 day")

ggplot(aps_elverum)+
  geom_line(mapping = aes(time_stamp, total_conc))+
  #theme(axis.text.y= element_blank(),
  #        axis.ticks.y = element_blank())+
  scale_x_datetime(date_breaks = "1 day")





overlandcolor <- "#F8766D"
conc_color <- "#619CFF"


ggplot(aps_elevation, aes(x =date_time))+
  geom_line(aes(y = avg_conc), color = conc_color, size = 2)+
  geom_line(aes(y =  percent*10), color = overlandcolor, size = 2)+
  scale_y_continuous(
    name = "Hourly avg particle concentration",
    sec.axis = sec_axis(~.*0.1, name="Percent over land")
  )+
  theme(
    axis.title.y = element_text(color = conc_color, size=14),
    axis.title.y.right = element_text(color = overlandcolor, size=14)
  ) 
#scale_color_manual(name = "test", breaks = c("Particle concentration", "Percent over land"), values = c("red" = "Particle concentration", "blue" = "Percent over land"))

ggplot(aps_elevation_elverum, aes(x =date_time))+
  geom_line(aes(y = avg_median), color = conc_color, size = 2)+
  geom_line(aes(y =  percent*10), color = overlandcolor, size = 2)+
  scale_y_continuous(
    name = "Hourly avg particle concentration",
    sec.axis = sec_axis(~.*0.1, name="Percent over land")
  )+
  theme(
    axis.title.y = element_text(color = overlandcolor, size=14),
    axis.title.y.right = element_text(color = conc_color, size=14)
  )

