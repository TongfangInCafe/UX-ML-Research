 install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("RColorBrewer")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("d3heatmap")
# install.packages("data.table")
# install.packages("ggthemes")
# install.packages("scales")
# install.packages("DescTools")
# install.packages("viridis")
# install.packages("animation")


library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(d3heatmap)
library(data.table)
library(ggthemes)
library(scales)
library(DescTools)
library(viridis)
library(animation)

data1 <- read.csv("C:/Users/z003nefs/Downloads/wowah_data.csv")
data1$type <- with(data1, paste(char, race, charclass, sep="."))
fdata <- data1 %>% filter(level==80) 
fdata2 <- fdata[!duplicated(fdata$type),]

raclass <- fdata2 %>% group_by(charclass,race) %>% summarise(n=n())

raclass.w <- spread(raclass,key=race,value=n,fill=0)

raclass.w



## Social Network Part
library(visNetwork)
# Level range 1 -- 30
fdata1 <- data1 %>% filter(level == 80) 

nodes <- fdata1$zone %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) 
nodes$id <- 1:nrow(nodes)
names(nodes) <- (c("label","value","id"))
nodes$title <- nodes$label

capital <- c("Shattrath City","Orgrimmar","Silvermoon City","Thunder Bluff","Undercity","Dalaran")
battleground <- c("Warsong Gulch", "Arathi Basin", "Alterac Valley", "Eye of the Storm","Wintergrasp", "Strand of the Ancients", "Isle of Conquest", "Battle for Gilneas")

Northrend <- c("Howling Fjord","Borean Tundra","Coldarra","Dragonblight","Grizzly Hills","Zul'Drak","Sholazar Basin", "The Storm Peaks","Crystalsong Forest","Icecrown", "Utgarde Keep","Azjol-Nerub","Ahn'kahet: The Old Kingdom","Ulduar","Naxxramas II","The Nexus","Icecrown Citadel","Drak'Tharon Keep","Chamber of Aspects","The Ciolet Hold","Crusaders' Coliseum","Gundrak","Vault of Archavon")

Outland <- c("Hellfire Peninsula","Zangarmarsh","Terokkar Forest","Nagrand","Blade's Edge Mountains","Netherstorm","Shadowmoon Valley","Black Temple","Hellfire Citadel","Tempest Keep","Coilfang: The Slave Pens","Auchindoun: Shadow Labyrinth","Gruul's Lair", "The Underbog", "Magtheridon's Lair", "The Mechanar", "The Arcatraz", "Hellfire Ramparts", "The Blood Furnace","The Steamvault")

nodes$group <- ifelse(nodes$label %in% capital, "Capital cities","Others")
nodes$group[nodes$label %in% battleground] <- "Battlegrounds"
nodes$group[nodes$label %in% Northrend] <- "Northrend"
nodes$group[nodes$label %in% Outland] <- "Outland"
nodes$group[grep("Arena",nodes$label)] <- "Arena"


fdata1 %>%
  select(type,zone) %>%
  unique() %>%
  arrange(type) -> zoneCheck



zoneCheck$lead <- lead(zoneCheck$zone, 1)
zoneCheck$leadt <- lead(zoneCheck$type, 1)
zoneCheck$diffc <- zoneCheck$leadt == zoneCheck$type
zoneCheck %>% 
  filter(diffc == TRUE) %>%
  select(fromName=zone,toName=lead) %>%
  mutate(from = match(fromName, nodes$label), to = match(toName, nodes$label)) -> movement


movement %>%  
  mutate(bof=paste(from,to,sep="_")) %>%
  group_by(bof) %>%
  summarize(value=n()) %>%
  separate(bof,c("from","to"),"_") -> edges

edges %>% filter(value > 50) -> edges1

visNetwork(nodes, edges1) %>% 
  visEdges(arrows = 'to') %>% 
  visOptions(highlightNearest = list(enabled =TRUE,degree = 5), selectedBy="group")

# Level range 31 - 60
fdata2 <- data1 %>% filter((level > 30) & (level < 61)) 
# Level range 61 - 80
fdata3 <- data1 %>% filter((level > 60) & (level < 71))
# Level range > 81
fdata4 <- data1 %>% filter(level > 60)



#################################### visualization #################################
nodes <- fdata4$zone %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) 
nodes$id <- 1:nrow(nodes)
names(nodes) <- (c("label","value","id"))
nodes$title <- nodes$label

capital <- c("Shattrath City","Orgrimmar","Silvermoon City","Thunder Bluff","Undercity","Dalaran")
battleground <- c("Warsong Gulch", "Arathi Basin", "Alterac Valley", "Eye of the Storm","Wintergrasp", "Strand of the Ancients", "Isle of Conquest", "Battle for Gilneas")

Northrend <- c("Howling Fjord","Borean Tundra","Coldarra","Dragonblight","Grizzly Hills","Zul'Drak","Sholazar Basin", "The Storm Peaks","Crystalsong Forest","Icecrown", "Utgarde Keep","Azjol-Nerub","Ahn'kahet: The Old Kingdom","Ulduar","Naxxramas II","The Nexus","Icecrown Citadel","Drak'Tharon Keep","Chamber of Aspects","The Ciolet Hold","Crusaders' Coliseum","Gundrak","Vault of Archavon")

Outland <- c("Hellfire Peninsula","Zangarmarsh","Terokkar Forest","Nagrand","Blade's Edge Mountains","Netherstorm","Shadowmoon Valley","Black Temple","Hellfire Citadel","Tempest Keep","Coilfang: The Slave Pens","Auchindoun: Shadow Labyrinth","Gruul's Lair", "The Underbog", "Magtheridon's Lair", "The Mechanar", "The Arcatraz", "Hellfire Ramparts", "The Blood Furnace","The Steamvault")

nodes$group <- ifelse(nodes$label %in% capital, "Capital cities","Others")
nodes$group[nodes$label %in% battleground] <- "Battlegrounds"
nodes$group[nodes$label %in% Northrend] <- "Northrend"
nodes$group[nodes$label %in% Outland] <- "Outland"
nodes$group[grep("Arena",nodes$label)] <- "Arena"


fdata4 %>%
  select(type,zone) %>%
  unique() %>%
  arrange(type) -> zoneCheck



zoneCheck$lead <- lead(zoneCheck$zone, 1)
zoneCheck$leadt <- lead(zoneCheck$type, 1)
zoneCheck$diffc <- zoneCheck$leadt == zoneCheck$type
zoneCheck %>% 
  filter(diffc == TRUE) %>%
  select(fromName=zone,toName=lead) %>%
  mutate(from = match(fromName, nodes$label), to = match(toName, nodes$label)) -> movement


movement %>%  
  mutate(bof=paste(from,to,sep="_")) %>%
  group_by(bof) %>%
  summarize(value=n()) %>%
  separate(bof,c("from","to"),"_") -> edges

edges %>% filter(value > 200) -> edges1

visNetwork(nodes, edges1) %>% 
  visEdges(arrows = 'to') %>% 
  visOptions(highlightNearest = list(enabled =TRUE,degree = 5), selectedBy="group")