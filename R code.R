library(dplyr)
library(RCurl)
library(tidyverse)

#####Download data#####
urlfile <- "https://github.com/lapalice88/SBMM-Analysis/raw/main/Players%20KDR.csv"
data <- read.csv2(urlfile)


##Number of unique players
dataunique <- data[!duplicated(data$id),]
nrow(dataunique)

#Number of unique opponents/teammates for each selected player
n.oppo <- dataunique %>%  
  group_by(Player) %>% 
  summarise(Freq = n()) 
n.matches <- data %>%  
  group_by(Player, idmatch) %>% 
  summarise(Freq = n()) 
n.matches <- as.data.frame(table(n.matches$Player))



########Descriptive statistics######

#Frequency tables
classkdr <- cut(dataunique$kd_br, breaks = c(0,0.75,1,2,3,max(data$kd_br)), 
                include.lowest = T, right = T,
                labels = c('KDR <= 0.75', '0.75 < KDR <=1',
                           '1 < KDR <= 2', '2 < KDR <=3', 'KDR > 3'))##Creating a class of KDR for the opponents
n.classKDR <- as.data.frame(table(classkdr))
perc.classKDR <- as.data.frame(round(prop.table(table(classkdr))*100, 2))

#Statistics
statistics <- data%>%
  group_by(Player, KDR)%>% 
  summarise(Mean=mean(kd_br), Max=max(kd_br), Min=min(kd_br), Median=median(kd_br), Std=sd(kd_br))


##Mean by idmatch
stati1 <- data%>%
  group_by(Player, idmatch, KDR)%>% 
  summarise(kd_br=mean(kd_br))
            
lobby.statistics <- stati1 %>%
  group_by(Player, KDR)%>% 
  summarise(Mean=mean(kd_br),  Min=min(kd_br), Max=max(kd_br), Median=median(kd_br), Std=sd(kd_br))

##Density plot
ggplot(stati1, aes(x = kd_br, fill = KDR)) + geom_density(alpha = 0.5) + 
  xlab('KDR') + 
  ylab('Density') +
  scale_x_continuous(breaks = round(seq(0.5, 1.75, by = 0.25),2), limits = c(0.5,1.75)) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size = 12))

##KDR Team and KDR lobby
players <- c('savyultras90', 'TeePee' , 'ThaPromise19', 
                'Frozone','BonqTTV', 'BlueDevil5104', 'FireQ','xPicos', 'JimxBZH',  
                'LtRico',  'TwistyRo', 'werd',   'Booduhlicous', 'Tetas', 'KLO BURSTE1438',
                'Preschl','visibleloki','stop_Nclutch','LEGEDARYxApex',  'LapadaX', 'Special__Tiger', 
                'PREACHEROFDEATHx','verca','CascaKiawah','Porchelamb899','riot_coach')

tmpteam <- data[data$id %in% players,]
tmpteam$PlaMat <- paste(tmpteam$Placement, tmpteam$idmatch, sep = '-')
tmpteamsingle <- tmpteam[!duplicated(tmpteam$id),]

data$PlaMat <- paste(data$Placement, data$idmatch, sep = '-')
data$Team <- 0
data$Team[data$PlaMat %in% tmpteam$PlaMat] <- 1

team.statistics <- aggregate(kd_br ~ Team + idmatch, data = data, FUN = mean)
plot(team.statistics$kd_br[team.statistics$Team == 0], team.statistics$kd_br[team.statistics$Team == 1], 
     ylab = 'KDR Team', xlab = 'KDR lobby',
     main = 'Relationship between team KDR and lobby KDR')
##Correlation
cor(team.statistics$kd_br[team.statistics$Team == 0], team.statistics$kd_br[team.statistics$Team == 1])

##By modality
mode.statistics <- aggregate(kd_br ~ Mode + KDR, data, FUN = mean)

#By console
console.statistics <- aggregate(kd_br ~ Console + KDR, data , FUN = mean)


####Analysis without teammates
data_wt <- data[data$Team == 0,]
#Statistics
statistics_wt <- data_wt%>%
  group_by(Player)%>% 
  summarise(Mean=mean(kd_br), Max=max(kd_br), Min=min(kd_br), Median=median(kd_br), Std=sd(kd_br))


##Mean by idmatch
stati1_wt <- data_wt%>%
  group_by(Player, idmatch, KDR)%>% 
  summarise(kd_br=mean(kd_br))

lobby.statistics_wt <- stati1_wt %>%
  group_by(Player, KDR)%>% 
  summarise(Mean=mean(kd_br),  Min=min(kd_br), Max=max(kd_br), Median=median(kd_br), Std=sd(kd_br))
