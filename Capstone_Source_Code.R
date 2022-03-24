# M Akbar Attallah
# Proyek Capstone DQLAB
# Hypothesis : How Carbon Tax can reduce Co2 Emissions and Increase Economic Activity
# Media : https://jakartaglobe.id/business/indonesia-to-impose-carbon-tax-in-april-2022-starting-with-coal-power-plants
# Data gathered from
# 1.https://github.com/owid/energy-data
# 2.https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
# 3.https://data.worldbank.org/indicator/EN.ATM.CO2E.PC
# 4.https://carbonpricingdashboard.worldbank.org/map_data

#Load Library
library(tidyverse)

#read data from csv file
GDP <- read_csv('GDPPerCapita.csv')
Co2p <- read_csv('CO2EmissionsPerCapita.csv')
Edata <- read_csv("Analisis_1_Electricity_Generated_Year.csv")
Co2 <- read_csv("annual-co-emissions-by-region.csv")

#Exploratory Data Analysis and Data Cleaning

GDP <- GDP %>%
  rename(Country = 'Country Name')

Co2p <- Co2p %>% 
  rename(Co2_p = Value)%>%
  rename(Year = Attribute)%>%
  rename(Country = 'Country Name')

GCo_join <- GDP %>%
  inner_join(Co2p,by = c("Country" = "Country","Year" = "Year"))

Energy <- Edata %>%
  group_by(Year) %>%
  summarise(Fossil_Energy= sum(Fossil_Energy),
            Nuclear_Energy= sum(Nuclear_Energy),
            Renewable_Electricity = sum(Renewable_Electricity))%>%
  pivot_longer(c(Fossil_Energy,Nuclear_Energy,Renewable_Electricity),
               names_to = "Energy_Type",values_to = "Energy")

Co2 <- Co2 %>%
  rename(Co2Emissions = 'Annual CO2 emissions (zero filled)')%>%
  group_by(Year)%>%
  summarise(Co2 = sum(Co2Emissions))%>%
  filter(Year >= 1900)

##Correlation on GDP and Co2 from a Country with Carbon Tax Implemented (United Kingdom)
UK_Cor <- GCo_join %>%
  filter(Country == "United Kingdom")%>%
  select(GDP,Co2_p,Year)

cor(UK_Cor$GDP, UK_Cor$Co2_p, use = "everything",
    method = c("pearson", "kendall", "spearman"))
## correlation coefficient = -0.889 which proves strong negative correlation of GDP and Co2 Values
Swe_Cor <- GCo_join %>%
  filter(Country == "Sweden")%>%
  select(GDP,Co2_p,Year)

cor(Swe_Cor$GDP, Swe_Cor$Co2_p, use = "everything",
    method = c("pearson", "kendall", "spearman"))
## correlation coefficient = -0.829 which proves strong negative correlation of GDP and Co2 Values

##Correlation on GDP and Co2 from a Country that haven't implemented carbon tax (Indonesia)
Indo_Cor <- GCo_join %>%
  filter(Country == "Indonesia")%>%
  select(GDP,Co2_p,Year)

cor(Indo_Cor$GDP, Indo_Cor$Co2_p, use = "everything",
    method = c("pearson", "kendall", "spearman"))
##correlation coefficient = 0.9001 which proves strong negative correlation of GDP and Co2 Values

India_Cor <- GCo_join %>%
  filter(Country == "India")%>%
  select(GDP,Co2_p,Year)

cor(India_Cor$GDP, India_Cor$Co2_p, use = "everything",
    method = c("pearson", "kendall", "spearman"))
##correlation coefficient = 0.9724 which proves strong negative correlation of GDP and Co2 Values


## Visualizing Data using ggplot2

#Viz 1 Annual GDP Vs Co2 Emissions Per Capita In the United Kingdom
GCo_join %>%
  filter(Country == "United Kingdom"& Year > 1990)%>%
  ggplot()+
  geom_line(aes(x=Year, y= Co2_p*4000,color = 'CO2/Capita'), stat="identity",size = 1.5) +
  geom_line(aes(x=Year, y= GDP, color = 'GDP/Capita'), size = 1.5)+
  scale_y_continuous(name = "GDP/Capita",
                     sec.axis = sec_axis(trans=~./4000, name="Co2/Capita"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 4))+
  geom_segment(aes(x = 1999,
                   y = 40000,
                   xend = 2003.2,
                   yend = 36500),
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = 2010,
                   y = 48000,
                   xend = 2013,
                   yend = 44000),
               arrow = arrow(length = unit(0.3, "cm")))+
  annotate("text", x=1999,y=41000,label= "EU ETS, 2005*"
           ,color = "Black",fontface = "bold",size = 3)+
  annotate("text", x=2010.5,y=49000,label= "UK CPS, 2013**"
           ,color = "Black",fontface = "bold",size = 3)+
  labs(title ="Annual GDP Vs Co2 Emissions Per Capita In the United Kingdom",
       caption = "*The European Union Emissions Trading System (EU ETS) is a  form of Carbon Pricing.
** UK Carbon Price Support (CPS) is an additonal form of Carbon Pricing.
       Correlation Coefficient = -0.889 (Strong Negative Correlation)")+
  scale_color_manual(values = c('#fa1ef6','#94f211'))+
  theme(
    legend.position = c(.20, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4,3, 3, 3),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = '#a8876a'),
    legend.title = element_blank(),
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size=7), plot.background = element_rect(fill = '#c4b3a3'),
    panel.background = element_rect(fill = "#c4b3a3",
                                    colour = "#c4b3a3",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#6e6a67"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#6e6a67"))+
  scale_fill_manual(values=c( "#5e3712",	"#7a49a5","#4974a5"))

#Viz 2 Annual GDP Vs Co2 Emissions Per Capita in Indonesia

GCo_join %>%
  filter(Country == "Indonesia"& Year > 1990)%>%
  ggplot()+
  geom_line(aes(x=Year, y= Co2_p*2000,color = 'CO2/Capita'), stat="identity",size = 1.5) +
  geom_line(aes(x=Year, y= GDP,color = 'GDP/Capita'), size = 1.5)+
  scale_y_continuous(name = "GDP/Capita",
                     sec.axis = sec_axis(trans=~./2000, name="Co2/Capita"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 4))+
  
  labs(title ="Annual GDP Vs Co2 Emissions Per Capita in Indonesia",
       caption = "Correlation Coefficient is 0.9001 which proves strong \n positive correlation  for GDP and Co2 Emissions in Indonesia")+
  scale_color_manual(values = c('#fa1ef6','#94f211'))+
  theme(
    legend.position = c(.20, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4,3, 3, 3),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = '#a8876a'),
    legend.title = element_blank(),
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size=7),
    plot.background = element_rect(fill = '#c4b3a3'),
    panel.background = element_rect(fill = "#c4b3a3",
                                    colour = "#c4b3a3",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#6e6a67"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#6e6a67"))
##Viz 3 Generated Electricity (TW) 1985- 2020
Energy %>%
  ggplot()+ 
  geom_area(aes(x=Year,y= Energy, fill = Energy_Type),color = "black",alpha = 0.6)+
  labs(y ="Generated Electricity (TW)", title = 'Generated Electricity (TW) 1985- 2020')+
  scale_y_continuous(breaks = seq(0, 28000, by = 6000))+
  theme(
    legend.position = c(.40, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4,3, 3, 3),
    legend.background = element_rect(fill = '#a8876a'),
    legend.box.background =  element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, 'cm'),
    legend.text = element_text(size=10),
    plot.background = element_rect(fill = '#c4b3a3'),
    panel.background = element_rect(fill = "#c4b3a3",
                                    colour = "#c4b3a3",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#6e6a67"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#6e6a67"))+
  scale_fill_manual(values=c( "#5e3712",	"#7a49a5","#4974a5"))
#Viz 4 
Co2 %>%
  ggplot() + geom_area(aes(x = Year,y=Co2),fill ='#5e3712',color ="black")+
  labs(title = 'Annual Global Co2 Emissions from fossil fuel 1900-2020'
  )+ theme(
    plot.background = element_rect(fill = '#c4b3a3'),
    panel.background = element_rect(fill = "#c4b3a3",
                                    colour = "#c4b3a3",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#6e6a67"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#6e6a67")
  )





# Viz 5 Annual GDP Vs Co2 Emissions Per Capita in Sweden
GCo_join %>%
  filter(Country == "Sweden"& Year > 1975)%>%
  ggplot()+
  geom_line(aes(x=Year, y= Co2_p*4500,color = 'CO2/Capita'), stat="identity",size = 1.5) +
  geom_line(aes(x=Year, y= GDP,color = 'GDP/Capita'), size = 1.5)+
  scale_y_continuous(name = "GDP/Capita",
                     sec.axis = sec_axis(trans=~./4500, name="Co2/Capita"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  
  labs(title ="Annual GDP Vs Co2 Emissions Per Capita in Sweden",
       caption = "*Carbon Tax Implementation Started on 1991
       Correlation Coefficient = -0.829 (Strong Negative Correlation)")+
  scale_color_manual(values = c('#fa1ef6','#94f211'))+
  theme(
    legend.position = c(.20, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4,3, 3, 3),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = '#a8876a'),
    legend.title = element_blank(),
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size=7),
    plot.background = element_rect(fill = '#c4b3a3'),
    panel.background = element_rect(fill = "#c4b3a3",
                                    colour = "#c4b3a3",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#6e6a67"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#6e6a67"))+
  geom_segment(aes(x = 1988,
                   y = 40000,
                   xend = 1991,
                   yend = 28000),
               arrow = arrow(length = unit(0.3, "cm")))+
  annotate("text", x=1988,y=41500,label= "Carbon Tax, 1991*"
           ,color = "Black",fontface = "bold",size = 3.5)

# Viz 6 Annual GDP Vs Co2 Emissions Per Capita in India

GCo_join %>%
  filter(Country == "India"& Year > 1990)%>%
  ggplot()+
  geom_line(aes(x=Year, y= Co2_p*1000,color = 'CO2/Capita'), stat="identity",size = 1.5) +
  geom_line(aes(x=Year, y= GDP,color = 'GDP/Capita'), size = 1.5)+
  scale_y_continuous(name = "GDP/Capita",
                     sec.axis = sec_axis(trans=~./1000, name="Co2/Capita"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 4))+
  
  labs(title ="Annual GDP Vs Co2 Emissions Per Capita in India",
       caption = "Correlation Coefficient is 0.972 which proves strong positive correlation for GDP and Co2 Emissions in India")+
  scale_color_manual(values = c('#fa1ef6','#94f211'))+
  theme(
    legend.position = c(.20, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4,3, 3, 3),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = '#a8876a'),
    legend.title = element_blank(),
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size=7),
    plot.background = element_rect(fill = '#c4b3a3'),
    panel.background = element_rect(fill = "#c4b3a3",
                                    colour = "#c4b3a3",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#6e6a67"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#6e6a67"))
