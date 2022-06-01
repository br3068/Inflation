library(WDI)
library(tidyverse)
library(htmlwidgets)
library(stringr)
library(plotly)

theme_ben <- function(){ 
  font <- "serif"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 11,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,                #Centre align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 9),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 9),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis family
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 9))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}





# Figure 2.2 Australia Inflation
## WB Get data
Au <- WDI(
  country = "AU",
  indicator = c("Inflation"= "FP.CPI.TOTL.ZG", "GDP_per_capita" = "NY.GDP.PCAP.KD.ZG"),
  start = 1961,
  end = 2020
)

Aul <- Au %>% 
  select(year, Inflation, GDP_per_capita) %>% 
  pivot_longer(!year, names_to = "Indicator", values_to = "Percent") %>% 
  rename(Year=year) 

Aul$Percent<-round(Aul$Percent, 1)



ggplot(Aul) + aes(x = Year, y = Percent, colour = Indicator) %>% 
  geom_line()+
  geom_text(aes(label=text), family="Times", fontface="italic", lineheight=.8) 



## Convert to wide
Auw <- Aul %>% 
  pivot_wider(names_from =  "Indicator", values_from = Percent) %>% 
  rename(`GDP per capita` = GDP_per_capita)

Auw



# Maddison data
# 
url1 <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx" 

mpgdp<-rio::import(file = url1,which = 4, skip=1) 

head(mpgdp)

apgdp <- mpgdp %>% 
  select(year,AUS) %>% 
  filter(year >= 1945 & year<=1960) %>% 
  mutate(`GDP per capita`=(AUS/lag(AUS)-1)*100) %>% 
  select(-AUS, Year=year) %>% 
  filter(!Year == 1945)
apgdp

# Git ya CPI bit 
acpi <- read.csv("Au_CPI1.csv") %>% 
  inner_join(apgdp, by = "Year") %>% 
  bind_rows(Auw)

acpi$`GDP per capita`<-round(acpi$`GDP per capita`,1)


acpi1<- acpi %>% 
  filter(Year <= 1970) %>% 
  mutate(minflation=mean(Inflation)) %>% 
  mutate(mgrth=mean(`GDP per capita`)) 
acpi1

acpi2<- acpi %>% 
  filter(Year > 1970 & Year <= 1990 ) %>% 
  mutate(minflation=mean(Inflation)) %>% 
  mutate(mgrth=mean(`GDP per capita`)) 
acpi2

acpi3<- acpi %>% 
  filter(Year > 1990 & Year <= 2020 ) %>% 
  mutate(minflation=mean(Inflation)) %>% 
  mutate(mgrth=mean(`GDP per capita`)) 
acpi3

acpi <- bind_rows(acpi1,acpi2,acpi3) %>% 
  rename('Long-run Ave Inflation'=minflation) %>% 
  rename('Long-run Ave GDP Per capita change'= mgrth)

acpi$'Long-run Ave Inflation'<-round(acpi$'Long-run Ave Inflation', 1)
acpi$'Long-run Ave GDP Per capita change'<-round(acpi$'Long-run Ave GDP Per capita change', 1)

## Create widget table

library(DT)
Auwd<- datatable(acpi,
          options = list(pageLength = 72),
          rownames = FALSE,
          
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color:black; font-size:110% ;','Table 2.1: Inflation and Per Capita Income Annual Percentage Change in Australia, 1949-2020'))

          # caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: right; color:black; font-size:90% ;','Source: ABS (2022); Maddison (2022); World Bank (2022)')
          

Auwd

# Export to html
library(htmlwidgets)
saveWidget(Auwd,"Tab2_1.html")


## Create widget figure
Aulg <- acpi %>% 
  pivot_longer(-Year,names_to = "Indicator", values_to = "Percent")
Aulg$Percent<-round(Aulg$Percent,1)

library(plotly)
plot1 <- ggplot(Aulg) + aes(x = Year, y = Percent, colour = Indicator) %>% 
  geom_line()+
  # scale_fill_discrete(labels = c("GDP per capita", "Inflation"))+
  theme_ben()+
  theme(legend.position="bottom")+
  ggtitle("Figure 2.2: Annual % Change in GDP Per capita and CPI in Australia, 1949-2020")
plotly1 <- ggplotly(plot1)
plot1
# Export to html
library(htmlwidgets)
saveWidget(plotly1,"fig2_2.html")







# Figure 1.1
# Australia and OECD
OECD <- inflation_OECD_HIC <- read_csv("inflation_OECD_HIC.csv")


OECD2 <- OECD %>% 
  filter(SUBJECT=="TOT") %>% 
  rename(Entity = LOCATION, Indicator = SUBJECT) %>% 
  arrange(Value) %>% 
  mutate(col=if_else(Value == 7.9045310, "OECD", ""))
  
  
OECD2$Indicator <- str_replace_all(OECD2$Indicator, "TOT","Total")
OECD2$Value<- round(OECD2$Value, 1)


plot2 <- ggplot(OECD2, aes(fill=col, y=Value, x=reorder(Entity,Value))) + 
  geom_bar(stat="identity") + 
  theme_ben()+
  theme(legend.position = "none")+
  ggtitle("Figure 1.1: First Quarter 2022 Inflation: Australia and OECD High-Income Countries compared")+
  labs(caption="Source: OECD (2022).")+
  xlab("Entity")+
  ylab("Per cent")
plot2

plotly2 <- ggplotly(plot2)
plotly2

# Export to html
library(htmlwidgets)
saveWidget(plotly2,"fig1_1.html")


# Fig 2.1 Time line 
library(vistime)
time_line <- read_csv("time_line.csv")

fig_2_1 <- gg_vistime(time_line, 
        col.event = "event", 
        col.start = "start",
        col.end = "end", 
        col.group = "group", 
        col.color = "color", 
        col.fontcolor = "fontcolor",
        col.tooltip = "tooltip", 
        optimize_y = TRUE, 
        linewidth = NULL, 
        title = "Figure 2.1: Australia and International Post-War Economic Timeline", 
        show_labels = TRUE, 
        background_lines = NULL)  
fig_2_1

fig_2_1p <- vistime(time_line, 
                      col.event = "event", 
                      col.start = "start",
                      col.end = "end", 
                      col.group = "group", 
                      col.color = "color", 
                      col.fontcolor = "fontcolor",
                      col.tooltip = "tooltip", 
                      optimize_y = TRUE, 
                      linewidth = NULL, 
                      title = "Figure 2.1: Australia and International Post-War Economic Timeline", 
                      show_labels = TRUE, 
                      background_lines = NULL)  
fig_2_1p


saveWidget(fig_2_1p,"fig_2_1.html")
