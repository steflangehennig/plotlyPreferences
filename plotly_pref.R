setwd("C:/Users/stefa/Dropbox/Dissertation/Data/Budget & Appropriations/Appropriations")

data <- read.csv(file = "approp bills info - for analysis 052118.csv")
theme_set(theme_classic()) ## can turn this off to get gray background/grid
theme_set(theme_bw())

library(plotly, magrittr)
install.packages('plotly', dependencies = TRUE)
library(shiny)

### set sytem environment w/ username and api key ###

## for ag over time

data2 <- data[which(data$aw_coding=='agriculture'), ]

# fitting size of figure
m <- list(
  l = 100,
  r = 50,
  b = 50,
  t = 100,
  pad = 4
)

### appropriations comms ###
s <- seq(1977, 2016, by = 1)
p <- plot_ly(data2, x = ~s, y = ~hac_pref, name = 'HAC Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~sac_pref, name = 'SAC Preference', mode = 'lines+markers') %>%
  add_trace(y = ~exec_pref, name = 'Executive Preference', mode = 'lines+markers')%>%
  layout(margin = m,
      title = "Appropriations Committee Preferences - Ag",
       xaxis = list(title = "Years"),
       yaxis = list (title = "Dollars"))
p
htmlwidgets::saveWidget(p, "approp_pref_ag.html")

# defense
data2 <- data[which(data$aw_coding=='transportation-hud'), ]

s <- seq(2008, 2016, by = 1)
p2 <- plot_ly(data2, x = ~s, y = ~hac_pref, name = 'HAC Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~sac_pref, name = 'SAC Preference', mode = 'lines+markers') %>%
  add_trace(y = ~exec_pref, name = 'Executive Preference', mode = 'lines+markers')%>%
  layout(margin = m,
         title = "Appropriations Committee Preferences - Transportation-HUD",
         xaxis = list(title = "Years"),
         yaxis = list (title = "Dollars"))
p2
htmlwidgets::saveWidget(p2, "approp_pref_transhud.html")

api_create(p2, filename = "approp_pref_transhud")

test <- subplot(p, p2)
test

### conference ###
data2 <- data[which(data$aw_coding=='energy'), ]

s <- seq(1981, 2016, by = 1)
p2 <- plot_ly(data2, x = ~s, y = ~conf_house_pref, name = 'House Conference Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~conf_sen_pref, name = 'Senate Conferece Preference', mode = 'lines+markers') %>%
  add_trace(y = ~conf_exec_pref, name = 'Executive Preference', mode = 'lines+markers')%>%
  layout(margin = m,
         title = "Conference Committee Preferences - Energy",
         xaxis = list(title = "Years"),
         yaxis = list (title = "Dollars"))
p2
htmlwidgets::saveWidget(p2, "conf_pref_energy.html")



####### panel plots ########
data2 <- data[which(data$aw_coding=='agriculture'), ]
s <- seq(1977, 2016, by = 1)
p <- plot_ly(data2, x = ~s, y = ~hac_pref, name = 'House Appropriations Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~sac_pref, name = 'Senate Appropriations Preference', mode = 'lines+markers') %>%
  add_trace(y = ~exec_pref, name = 'Executive Preference', mode = 'lines+markers')
p


# defense
data2 <- data[which(data$aw_coding=='defense'), ]

s <- seq(1977, 2016, by = 1)
p2 <- plot_ly(data2, x = ~s, y = ~hac_pref, name = 'House Appropriations Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~sac_pref, name = 'Senate Appropriations Preference', mode = 'lines+markers') %>%
  add_trace(y = ~exec_pref, name = 'Executive Preference', mode = 'lines+markers')
p2

# labor
data2 <- data[which(data$aw_coding=='labor'), ]

s <- seq(1977, 2016, by = 1)
p3 <- plot_ly(data2, x = ~s, y = ~hac_pref, name = 'House Appropriations Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~sac_pref, name = 'Senate Appropriations Preference', mode = 'lines+markers') %>%
  add_trace(y = ~exec_pref, name = 'Executive Preference', mode = 'lines+markers')
p3

# transportation
data2 <- data[which(data$aw_coding=='transportation'), ]

s <- seq(1977, 2016, by = 1)
p4 <- plot_ly(data2, x = ~s, y = ~hac_pref, name = 'House Appropriations Preference', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~sac_pref, name = 'Senate Appropriations Preference', mode = 'lines+markers') %>%
  add_trace(y = ~exec_pref, name = 'Executive Preference', mode = 'lines+markers')
p4


sub1 <- subplot(p, p2, p3, p2, nrows = 2, margin = 0.05)
sub1

#########################################
# Panel plots w/ ggplot
#########################################

library(ggplot2)
### approps preferences over time, all approps bills ###
bp <- ggplot(data, aes(x=fy, group=1)) + 
  geom_line(aes(y = hac_min, colour = "House Preference")) +
  geom_line(aes(y = sac_min, colour = "Senate Preference")) +
  geom_line(aes(y = exec_min, colour = "Executive Preference")) +
  scale_color_manual(name="",
                     #breaks=c("conf_house_pref", "conf_sen_pref", "conf_exec_pref", "conf_final"),
                     #labels=c("House Preference", "Senate Preference", "Executive Preference", "Conference Final"),
                     values = c("green", "red", "blue")) +
  labs(title="",
       x="Year", y = "Dollars (millions)")
#bp+scale_colour_discrete(name = "") 

#bp + facet_grid(. ~ aw_coding)
to<-bp + facet_wrap(~ aw_coding, scales='free', ncol=4)  #+ scale_colour_discrete(name = "") 
print(to + ggtitle("Appropriations Committee Preferences"))

### conference preferences over time, all approps bills ###
bp2 <- ggplot(data, aes(x=fy, group=1)) + 
  geom_line(aes(y = conf_house_min, colour = "House Preference")) +
  geom_line(aes(y = conf_sen_min, colour = "Senate Preference")) +
  geom_line(aes(y = conf_exec_min, colour = "Executive Preference")) +
  geom_line(aes(y = conf_final_min, colour = "Conference Final"), linetype="dotted") +
  scale_color_manual(name="",
                    #breaks=c("conf_house_pref", "conf_sen_pref", "conf_exec_pref", "conf_final"),
                    #labels=c("House Preference", "Senate Preference", "Executive Preference", "Conference Final"),
                    values = c("black", "green", "red", "blue")) +
  labs(title="",
       x="Year", y = "Dollars (millions)")
#bp2 + scale_colour_discrete(name = "") 

#bp2 + facet_grid(. ~ aw_coding)
go<-bp2 + facet_wrap(~ aw_coding, scales='free', ncol=4)  #+ scale_colour_discrete(name = "") 
print(go + ggtitle("Conference Committee Preferences"))

### when do approps bills goes to conference? ###
as <- ggplot(data, aes(x = conference)) + 
  #scale_fill_manual(values = c("red", "blue")) +
  geom_bar(width = 0.4, color = "black") +
  scale_x_continuous(breaks = c(0,1), labels = c("No Conference", "Conference")) +
  labs(title="",
       x="", y = "Count")

as
as2 <- as + facet_wrap(~ aw_coding, scales = "free", ncol=4)
print(as2 + ggtitle("Frequency Appropriations Bills go to Conference"))

### when do approps bills get put into omnibus? ###
az <- ggplot(data, aes(x = omnibus)) + 
  #scale_fill_manual(values = c("red", "blue")) +
  geom_bar(width = 0.4, color = "black") +
  scale_x_continuous(breaks = c(0,1), labels = c("No Omnibus", "Omnibus")) +
  labs(title="",
       x="", y = "Count")

az
az2 <- az + facet_wrap(~ aw_coding, scales = "free", ncol=4)
print(az2 + ggtitle("Frequency Appropriations Bills in Omnibus"))

###############################################################################




