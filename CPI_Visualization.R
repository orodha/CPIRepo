
# Creating plots to visualize the trends of cpi
#----------------------------------------------

plot1 <- ggplot(combined_cpi%>%filter(Products=="GENERAL INDEX (CPI)"))+
  geom_line(aes(x=Date, y=CPI, colour=Source))+
  scale_x_date(date_labels = "%b %y", breaks = "18 months")+
  labs(x="", y="Index",
       title="General Index (CPI)",
       caption="Source: NISR")+
  theme_classic()
plot1


plot2 <- ggplot(combined)+
  geom_line(aes(x=Date, y=Index, colour=Source))+
  facet_wrap(~Products)
plot2
