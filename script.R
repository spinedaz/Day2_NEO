
library(tidyverse)
library(here)
library(gridExtra)
library(ggpubr)
library(camcorder)
library(showtext)
library(ggtext)


# Data from university research personnel 

data2 <- readxl::read_xlsx(here("Data/RH.03.PDINV.-PDI-Doctor_descarregar.xlsx"),
                           col_names = F)


data2 <- data2[-(1:4),] # Delete 3 first rows
colnames <- data2[1, ] # create vector with the names of the first row
colnames(data2) <- colnames #transfer vector names to column names

data2 <- data2[-1, ]  # final dataset



# - -----------------------------------------------------------------------

# Summarize data by gender

data2 <- data2 %>% 
  rename("Gender" = "Sexe",
         "Year" = "Any",
         "Number_of_Doctors" = "PDI Doctor",
         "Country" = "País") %>% 
  mutate(Number_of_Doctors = as.numeric(Number_of_Doctors)) %>% 
  drop_na()
 
a <- data2 %>% 
  group_by(Gender, Year) %>% 
  summarise(n=sum(Number_of_Doctors)) %>% 
  mutate(Gender = case_when(Gender == "DONA" ~ "Women",
                            Gender == "HOME" ~ "Man")) %>% 
  drop_na() #empty fields on number of doctors 


# Summarize the number of Spanish and foreign doctors for each year
summary_data <- data2 %>% 
  group_by(Year, Country) %>%
  summarise(n = sum(Number_of_Doctors)) %>%
  pivot_wider(names_from = Country, values_from = n, values_fill = 0) %>%
  mutate(total = Espanyols + Estrangers)

# Plot the bar plot
ggplot() +
  geom_bar(data=a , aes(x = Year, y = n, fill = Gender), position = "dodge", stat = "identity", color="white") +
  geom_text(data = summary_data, aes(x = Year, y = total, label = Espanyols), vjust = 8, hjust = 1, color = "lightgrey") +
  geom_text(data = summary_data, aes(x = Year, y = total, label = Estrangers), vjust = 10, hjust = 1, color = "white") +
  labs(title = "Research teaching staff
  in Catalonian universities",
       caption = " **Plot** | @PinusPineda and @guiriflautico",
  subtitle = "PDI investigadors dades obertes Catalunya") +
  ylab("Number of doctors") +
  scale_fill_manual(values = gender_palette) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.title = element_blank()
        ) +
  theme(
    plot.background = element_rect(fill="#222725",color=NA),
    plot.title = element_text(family="cata",size=18,hjust=0.5, vjust= 9,color="white",face='bold',margin=margin(1.5,0,0,0,'cm')),
    plot.subtitle = element_text(family="ral",size=12,hjust=0.5,vjust = 12,color="white",margin=margin(0.25,0,-1.5,0,'cm')),
    plot.caption = element_markdown(family="ral",size=10,hjust=0.5,vjust=-0.65, color="white",margin=margin(-1.25,0,0.5,0,'cm'))
  ) +
  annotate("text", x = 5.2, y = 9800, label = "Nationals \n Foreigners",
           hjust = 0.3, vjust = 2.8, size = 4, color = "white")

  
 gender_palette <- c("Man" = "#00FF41", "Women" = "black")


