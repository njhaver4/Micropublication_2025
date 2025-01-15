rm(list = ls())

##Figures for the micropub##
#Figure 1A: map of the location of the 10 strains
#Figure 1B: Scatter plot showing Spearman's correlation between substrate temperature and ambient temperature
#Figure 1C: the plot showing all the strains at the 4 tempertures

library(ggplot2)
library(tidyr)
library(ggplot2)
library(dplyr)

library(ggrepel)
#install.packages("cowplot")
library(cowplot)
library(RColorBrewer)

##To set colors for each strain:

display.brewer.pal(10, "Paired")
brewer.pal(10, "Paired")


####Go to R color brewer pelletes


strain.colors <- c("ECA1637"="#A6CEE3", "ECA2115"="#1F78B4", "ECA2373"="#B2DF8A",
                 "ECA2299"="#33A02C", "ECA1657"="#FB9A99", 
                 "ECA1443" = "#E31A1C", "ECA1170" = "#FDBF6F", "ECA1129" = "#FF7F00",
                 "ECA1439" = "#CAB2D6", "ECA1429" = "#6A3D9A")

######For figure 1C############
#Load your data
data_30<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/c_briggsae/Cbriggsae_temp_adaptation/Raw_data/Brood_size_30C_morereplicates.csv",header = TRUE,sep = ',')
data_25<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/c_briggsae/Cbriggsae_temp_adaptation/Raw_data/Brood_size_25C_morereplicates.csv",header = TRUE,sep = ',')
data_20<-read.csv(file ="/home/njhaver4/vast-eande106/projects/Nikita/c_briggsae/Cbriggsae_temp_adaptation/Raw_data/Brood_size_20C_morereplicates.csv", header = TRUE, sep = ',')
data_15<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/c_briggsae/Cbriggsae_temp_adaptation/Raw_data/Brood_size_15C_morereplicates.csv", header = TRUE, sep = ',')


# convert wide format into long format and add temperature info
data_long_30C <- data_30 %>%
  dplyr::mutate(repID=paste0("rep","_",row_number())) %>%
  tidyr::pivot_longer(cols = -c(repID))

colnames(data_long_30C)<-c("Rep","Strains","Brood_size")
data_long_30C <- data_long_30C %>% 
  dplyr::mutate(Temperature="30\u00B0C") %>%
  dplyr:::mutate(Condition = ifelse(
    Strains %in% c("ECA1443", "ECA1170", "ECA1129", "ECA1439", "ECA1429"),
    "Hot",
    "Cold"))

data_long_25C <- data_25 %>%
  dplyr::mutate(repID=paste0("rep","_",row_number())) %>%
  tidyr::pivot_longer(cols = -c(repID))



colnames(data_long_25C)<-c("Rep","Strains","Brood_size")
data_long_25C <- data_long_25C %>% 
  dplyr::mutate(Temperature="25\u00B0C") %>%
  dplyr:::mutate(Condition = ifelse(
    Strains %in% c("ECA1443", "ECA1170", "ECA1129", "ECA1439", "ECA1429"),
    "Hot",
    "Cold"))


data_long_20C <- data_20 %>%
  dplyr::mutate(repID=paste0("rep","_",row_number())) %>%
  tidyr::pivot_longer(cols = -c(repID))


colnames(data_long_20C)<-c("Rep","Strains","Brood_size")
data_long_20C <- data_long_20C %>% 
  dplyr::mutate(Temperature="20\u00B0C") %>%
  dplyr:::mutate(Condition = ifelse(
    Strains %in% c("ECA1443", "ECA1170", "ECA1129", "ECA1439", "ECA1429"),
    "Hot",
    "Cold"))

data_long_15C<- data_15 %>%
  dplyr::mutate(repID=paste0("rep","_",row_number())) %>%
  tidyr::pivot_longer(cols = -c(repID))

colnames(data_long_15C)<-c("Rep","Strains","Brood_size")
data_long_15C <- data_long_15C %>% 
  dplyr::mutate(Temperature="15\u00B0C") %>%
  dplyr:::mutate(Condition = ifelse(
    Strains %in% c("ECA1443", "ECA1170", "ECA1129", "ECA1439", "ECA1429"),
    "Hot",
    "Cold"))

# merge long format data set
data_long_tmp<-rbind(data_long_15C,
                     data_long_20C,
                     data_long_25C,
                     data_long_30C)

# add Hot or Cold info
data_long<-data_long_tmp %>%
  dplyr::mutate(Condition = ifelse(
    Strains %in% c("ECA1443", "ECA1170", "ECA1129", "ECA1439", "ECA1429"),
    "Hot",
    "Cold")) %>%
  dplyr::mutate(Strains = forcats::fct_relevel(Strains, c("ECA1637", "ECA1657", "ECA2115", "ECA2299", "ECA2373",
                                                         "ECA1129", "ECA1170", "ECA1429", "ECA1439", "ECA1443"))) %>%
  dplyr::mutate(Temperature=gsub("°C","",Temperature))


# plot
temp.colours<-c("Hot"= "#FC8D62","Cold"="#8DA0CB")


#########Figure 1C (p2)####################################
data_plot <- data_long %>% 
  dplyr::group_by(Temperature,Strains) %>%
  dplyr::mutate(gi=cur_group_id()) %>%
  dplyr::mutate(
               avgBS = mean(Brood_size,na.rm=T),
               sem = sd(Brood_size, na.rm = T)/ sqrt(n())
                ) %>%# Calculate SEM
  ##write a line here to mutate SEM (this will add a column)
  dplyr::distinct(gi,.keep_all = T) %>%
  dplyr::select(-Brood_size,-Rep,-gi) %>%
  dplyr::ungroup() #%>%
  #dplyr::mutate(Temperature=gsub("°C","",Temperature))

facet_labels <- c("Hot" = "High-temperature isolates",
                  "Cold" = "Low-temperature isolates")

p5 <- ggplot(data_plot, aes(x = Temperature, y = avgBS, color = Strains, group = Strains)) +
  # Add jittered points for individual data
  geom_jitter(data = data_long, aes(y = Brood_size), width = 0.2, alpha = 0.5, size = 1.5) +
  # Add error bars
  geom_segment(aes(
    x = Temperature, xend = Temperature,
    y = avgBS - sem, yend = avgBS + sem, color = Strains
  )) +
  # Add average points
  geom_point(size = 2, shape = 15) +
  # Add connecting lines
  geom_line() +
  # Facet by condition
  facet_wrap(~Condition, nrow = 1, ncol = 2, labeller = labeller(Condition = facet_labels)) +
  # Add labels
  ylab("Average lifetime fecundity") +
  xlab("Temperature (°C)") +
  # Custom color scale
  scale_color_manual(values = strain.colors) +
  # Theme customization
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

p5

##############To make a scatter plot of substrate_Temp vs ambient_temp - figure 1B#################
#Read the raw data file
data_subs_amb<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/c_briggsae/Cbriggsae_temp_adaptation/Raw_data/Substratetemp_Ambienttemp.csv",header = TRUE,sep = ',')

# Calculate Spearman's correlation coefficient
cor_test <- cor.test(data_subs_amb$Substrate_temp, data_subs_amb$Ambient_Temp, method = "spearman")

cor_value <- cor_test$estimate
p_value <- cor_test$p.value


# Plot
p3 <- ggplot(data_subs_amb, aes(x = Substrate_temp, y = Ambient_Temp, color = Strains)) +
  geom_point() +
  
  #Add best-fit line
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  
  #Add reference line (slope = 1, intercept = 0)
  geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_color_manual(values = strain.colors) + 
  # Customize theme
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        #panel.grid.major = element_line(color = "lightgrey"),
       legend.position = "none",  #Hide the legend 
         #Set the axis titles size
       axis.title.x = element_text(size = 10),
       axis.title.y = element_text(size = 10),
       
         #Set the axis text (Labels) size
       axis.text.x = element_text(size = 10),
       axis.text.y = element_text(size = 10)) +       
         
  # Add title and axis labels
  labs(x = expression("Substrate temperature (°C)"),
       y = expression("Ambient temperature (°C)")) +
  
  # Annotate with Spearman's correlation coefficient
  annotate("text", x = (min(data_subs_amb$Substrate_temp) + max(data_subs_amb$Substrate_temp))/2, 
           y = max(data_subs_amb$Ambient_Temp), 
           label = paste("Spearman's ρ =", round(cor_value, 2)),
                        # "\nP-value =", round(p_value, 2)),
           hjust = 0.5, vjust = 1.1, size = 3.5)

# Display the plot
p3


###########To plot figure 1A##################### Map showing the locations of the 10 strains

#Read the raw file#
all_data_C.briggsae_Strains <- readxl::read_excel("/home/njhaver4/vast-eande106/projects/Nikita/c_briggsae/cbriggsae_popgen/raw_data/C. briggsae WI strain info.xlsx")
all_data_C.briggsae_Strains$latitude<-ifelse(all_data_C.briggsae_Strains$latitude == "NA",NA,all_data_C.briggsae_Strains$latitude)

#Filter the 10 strains#
ten_strains <- all_data_C.briggsae_Strains %>%
  dplyr::filter(strain == isotype)  %>% 
  dplyr::filter(isotype %in% c("ECA1637", "ECA2115", "ECA2373", "ECA2299", "ECA1657",
                               "ECA1443", "ECA1170", "ECA1129", "ECA1439", "ECA1429")) %>%
  dplyr::select(isotype,latitude,longitude)%>%
  dplyr::filter(!is.na(latitude) & !is.na(longitude)) %>%
  dplyr::rename("lat"="latitude", "long"="longitude") 
colnames(ten_strains)

#Rename the column isotype to Strains
ten_strains <- ten_strains %>%
              rename (Strains = isotype)
          

ten_strains$lat<-as.numeric(ten_strains$lat)
ten_strains$long<-as.numeric(ten_strains$long)

#1. Hawaii
df_hw_isotype <- ten_strains %>%
  dplyr::filter(long > -176  & long < -129 & lat > 4 & lat < 46) 
hw_isotype <- as.character(df_hw_isotype$Strains)

#plot the map

world <- map_data('world')
world <- world[world$region != "Antarctica",] # intercourse antarctica

top <- c("ECA1443", "ECA1439", "ECA1429")
mix <- c("ECA1129")
mix2 <- c("ECA1170")

plot_hw <- ggplot()+ geom_map(data=world, map=world,
                              aes(map_id=region),
                              color="black", fill="#F7F7F7", size=0.2)+
  geom_point(data = ten_strains, aes(x=as.numeric(long), y=as.numeric(lat), color = Strains), shape =16, size =2,) +
  geom_text_repel(data = ten_strains %>% dplyr::filter(Strains %in% top),
                  aes(x=as.numeric(long), y=as.numeric(lat), label = Strains, color = Strains), 
                  size = 3.5,
                  force = 800, #111
                  seed = 21957, #363 #900 #1000 #21957
                  box.padding=0.5,
                  xlim = c(-160,-156),
                  ylim = c(22.7,23)) +
  geom_text_repel(data = ten_strains %>% dplyr::filter(!(Strains %in% top) & !(Strains %in% mix) & !(Strains %in% mix2)),
                  aes(x=as.numeric(long), y=as.numeric(lat), label = Strains, color = Strains), 
                  size = 3.5,
                  force = 1000, #111
                  seed = 459, #363 #900 #1000 #21957
                  box.padding=0.5,
                  xlim = c(-159.4,-159.4),
                  #xlim = c(-159.3,-159.3),
                  ylim = c(18.7,21.5)) +
  geom_text_repel(data = ten_strains %>% dplyr::filter(Strains %in% mix),
                  aes(x=as.numeric(long), y=as.numeric(lat), label = Strains, color = Strains), 
                  size = 3.5,
                  force = 800, #111
                  seed = 21957, #363 #900 #1000 #21957
                  box.padding=0.5,
                  xlim = c(-155.5,-155.9),
                  ylim = c(21.3,21.1)) +
  geom_text_repel(data = ten_strains %>% dplyr::filter(Strains %in% mix2),
                  aes(x=as.numeric(long), y=as.numeric(lat), label = Strains, color = Strains), 
                  size = 3.5,
                  force = 800, #111
                  seed = 21957, #363 #900 #1000 #21957
                  box.padding=0.5,
                  xlim = c(-156.5,-155.9),
                  ylim = c(22.3,22.1)) +
                  
  ##Add the geomtext repel function
  scale_color_manual(values = strain.colors)  +
  theme_void()+
  
  theme(text = element_text(size=12)) +
  lims(x=c(-160.3, -155),y=c(18.7,23)) +  # change the range of zoom-in range 
  theme(legend.position = "none")+
  geom_text(aes(x = -160, y = 17.5, label = "Hawaii"), hjust = 0, vjust = 0, size = 4)+
  labs(color = NULL)+
  theme(axis.text = element_blank(),    # Conceal Tick Marks
        axis.title = element_blank())   # Conceal Tick Marks

plot_hw

#####PLOT A COW PLOT:
first_row <- plot_grid(plot_hw, p3, ncol = 2,
                       labels = c("A", "B"))

first_row

combined_plot <- plot_grid(first_row, p5, 
                           ncol=1, 
                           labels = c(NA, "C"),
                           rel_heights =c(1,1))
combined_plot


ggsave("/vast/eande106/projects/Nikita/c_briggsae/Cbriggsae_temp_adaptation/Micropublication/Plots/Manuscriptplot_20241206_withjitterpoints.png", plot = combined_plot, width = 7.5, height = 7.5* .75, unit = "in", dpi = 600)


#########To do the statistics
##Wilcoxon test:for differences between cold and hot adapted strains at each temperatures
wilcox.test(Brood_size ~ Condition, data = data_long_30C, exact = FALSE)
wilcox.test(Brood_size ~ Condition, data = data_long_25C, exact = FALSE)
wilcox.test(Brood_size ~ Condition, data = data_long_20C, exact = FALSE)
wilcox.test(Brood_size ~ Condition, data = data_long_15C, exact = FALSE)
