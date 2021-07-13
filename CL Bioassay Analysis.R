library(tidyverse)
library(MaizePal)
install.packages("devtools")
devtools::install_github("AndiKur4/MaizePal")
install.packages("ggplot2")
install.packages("dplyr")
library(RColorBrewer)
library(scales)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/hosan/OneDrive/Kasson Lab EHS/Lab Data")
CL.dat <- read.csv("Bioassay 1.csv")

CL.dat2 <-  CL.dat %>%
  select(Day, Treatment, Solvent, Asymptomatic, Symptomatic, Dead, Mycosed) %>%
  pivot_longer(cols=c(-Day, -Treatment, -Solvent), names_to="Status", values_to = "Individuals") %>%
  mutate(Status=as.factor(Status)) %>%
  mutate(Status=factor(Status, levels=levels(Status)[c(3, 2, 4, 1)])) %>%
  drop_na(Individuals)

  
  
write.csv(CL.dat2,"Bioassay 1a.csv", row.names = FALSE)
palette1 <- maize_pal("HopiBlue")

theme <- theme_bw() + theme(text = element_text(size=20), axis.title.x = element_text(size=20), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), title = element_text(size=25), legend.title = element_text(size=20), legend.text = element_text(size=15), plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(CL.dat2, aes(x=Day, y=Individuals, fill=Status))+
  scale_fill_manual(values=palette1)+ theme+
  geom_area()+facet_wrap(~paste(Treatment, Solvent, sep="-"))

ggplot(CL.dat2, aes(x=Day, y=Individuals, fill=paste(Treatment, Solvent, sep="-")))+geom_col(position="dodge")+
  scale_fill_manual(values=palette1)+ theme+
  facet_wrap(~Status)+labs(fill='Treatment') 

ggplot(CL.dat2, aes(x=Day, y=Individuals, fill=paste(Treatment, Solvent, sep="-")))+geom_col()+
  scale_fill_manual(values=palette1)+ theme+
  facet_wrap(~Status)+labs(fill='Treatment') 

ggplot(CL.dat2, aes(x=Day, y=Individuals, fill=Status))+geom_col()+
  scale_fill_manual(values=palette1)+ theme+
  facet_wrap(~paste(Treatment, Solvent, sep="-"))+labs(fill='Treatment') 

ggplot(CL.dat2, aes(x=Day, y=Individuals, color=Status))+geom_line(size=2)+
  scale_color_manual(values=palette1)+ theme+
  facet_wrap(~paste(Treatment, Solvent, sep="-"),scales="free")

ggplot(CL.dat2, aes(x=Day, y=Individuals, color=Status))+geom_area()+
  scale_color_manual(values=palette1)+ theme+
  facet_wrap(~paste(Treatment, Solvent, sep="-"),scales="free")
