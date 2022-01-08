library(dplyr)
library(ggplot2)
library(tidyverse)
library("ggpubr")
library(readxl)
library(zoo)
library(lubridate)
library(writexl)
library(tidyr)

setwd("D:/Data Sains/Produksi PPN Pemangkat")
mydata <- read_excel("ikan_ekonomis.xlsx")

produksi <- data_ppn_pemangkat %>% 
  group_by(Tahun = tahun) %>% 
  summarise(Produksi = sum(produksi))

#Plot Trend Produksi
plot_produksi <- ggplot(produksi, aes(x=Tahun, y=Produksi)) + 
  geom_bar(stat='identity',width = 0.5, fill="#3399FF") +
  geom_line(size=0.75) + geom_point(colour='blue',size=3) +
  coord_cartesian(ylim=c(5000000,12000000)) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Tahun") + ylab("Produksi(Kg)") +
  geom_text(aes(label=prettyNum(Produksi,big.mark=".",scientific=FALSE), vjust=-2), size = 3)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_rect(fill = "white", colour = "grey50")
  )
plot_produksi

ikan_ekonomis <- data_ppn_pemangkat %>%
  group_by(jenis_ikan = ikan) %>%
  summarise(Produksi = sum(produksi), Nilai = format(round(mean(nilai), digits = 0), scientific = FALSE)) %>%
  arrange(desc(Produksi)) %>% head(10)

write_xlsx(ikan_ekonomis, "D:\\Data Sains\\Produksi PPN Pemangkat\\ikan_ekonomis.xlsx")

#Bar and Line
summary(mydata)
plot_ikan_ekonomis <- mydata %>% ggplot()+
  geom_bar(mapping = aes(x=Ikan, y=Produksi * 2124066/10000000), stat="identity", fill = "#3399FF") + 
  geom_point(mapping = aes(x = Ikan, y = Nilai)) +
  geom_line(mapping = aes(x = Ikan, y = Nilai, group=1)) +
  scale_y_continuous(name = expression("Nilai Produksi (Rp)"), sec.axis = sec_axis(~ . * 10000000 / 2124066 , name = "Produksi (Kg)"), limits = c(0,2124066))+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

plot_ikan_ekonomis

#Plot Ikan Ekonomis Penting
scaleFactor <- max(mydata$Produksi)/ max(mydata$Nilai)
plot_ikan_ekonomis1 <- ggplot(mydata, aes(x=Ikan,  width=.4)) +
  geom_col(aes(y=Produksi), fill="#1974D2", position = position_nudge(x = -.4)) +
  geom_col(aes(y=Nilai * scaleFactor), fill="#FF4500") +
  scale_y_continuous(labels = scales::comma,name="Produksi (Kg)", sec.axis=sec_axis(~./scaleFactor, name="Nilai Produksi (Rp)", labels = scales::comma)) +
  theme(
    axis.title.y.left=element_text(color="#1974D2"),
    axis.text.y.left=element_text(color="#1974D2"),
    axis.title.y.right=element_text(color="#FF4500"),
    axis.text.y.right=element_text(color="#FF4500"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.line = element_line(colour = "black")
  ) 
plot_ikan_ekonomis1
options(scipen = 999)
