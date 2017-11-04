library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
#detach(package:plyr)
setwd("D:/Centotec")

Consumption <- read.csv("household_power_consumption.txt", TRUE, ";", na.strings = c("NA","?"))

#-------------------

### Pre-proceso de los datos. 
#Creamos una nueva columna con Fecha y Hora
Consumption <-cbind(Consumption,paste(Consumption$Date,Consumption$Time), stringsAsFactors=FALSE)
colnames(Consumption)[10] <-"DateTime"
Consumption <- Consumption[,c(ncol(Consumption), 1:(ncol(Consumption)-1))]
#Añadimos una columna con el GAP a W·h
Consumption <- cbind(Consumption, Consumption$Global_active_power*1000/60)
colnames(Consumption)[11] <- "Global_Consumption"
#Pasamos a númerico
Consumption$Global_active_power <- as.numeric(Consumption$Global_active_power)
Consumption$Global_reactive_power <- as.numeric(Consumption$Global_reactive_power)
Consumption$Voltage <- as.numeric(Consumption$Voltage)
Consumption$Sub_metering_1 <- as.numeric(Consumption$Sub_metering_1)
Consumption$Sub_metering_2 <- as.numeric(Consumption$Sub_metering_2)
Consumption$Sub_metering_3 <- as.numeric(Consumption$Sub_metering_3)
Consumption$Global_Consumption <- as.numeric(Consumption$Global_Consumption)
#Creamos una nueva columna con los d??as de la semana
Consumption <- cbind(Consumption,weekdays(as.Date(Consumption$Date, '%d/%m/%Y')), stringsAsFactors=FALSE)
colnames(Consumption)[12] <- "Day"
#Creamos una nueva columna con el mes
Consumption <- cbind(Consumption,months(as.Date(Consumption$Date, '%d/%m/%Y')), stringsAsFactors=FALSE)
colnames(Consumption)[13] <- "Month"
#Creamos una columna de mes/año
MonthYear <- separate(Consumption, Date, into=c("day", "month", "year"))
MonthYear <- paste(MonthYear$month, MonthYear$year, sep="/")
Consumption <-cbind(Consumption, MonthYear)
#Filtramos el dataset en datos de cada 15 minutos
Minute <- separate(Consumption, Time, into = c("Hour", "Minute", "Second"))
Minute <- Minute$Minute
Consumption <-cbind(Consumption, Minute)
Consumption$Minute <- as.numeric(Consumption$Minute)
Reduced_data <- subset(Consumption, Minute==1 | Minute==31)
#Añadimos columna de semanas 
semana <- c(1:205) 
initial <- c()
for (i in semana){
  initial <- c(initial, rep(semana[[i]], 10080))
}
Consumption$Week <- c(initial, rep(206, 8859))
#Dataset sin missing values
cleanConsumption <- na.omit(Consumption)

-------------------
  
  ###Enero
  january <- subset(Reduced_data, Month == "enero")
january_weekend <- subset(Reduced_data, Month == "enero" & (Day=="s?bado" | Day=="domingo"))
january_weekday <- subset(Reduced_data, Month == "enero" & Day!="s?bado" & Day!="domingo")
#Sub1
j_sub_1_wd <- ggplot(january_weekday, aes(x=Time)) + geom_point(aes(y=Sub_metering_1), color = "red")
j_sub_1_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 1") + ggtitle("Sub-Metering 1 en Enero, Lunes-Viernes")

-------------------
  
  j_sub_1_we <- ggplot(january_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_1), color = "blue")
j_sub_1_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 1") + ggtitle("Sub-Metering 1 en Enero, fin de semana")

#Sub2
j_sub_2_wd <- ggplot(january_weekday, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "red")
j_sub_2_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 2") + ggtitle("Sub-Metering 2 en Enero, Lunes-Viernes")

-------------------
  
  j_sub_2_we <- ggplot(january_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "blue")
j_sub_2_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 2") + ggtitle("Sub-Metering 2 en Enero, fin de semana")

-------------------
  
  #Sub3
  j_sub_3_wd <- ggplot(january_weekday, aes(x=Time)) + geom_point(aes(y=Sub_metering_3), color = "red")
j_sub_3_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 3") + ggtitle("Sub-Metering 3 en Enero, Lunes-Viernes")

-------------------
  
  j_sub_3_we <- ggplot(january_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "blue")
j_sub_3_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 3") + ggtitle("Sub-Metering 3 en Enero, fin de semana")

-------------------
  
  #GAP
  j_gap_wd <- ggplot(january_weekday, aes(x=Time)) + geom_point(aes(y=Global_Consumption),color = "red")
j_gap_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Consumo Global Activo") + ggtitle("Consumo Global Activo en Enero, Lunes-Viernes")

-------------------
  
  j_gap_we <- ggplot(january_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "blue")
j_gap_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Consumo Global Activo") + ggtitle("Consumo Global Activo en Enero, fin de semana")

-------------------
  
  
  ###Agosto
  august <- subset(Reduced_data, Month == "agosto")
august_weekend <- subset(Reduced_data, Month == "agosto" & (Day=="s?bado" | Day=="domingo"))
august_weekday <- subset(Reduced_data, Month == "agosto" & Day!="s?bado" & Day!="domingo")
#Sub1
a_sub_1_wd <- ggplot(august_weekday, aes(x=Time)) + geom_point(aes(y=Sub_metering_1), color = "red")
a_sub_1_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 1") + ggtitle("Sub-Metering 1 en Agosto, Lunes-Viernes")

-------------------
  
  a_sub_1_we <- ggplot(august_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_1), color = "blue")
a_sub_1_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 1") + ggtitle("Sub-Metering 1 en Agosto, fin de semana")

-------------------
  
  #Sub2
  a_sub_2_wd <- ggplot(august_weekday, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "red")
a_sub_2_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 2") + ggtitle("Sub-Metering 2 en Agosto, Lunes-Viernes")

-------------------
  a_sub_2_we <- ggplot(august_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "blue")
a_sub_2_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 2") + ggtitle("Sub-Metering 2 en Agosto, fin de semana")

-------------------
  #Sub3
  a_sub_3_wd <- ggplot(august_weekday, aes(x=Time)) + geom_point(aes(y=Sub_metering_3), color = "red")
a_sub_3_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 3") + ggtitle("Sub-Metering 3 en Agosto, Lunes-Viernes")
-------------------
  a_sub_3_we <- ggplot(august_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "blue")
a_sub_3_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Sub-Metering 3") + ggtitle("Sub-Metering 3 en Agosto, fin de semana")

-------------------
  #GAP
  a_gap_wd <- ggplot(august_weekday, aes(x=Time)) + geom_point(aes(y=Global_Consumption),color = "red")
a_gap_wd + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Consumo Global Activo") + ggtitle("Consumo Global Activo en Agosto, Lunes-Viernes")
-------------------
  a_gap_we <- ggplot(august_weekend, aes(x=Time)) + geom_point(aes(y=Sub_metering_2), color = "blue")
a_gap_we + theme(axis.text.x = element_text(size=6, angle=90)) + xlab("Hora") + ylab("Consumo Global Activo") + ggtitle("Consumo Global Activo en Agosto, fin de semana")
-------------------
  ###Calculamos las medias por cada hora en cada mes
  #Enero
  january_mean <- january %>%
  group_by(Time) %>%
  summarise(
    Sub_1_mean_january <- mean(Sub_metering_1, na.rm = TRUE),
    Sub_2_mean_january <- mean(Sub_metering_2, na.rm = TRUE),
    Sub_3_mean_january <- mean(Sub_metering_3, na.rm = TRUE),
    GAP_mean_january <- mean(Global_Consumption, na.rm = TRUE)
  )
colnames(january_mean) <- c("Time","Sub_1_mean","Sub_2_mean","Sub_3_mean","GAP_mean")
j_mean <- ggplot(january_mean, aes(x=Time, group=1)) + geom_line(aes(y=Sub_1_mean, color="Sub-Metering 1")) + 
  geom_line(aes(y=Sub_2_mean, color="Sub-Metering 2")) + 
  geom_line(aes(y=Sub_3_mean, color="Sub-Metering 3")) + 
  geom_line(aes(y=GAP_mean, color="Global Active Power")) +
  scale_color_manual("", breaks=c("Sub-Metering 1","Sub-Metering 2","Sub-Metering 3","Global Active Power"),
                     values=c("red","blue","brown","green"))
j_mean + theme(axis.text.x = element_text(size=6, angle=90)) + ylab("W?h") +
  labs(title="Consumo medio en un día de Enero") + xlab("Hora")
-------------------
  #Agosto
  august_mean <- august %>%
  group_by(Time) %>%
  summarise(
    Sub_1_mean_august <- mean(Sub_metering_1, na.rm = TRUE),
    Sub_2_mean_august <- mean(Sub_metering_2, na.rm = TRUE),
    Sub_3_mean_august <- mean(Sub_metering_3, na.rm = TRUE),
    GAP_mean_august <- mean(Global_Consumption, na.rm = TRUE)
  )
colnames(august_mean) <- c("Time","Sub_1_mean","Sub_2_mean","Sub_3_mean","GAP_mean")
a_mean <- ggplot(august_mean, aes(x=Time, group=1)) + geom_line(aes(y=Sub_1_mean, color="Sub-Metering 1")) + 
  geom_line(aes(y=Sub_2_mean, color="Sub-Metering 2")) + 
  geom_line(aes(y=Sub_3_mean, color="Sub-Metering 3")) + 
  geom_line(aes(y=GAP_mean, color="Global Active Power")) +
  scale_color_manual("", breaks=c("Sub-Metering 1","Sub-Metering 2","Sub-Metering 3","Global Active Power"),
                     values=c("red","blue","brown","green"))
a_mean + theme(axis.text.x = element_text(size=6, angle=90)) + ylab("W?h") +
  labs(title="Consumo medio en un día de Agosto") + xlab("Hora")
-------------------
  Year <- separate(cleanConsumption, Date, into = c("day", "month", "Year"))
Date_2 <- as.Date(paste(Year$day, Year$month, Year$Year), "%d %m %Y", tz="GMT")
Year <- Year$Year
cleanConsumption <-cbind(cleanConsumption, Year, Date_2)
cleanConsumption$Year <- as.numeric(cleanConsumption$Year)
data_2009 <- subset(cleanConsumption, Year==4)
year <- data_2009 %>%
  group_by(Date_2) %>%
  summarise(
    Sub_1_mean <- mean(Sub_metering_1, na.rm = TRUE),
    Sub_2_mean <- mean(Sub_metering_2, na.rm = TRUE),
    Sub_3_mean <- mean(Sub_metering_3, na.rm = TRUE),
    GAP_mean <- mean(Global_Consumption, na.rm = TRUE)
  )
colnames(year) <- c("Date","Sub_1_mean","Sub_2_mean","Sub_3_mean","GAP_mean")
consumption_2009 <- ggplot(year, aes(x=Date, group=1)) + geom_line(aes(y=Sub_1_mean, color="Sub-Metering 1")) + 
  geom_line(aes(y=Sub_2_mean, color="Sub-Metering 2")) + 
  geom_line(aes(y=Sub_3_mean, color="Sub-Metering 3")) + 
  geom_line(aes(y=GAP_mean, color="Global Active Power")) +
  scale_color_manual("", breaks=c("Sub-Metering 1","Sub-Metering 2","Sub-Metering 3","Global Active Power"),
                     values=c("red","blue","brown","green"))
consumption_2009 + theme(axis.text.x = element_text(size= 10, angle=45)) + ylab("W·h") +
  labs(title="Consumo en 2009")
-------------------
  #Evolución del consumo a lo largo de todos los años
  Year <- separate(cleanConsumption, Date, into = c("day", "month", "Year"))
Date_2 <- as.Date(paste(Year$day, Year$month, Year$Year), "%d %m %Y", tz="GMT")
day <- Year$day
Year <- Year$Year
cleanConsumption <-cbind(cleanConsumption, Year, Date_2, day)
lesspoints <- subset(cleanConsumption, day==1 | day==5 | day==10 | day==15 | day==20 | day==25 | day==30)
evolution <- lesspoints %>%
  group_by(Date_2) %>%
  summarise(
    Sub_1_mean <- mean(Sub_metering_1, na.rm = TRUE),
    Sub_2_mean <- mean(Sub_metering_2, na.rm = TRUE),
    Sub_3_mean <- mean(Sub_metering_3, na.rm = TRUE),
    GAP_mean <- mean(Global_Consumption, na.rm = TRUE)
  )
colnames(evolution) <- c("Date","Sub_1_mean","Sub_2_mean","Sub_3_mean","GAP_mean")
cons_evol <- ggplot(evolution, aes(x=Date, group=1)) + geom_line(aes(y=Sub_1_mean, color="Sub-Metering 1")) + 
  geom_line(aes(y=Sub_2_mean, color="Sub-Metering 2")) + 
  geom_line(aes(y=Sub_3_mean, color="Sub-Metering 3")) + 
  geom_line(aes(y=GAP_mean, color="Global Active Power")) +
  scale_color_manual("", breaks=c("Sub-Metering 1","Sub-Metering 2","Sub-Metering 3","Global Active Power"),
                     values=c("red","blue","brown","green"))
cons_evol + theme(axis.text.x = element_text(size= 10, angle=45)) + ylab("W?h") + xlab("Fecha") +
  labs(title="Evolución del consumo")
