rm(list=ls())
setwd("~/Desktop/analysis_R/data_logs")

library(dplyr)
require(tidyverse)
library(ggplot2)
library(Hmisc)
library(iptools)

#subsys_namehierarchynum_cgroupsenabled;
#cpuset 411;
#cpu271;
#cpuacct331;
#blkio111;
#memory851;
#devices511;
#freezer621;
#net_cls011;
#perf_event911;
#net_prio011;
#hugetlb711;
#pids1011;

#INST_PRIV_IP_DST = "8.8.8.8"

#########################################################################################


# DATA CLEAN-UP 

# LOAD FILES, EXTRACT PRIVATE & PUBLIC VMS, AND MEMORY & PROCESSOR 

# 1st 6am-8am: 2h, 0-5 min, 1 concurrent
interval_1 <-read.csv(file = "Interval_1.csv", header = TRUE)
colnames(interval_1) <- c("timestamp","message")

# delete "start"START" to have clean sections starting with "END" of the previous one
# "END" includes execution time 
interval_1 <-interval_1[!grepl('START ', interval_1$message),]


# all private IP adresses 
ips <- interval_1[grep('169', interval_1$message),]
# delete wrong entry 
ips <- ips[grepl('^169', ips$message), ]


# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all 
private_ips_VM_1 <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


# all public ip addresses
# all to same VM (34.207.242.55) -> instance warm 
public_ips_1 <- interval_1[grepl("b'", interval_1$message),]

# formatting
public_ips_1$message <- sub("'", '', public_ips_1$message)
public_ips_1$message <- sub("'", '', public_ips_1$message)
public_ips_1$message <- sub("b", '', public_ips_1$message)
public_ips_1 <- public_ips_1[!grepl('8492.27', public_ips_1$message),]


#less memory free over time (same VM!) - ??? 
memory1<- interval_1[grepl("MemTotal", interval_1$message),]
memory1 <- str_split_fixed(memory1$message, ",", 50)

processor1<- interval_1[grepl("processor:", interval_1$message),]
processor1 <- str_split_fixed(processor1$message, ";", 60)


# VM Uptime calculations 

# get uptime information from data frame 
uptime1 <- interval_1[!grepl('169.', interval_1$message),]
uptime1 <- uptime1[!grepl('END ', uptime1$message),]
uptime1 <- uptime1[!grepl('REPORT ', uptime1$message),]
uptime1 <- uptime1[!grepl("b'", uptime1$message),]
uptime1 <- uptime1[!grepl("MemTotal", uptime1$message),]
uptime1 <- uptime1[!grepl("us-east-1", uptime1$message),]
uptime1 <- uptime1[!grepl("169.", uptime1$message),]
uptime1 <- uptime1[!grepl("processor", uptime1$message),]


uptime1 <- str_split_fixed(uptime1$message, ",", 2)
uptime1 <- data.frame(uptime1)
colnames(uptime1) <- c("A","B")

uptime1$C <- (as.numeric(as.character(uptime1$B)) -as.numeric(as.character(uptime1$A)))
uptime1$D <- (as.numeric(as.character(uptime1$A)) -as.numeric(as.character(uptime1$B)))
rm(uptime1)




# 2nd 8am-10am 2h, 0-4 min, 1 concurrent
interval_2 <-read.csv(file = "Interval_2.csv", header = TRUE)
colnames(interval_2) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
interval_2 <-interval_2[!grepl('START ', interval_2$message),]


# all private IP adresses 
ips <- interval_2[grep('169', interval_2$message),]
# delete wrong entries 
ips <- ips[grepl('^169', ips$message), ]

# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all 
private_ips_VM_2 <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


# all public ip addresses
public_ips_2 <- interval_2[grepl("b'", interval_2$message),]

# formatting
public_ips_2$message <- sub("'", '', public_ips_2$message)
public_ips_2$message <- sub("'", '', public_ips_2$message)
public_ips_2$message <- sub("b", '', public_ips_2$message)



memory2<- interval_2[grepl("MemTotal", interval_2$message),]
memory2 <- str_split_fixed(memory2$message, ",", 50)

processor2<- interval_2[grepl("processor:", interval_2$message),]
processor2 <- str_split_fixed(processor2$message, ";", 60)




# 3rd 10am-12pm: 4h, 0-3 min, 2 concurrent
interval_3 <-read.csv(file = "Interval_3.csv", header = TRUE)
colnames(interval_3) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
interval_3 <-interval_3[!grepl('START ', interval_3$message),]

# all public ip addresses; same for all ! 
public_ips_3 <- interval_3[grepl("b'", interval_3$message),]

# formatting
public_ips_3$message <- sub("'", '', public_ips_3$message)
public_ips_3$message <- sub("'", '', public_ips_3$message)
public_ips_3$message <- sub("b", '', public_ips_3$message)


# all private IP adresses 
ips <- interval_3[grep('169', interval_3$message),]
# delete wrong entry 
ips <- ips[grepl('^169', ips$message), ]


# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all  
private_ips_VM_3 <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


memory3<- interval_3[grepl("MemTotal", interval_3$message),]
memory3 <- str_split_fixed(memory3$message, ",", 50)

processor3<- interval_3[grepl("processor:", interval_3$message),]
processor3 <- str_split_fixed(processor3$message, ";", 60)




# 4th 2am-18am:  4h, 0-2 min, 4 concurrent  
interval_4 <-read.csv(file = "Interval_4_2.csv", header = TRUE)
colnames(interval_4) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
interval_4 <-interval_4[!grepl('START ', interval_4$message),]

# all public ip addresses; same for all ! 
public_ips_4 <- interval_4[grepl("b'", interval_4$message),]
# formatting
public_ips_4$message <- sub("'", '', public_ips_4$message)
public_ips_4$message <- sub("'", '', public_ips_4$message)
public_ips_4$message <- sub("b", '', public_ips_4$message)


# all private IP adresses 
ips <- interval_4[grep('169', interval_4$message),]
# delete wrong entry 
ips <- ips[grepl('^169', ips$message), ]

# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all  
private_ips_VM_4 <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


memory4<- interval_4[grepl("MemTotal", interval_4$message),]
memory4 <- str_split_fixed(memory4$message, ",", 50)

processor4<- interval_4[grepl("processor:", interval_4$message),]
processor4 <- str_split_fixed(processor4$message, ";", 60)




# 5th 6pm-10pm:  6h, 0-1 min, 2 concurrent 
interval_5 <-read.csv(file = "Interval_5.csv", header = TRUE)
colnames(interval_5) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
interval_5 <-interval_5[!grepl('START ', interval_5$message),]


# all public ip addresses; same for all ! 
public_ips_5 <- interval_5[grepl("b'", interval_5$message),]

# formatting
public_ips_5$message <- sub("'", '', public_ips_5$message)
public_ips_5$message <- sub("'", '', public_ips_5$message)
public_ips_5$message <- sub("b", '', public_ips_5$message)


# all private IP adresses 
ips <- interval_5[grep('169', interval_5$message),]
# delete wrong entry 
ips <- ips[grepl('^169', ips$message), ]

# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all  
private_ips_VM_5 <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


memory5<- interval_5[grepl("MemTotal", interval_5$message),]
memory5 <- str_split_fixed(memory5$message, ",", 50)

processor5<- interval_5[grepl("processor:", interval_5$message),]
processor5 <- str_split_fixed(processor5$message, ";", 60)



# 6th 10pm-6am:  8h, 0-10min, 2 concurrent 
interval_6 <-read.csv(file = "Interval_6.csv", header = TRUE)
colnames(interval_6) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
interval_6 <-interval_6[!grepl('START ', interval_1$message),]

# all public ip addresses; same for all ! 
public_ips_6 <- interval_6[grepl("b'", interval_6$message),]

# formatting
public_ips_6$message <- sub("'", '', public_ips_6$message)
public_ips_6$message <- sub("'", '', public_ips_6$message)
public_ips_6$message <- sub("b", '', public_ips_6$message)


# all private IP adresses 
ips <- interval_6[grep('169', interval_6$message),]
# delete wrong entry 
ips <- ips[grepl('^169', ips$message), ]


# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all  
private_ips_VM_6 <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)

private_ips_VM_6 <- private_ips_VM_6[!grepl('1699.64,3397.32', private_ips_VM_6$message),]
private_ips_VM_6 <- private_ips_VM_6[!grepl('START ', private_ips_VM_6$message),]
private_ips_VM_6 <- private_ips_VM_6[!grepl('1040 ', private_ips_VM_6$message),]


memory6<- interval_6[grepl("MemTotal", interval_6$message),]
memory6 <- str_split_fixed(memory6$message, ",", 50)

processor6<- interval_6[grepl("processor:", interval_6$message),]
processor6 <- str_split_fixed(processor6$message, ";", 60)

memories <- rbind(memory1,memory2,memory3,memory4,memory5,memory6)
colnames(memories) <- c(1:50)
describe(memories)


processors <- rbind(processor1,processor2,processor3,processor4,processor5,processor6)
colnames(processors) <- c(1:60)
describe(processors)


#########################################################################################


# GRAPHICAL VISUALISATION OF IP ADRESS CHANGES FOR THE INTERVALS 


describe(private_ips_VM_1)
# n = 50, 1 IP


# Frequency of IPs in Interval 1
ggplot(data=private_ips_VM_1, aes(x=message)) +
  geom_bar()+
  labs(title="IP Adresses Interval 1", x= "IP", y= "Occurance")


# Time-Series of IPs in Interval 1

private_ips_VM_1$timestamp <- as.POSIXct(private_ips_VM_1$timestamp)

ggplot(private_ips_VM_1, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5, color='blue') +
  scale_x_datetime() +
  labs(title="VM IP Time Series Interval 1", x= "Time", y="")


describe(private_ips_VM_2)
# n = 54, 3 IPs


# Frequency of IPs in Interval 2
ggplot(data=private_ips_VM_2, aes(x=message)) +
  geom_bar()+
  labs(title="IP Adresses Interval 2", x= "IP", y= "Occurance")


# Time-Series of IPs in Interval 2

private_ips_VM_2$timestamp <- as.POSIXct(private_ips_VM_2$timestamp)

ggplot(private_ips_VM_2, aes(x=timestamp, y=message, color = message)) + 
  geom_point(size=0.5,) +
  scale_x_datetime() +
  labs(title="VM IP Time Series Interval 2", x= "Time", y="") + 
  theme(legend.position = "none") 


describe(private_ips_VM_3)
# n = 306, 5 IPs 

# Frequency of IPs in Interval 3
ggplot(data=private_ips_VM_3, aes(x=message)) +
  geom_bar()+
  labs(title="IP Adresses Interval 3", x= "IP", y= "Occurance")


# Time-Series of IPs in Interval 3

private_ips_VM_3$timestamp <- as.POSIXct(private_ips_VM_3$timestamp)

ggplot(private_ips_VM_3, aes(x=timestamp, y=message, color = message)) + 
  geom_point(size=0.5,) +
  scale_x_datetime() +
  labs(title="VM IP Time Series Interval 3", x= "Time", y="") + 
  theme(legend.position = "none") 


describe(private_ips_VM_4)
# n = 930, 11 IPs

# Frequency of IPs in Interval 4
ggplot(data=private_ips_VM_4, aes(x=message)) +
  geom_bar()+
  labs(title="IP Adresses Interval 4", x= "IP", y= "Occurance")+
  theme(axis.text.x=element_blank())


# Time-Series of IPs in Interval 4

private_ips_VM_4$timestamp <- as.POSIXct(private_ips_VM_4$timestamp)

ggplot(private_ips_VM_4, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="VM IP Time Series Interval 4", x= "Time", y="")



describe(private_ips_VM_5)
# n = 920, 7 IPs

# Frequency of IPs in Interval 5
ggplot(data=private_ips_VM_5, aes(x=message)) +
  geom_bar()+
  labs(title="IP Adresses Interval 5", x= "IP", y= "Occurance")+
  theme(axis.text.x=element_blank())


# Time-Series of IPs in Interval 5

private_ips_VM_5$timestamp <- as.POSIXct(private_ips_VM_5$timestamp)

ggplot(private_ips_VM_5, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="VM IP Time Series Interval 5", x= "Time", y="")


describe(private_ips_VM_6)
# n = 174, 31 Ips

# Frequency of IPs in Interval 6
ggplot(data=private_ips_VM_6, aes(x=message)) +
  geom_bar()+
  labs(title="IP Adresses Interval 6", x= "IP", y= "Occurance")+
  theme(axis.text.x=element_blank())


# Time-Series of IPs in Interval 6

private_ips_VM_6$timestamp <- as.POSIXct(private_ips_VM_6$timestamp)

ggplot(private_ips_VM_6, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="VM IP Time Series Interval 6", x= "Time", y="")


##

allpublic <- rbind(private_ips_VM_1, private_ips_VM_2, private_ips_VM_3, 
private_ips_VM_4, private_ips_VM_5, private_ips_VM_6)

describe(allpublic)

#51

allpublic$timestamp <- as.POSIXct(allpublic$timestamp)

ggplot(allpublic, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.2) + 
  scale_x_datetime() + 
  xlab("All Intervals") + 
  ylab("IPs")

##


#Correlation Test between different variables 

x <- as.numeric(private_ips_VM_2$timestamp)
y <- as.numeric(private_ips_VM_2$message)

cor.test(x, y, method=c("pearson", "kendall", "spearman"))
# 0.747382  = A strong uphill (positive) linear relationship


x <- as.numeric(private_ips_VM_3$timestamp)
y <- as.numeric(private_ips_VM_3$message)

cor.test(x, y, method=c("pearson", "kendall", "spearman"))
#0.2128536 

x <- as.numeric(private_ips_VM_4$timestamp)
y <- as.numeric(private_ips_VM_4$message)

cor.test(x, y, method=c("pearson", "kendall", "spearman"))
#-0.1591233 

x <- as.numeric(private_ips_VM_5$timestamp)
y <- as.numeric(private_ips_VM_5$message)

cor.test(x, y, method=c("pearson", "kendall", "spearman"))
#0.5415413

x <- as.numeric(private_ips_VM_6$timestamp)
y <- as.numeric(private_ips_VM_6$message)

cor.test(x, y, method=c("pearson", "kendall", "spearman"))
#0.2814023


##

publicIPs <- rbind(public_ips_1, public_ips_2, public_ips_3, public_ips_4, public_ips_5, public_ips_6)

rm(public_ips_1,public_ips_2,public_ips_3,public_ips_4,public_ips_5,public_ips_6)

describe(publicIPs) # 48 distinct IPs 

write.csv(publicIPs, file ="test.csv", row.names=FALSE, col.names = FALSE, sep = ",", eol=",")


#########################################################################################


run5min<-read.csv(file = "run_5min.csv", header = TRUE)
colnames(run5min) <- c("timestamp","message")


# Private IPs

# all private IP adresses 
ips <- run5min[grep('169', run5min$message),]
# delete wrong entry 
ips <- ips[grepl('^169', ips$message), ]
# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all  
run5min_private <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)

run5min_private$timestamp <- as.POSIXct(run5min_private$timestamp)

ggplot(run5min_private, aes(x=timestamp, y=message, color = message)) + 
  geom_point(size=0.5,) +
  scale_x_datetime() +
  labs(title="VM IP Time Series 5 Min Run", x= "Time", y="") + 
  theme(legend.position = "none") 


## Public IPs

run5min_ips <- run5min[grepl("b'", run5min$message),]
colnames(run5min_ips) <- c("timestamp","message")

run5min_ips_test<- sub('b', '', run5min_ips$message)

run5min_ips <- data.frame(run5min_ips$timestamp,run5min_ips_test)
colnames(run5min_ips) <- c("timestamp","message")

run5min_ips_test<- sub('b', '', run5min_ips$message)
run5min_ips_test <- sub("'", '', run5min_ips_test)
run5min_ips_test <- sub("'", '', run5min_ips_test)

run5min_ips <- data.frame(run5min_ips$timestamp,run5min_ips_test)
colnames(run5min_ips) <- c("timestamp","message")
rm(run5min_ips_test)

describe(run5min_ips) # distinct 3, Init Duration 2

run5min_ips$timestamp <- as.POSIXct(run5min_ips$timestamp)

run5min_ips$message <- as.vector(run5min_ips$message)


ggplot(run5min_ips, aes(x=timestamp, y=message, color = message)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="Public IP Time Series 5 Min Run", x= "Time", y="") + 
  theme(legend.position = "none")


#Init Duration / Execution Time 
test <- data.frame(str_detect(run5min$message, "Init Duration"))
which(test$str_detect.run5min.message...Init.Duration..==TRUE)

length(test$str_detect.run5min.message...Init.Duration..[test$str_detect.run5min.message...Init.Duration.. == TRUE])


firstChange <- run5min[20:42,,drop=F]

#REPORT RequestId: ba26617e-4482-43bb-bbe1-d3bec76bd05e 
#Duration: 401.11 ms 
#Billed Duration: 500 ms 
#Memory Size: 128 MB 
#Max Memory Used: 79 MB


#REPORT RequestId: 45036707-50be-4e19-a4ae-211283d18b60 
#Duration: 1862.08 ms 
#Billed Duration: 1900 ms 
#Memory Size: 128 MB Max 
#Memory Used: 79 MB 
#Init Duration: 320.53 ms


#REPORT RequestId: d2c2d1ff-bf1a-4628-b384-431392d07bf3
#Duration: 374.34 ms 
#Billed Duration: 400 ms 
#Memory Size: 128 MB 
#Max Memory Used: 81 MB


secondChange <- run5min[290:312,,drop=F]

#REPORT RequestId: 4883f0da-c83b-42cd-8626-050e3664364d 
#Duration: 504.08 ms 
#Billed Duration: 600 ms 
#Memory Size: 128 MB 
#Max Memory Used: 79 MB

#REPORT RequestId: 742a66c3-3c8b-4696-bb9c-50a53001fe62 
#Duration: 1695.34 ms 
#Billed Duration: 1700 ms 
#Memory Size: 128 MB 
#Max Memory Used: 78 MB 
#Init Duration: 298.14 ms

#REPORT RequestId: e28dce15-5300-444f-9556-cc3b7fc682f2 
#Duration: 315.44 ms 
#Billed Duration: 400 ms 
#Memory Size: 128 MB 
#Max Memory Used: 81 MB


##

run12min<-read.csv(file = "run_12min.csv", header = TRUE)
colnames(run12min) <- c("timestamp","message")

test <- data.frame(str_detect(run12min$message, "Init Duration"))
which(test$str_detect.run12min.message...Init.Duration..==TRUE)

length(test$str_detect.run12min.message...Init.Duration..[test$str_detect.run12min.message...Init.Duration.. == TRUE])


run12min_ips <- run12min[grepl("b'", run12min$message),]
colnames(run12min_ips) <- c("timestamp","message")

run12min_ips_test<- sub('b', '', run12min_ips$message)
run12min_ips_test <- sub("'", '', run12min_ips_test)
run12min_ips_test <- sub("'", '', run12min_ips_test)

run12min_ips <- data.frame(run12min_ips$timestamp,run12min_ips_test)
colnames(run12min_ips) <- c("timestamp","message")
rm(run12min_ips_test)

describe(run12min_ips) # distinct 26, Init Duration 25

run12min_ips$timestamp <- as.POSIXct(run12min_ips$timestamp)

run12min_ips$message <- as.vector(run12min_ips$message)

write.csv(run5min_ips$message, file ="run12min_ips.csv", row.names=FALSE, col.names = FALSE, sep = ",", eol=",")


ggplot(run12min_ips, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="Public IP Time Series 12 Min Run", x= "12 Min", y="")


#########################################################################################


hello_world<-read.csv(file = "hello-world.csv", header = TRUE)
colnames(hello_world) <- c("timestamp","message")


hello_world <- hello_world[-c(191), ] 

helloworld_ips <- hello_world[!grepl('REPORT ', hello_world$message),]
colnames(helloworld_ips) <- c("timestamp","message")

helloworld_ips <- helloworld_ips[!grepl('START ', helloworld_ips$message),]
helloworld_ips <- helloworld_ips[!grepl('END ', helloworld_ips$message),]


helloworld_ips$timestamp <- as.POSIXct(helloworld_ips$timestamp)

ggplot(helloworld_ips, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="Public IP Time Series Hello World Run", x= "1 Min / 10 Min Request Rate", y="")


#########################################################################################


interval <- c('20 Min','15 Min', '10 Min', '5 Min')
tcp <- c(103.04,109.47,103.36,109.9)
rtt <- c(2750,3100,1430,1030)

test <- data.frame(interval, tcp, rtt)


test$interval <- factor(test$interval,levels = c('20 Min','15 Min', '10 Min', '5 Min'))


ggplot() + 
  geom_line(data=test,aes(y=tcp,x=interval,colour="blue", group=1))+
  geom_line(data=test,aes(y=rtt,x=interval,colour="red", group=1)) +
  scale_color_discrete(name = "", labels = c("tcp", "rtt")) +
  scale_y_continuous(breaks = seq(0, 3200, 100))+
  labs(title="Postman TCP and RTT", x= "Intervals", y="m/s")


#########################################################################################

#Test if VMs re-occour in other experiments

test <- data.frame(run5min_ips$message %in% publicIPs$message)

which(test$run5min_ips.message..in..publicIPs.message==TRUE) # none 

test <- data.frame(publicIPs$message %in% run12min_ips$message)

which(test$publicIPs.message..in..run12min_ips.message == TRUE) #none

test <- data.frame(helloworld_ips$message %in% publicIPs$message)

which(test$helloworld_ips.message..in..publicIPs.message == TRUE) #none

rm(test)

test <- unique(publicIPs)

#Test VM locations

test <- unique(helloworld_ips) # all
test <- unique(run12min_ips) # all
test <- unique(run5min_ips) # all

test <- unique(publicIPs) # 1940

ips <- rbind(publicIPs,helloworld_ips,run5min_ips,run12min_ips) #2485
test <- unique(ips) #2281 

ips <- ips$message

jsonData <- toJSON(ips)

write.csv(ips, file ="ips.csv", row.names=FALSE, col.names = FALSE, sep = ",", eol=",")


##subsys_namehierarchynum_cgroupsenabled;cpuset411;cpu271;cpuacct331;blkio111;memory851;devices511;freezer621;net_cls011;perf_event911;net_prio011;hugetlb711;pids1011;


#########################################################################################


OneSecRun <-read.csv(file = "1Sec_Run.csv", header = TRUE)
colnames(OneSecRun) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
OneSecRun <-OneSecRun[!grepl('START ', OneSecRun$message),]


# all private IP adresses 
ips <- OneSecRun[grep('169', OneSecRun$message),]
# delete wrong entries 
ips <- ips[grepl('^169', ips$message), ]

# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all 
private_ips_OneSecRun <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


# all public ip addresses
public_ips_OneSecRun <- OneSecRun[grepl("b'", OneSecRun$message),]

# formatting
public_ips_OneSecRun$message <- sub("'", '', public_ips_OneSecRun$message)
public_ips_OneSecRun$message <- sub("'", '', public_ips_OneSecRun$message)
public_ips_OneSecRun$message <- sub("b", '', public_ips_OneSecRun$message)



private_ips_OneSecRun$timestamp <- as.POSIXct(private_OneSecRun$timestamp)

ggplot(private_ips_OneSecRun, aes(timestamp, message, group = 1)) + 
  geom_point(size=0.5) +
  scale_x_datetime() +
  labs(title="Public IP Time Series OneSecRun Run", x= "1 Sec Request Rate", y="")

# 174 # not all !!! 

requests <- Two5_Concurrent[grepl("END ", Two5_Concurrent$message),]

#########################################################################################


Two5_Concurrent <-read.csv(file = "205_concurrent.csv", header = TRUE)
colnames(Two5_Concurrent) <- c("timestamp","message")


# delete "start"START" to have clean sections starting with "END" of the previous one
Two5_Concurrent <-Two5_Concurrent[!grepl('START ', OneSecRun$message),]


# all private IP adresses 
ips <- Two5_Concurrent[grep('169', Two5_Concurrent$message),]
# delete wrong entries 
ips <- ips[grepl('^169', ips$message), ]

# exclude 169.254.76.1 = instance private IP to extract VM private IP 
# same for all 
private_ips_Two5_Concurrent <- ips[!grepl('169.254.76.1', ips$message),]
rm(ips)


# all public ip addresses
public_ips_Two5_Concurrent <- Two5_Concurrent[grepl("b'", Two5_Concurrent$message),]

# formatting
public_ips_Two5_Concurrent$message <- sub("'", '', public_ips_Two5_Concurrent$message)
public_ips_Two5_Concurrent$message <- sub("'", '', public_ips_Two5_Concurrent$message)
public_ips_Two5_Concurrent$message <- sub("b", '', public_ips_Two5_Concurrent$message)



private_ips_Two5_Concurrent$timestamp <- as.POSIXct(private_ips_Two5_Concurrent$timestamp)

ggplot(private_ips_Two5_Concurrent, aes(timestamp, message, group = 1)) + 
  geom_line(size=0.5) +
  scale_x_datetime() +
  labs(title="Public IP Time Series OneSecRun Run", x= "1 Sec Request Rate", y="")

#204 unique -> used > 200 concurrent requests ! 
test <- data.frame(unique(private_ips_Two5_Concurrent$message))
