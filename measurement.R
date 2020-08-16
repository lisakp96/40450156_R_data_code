rm(list=ls())
setwd("~/Desktop/analysis_R/data_measurement")

library(dplyr)
require(tidyverse)
library(ggplot2)
library(Hmisc)
library(iptools)
library(xtable)

# https://www.rdocumentation.org
# https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
# https://statistics.berkeley.edu/computing/r-t-tests
# https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
# https://www.pluralsight.com/guides/finding-relationships-data-with-r

# MEAN VS MEDIAN ???
# Coefficient of Variation (CV) = sd/mean -> higher CV = performance of instances differ more  -> calculate
# CORRELTAION TEST = LINIAR 
# null hypothesis. This is simply the hypothesis that nothing has happened.
# high p-value then you can say that it is reasonable that this result happened under the null hypothesis, 
# so the null hypothesis may well be true.
# convention you normally declare that whenever the p-value is less than 0.05 then that is a low enough 
# probability to reject the null hypothesis

# Chi Quadrat: Kategorical https://www.beratung-statistik.de/statistik-beratung-infos/r-tutorial/statistik-r-chiquadrat/

chisq.test(coldstart_pythonversions$runtime, coldstart_pythonversions$response_processing_time)
chisq.test(coldstart_pythonversions$requestid, coldstart_pythonversions$response_processing_time)
chisq.test(coldstart_pythonversions$response_processing_time, coldstart_pythonversions$request_rrt)
#p-value > 0.05 -> kein Zusammenhang

cordata = coldstart_pythonversions[,c(18,20)]
corr <- round(cor(cordata), 1)
corr

########################################################################################################


# original run (without fileupload stream)

coldstart_pythonversions  <-read.csv(file = "coldstart_pythonversions.csv", header = FALSE, sep="#")
coldstart_pythonversions = subset(coldstart_pythonversions, select = -c(V18,V19,V20,V21,V22) )

colnames(coldstart_pythonversions) <- c("round_id", "sub-round_id", "worker_no", "worker_id", "region",
                          "runtime", "memory", "function_name", "instance_ID_generated_by_function", 
                          "requestid", "VM_id", "instance_id", "instanceVM_ip", "public_VM_IP", 
                          "private_instance_ip","VM_uptime", "vCPU_No_and_model", 
                          "request_received_time_from function_", "response_sent_time_from_function", 
                          "response_processing_time", "request_sent_time_vintage", 
                          "response_received_time_vintage", "request_rrt")


pythonversion_coldstart <- as.vector(coldstart_pythonversions$response_processing_time)

summary(pythonversion_coldstart)

# Calculation (Without Fileupload Stream)
#Min.   :22103                             
#1st Qu.:22797                            
#Median :23302                             
#Mean   :23320 
#median and mean similar                             
#3rd Qu.:23922                            
#Max.   :24645  

# Standard Derivation 
sd(pythonversion_coldstart) #695.2818

# Coefficient of Variation (CV) -> high = more difference 
sd(pythonversion_coldstart) / mean(pythonversion_coldstart) #0.02981433


# ROUND TRIP TIME = request rtt (response received time - request sent time, ms) 
pythonversion_roundtrips <- as.vector(coldstart_pythonversions$request_rrt)

summary(pythonversion_roundtrips)

# Standard Derivation 
sd(pythonversion_roundtrips) 

# Coefficient of Variation (CV) -> high = more difference 
sd(pythonversion_roundtrips) / mean(pythonversion_roundtrips)



#Boxplot Coldstart
ggplot(coldstart_pythonversions, aes(x=runtime, y=response_processing_time, fill=runtime)) + 
  geom_boxplot()+  
  labs(title="Coldstart Measures of Python Versions", x= "Python Version", y= "Coldstart (m/s)")+
  scale_y_continuous(breaks = seq(0, 25000, 250)) +
  scale_fill_brewer(type = "seq",
                    palette = 1)

#Boxplot Request RTT
ggplot(coldstart_pythonversions) + 
  geom_boxplot(aes(x=coldstart_pythonversions$runtime, y=coldstart_pythonversions$request_rrt, fill=runtime))+  
  labs(title="Round-Trip Time Measures of Python Versions", x= "Python Version", y= "Round-Trip-Time (m/s)") + 
  scale_y_continuous(breaks = seq(0, 150000, 100))+
  scale_fill_brewer(type = "seq",
                    palette = 1)


python2.7 <-coldstart_pythonversions[grepl('python2.7', coldstart_pythonversions$runtime),]
coldstart2.7 <- as.vector(python2.7$response_processing_time)
mean(coldstart2.7) #23436.02 (Old) #34732.35 (New)

python3.6 <-coldstart_pythonversions[grepl('python3.6', coldstart_pythonversions$runtime),]
coldstart3.6 <- as.vector(python3.6$response_processing_time)
mean(coldstart3.6) #23309.23 (Old) #35090.86 (New)

python3.7 <-coldstart_pythonversions[grepl('python3.7', coldstart_pythonversions$runtime),]
coldstart3.7 <- as.vector(python3.7$response_processing_time)
mean(coldstart3.7) #23215.92 (Old) #34500.35 (New)


roundtriptime2.7 <- as.vector(python2.7$request_rrt)
mean(roundtriptime2.7) #146330.1 (Old) #221323.4 (New)

roundtriptime3.6 <- as.vector(python3.6$request_rrt)
mean(roundtriptime3.6) #146280.3 (Old) #220738.7 (New)

roundtriptime3.7 <- as.vector(python3.7$request_rrt)
mean(roundtriptime3.7) #146314.1 (Old) # 220681.5 (New)


#Boxplot Coldstart Version and Memory 

ggplot(data=python2.7)+
  geom_boxplot(aes(x=python2.7$memory, y=python2.7$response_processing_time, group = python2.7$memory))+
  labs(title="Memory Cold Start 2.7", x= "Python Version 2.7", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))

ggplot(data=python3.6)+
  geom_boxplot(aes(x=python3.6$memory, y=python3.6$response_processing_time, group = python3.6$memory))+
  labs(title="Memory Cold Start 3.6", x= "Python Version 3.6", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))

ggplot(data=python3.7)+
  geom_boxplot(aes(x=python3.7$memory, y=python3.7$response_processing_time, group = python3.7$memory))+
  labs(title="Memory Cold Start 3.7", x= "Python Version 3.7", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))


#Boxplot Request RTT Version and Memory 

ggplot(data=python2.7)+
  geom_boxplot(aes(x=python2.7$memory, y=python2.7$request_rrt, group = python2.7$memory))+
  labs(title="Memory Round-Trip-Time 2.7", x= "Python Version 2.7", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))

ggplot(data=python3.6)+
  geom_boxplot(aes(x=python3.6$memory, y=python3.6$request_rrt, group = python3.6$memory))+
  labs(title="Memory Round-Trip-Time 3.6", x= "Python Version 3.6", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))

ggplot(data=python3.7)+
  geom_boxplot(aes(x=python3.7$memory, y=python3.7$request_rrt, group = python3.7$memory))+
  labs(title="Memory Round-Trip-Time 3.7", x= "Python Version 3.7", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))


# check for duplicated VMs
# same instance id = same instance ip = same VM public ip (!) 

test <- data.frame(!coldstart_pythonversions$public_VM_IP %in% coldstart_pythonversions$public_VM_IP[duplicated(coldstart_pythonversions$public_VM_IP)])
coldstart_pythonversions <- cbind(coldstart_pythonversions, test)
rm(test)

duplicated <- subset(coldstart_pythonversions, coldstart_pythonversions$X.coldstart_pythonversions.public_VM_IP..in..coldstart_pythonversions.public_VM_IP.duplicated.coldstart_pythonversions.public_VM_IP.. == "TRUE")
rm(duplicated)

specificIp1<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=='3.236.218.254'),]
specificIp2<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=="b'100.24.27.41'"),]
specificIp3<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=="b'54.159.6.73'"),]
specificIp4<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=='54.80.192.16'),]
specificIp5<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=="b'3.237.48.93'"),]
specificIp6<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=='100.27.13.170'),]
specificIp7<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=="b'3.234.210.198'"),]
specificIp8<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=="b'54.211.235.86'"),]
specificIp9<- coldstart_pythonversions[ which(coldstart_pythonversions$public_VM_IP=="b'54.221.41.240'"),]

specificIp<- rbind(specificIp1,specificIp2,specificIp3,specificIp4,specificIp5,specificIp6,specificIp7,specificIp8,specificIp9)
rm(specificIp1,specificIp2,specificIp3,specificIp4,specificIp5,specificIp6,specificIp7,specificIp8,specificIp9)

specificIp = subset(specificIp, select = -c(X.coldstart_pythonversions.public_VM_IP..in..coldstart_pythonversions.public_VM_IP.duplicated.coldstart_pythonversions.public_VM_IP..))

specificIp = subset(specificIp, select = -c(round_id,worker_no, worker_id,region,vCPU_No_and_model,VM_uptime,response_sent_time_from_function,request_sent_time_vintage,response_received_time_vintage))

specificIp = subset(specificIp, select = -c(`request_received_time_from function_`))


options("scipen"=100, "digits"=4)

first_round <- specificIp[1:6,,drop=F]

xtable(first_round)



# AS FACTORS ??? 

# Plot frequency of Public VM IPs 

public_ips <- data.frame(coldstart_pythonversions$public_VM_IP)
colnames(public_ips) <- c("ip")

public_ips<- sub('b', '', public_ips$ip)
public_ips<- sub("'", '', public_ips)
public_ips<- sub("'", '', public_ips)

public_ips<- data.frame(public_ips,coldstart_pythonversions$`request_received_time_from function_`)

public_ips$coldstart_pythonversions..request_received_time_from.function_. <- ts(public_ips$coldstart_pythonversions..request_received_time_from.function_.)


ggplot(data=public_ips, aes(x=coldstart_pythonversions..request_received_time_from.function_., y=public_ips)) +
  geom_point()+
  labs(title="Distribution of Public VM IPs (Python Versions)", x= "IP", y="")+
  theme(axis.text.x=element_blank())

ggplot(public_ips) +
  geom_bar()+
  coord_flip()+
  labs(title="Public VM IPs (Python Versions)", x= "IP", y= "Occurance")


# Plot of frequency of Instance VM IPs 
instance_vm_ips <- data.frame(coldstart_pythonversions$instanceVM_ip)

instance_vm_ips<- data.frame(instance_vm_ips,coldstart_pythonversions$`request_received_time_from function_`)

instance_vm_ips$coldstart_pythonversions..request_received_time_from.function_. <- ts(instance_vm_ips$coldstart_pythonversions..request_received_time_from.function_.)
colnames(instance_vm_ips) <- c("ip", "time")


ggplot(data=instance_vm_ips, aes(x=time, y=ip))+
  geom_point()+
  labs(title="Distribution of Instance VM IPs (Python Versions)", x= "IP", y="")+
  theme(axis.text.x=element_blank())


ggplot(data=instance_vm_ips, aes(x=ip)) +
  geom_bar()+
  coord_flip()+
  labs(title="Instance VM IPs (Python Versions)", x= "IP", y= "Occurance")


#Memory 
unique(coldstart_pythonversions$vCPU_No_and_model)
#2;Intel(R) Xeon(R) Processor @ 2.50GHz

  
test <- as.numeric(coldstart_pythonversions$memory) # -0.2760034 
cor.test(test, pythonversion_coldstart, method=c("pearson", "kendall", "spearman"))
rm(test)

test <- as.numeric(coldstart_pythonversions$runtime) # -0.2760034 
cor.test(test, pythonversion_coldstart, method=c("pearson", "kendall", "spearman"))
rm(test)
  

########################################################################################################

# coldstart_fileupload3.7

coldstart_fileupload3.7 <-read.csv(file = "coldstart_fileupload3.7.csv", header = FALSE, sep="#")

coldstart_fileupload3.7 <-read.csv(file = "tmp.csv", header = FALSE, sep="#")


colnames(coldstart_fileupload3.7) <- c("round_id", "sub-round_id", "worker_no", "worker_id", "region",
                                        "runtime", "memory", "function_name", "instance_ID_generated_by_function", 
                                        "requestid", "VM_id", "instance_id", "instanceVM_ip", "public_VM_IP", 
                                        "private_instance_ip","VM_uptime", "vCPU_No_and_model", 
                                        "request_received_time_from function_", "response_sent_time_from_function", 
                                        "response_processing_time", "request_sent_time_vintage", 
                                        "response_received_time_vintage", "request_rrt")


coldstart_fileupload3.7 %>% group_by(public_VM_IP)%>% summarise(n = n())


#Histograms are used to show distributions of variables while bar charts are used to compare variables. 
#Histograms plot quantitative data with ranges of the data grouped into bins or intervals
#Bar charts plot categorical data.

ggplot(coldstart_fileupload3.7) +
  aes(x = public_VM_IP) + geom_bar(bins=coldstart_fileupload3.7$memory)

ggplot(coldstart_fileupload3.7) +
  aes(x = public_VM_IP) + geom_histogram(bins= coldstart_fileupload3.7$memory)


####


fileupload_coldstart <- as.vector(coldstart_fileupload3.7$response_processing_time)

summary(fileupload_coldstart)

#Min.   :21579                             
#1st Qu.:22494                            
#Median :23064                             
#Mean   :23042                            
#3rd Qu.:23576                            
#Max.   :24360  

#34612
#36213
#37185
#37204
#38316
#38997

# Standard Derivation 
sd(fileupload_coldstart) #769.4985

# Coefficient of Variation (CV) -> high = more difference 
sd(fileupload_coldstart) / mean(fileupload_coldstart) #0.03339545


# ROUND TRIP TIME = request rtt (response received time - request sent time, ms) 
fileupload_roundtrips <- as.vector(coldstart_fileupload3.7$request_rrt)

summary(fileupload_roundtrips)

#Min.   :146115                             
#1st Qu.:146270                            
#Median :146437                             
#Mean   :146750                            
#3rd Qu.:146966                            
#Max.   :148021 

# Standard Derivation 
sd(fileupload_roundtrips) #719.1568

# Coefficient of Variation (CV) -> high = more difference 
sd(fileupload_roundtrips) / mean(fileupload_roundtrips) #0.004900547


#Boxplot Coldstart
  ggplot(coldstart_fileupload3.7) + 
  geom_line(data=coldstart_fileupload3.7, aes(x=coldstart_fileupload3.7$runtime, y=coldstart_fileupload3.7$response_processing_time))+
  labs(title="Coldstart Measures of Fileupload Test", x= "Python Version", y= "Coldstart (m/s)") + geom_boxplot(aes(x=coldstart_fileupload3.7$runtime, y=coldstart_fileupload3.7$response_processing_time))
  scale_y_continuous(breaks = seq(0, 25000, 250))

#Boxplot Request RTT 
ggplot(coldstart_fileupload3.7) + 
  geom_line(data=coldstart_fileupload3.7, aes(x=coldstart_fileupload3.7$runtime, y=coldstart_fileupload3.7$request_rrt))+
  labs(title="Round-Trip Time Measures of Fileupload Test", x= "Python Version", y= "Round-Trip-Time (m/s)") + geom_boxplot(aes(x=coldstart_fileupload3.7$runtime, y=coldstart_fileupload3.7$request_rrt))+
  scale_y_continuous(breaks = seq(0, 150000, 250))


# Boxplot Coldstart Memory
ggplot(data=coldstart_fileupload3.7)+
  geom_boxplot(aes(x=coldstart_fileupload3.7$memory, y=coldstart_fileupload3.7$response_processing_time, group = coldstart_fileupload3.7$memory))+
  labs(title="Memory Cold Start Fileupload", x= "Memory (128, 3008)", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))

# Boxplot Request Memory
ggplot(data=coldstart_fileupload3.7)+
  geom_boxplot(aes(x=coldstart_fileupload3.7$memory, y=coldstart_fileupload3.7$request_rrt, group = coldstart_fileupload3.7$memory))+
  labs(title="Memory Round-Trip-Time Fileupload", x= "Memory (128, 3008)", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 3010, 1500))
  
  
test <- as.numeric(coldstart_fileupload3.7$memory) #-0.15276 
cor.test(test, fileupload_coldstart, method=c("pearson", "kendall", "spearman"))
rm(test)


########################################################################################################


# Scale Test Results 
# Deploy & invoke functions and scale up to test cold start again
# In a step of 1, the concurrent requests have been scaled up to 10 concurrent 
# step = 1; max_concurrent = 10, sleep_tm = 10, wait_time = 5

scale_fileupload <-read.csv(file = "scale_fileupload.csv", header = FALSE, sep = "#")
colnames(scale_fileupload) <- c("round_id", "sub-round_id", "worker_no", "worker_id", "region",
                     "runtime", "memory", "function_name", "instance_ID_generated_by_function", 
                     "requestid", "VM_id", "instance_id", "instanceVM_ip", "public_VM_IP", 
                     "private_instance_ip","VM_uptime", "vCPU_No_and_model", 
                     "request_received_time_from function_", "response_sent_time_from_function", 
                     "response_processing_time", "request_sent_time_vintage", 
                     "response_received_time_vintage", "request_rrt")

scalefileupload <- as.vector(scale_fileupload$response_processing_time)

summary(scalefileupload)

#Min.   :25086 (21579)                            
#1st Qu.:26167 (22494)                           
#Median :26682 (23064)                             
#Mean   :26753 (23042)                           
#3rd Qu.:27357 (23576)                            
#Max.   :29312 (24360)

# Standard Derivation 
sd(scalefileupload) # 869.8486 (769.4985)

# Coefficient of Variation (CV) -> high = more difference 
sd(scalefileupload) / mean(scalefileupload) # 0.0325144 (0.03339545)


# ROUND TRIP TIME = request rtt (response received time - request sent time, ms) 
roundtrips <- as.vector(scale_fileupload$request_rrt)
roundtrips <- sub(';', '', roundtrips)
roundtrips <- as.numeric(roundtrips)

summary(roundtrips)

# Standard Derivation 
sd(roundtrips) 

# Coefficient of Variation (CV) -> high = more difference 
sd(roundtrips) / mean(roundtrips)

rm(roundtrips)


# scale up use the same VM, resuting in resource contention & longer coldstart latency ? 
# only 2 VMs re-used 

specificIp1<- scale_fileupload[ which(scale_fileupload$public_VM_IP=="b'34.201.32.226'"),] #90, 101
specificIp2<- scale_fileupload[ which(scale_fileupload$public_VM_IP=="b'3.237.2.145'"),] #79,103


#Boxplot Coldstart
ggplot(scale_fileupload) + 
  geom_boxplot(aes(x=scale_fileupload$runtime, y=scale_fileupload$response_processing_time))+
  labs(title="Coldstart Measures of Fileupload Scale Test", x= "Python Version", y= "Coldstart (m/s)") + 
  scale_y_continuous(breaks = seq(0, 500000, 500))

#Boxplot Request RTT

scale_fileupload$request_rrt <- sub(';', '', scale_fileupload$request_rrt)
scale_fileupload$request_rrt <- as.numeric(scale_fileupload$request_rrt)

ggplot(scale_fileupload) + 
  geom_boxplot(aes(x=scale_fileupload$runtime, y=scale_fileupload$request_rrt, group=1))+  
  labs(title="Round-Trip Time Measures of Fileupload Scale Test", x= "Python Version", y= "Round-Trip-Time (m/s)")+
  scale_y_continuous(breaks = seq(0, 154000, 250))


test <- as.numeric(scale_fileupload$memory) #0.09896074
cor.test(test, scalefileupload, method=c("pearson", "kendall", "spearman"))
rm(test)


scale_fileupload$groups <- paste(scale_fileupload$memory,scale_fileupload$worker_no)

scale_fileupload$groups <- ts(scale_fileupload$groups)

plot(scale_fileupload$groups,scale_fileupload$response_processing_time )


# Boxplot Request Memory

scale_fileupload_128<- scale_fileupload[which(scale_fileupload$memory=="128"),]

ggplot(data=scale_fileupload_128)+
  geom_point(aes(x=scale_fileupload_128$worker_no, y=scale_fileupload_128$response_processing_time, group = scale_fileupload_128$memory)) +
  geom_smooth(aes(x=scale_fileupload_128$worker_no, y=scale_fileupload_128$response_processing_time))+
  labs(title="128 Memory Concurrency Coldstart Scale Test Fileupload", x= "Grouped by Worker", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 10, 1))

scale_fileupload_3008<- scale_fileupload[which(scale_fileupload$memory=="3008"),]

ggplot(data=scale_fileupload_3008)+
  geom_point(aes(x=scale_fileupload_3008$worker_no, y=scale_fileupload_3008$response_processing_time, group = scale_fileupload_3008$memory))+
  geom_smooth(aes(x=scale_fileupload_3008$worker_no, y=scale_fileupload_3008$response_processing_time))+
  labs(title="3008 Memory Concurrency Coldstart Scale Test Fileupload", x= "Grouped by Worker", y= "Coldstart (m/s)")+
  scale_x_continuous(breaks = seq(0, 10, 1))

mean(scale_fileupload_128$response_processing_time)
boxplot(scale_fileupload_128$response_processing_time)
mean(scale_fileupload_3008$response_processing_time)
boxplot(scale_fileupload_3008$response_processing_time)

########################################################################################################


# Placement Test Results
# memory_list = random !!! 
# step = 1; max_concurrent = 5, sleep_tm = 0, wait_time = 5
# Scale Up & Longer Wait Time 

placement_fileupload <-read.csv(file = "placement_fileupload.csv", header = FALSE, sep = "#")
colnames(placement_fileupload) <- c("round_id", "sub-round_id", "worker_no", "worker_id", "region",
                               "runtime", "memory", "function_name", "instance_ID_generated_by_function", 
                               "requestid", "VM_id", "instance_id", "instanceVM_ip", "public_VM_IP", 
                               "private_instance_ip","VM_uptime", "vCPU_No_and_model", 
                               "request_received_time_from function_", "response_sent_time_from_function", 
                               "response_processing_time", "request_sent_time_vintage", 
                               "response_received_time_vintage", "request_rrt" )

placementfileupload <- as.vector(placement_fileupload$response_processing_time)

summary(placementfileupload)

#Min.   :15255 (25086) (21579)                            
#1st Qu.:16001 (26167) (22494)                           
#Median :16623 (26682) (23064)                             
#Mean   :16703 (26753) (23042)                           
#3rd Qu.:17127 (27357) (23576)                            
#Max.   :18722 (29312) (24360)


# Standard Derivation 
sd(placementfileupload) # 868.6704 (869.8486) (769.4985)

# Coefficient of Variation (CV) -> high = more difference 
sd(placementfileupload) / mean(placementfileupload) # 0.0520072 (0.0325144) (0.03339545)


# ROUND TRIP TIME = request rtt (response received time - request sent time, ms) 
roundtrips <- as.vector(placement_fileupload$request_rrt)

summary(roundtrips)
#Min.   : 139824                          
#1st Qu.: 140123                        
#Median : 140238                            
#Mean   : 140344                        
#3rd Qu.: 140362                           
#Max.   : 141160

# Standard Derivation 
sd(roundtrips)  #410.7435

# Coefficient of Variation (CV) -> high = more difference 
sd(roundtrips) / mean(roundtrips) #0.002926701

rm(roundtrips)


unique(placement_fileupload$public_VM_IP) # all unique 


ggplot(placement_fileupload, aes(x=memory, y=response_processing_time))+ 
  geom_point()+  
  geom_smooth()+
  geom_hline(yintercept = 16623, color="blue")+
  labs(title="Coldstart and Memory (Placement Test)", x= "Python Version", y= "Coldstart (m/s)") 




ggplot(placement_fileupload, aes(x=memory, y=request_rrt))+ 
  geom_point()+  
  geom_smooth()+
  geom_hline(yintercept = 140238, color="blue")+
  labs(title="Round-Trip-Time and Memory (Placement Test)", x= "Python Version", y= "Round-Trip-Time (m/s)") 


test <- data.frame(placement_fileupload$memory)


test <- as.numeric(placement_fileupload$memory) #0.05560022 
cor.test(test, placementfileupload, method=c("pearson", "kendall", "spearman"))
rm(test)



########################################################################################################


#Consistency Test Results 
# re = + role; threat_no = 5, sleep_tm = 5
#same tenant = same set of VMs ? 

consistency_fileupload <- read.csv(file = "consistency_fileupload.csv", header = FALSE, sep = "#")
colnames(consistency_fileupload) <- c("round_id", "sub-round_id", "worker_no", "worker_no", "region",
                         "runtime", "memory", "function_name", "instance_ID_generated_by_function", 
                         "requestid", "VM_id", "instance_id", "instanceVM_ip", "public_VM_IP", 
                         "private_instance_ip","VM_uptime", "vCPU_No_and_model", 
                         "request_received_time_from function_", "response_sent_time_from_function", 
                         "response_processing_time", "request_sent_time_vintage", 
                         "response_received_time_vintage", "reqeuest_rrt", "role" )

consistency_fileupload <- consistency_fileupload[ , -which(names(consistency_fileupload) %in% c("worker_no","worker_no", "region"))]

consistency_fileupload <- consistency_fileupload[ , -which(names(consistency_fileupload) %in% c("runtime","memory", "request_received_time_from function_", "response_sent_time_from_function", 
                                                                                                "response_processing_time", "request_sent_time_vintage", 
                                                                                                "response_received_time_vintage", "reqeuest_rrt"))]
consistency_fileupload <- consistency_fileupload[ , -which(names(consistency_fileupload) %in% c("instance_ID_generated_by_function", "VM_uptime"))]


########################################################################################################


