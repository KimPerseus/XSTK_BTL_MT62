library(zoo)
library(car)
library(MASS)
library(tidyr)
library(dplyr)
library(caret)
library(plotly)
library(Metrics)
library(stringr)
library(mltools)
library(ggplot2)
library(reshape2)
library(DescTools)


# ĐỌC SỐ LIỆU####################
# đọc dữ liệu và gán cácchuỗi rỗng thành NA
Intel_CPUs <- read.csv("D:/uni_data/secondYear/s2/P&S/rproject/Intel_CPUs.csv",na.strings = c("", "N/A")) 

# Lựa chọn các cột cần sử dụng
CPUs_data <- Intel_CPUs[,c("Product_Collection","Vertical_Segment","Status","Launch_Date",
                           "Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                           "Processor_Base_Frequency","Cache","Instruction_Set","TDP","Max_Memory_Size",
                           "Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")]
# in ra bảng thống kê sơ bộ của dữ liệu
summary(CPUs_data) 

# XỬ LÝ DỮ LIỆU KHUYẾT
apply(is.na(CPUs_data),2,sum)   # kiem tra so luong nhung du lieu khuyet


# PRODUCT COLLECTION ##################

groups <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')

for (item in groups) {
  CPUs_data$Product_Collection <- ifelse(grepl(item, CPUs_data$Product_Collection), item, CPUs_data$Product_Collection) 
}

# LAUNCH DATE ##################
# bỏ nhưng dòng không có dữ liệu
CPUs_data <- CPUs_data[complete.cases(CPUs_data$Launch_Date), ]                                                     
                                                   
# tách bỏ quý
CPUs_data$Launch_Date <- substr(CPUs_data$Launch_Date,nchar(CPUs_data$Launch_Date)-1,nchar(CPUs_data$Launch_Date))  

CPUs_data$Launch_Date <- as.integer(CPUs_data$Launch_Date)
# biến đổi về năm (dữ liệu được thu thập vào năm 2022)
CPUs_data$Launch_Date <- ifelse(CPUs_data$Launch_Date>22,1900+CPUs_data$Launch_Date,2000+CPUs_data$Launch_Date) 
# sắp xếp lại dataframe theo trật tự ưu tiên sau: năm, loại CPU, loại phân khúc
CPUs_data <- CPUs_data[order(CPUs_data$Product_Collection,CPUs_data$Vertical_Segment), ] 


# LITHOGRAPHY #################### 
CPUs_data$Lithography<- na.locf(CPUs_data$Lithography) 
# Last Observation Carried Forward (filling in any missing values in that column with the last observed value.)
CPUs_data$Lithography <-as.double( gsub(" nm$", "", CPUs_data$Lithography)) # chuyển định dạng thành số thực


# RECOMMEND CUSTOMER PRICE #################### 
# sửa định dạng chuỗi
# vì $ là ký tự đặt biệt -> \\$
CPUs_data$Recommended_Customer_Price <- gsub("\\$", "", CPUs_data$Recommended_Customer_Price) 
CPUs_data$Recommended_Customer_Price <- gsub(",", "", CPUs_data$Recommended_Customer_Price)

recommend_price <- function(price_range) {
  if(grepl('-', price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return (price_range)
}
# apply hàm để xử lý số liệu
CPUs_data$Recommended_Customer_Price <- sapply(CPUs_data$Recommended_Customer_Price, recommend_price) 
CPUs_data$Recommended_Customer_Price <- as.double(CPUs_data$Recommended_Customer_Price) # đưa phần còn lại về dạng định lượng

CPUs_data <- CPUs_data %>%  # piping
  group_by(Product_Collection) %>%  # nhóm theo production collection
  fill(Recommended_Customer_Price, .direction = "updown") # ưu tiên điền the forward carried rồi backward

# CPUs_data$Recommended_Customer_Price <- log(CPUs_data$Recommended_Customer_Price) 

# PROCESSOR BASE FREQUENCY #################### 
frequency_clean <- function(f){
  if (grepl(' GHz',f)) {
    return (as.double(gsub(" GHz","",f)))
  }
  return (as.double(gsub(" MHz","",f)) /1000)
}
(head(CPUs_data$Processor_Base_Frequency))
CPUs_data$Processor_Base_Frequency <- as.double( sapply(CPUs_data$Processor_Base_Frequency,frequency_clean))      


subset <- CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "Processor_Base_Frequency"]
CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "Processor_Base_Frequency"] <- na.locf(subset)     
plot(CPUs_data$Processor_Base_Frequency)
hist(CPUs_data$Processor_Base_Frequency)
# NUMBER OF THREAD  #################### 
ratio <- as.double(CPUs_data$nb_of_Threads/CPUs_data$nb_of_Cores)
summary(ratio)
(correlation <- cor(CPUs_data$nb_of_Threads, CPUs_data$nb_of_Cores, method = "pearson"))

CPUs_data$nb_of_Threads <- ifelse(is.na(CPUs_data$nb_of_Threads), CPUs_data$nb_of_Cores * 2, CPUs_data$nb_of_Threads) 

# CACHE   #################### 
Cache_Size_Clean <- function(size){  # default: MB
  if(grepl('M',size)){
    return (as.double(gsub(" M","",size)))
  }
  else{
    return (as.double(gsub(" K","",size)) /1024)
  }
}
# Tách dữ liệu => type & cache
CPUs_data <- separate(CPUs_data,Cache,into = c("Cache_Size","Cache_Type"),sep="B") 

# Thêm loại normal và biến rỗng
CPUs_data$Cache_Type <- ifelse(CPUs_data$Cache_Type == "", "Normal", sub(" ","",CPUs_data$Cache_Type)) 

# xử lý chuỗi và đưa về kiểu số thực
CPUs_data$Cache_Size <- sapply(CPUs_data$Cache_Size,Cache_Size_Clean) 
summary(CPUs_data$Cache_Size)


# TDP  #################### 
# Dưa về dạng định lượng
CPUs_data$TDP <-as.double( gsub(" W", "", CPUs_data$TDP))

# vì dữ liệu mất là các mobile nên sẽ fill theo nhóm mobile bằng kỹ thuật Last Observation Carried Forward
CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "TDP"] <- na.locf(CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "TDP"]) 



# MAX MEMORY SIZE ####################  
CPUs_data <- CPUs_data[complete.cases(CPUs_data$Max_Memory_Size), ] # loại bỏ ô có dữ liệu bị khuyết                  
Mem_size_func <- function(size){  
  if(grepl('G',size)){
    return ( as.double(gsub(" GB","",size)) )
  }
  return ( as.double(gsub(" TB","",size)) * 1024 )
}
# Xử lý từng ô dữ liệu: chuyển TB thành GB và đổi về cùng dạng định lượng
CPUs_data$Max_Memory_Size <- sapply(CPUs_data$Max_Memory_Size,Mem_size_func)  

# MAX MEMORY BANDWIDTH  #################### 

bandwidth_clean <- function(mem){
  return ( as.double(strsplit(mem," ")[[1]][1]) ) # truy cập đầu đến khoảng trắng đầu tiên trong 'mem'
}

CPUs_data$Max_Memory_Bandwidth <- sapply(CPUs_data$Max_Memory_Bandwidth,bandwidth_clean)

# Tính hệ số tương quan Pearson giữa hai Max_Memory_Bandwidth và Max_nb_of_Memory_Channels
(correlation <- cor(CPUs_data$Max_Memory_Bandwidth, CPUs_data$Max_nb_of_Memory_Channels, use = "complete.obs", method = "pearson"))
summary(CPUs_data$Max_Memory_Bandwidth)
summary(CPUs_data$Max_nb_of_Memory_Channels)


fill_na_with_group_median <- function(data, group_var, fill_var) {
  data <- data %>%
    group_by({{group_var}}) %>%
    mutate({{fill_var}} := if_else(is.na({{fill_var}}), median({{fill_var}}, na.rm = TRUE), {{fill_var}})) %>%
    ungroup()
  
  return(data)
}

CPUs_data <- fill_na_with_group_median(CPUs_data, Max_nb_of_Memory_Channels, Max_Memory_Bandwidth)


# INSTRUCTION SET #################### 
(SubData <- CPUs_data[complete.cases(CPUs_data$Instruction_Set), ])
(temp <- as.double( gsub("-bit", "", SubData$Instruction_Set)))
summary(temp)

# seeing temp having 64 as  median, I fill all the missing gap with 64-bit 
CPUs_data$Instruction_Set <- na.fill(CPUs_data$Instruction_Set,"64-bit") 



# KIỂM TRA LẠI DỮ LIỆU #################### 

apply(is.na(CPUs_data),2,sum) 


# kiểm tra lại số liệu và định dạng  #################### 

str(CPUs_data) 





# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
