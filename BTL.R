library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)
library(caret)
library(car)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools)
library(plotly)

# ĐỌC SỐ LIỆU####################
# đọc dữ liệu và gán các giá trị chuỗi rỗng và "N/A" thành NA
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



# LAUNCH DATE ##################
# bỏ nhưng dòng không có dữ liệu
CPUs_data <- CPUs_data[complete.cases(CPUs_data$Launch_Date), ]                                                     
                                                   
# tách bỏ quý
CPUs_data$Launch_Date <- substr(CPUs_data$Launch_Date,nchar(CPUs_data$Launch_Date)-1,nchar(CPUs_data$Launch_Date))  

CPUs_data$Launch_Date <- as.integer(CPUs_data$Launch_Date)
# biến đổi về năm (dữ liệu được thu thập vào năm 2022)
CPUs_data$Launch_Date <- ifelse(CPUs_data$Launch_Date>22,1900+CPUs_data$Launch_Date,2000+CPUs_data$Launch_Date) 
# sắp xếp lại dataframe theo trật tự ưu tiên sau: năm, loại CPU, loại phân khúc
CPUs_data <- CPUs_data[order(CPUs_data$Launch_Date,CPUs_data$Product_Collection,CPUs_data$Vertical_Segment), ] 


# LITHOGRAPHY #################### 
CPUs_data$Lithography<- na.locf(CPUs_data$Lithography) 
# Last Observation Carried Forward (filling in any missing values in that column with the last observed value.)
CPUs_data$Lithography <-as.double( gsub(" nm$", "", CPUs_data$Lithography)) # chuyển định dạng thành số thực


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

# NUMBER OF THREAD  #################### 
ratio <- as.double(CPUs_data$nb_of_Threads/CPUs_data$nb_of_Cores)
summary(ratio)
(correlation <- cor(CPUs_data$nb_of_Threads, CPUs_data$nb_of_Cores, method = "pearson"))

CPUs_data$nb_of_Threads <- ifelse(is.na(CPUs_data$nb_of_Threads), CPUs_data$nb_of_Cores * 2, CPUs_data$nb_of_Threads) 


# MAX MEMORY BANDWIDTH  #################### 


bandwidth_clean <- function(mem){
  return ( as.double(strsplit(mem," ")[[1]][1]) ) # truy cập đầu đến khoảng trắng đầu tiên trong 'mem'
}

CPUs_data$Max_Memory_Bandwidth <- sapply(CPUs_data$Max_Memory_Bandwidth,bandwidth_clean)

# Tính hệ số tương quan Pearson giữa hai Max_Memory_Bandwidth và Max_nb_of_Memory_Channels
(correlation <- cor(CPUs_data$Max_Memory_Bandwidth, CPUs_data$Max_nb_of_Memory_Channels, method = "pearson"))


for( i in unique(CPUs_data$Max_nb_of_Memory_Channels) ){
  subset <- CPUs_data[CPUs_data$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth']
  fillValue =  median(CPUs_data[CPUs_data$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth'],na.rm = TRUE)
  CPUs_data[CPUs_data$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth'] = na.fill(subset,fillValue) 
}

# PRODUCT COLLECTION #################### 
product_collect <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')

for (i in product_collect) {
  CPUs_data$Product_Collection <- ifelse(grepl(i, CPUs_data$Product_Collection), i, CPUs_data$Product_Collection) 
}

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

#CPUs_data$Recommended_Customer_Price <- log(CPUs_data$Recommended_Customer_Price) 

# PROCESSOR BASE FREQUENCY #################### 
frequency_clean <- function(f){
  if (grepl(' GHz',f)) {
    return (as.double(gsub(" GHz","",f)))
  }
  return (as.double(gsub(" MHz","",f)) /1000)
}
CPUs_data$Processor_Base_Frequency <- as.double( sapply(CPUs_data$Processor_Base_Frequency,frequency_clean))      


subset <- CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "Processor_Base_Frequency"]
CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "Processor_Base_Frequency"] <- na.locf(subset)     

# TDP  #################### 
# Dưa về dạng định lượng
CPUs_data$TDP <-as.double( gsub(" W", "", CPUs_data$TDP))

# vì dữ liệu mất là các mobile nên sẽ fill theo nhóm mobile bằng kỹ thuật Last Observation Carried Forward
CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "TDP"] <- na.locf(CPUs_data[CPUs_data$Vertical_Segment == "Mobile", "TDP"]) 



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


# INSTRUCTION SET #################### 
(SubData <- CPUs_data[complete.cases(CPUs_data$Instruction_Set), ])
(temp <- as.double( gsub("-bit", "", temp$Instruction_Set)))
summary(temp)

CPUs_data$Instruction_Set <- na.fill(CPUs_data$Instruction_Set,"64-bit")   


# KIỂM TRA LẠI DỮ LIỆU #################### 

apply(is.na(CPUs_data),2,sum) 


# kiểm tra lại số liệu và định dạng  #################### 

str(CPUs_data) 


# LÀM RÕ DỮ LIỆU  #################### 

numerical_cols = c("Launch_Date","Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                   "Processor_Base_Frequency","TDP","Cache_Size","Max_Memory_Size","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")
categorical_cols = c("Product_Collection","Vertical_Segment","Status","Cache_Type","Instruction_Set")

summary_numeric_table <- data.frame(
  Staticstic=c("Count", "Mean", "STD", "Min", "First Quantile", "Median", "Third Quantile", "Max")
)
for (i in numerical_cols){
  count <- length(CPUs_data[[i]])
  mean<- mean(CPUs_data[[i]])
  std <- sd(CPUs_data[[i]])
  min <- min(CPUs_data[[i]])
  first_quantile <- sapply(CPUs_data[i], function(x) quantile(x, 0.25) )[[1]]
  median <- median(CPUs_data[[i]])
  third_quantile <- sapply(CPUs_data[i], function(x) quantile(x, 0.75))[[1]]
  max <- max(CPUs_data[[i]])
  summary_numeric_table <- cbind(summary_numeric_table,new_col=c(count,mean,std,min,first_quantile,median,third_quantile,max))
}
colnames(summary_numeric_table) <- c("",numerical_cols)

summary_categorical_table <- data.frame(
  Staticstic = c("Count","Unique","Mode","Freq")
)
for (i in categorical_cols) {
  count <- length(CPUs_data[[i]])
  unique <- length( unique(CPUs_data[[i]]))
  mode <- Mode(CPUs_data[[i]])
  freq <- attr(mode,"freq")
  summary_categorical_table <- cbind(summary_categorical_table,new_col=c(count,unique,mode,freq))
}
colnames(summary_categorical_table) <- c("",categorical_cols)
# summary table
summary_numeric_table

cor(CPUs_data[numerical_cols])

summary_categorical_table
# Change in launch year

Litho_year <- CPUs_data %>% 
  group_by(Launch_Date) %>%
  summarize(
    mean_thick= mean(Lithography),
    .groups = "drop"
  )

ggplot(Litho_year, aes(Launch_Date,mean_thick)) +
  geom_line() +
  labs(x = "Launch Date", y = "Thickness", title = "Line plot of thickness of CPU over year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Date), max(Litho_year$Launch_Date), by = 1))

ggplot(data = CPUs_data, aes(y = Status, x = Launch_Date, fill = Status)) +
  geom_boxplot() +
  labs(x = "Status", y = "Launch Date",title = "Boxplot of Status over Year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Date), max(Litho_year$Launch_Date), by = 1)) 
ggplot(data = CPUs_data, aes(y = Instruction_Set, x = Launch_Date, fill = Instruction_Set)) +
  geom_violin() +
  labs(x = "Launch Date", y = "Instruction set",title = "Violinplot of Instruction set over Year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Date), max(Litho_year$Launch_Date), by = 1)) # Instruction set
# core and thread 
ggplot(CPUs_data, aes(x = nb_of_Threads, y = nb_of_Cores)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="number of Threads",y="number of Cores",title = "Analysis of the Relationship between Number of Threads and Number of Cores")

# memory

ggplot(CPUs_data, aes(Max_nb_of_Memory_Channels,Max_Memory_Bandwidth)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x="Max number of Memory Channels",y="Max Memory Bandwith",title="Relationship of Max Number of Memory Channels and Max Memory BandWidth")

# Frequency 
ggplot(CPUs_data,aes(Processor_Base_Frequency,Vertical_Segment,fill=Vertical_Segment)) + 
  geom_violin() +
  labs(y="Vertical Segment",x="Frequency",title="violinplot with Vertical Segment and Frequency")
#Vertical Segment and max mem size

ggplot(CPUs_data,aes(Vertical_Segment,Max_Memory_Size,fill=Vertical_Segment)) + 
  geom_boxplot() +
  labs(x="Vertical Segment",y="Max Memory Size",title="boxplot with Vertical Segment and Max Memory Size")

# recommended customer price 
ggplot(CPUs_data,aes(Recommended_Customer_Price))+
  geom_histogram(aes(y=after_stat(density)))+
  geom_density() +
  labs(title = "Histogram of Recommended Customer Price")

Price_year<- CPUs_data %>%
  group_by(Launch_Date, Product_Collection) %>%
  summarize(
    mean_price = mean(Recommended_Customer_Price),
    .groups = "drop",
    lower = if (length(Recommended_Customer_Price) > 5& sd(Recommended_Customer_Price) != 0) {
      t.test(Recommended_Customer_Price, conf.level = 0.95)$conf.int[1]
    } else {
      min(Recommended_Customer_Price)
    },
    upper = if (length(Recommended_Customer_Price) > 5 & sd(Recommended_Customer_Price) != 0) {
      t.test(Recommended_Customer_Price, conf.level = 0.95)$conf.int[2]
    } else {
      max(Recommended_Customer_Price)
    }
  )
ggplot(Price_year, aes(Launch_Date, mean_price,colour = Product_Collection,fill=Product_Collection)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x="Launch Date",y="Price",title="Lineplot of Recommended Customer Price of Product Collection over Year")+
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Date), max(Litho_year$Launch_Date), by = 1))

ggplot(CPUs_data,aes(TDP,Recommended_Customer_Price,colour=Vertical_Segment))+
  geom_point() +
  labs(y="Price",title="Scatterplot of Price and TDP with Vertical Segment")

ggplot(CPUs_data,aes(Recommended_Customer_Price,Cache_Size,colour=Cache_Type)) +
  geom_point() +
  labs(x="Price",y="Cache Size",title="Scatterplot of Price and Cache Size with Cache Type")

# KIỂM ĐỊNH GIẢ THUYẾT #################### 

#kiem dinh sharpiro
for (i in unique(CPUs_data$Product_Collection)){
  test <- shapiro.test((CPUs_data[CPUs_data$Product_Collection==i,]$Recommended_Customer_Price))
  cat("p value of ",i,": ",test[[2]],'\n')
}

#Ve bieu do kiem tra pp chuan
plot(aov(CPUs_data$Recommended_Customer_Price~CPUs_data$Product_Collection),2)

#Kiem tra phuong sai
LeveneTest(Recommend_Price~Product_Collection)

#Kiem dinh Anova

anova <- aov(Recommend_Price~Product_Collection, data = CPUs_data)
summary(anova)

TukeyHSD(anova)

# CHUẨN BỊ DỮ LIỆU 

# one hot encoder cho dữ liệu
dummy <- dummyVars('~.',data=CPUs_data,sep = ".") 
CPUs_data <- data.frame(predict(dummy, newdata = CPUs_data))

# chia dữ liệu thành tập train và test
train_index <- createDataPartition(CPUs_data$Recommended_Customer_Price, p = 0.8, list = FALSE)
train_data = CPUs_data[train_index,]
test_data = CPUs_data[-train_index,]

melted_corr_mat <- melt(cor(train_data))
ggplot(data = melted_corr_mat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name= "Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title = "Heatmap of Correlation in train data",x="",y="")

# XÂY DỰNG MÔ HÌNH ####################

model <- lm(Recommended_Customer_Price ~.,data=train_data)
summary(model)

# ĐÁNH GIÁ MÔ HÌNH ####################

y_train_pred <- predict(model,newdata=train_data,response = "Recommended_Customer_Price")
y_test_pred <- predict(model, newdata = test_data,response = "Recommended_Customer_Price")
mse_train<- mse(y_train_pred,train_data$Recommended_Customer_Price)
mse_test<- mse(y_test_pred,test_data$Recommended_Customer_Price)
mae_train <- mae(y_train_pred,train_data$Recommended_Customer_Price)
mae_test <- mae(y_test_pred,test_data$Recommended_Customer_Price)
metric <- data.frame(
  variable = c("MSE","MSE","MAE","MAE"),
  value = c(mse_train,mse_test, mae_train, mae_test),
  type = c("train","test","train","test")
)
ggplot(metric,aes(x = variable, y = value,color = type,group=type)) +
  geom_line() +
  geom_point(size=4) +
  labs(x = "", y = "Value", color = "Type")





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
