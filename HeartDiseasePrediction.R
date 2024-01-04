library(ggplot2)
library(gridExtra)
install.packages("gridExtra")
library(reshape2)
install.packages("reshape2")
library(grid)
data<-read.csv("C:\\dataset\\HeartFailure\\heart.csv")
head(data,n=5)
str(data)
missing_val<-colSums(is.na(data))
missing_val
library(DT)
install.packages("DT")
library(dplyr)
install.packages("dplyr")
names(data)
library(cowplot)
install.packages("cowplot")
names<-c('Age','Sex','ChestPainType','RestingBP','Cholesterol',
         'FastingBS','RestingECG','MaxHR','ExerciseAngina',
         'Oldpeak','ST_Slope','HeartDisease')
custom_theme<-theme_minimal()+
  theme(
    text=element_text(size=14),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    legend.text=element_text(size=15)
  )
age_hist_plot<-ggplot(data,aes(x=Age))+
  geom_histogram(bins=10,fill="lightblue",color="black")+
  labs(title="Histogram Plot of Age",x="Age",y="Frequency")+
  custom_theme
ggsave("age_histogram.png",plot=age_hist_plot,width=10,height=5,units="in")
print(age_hist_plot)
heart_counts<-table(data$'HeartDisease')
heart_counts_df<-data.frame(Disease_Status=factor(names(heart_counts),
                                                  levels=names(heart_counts)),
                            Count=as.numeric(heart_counts))
bar_plot<-ggplot(heart_counts_df,aes(x=Disease_Status,y=Count,
                                     fill=Disease_Status))+
  geom_bar(stat="identity")+
  labs(x="Heart Disease Status",y="Count")+
  scale_fill_manual(values=c("0"="darkgreen","1"="red"))+
  theme_minimal()
print(bar_plot)
selected_column<-data$Sex
category_counts<-table(selected_column)
percentages<-round((category_counts/sum(category_counts)) * 100, 0)
custom_colors<-c("skyblue","lightcoral")
par(mar = rep(0,4))
par(oma = rep(0,4))
pie(category_counts, labels = paste(names(category_counts), "(",
                                    percentages, "%)"), 
    col = custom_colors, main = "Pie Chart of Gender")
X <- select(data, -'HeartDisease')
y<-data$'HeartDisease'
set.seed(123)
sample_indices<-sample(nrow(data),0.8*nrow(data))
train_data<-data[sample_indices, ]
test_data<-data[-sample_indices, ]
dim(X)
dim(train_data)
dim(test_data)
model<-glm(HeartDisease~ ., data=train_data, family=binomial)
predictions<-predict(model, newdata=test_data, type="response")
threshold<-0.5
predicted_classes<-ifelse(predictions>threshold, 1, 0)
confusion_matrix<-table(Actual=test_data$HeartDisease, Predicted=predicted_classes)
accuracy<-sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Logistic Regression Accuracy Score for the Heart Disease Prediction is: ",
    accuracy, "\n")
new_data<-data.frame(
  Age=c(35,65),
  Sex=c("F","M"),
  ChestPainType=c("ATA","TA"),
  RestingBP=c(120,160),
  Cholesterol=c(180,250),
  FastingBS=c(0,1),
  RestingECG=c("Normal","ST"),
  MaxHR=c(180,140),
  ExerciseAngina=c("N","Y"),
  Oldpeak=c(0.0,2),
  ST_Slope=c("Up","Down")
)
new_predictions<-predict(model,newdata=new_data,type="response")
threshold<-0.5
new_predicted_classes<-ifelse(new_predictions>threshold,1,0)
new_predicted_classes
