rm(list = ls()) # Clearing all previous variables
dataset <-
  read.csv("C:/Users/Asus/Desktop/Data Mining Project/global air pollution dataset.csv")

# Importing Dataset
View(dataset)

# Deleting rows with blank values in Country/City column
dataset <- dataset[dataset$Country!="", ]
dataset <- dataset[dataset$City!="", ]
View(dataset)


#Let us visualize the relation between various pollutants and air 
#quality.
 
library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = Ozone_Value, y = AQI_Value)) + 
  geom_point(color = "orange", alpha = .7, size = 2)+ #Specifying color, opacity and size 
  geom_smooth( method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs Ozone Value in air") + 
  #Title/heading of the plot
  xlab("Ozone Value") + #Label of x axis
  ylab("AQI Value") #Label of y axis





# (Plotting AQI vs Carbon-Monixide Value)
library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = CO_Value, y = AQI_Value)) + 
  geom_point(color = "green", alpha = 1, size = 2)+ #Specifying color, opacity and size 
  geom_smooth(method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs Carbon-Monoxide Value in air") + 
  #Title/heading of the plot
  xlab("Carbon Monoxide Value") + #Label of x axis
  ylab("AQI Value") #Label of y axis


# (Plotting AQI vs Nitrous Oxide Value)
library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = NO2_Value, y = AQI_Value)) + 
  geom_point(color = "brown", alpha = 1, size = 2)+ #Specifying color, opacity and size 
  geom_smooth(method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs Nitrous Oxide Value in air") + 
  #Title/heading of the plot
  xlab("Nitrous Oxide Value") + #Label of x axis
  ylab("AQI Value") #Label of y axis


# (Plotting AQI vs PM 2.5 Value)
library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = PM_2.5_Value, y = AQI_Value)) + 
  geom_point(color = "black", alpha = 1, size = 2)+ #Specifying color, opacity and size 
  geom_smooth(method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs value of 2.5 μm particulates in air") + 
  #Title/heading of the plot
  xlab("2.5 μm particulates value") + #Label of x axis
  ylab("AQI Value") #Label of y axis



# (Barchart for each class of label)
# Factoring our label from ‘Good’ to ‘Hazardous’
AQI_Category_list <-
  factor(dataset$AQI_Category, levels=c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 
                                        'Unhealthy', 'Very Unhealthy', 'Hazardous'))
# Making a barplot
barplot(table(AQI_Category_list),
        main= "Number of cities divided by quality of air",
        xlab= "Grade",
        ylab= "Count",
        border= "red",
        col=c( "green" , "yellow" , "orange" , "red" , "brown" , "black" ))




# Deleting unnecessary columns
dataset<-dataset[ , !(names(dataset) %in% 
                        c('AQI_Value','Country','City','CO_Value','Ozone_Value','NO2_Value','PM_2.5_Value'))]
# Reordering
dataset<-dataset[, c(5, 3, 4, 2,1 )]
# Giving the label a simpler name
names(dataset)[5]='class'


# TDIDT tree training function


train_tdidt_tree<-function(DF, parent_name, parent_split_by){
  if(names(DF[1])=='X')
  {
    this_column<-DF[2]
  }
  else{
    this_column<-DF[1]
  }
  column_name<-names(this_column)
  #print(column_name)
  if(column_name=='class' || is.null(column_name)){return(0)}
  dict<- hash(keys=c('name','parent_name','parent_split_by'), values=c(column_name, 
                                                                       parent_name, parent_split_by))
  for(j in unique(this_column))
  {
    #print(names(this_column))
    for(k in 1:length(j))
    {
      if(length(unique(subset(DF, DF[names(this_column)]==j[k])$class))==1)
      {
        dict[j[k]]=unique(DF[DF[names(this_column)]==j[k], ]$class)
      }else{ #print('not unique')
        dict[j[k]]=names(DF)[match(names(this_column),names(DF))+1]
        
        list1<-append(list1,dict)
        DFx<-DF[DF[names(this_column)]==j[k],]
        DFx<-DFx[ , !(names(DFx) %in% c(names(this_column)))]
        parent_name<-names(this_column)
        parent_split_by<-j[k]
        list1<-append(list1,train_tdidt_tree(DFx,parent_name,parent_split_by))
      }
    }
  }
  list1<-append(list1,dict)
  
  return(unique(list1))
}
# Splitter function
splitter = function(dataset, training_ratio)
{ # This function will split the dataset...
  # ...into training and testing sets and then return them in a list
  
  if(training_ratio>=1 | training_ratio<=0)
  {return("Training ratio has to be a fraction value i range: 0<x<1")
  }else
  {training_row_count<-round(training_ratio*nrow(dataset)) # No. Of rows
  testing_row_count<-round((1-training_ratio)*nrow(dataset))
  training_dataset<-head(dataset, training_row_count) # Splitting by head()
  testing_dataset<-tail(dataset, testing_row_count) # Splitting by tail()
  return_values <- list(training_dataset, testing_dataset) # Binding in a list
  return(return_values) # Returning the list
  }
  
}





# Function to count branches of the induced tree
branch_counter<-function(induced_tree)
{
  branch_count=0
  for(i in 1:length(induced_tree))
  {
    
    branch_count=branch_count+length(keys(induced_tree[i][[1]]))-3
    
  }
  
  return(branch_count)
}



# Calling the TDIDT tree training function


train_set<-data.frame(splitter(dataset,0.80)[1]) 
test_set<-data.frame(splitter(dataset,0.80)[2])
list1<-list() # Tree list to store all the nodes
induced_tree<-train_tdidt_tree(train_set,"", "")
print(induced_tree)
branch_counter(induced_tree)



# Function to generate E_start
get_E_start<-function(train_df)
{
  E_start=0
  for(i in 1:length(table(train_df$class)))
  {
    ratio<-(table(train_df$class)[[i]]/nrow(train_df)) 
    
    E_start=E_start+(-1)*ratio*log(ratio, base=2)
    
  }
  
  return(E_start) 
}


# Function to generate E_new values for a column

get_E_new<-function(train_df,column_name)
{ E_new=0
for(i in 1:nrow(unique(train_set[column_name])))
{ 
  
  
  unique_value<-unique(train_df[column_name])[[1]][i]
  
  ratio<-table(train_set[column_name])[[i]]/nrow(train_set)
  
  log_result<-get_E_start(train_df[train_df[column_name]==unique_value,])
  
  E_new=E_new + ratio*log_result
  
}
return(E_new)
}


# Function to generate IG value for a column
get_IG<-function(train_df,column_name)
{ E_new=0
for(i in 1:nrow(unique(train_df[column_name])))
{ 
  
  
  unique_value<-unique(train_df[column_name])[[1]][i]
  
  ratio<-table(train_df[column_name])[[i]]/nrow(train_df)
  
  
  log_result<-get_E_start(train_df[train_df[column_name]==unique_value,])
  
  E_new=E_new + ratio*log_result
  
}
IG<-get_E_start(train_df)-E_new
return(IG)
}



# Function to generate split information for training set
get_SI<-function(train_df,column_name)
{
  SI=0
  for(i in 1:length(table(train_df[column_name])))
  {
    ratio<-(table(train_df[column_name])[[i]]/nrow(train_df)) 
    
    SI=SI+(-1)*ratio*log(ratio, base=2)
    
  }
  
  return(SI) 
}

# Information gain for the columns
print('------------------')
get_IG(train_set,'PM_2.5_Category') 
get_IG(train_set,'Ozone_Category') 
get_IG(train_set,'NO2_Category') 
get_IG(train_set,'CO_Category')


# Evaluating accuracy

evaluate_accuracy(induced_tree, test_set)

# let us create a confusion matrix for the model
test_set2<-evaluate_accuracy(induced_tree, test_set)
test_set2[test_set2 == "Unhealthy for Sensitive Groups"] <- "UFSG"
set.seed(123)
data <- data.frame(Actual = test_set2$class,
                   Prediction = test_set2$predicted_class
)
table(data$Prediction, data$Actual)







