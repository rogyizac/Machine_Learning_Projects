library(ggplot2)
#-----------------------------------------------------------------------------------------------------------------
#------------------------------FUNCTIONS for plotting Learning and Complexity curves------------------------------
#-----------------------------------------------------------------------------------------------------------------

#RMSE function to calculate rmse values
rmse_func <- function(predictions, test_target){
  rmse <- sqrt(mean((predictions - test_target)^2))
  return(rmse)
} 

#Input arguments------------------COMPLEXITY CURVE FUNCTION---------------------------------------------------
#train : Training data(Dataframe)
#test : Testing dataset(Dataframe)
#target : target variable in string format. Other variables in dataframe considered as predictors
#degree : Set till which order of polynomial degrees is RMSE required
#example if degree is set to 3 then rmse for degree 1, degree 2 and degree 3 will be calculate
#function returns dataframe with order and rmse value - functions also works with many predictors
complexity_curve <- function(train, test, target, degree = 1){
  #initialise variables
  txt = ""
  order = c()
  test_rmse = c()
  train_rmse = c()
  #set target variable and predictors
  target_ind <- which(colnames(train) == target)
  predictors <- colnames(train)[colnames(train) != target]
  
  #looping to create formula for polynomial regression
  for(i in 1:degree){
    if(i != 1){
      txt <- paste(txt, " + ", sep = "")
    }
    txt <- paste(txt, paste("I(", predictors,"^",i, ")", sep = "", collapse = " + "), sep = "")
    
    rhs <- paste(target, txt, sep=" ~ ")
    formula_regress <- as.formula(rhs)
    
    order <- c(order, i)
    #test results rmse
    model <- lm(formula_regress, data = train)
    predictions <- predict(model, test)
    rmse <- rmse_func(predictions, test[,target])
    test_rmse <- c(test_rmse, rmse)
    #train results rmse
    predictions <- predict(model, train)
    rmse <- rmse_func(predictions, train[,target])
    train_rmse <- c(train_rmse ,rmse)
  }
  #Convert to dataframe and return
  comp_df <- data.frame(list(order, train_rmse, test_rmse))
  colnames(comp_df) <- c("Order", "Train_rmse", "Test_rmse")
  return(comp_df)
}

#Set required seed
set.seed(100)

#Read data file
setwd("C:\\Users\\rohan\\Desktop\\Praxis coureswork\\ML and Python - Gaurab Nath\\machine learning\\Bike-Sharing-Dataset")
df <- read.csv("hour.csv")

#Filtering out required columns and removing bad data
df <- subset(df, windspeed != 0)

#load required columns
df <- df[,c("temp", "cnt")]

#Run the learning curves function
comp_df <- complexity_curve(train, test, "cnt", 3)

# Plotting the complexity graph
p = ggplot(colour = "line") + geom_smooth(aes(x = Order, y = Test_rmse, color = "Test RMSE"), data = comp_df, geom = "smooth") + 
  geom_smooth(aes(x = Order, y = Train_rmse, color = "Train RMSE"), data = comp_df, geom = "smooth")   +
  xlab("Order") + ylab("RMSE") + scale_color_manual(values = c("blue","red")) + labs(colour = "Legend")  + ggtitle("Complexity Curves") +
  theme(plot.title = element_text(hjust = 0.5, size = 22)) + ylim(162, 170)
print(p)

#input arguments---------------------------LEARNING CURVE FUNCTION-----------------------------------------
#df : your entire dataframe
#test_size : setting fixed test size
#target : target variable in string formay example : "colname"
#degree : degree of polynomial for which learning curve is required
#No of splits to perform on training data, default is 10
learning_curve <- function(df, test_size, target, degree, splits = 10){
  #intialize values
  train_size <- c()
  test_rmse <- c()
  train_rmse <- c()
  rhs = ""
  txt = ""
  #split data into train and a fixed test
  learn_obs <- sample(nrow(df), test_size)
  df <- df[-learn_obs,]
  test_df <- df[learn_obs,]
  #Creating train sizes of various lengths
  train_sizes <- seq(100, nrow(df) - 10, length.out = splits)  #seq(100, nrow(df), 400)

  #selecting target and predictors
  target_ind <- which(colnames(df) == target)
  predictors <- colnames(train)[colnames(train) != target]
  
  #Creating polynomial regression formula
  for(i in 1:degree){
    if(i != 1){
      txt <- paste(txt, " + ", sep = "")
    }
    txt <- paste(txt, paste("I(", predictors,"^",i, ")", sep = "", collapse = " + "), sep = "")
  }
  
  rhs <- paste(target, txt, sep=" ~ ")
  formula_regress <- as.formula(rhs)
  formula_regress
  #Looping through each train size, create model and evaluate rmse
  for(i in train_sizes){
    
    obs <- sample(nrow(df), i)
    
    train_size <- c(train_size, length(obs))
    train <- df[obs,]
    
    model <- lm(formula_regress, data = train)
    
    predictions <- predict(model, test)
    rmse <- rmse_func(predictions, test[,target])
    test_rmse <- c(test_rmse, rmse)
    
    predictions <- predict(model, train)
    rmse <- rmse_func(predictions, train[,target])
    train_rmse <- c(train_rmse, rmse)
  }
  #Return a dataframe consisting of columns - sample size, test rmse, train rmse..
  learn_df <- data.frame(list(train_size, test_rmse, train_rmse))
  colnames(learn_df) <- c("sample_size","test_rmse", "train_rmse")
  return(learn_df)
}

#example
learn_df <- learning_curve(df_learn, 2000, "cnt", 7, split = 500)
View(learn_df)

#plotting the lines
p = ggplot(colour = "line") + geom_smooth(aes(x = sample_size, y = test_rmse, color = "Test RMSE"), data = learn_df, geom = "smooth") + 
  geom_smooth(aes(x = sample_size, y = train_rmse, color = "Train RMSE"), data = learn_df, geom = "smooth") + ylim(160, 170) +
  xlab("Train Sizes") + ylab("RMSE") + scale_color_manual(values = c("blue","red")) + labs(colour = "Legend")  + ggtitle("Polynomial Order - 7") +
  theme(plot.title = element_text(hjust = 0.5, size = 22))
print(p)

#----------------------------COMBINED PLOTS---------------------------------------------------------------------
#--------------------------Plotting combined Learning curve graphs with test rmse--------------------------------------------------------------------
#Initialization, change degree according to requirment
degree = 5
predictors = "temp"
target = "cnt"
txt = ""
train_size <- c()
test_rmse5 <- c()

#Formula generator
for(i in 1:degree){
  if(i != 1){
    txt <- paste(txt, " + ", sep = "")
  }
  txt <- paste(txt, paste("I(", predictors,"^",i, ")", sep = "", collapse = " + "), sep = "")
}

rhs <- paste(target, txt, sep=" ~ ")
formula_regress <- as.formula(rhs)
formula_regress

#selecting different train sizes
train_sizes <- seq(100, nrow(df) - 10, length.out = 500)

for(i in train_sizes){
  
  obs <- sample(nrow(df), i)
  
  train_size <- c(train_size, length(obs))
  train <- df[obs,]
  
  model <- lm(formula_regress, data = train)
  
  predictions <- predict(model, test)
  rmse <- rmse_func(predictions, test[,target])
  #Change the test_rmse according degree. example if degree = 4 then test_rmse4
  test_rmse5 <- c(test_rmse5, rmse)
}

#Various rmse values
test_rmse1
test_rmse3
test_rmse5
test_rmse7
test_rmse9
train_size

#plotting combined graph using ggplot
p = ggplot(colour = "line") + geom_smooth(aes(x = train_size, y = test_rmse1, color = "Test RMSE Order 1"), data = learn_df, geom = "smooth") + 
  geom_smooth(aes(x = train_size, y = test_rmse3, color = "Test RMSE Order 3"), data = learn_df, geom = "smooth") +
  geom_smooth(aes(x = train_size, y = test_rmse5, color = "Test RMSE Order 5"), data = learn_df, geom = "smooth") +
  geom_smooth(aes(x = train_size, y = test_rmse7, color = "Test RMSE Order 7"), data = learn_df, geom = "smooth") +
  xlab("Train Sizes") + ylab("RMSE") + labs(colour = "Legend")  + ggtitle("Combined Test Error Graphs") +
  theme(plot.title = element_text(hjust = 0.5, size = 22)) 
print(p)

#-----------------------------------Polynomial regression model plotting against X and Y-----------------------------------------------------
degree = 1
predictors = "temp"
target = "cnt"
txt = ""
train_size <- c()
#change according to requirement
test_rmse1 <- c()
#Formula generation for a degree 'n'
for(i in 1:degree){
  if(i != 1){
    txt <- paste(txt, " + ", sep = "")
  }
  txt <- paste(txt, paste("I(", predictors,"^",i, ")", sep = "", collapse = " + "), sep = "")
}

rhs <- paste(target, txt, sep=" ~ ")
formula_regress <- as.formula(rhs)

#individual graphs
ggplot(aes(temp,cnt), data = df) + geom_point() + stat_smooth(aes(colour="Order 7"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 7, raw=TRUE)) + 
  ggtitle("Regression Order - 7") + theme(plot.title = element_text(hjust = 0.5, size = 22)) + xlab("Temperature") + ylab("Count")  + labs(colour = "Legend") +
  scale_color_manual(values = "red")

ggplot(aes(temp,cnt), data = df) + geom_point() + stat_smooth(aes(colour="Order 5"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 5, raw=TRUE)) + 
  ggtitle("Regression Order - 5") + theme(plot.title = element_text(hjust = 0.5, size = 22)) + xlab("Temperature") + ylab("Count")  + labs(colour = "Legend") +
  scale_color_manual(values = "red")

ggplot(aes(temp,cnt), data = df) + geom_point() + stat_smooth(aes(colour="Order 3"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE)) + 
  ggtitle("Regression Order - 3") + theme(plot.title = element_text(hjust = 0.5, size = 22)) + xlab("Temperature") + ylab("Count")  + labs(colour = "Legend") +
  scale_color_manual(values = "red")

ggplot(aes(temp,cnt), data = df) + geom_point() + stat_smooth(aes(colour="Order 1"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 1, raw=TRUE)) + 
  ggtitle("Regression Order - 1") + theme(plot.title = element_text(hjust = 0.5, size = 22)) + xlab("Temperature") + ylab("Count")  + labs(colour = "Legend") +
  scale_color_manual(values = "red")

#Combined polynomial regression graph
ggplot(aes(temp,cnt), data = df) + geom_point() + stat_smooth(aes(colour="Order 1"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 1, raw=TRUE)) + 
  stat_smooth(aes(colour="Order 3"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE)) + 
  stat_smooth(aes(colour="Order 5"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 5, raw=TRUE)) + 
  stat_smooth(aes(colour="Order 7"),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 7, raw=TRUE)) +
  ggtitle("Regression - All Orders") + theme(plot.title = element_text(hjust = 0.5, size = 22)) + xlab("Temperature") + ylab("Count")  + labs(colour = "Legend")