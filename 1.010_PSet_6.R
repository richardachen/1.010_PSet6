library('pacman')
pacman::p_load(pacman, dplyr, ggplot2)
#reading files: geeksforgeeks.org/how-to-import-data-from-a-file-in-r-programming/
path <- "/Users/richy/Downloads/Classes/Fall 2022/1.010/water-treatment.csv"
all_columns <- c("Date", "Input Flow", "Input Zinc", "Input pH", 
                 "Input BOD", "Input COD", "Input Suspended Solids", 
                 "Input Volatile Suspended Solids", "Input Sediment", 
                 "Input Conductivity", "Primary Settler pH", "Primary Settler BOD", 
                 "Primary Settler Suspended Solids", "Primary Settler Volatile Suspended Solids", 
                 "Primary Settler Sediments", "Primary Settler Conductivity", 
                 "Secondary Settler pH", "Secondary Settler BOD", 
                 "Secondary Settler COD", "Secondary Settler Suspended Solids", 
                 "Secondary Settler Volatile Suspended Solids", "Secondary Settler Sediments", 
                 "Secondary Settler Conductivity", "Output pH", 
                 "Output BOD", "Output COD", "Output Suspended Solids", 
                 "Output Volatile Suspended Solids", "Output Sediments", 
                 "Output Conductivity", "Primary Settler BOD Performance", 
                 "Primary Settler Suspended Solids Performance", "Primary Settler Sediment Performance",
                 "Secondary Settler BOD Performance", "Secondary Settler COD Performance", 
                 "Global BOD Performance", "Global COD Performance", 
                 "Global Suspended Solids Performance", "Global Sediments Performance")
water_qualities <- read.csv(path, col.names = all_columns)

#clean up the data slightly
remove_q_marks <- c()

for (i in names(water_qualities)) {
  for (a in water_qualities[[i]]) {
    if (a == "?"){
      remove_q_marks <- append(remove_q_marks, i)
      # https://www.delftstack.com/howto/r/break-a-for-loop-in-r/
      break
    }
  }
}

#https://www.tutorialspoint.com/how-to-remove-a-column-from-an-r-data-frame
water_qualities <- water_qualities %>% select(-all_of(remove_q_marks))

#Problem 2

#Plot PDF
par(mfrow=c(4,2))
for (i in names(water_qualities)) {
  if (i != "Date"){
    hist(water_qualities[[i]], xlab = i, main = paste("Empirical PDF of ", i))
  }
}

#Plot CDF
# https://www.statology.org/cdf-in-r/
par(mfrow=c(4,2))
for (i in names(water_qualities)) {
  if (i != "Date"){
  p = ecdf(water_qualities[[i]])
    plot(p, main = paste("Empirical CDF of ", i))
  }
}

#Report Mean, Median
for (i in names(water_qualities)) {
  if (i != "Date"){
    mean <- mean(water_qualities[[i]])
    median <- median(water_qualities[[i]])
    print(paste("Mean of ", i, " is ", mean))
    print(paste("Median of ", i, " is ", median))
    print(paste("Difference between mean and median of ", i, " is ", abs(mean-median)))
  }
}

# [1] "Mean of  Input.pH  is  7.81007604562738"
# [1] "Median of  Input.pH  is  7.8"
# [1] "Difference between mean and median of  Input.pH  is  0.0100760456273763"
# [1] "Mean of  Input.Conductivity  is  1477.42015209125"
# [1] "Median of  Input.Conductivity  is  1405.5"
# [1] "Difference between mean and median of  Input.Conductivity  is  71.9201520912547"
# [1] "Mean of  Primary.Settler.pH  is  7.82984790874525"
# [1] "Median of  Primary.Settler.pH  is  7.8"
# [1] "Difference between mean and median of  Primary.Settler.pH  is  0.0298479087452472"
# [1] "Mean of  Primary.Settler.Suspended.Solids  is  254.001901140684"
# [1] "Median of  Primary.Settler.Suspended.Solids  is  220"
# [1] "Difference between mean and median of  Primary.Settler.Suspended.Solids  is  34.0019011406844"
# [1] "Mean of  Primary.Settler.Conductivity  is  1494.84790874525"
# [1] "Median of  Primary.Settler.Conductivity  is  1419"
# [1] "Difference between mean and median of  Primary.Settler.Conductivity  is  75.8479087452472"
# [1] "Mean of  Secondary.Settler.pH  is  7.81178707224335"
# [1] "Median of  Secondary.Settler.pH  is  7.8"
# [1] "Difference between mean and median of  Secondary.Settler.pH  is  0.0117870722433464"
# [1] "Mean of  Secondary.Settler.Conductivity  is  1489.58174904943"
# [1] "Median of  Secondary.Settler.Conductivity  is  1427.5"
# [1] "Difference between mean and median of  Secondary.Settler.Conductivity  is  62.0817490494296"

#Verification of conformance to Chebychev's inequality
for (i in names(water_qualities)){
  count <- 0
  if (i != "Date"){
    stdev <- sd(water_qualities[[i]])
    avg <- mean(water_qualities[[i]])
    for (elem in water_qualities[[i]]){
      if (abs(elem-avg) < 2*stdev){
        count <- count + 1
      }
    }
    prob_stdev <- count/length(water_qualities[[i]])
    if (prob_stdev > 0.75){
      tf <- "satisfies"
    }
    else{
      tf <- "doesn't satisfy"
    }
    print(paste("The probability a data point of ", i ," is within 2 standard deviations of the mean is ", prob_stdev, " which ", tf, " Chebychev's inequality."))
  }
}
# [1] "The probability a data point of  Input.pH  is within 2 standard deviations of the mean is  0.952471482889734  which  satisfies  Chebychev's inequality."
# [1] "The probability a data point of  Input.Conductivity  is within 2 standard deviations of the mean is  0.956273764258555  which  satisfies  Chebychev's inequality."
# [1] "The probability a data point of  Primary.Settler.pH  is within 2 standard deviations of the mean is  0.942965779467681  which  satisfies  Chebychev's inequality."
# [1] "The probability a data point of  Primary.Settler.Suspended.Solids  is within 2 standard deviations of the mean is  0.967680608365019  which  satisfies  Chebychev's inequality."
# [1] "The probability a data point of  Primary.Settler.Conductivity  is within 2 standard deviations of the mean is  0.954372623574145  which  satisfies  Chebychev's inequality."
# [1] "The probability a data point of  Secondary.Settler.pH  is within 2 standard deviations of the mean is  0.958174904942966  which  satisfies  Chebychev's inequality."
# [1] "The probability a data point of  Secondary.Settler.Conductivity  is within 2 standard deviations of the mean is  0.960076045627376  which  satisfies  Chebychev's inequality."





#Problem 3
# Y: Secondary.Settler.Conductivity
# X1: Input.pH
# X2: Input.Conductivity
# X3: Primary.Settler.Conductivity

#regular variance
variance <- var(water_qualities[['Secondary.Settler.Conductivity']])
print(paste("The variance of Secondary.Settler.Conductivity is ", variance))
#[1] "The variance of Secondary.Settler.Conductivity is  159782.502828173"

variance_conditioning <- function(dataf, Y, X){
  Y_conditioned_X <- c()
  X_mean <- mean(dataf[[X]])
  for (i in 1:nrow(dataf)){
    if (dataf[i, X] <= X_mean){
      Y_conditioned_X <- append(Y_conditioned_X, dataf[i, Y])
    }
  }
  variance_conditioned_X <- var(Y_conditioned_X)
  print(paste("The variance of ", Y, " conditioned on ", X, " is ", variance_conditioned_X))
}


#variance conditioned on X1
variance_conditioning(water_qualities, 'Secondary.Settler.Conductivity', 'Input.pH')
#[1] "The variance of  Secondary.Settler.Conductivity  conditioned on  Input.pH  is  147042.787010846"

#variance conditioned on X2
variance_conditioning(water_qualities, 'Secondary.Settler.Conductivity', 'Input.Conductivity')
#[1] "The variance of  Secondary.Settler.Conductivity  conditioned on  Input.Conductivity  is  47394.7986364426"

#variance conditioned on X3
variance_conditioning(water_qualities, 'Secondary.Settler.Conductivity', 'Primary.Settler.Conductivity')
#[1] "The variance of  Secondary.Settler.Conductivity  conditioned on  Primary.Settler.Conductivity  is  44225.6392402386"

#based on calculations, we can see that the Primary.Settler.Conductivity provides the most information about Secondary.Settler.Conductivity. This makes a lot of sense, as the conductivity in the primary settler should be closely correlated to the conductivity in the second settler.

#Question 4
?cov

# https://www.r-bloggers.com/2021/01/correlation-analysis-in-r-part-1-basic-theory/
#detect correlation, determine whether it is statistical detection given alpha = 0.01
#since alpha = 0.01, we look for P(Z<z)=0.995 on the T test table, with 526 observations --> T_524,0.995 = 2.600

correlation_detection <- function(dataf, Y, X){
  corr <- cor(dataf[[Y]], dataf[[X]])
  print(paste("The correlation between ", Y, " and ", X, " is ", corr, "."))
  t_value <- corr*sqrt(nrow(dataf)-2)/sqrt(1-corr^2)
  if (t_value > 2.600){
    tf <- " allows us to reject "
  }
  else{
    tf <- " prohibits us from rejecting "
  }
  print(paste("The t-value between ", Y, " and ", X, " is ", t_value, " and", tf, "the null hypothesis"))
  }
  #

correlation_detection(water_qualities, 'Secondary.Settler.Conductivity', 'Input.pH')
#[1] "The correlation between  Secondary.Settler.Conductivity  and  Input.pH  is  0.26612523947512 ."
#[1] "The t-value between  Secondary.Settler.Conductivity  and  Input.pH  is  6.31978639687991  and  allows us to reject  the null hypothesis"

correlation_detection(water_qualities, 'Secondary.Settler.Conductivity', 'Input.Conductivity')
#[1] "The correlation between  Secondary.Settler.Conductivity  and  Input.Conductivity  is  0.940008911135854 ."
#[1] "The t-value between  Secondary.Settler.Conductivity  and  Input.Conductivity  is  63.0742912898615  and  allows us to reject  the null hypothesis"

correlation_detection(water_qualities, 'Secondary.Settler.Conductivity', 'Primary.Settler.Conductivity')
#[1] "The correlation between  Secondary.Settler.Conductivity  and  Primary.Settler.Conductivity  is  0.947420287006798 ."
#[1] "The t-value between  Secondary.Settler.Conductivity  and  Primary.Settler.Conductivity  is  67.7749760750327  and  allows us to reject  the null hypothesis"

# clean things up at the end
# https://community.rstudio.com/t/how-to-clear-the-r-environment/14303/6
pacman::p_unload(all)