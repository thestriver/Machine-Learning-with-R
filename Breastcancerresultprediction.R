wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
#informative labelling
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
#closer look at 3 measurement of the features
summary(wbcd[c("radius_mean", "area_mean" , "smoothness_mean")])

#normalize huge variances in smoothness mean range and area mean range
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#dividing into training and testing datasets
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#adding diagnosis factor for training and testing
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#downloading with install.packages("class") to make datasets classification 
library(class)

#using knn clasification where knn(train, test, class, k) k is square root of 469 (training set) which is 21

#using the gmodels' CrossTable() function will help evaluate the predicted class. prop.chisq is false to remove unnecessary chi-sqaure values from the output

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,                       prop.chisq=FALSE)

#the results divides into true / false postivies and true/false negatives

#2 malign samples were incorrectly predicted as benign. To correct this, one can use better scaling like zcore
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean) #N.B -3 or > 3 score are extremely rare

#follow same step as from line 17
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,                        cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,                       prop.chisq = FALSE)
#it gave same value though textbook sample was different. A change in k might give different result
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,                        cl = wbcd_train_labels, k = 11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,                       prop.chisq = FALSE)
#Oops.Lesser malignant error of 1 but predicted a benign as malignant which can put more emotional and/or financial strain on the health system and patient.



