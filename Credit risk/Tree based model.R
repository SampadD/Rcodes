loan_data<-readRDS("C:/Users/Sampad/Downloads/loan_data_ch1.rds")

#install.packages("rpart")
library(rpart)

#Tree with prior probabilities
tree_prior <- rpart(loan_status ~ ., method = "class",
                    data = training_set,parms= list(prior=c(0.7, 0.3)),control = rpart.control(cp = 0.001))


# Plot the decision tree
plot(tree_prior,uniform=TRUE)

# Add labels to the decision tree
text(tree_prior)


# Change the code below such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily for misclassified defaults.
tree_loss_matrix <- rpart(loan_status ~ ., method = "class",
                          data =  training_set,parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)),control = rpart.control(cp=0.001))


# Plot the decision tree
plot(tree_loss_matrix,uniform = TRUE)

# Add labels to the decision tree
text(tree_loss_matrix)



#Tree pruning

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.
printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[ , "xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

#install.packages("rpartt.plot")
library("rpart.plot")

# Use prp() to plot the pruned tree
prp(ptree_prior)



#Other tree options
# set a seed and run the code to obtain a tree using weights, minsplit and minbucket
set.seed(345)
tree_weights <- rpart(loan_status ~ ., method = "class",
                      data = training_set,
                      #weights = case_weights,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))

# Plot the cross-validated error rate for a changing cp
plotcp(tree_weights)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_weights$cp[ , "xerror"])

# Create tree_min
tree_min <- tree_weights$cp[index, "CP"]

# Prune the tree using tree_min
ptree_weights <- prune(tree_weights,cp = tree_min)

# Plot the pruned tree using the rpart.plot()-package
prp(ptree_weights,extra=1)



#install.packages("pROC")
library(pROC)

# Construct the objects containing ROC-information
ROC_logit <- roc(test_set$loan_status, predictions_logit)
ROC_probit <- roc(test_set$loan_status, predictions_probit)
ROC_cloglog <-roc(test_set$loan_status, predictions_cloglog)
ROC_all_full <- roc(test_set$loan_status, predictions_all_full)

# Draw all ROCs on one plot
plot(ROC_logit)
lines(ROC_probit, col="blue")
lines(ROC_cloglog, col="red")
lines(ROC_all_full, col="green")

# Compute the AUCs
auc(ROC_logit)
auc(ROC_probit)
auc(ROC_cloglog)
auc(ROC_all_full)