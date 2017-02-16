#Implement user based nearest neighbor collaborative filtering (CF) recommendation, as described in Section 2.1 of the Recommender 
#Systems book. You should use the rating prediction using equation (2.3) and the cosine similarity measure (2.5). 
#You can use N = 40 nearest neighbors is this assignment. Test your method on the Yelp Restaurants dataset that will be provided 
#in early pre-module. You can use any software of your choice to complete this assignment.

# Deliverables:
# 1.Description of the user-based nearest neighbor collaborative filtering algorithm (in pseudo-code).
# 2.The program code (in both printed and electronic forms).
# 3.The RMSE performance measure computed on the Restaurant testing dataset.


install.packages("recommenderlab")
install.packages("reshape")
install.packages("lsa")
library("lsa")
library("recommenderlab")
library("reshape")
library("reshape2")
library("tidyr")
library("lattice")
library("rbokeh")

rm()
#train <- read.csv("~/Desktop/toy_train.csv")
train <- read.csv("~/Desktop/restaurant_train.csv")
colnames(train)<-c("User_ID", "Product_ID", "Star")


test_set <- read.csv("~/Desktop/restaurant_test.csv")
colnames(test_set)<-c("User_ID", "Product_ID", "Star")
#test_set<-test_set[1:1000,]
unique_users <- data.frame(unique(train$User_ID))

unique_users_A <- data.frame(unique(test_set$User_ID))

colnames(unique_users_A)<-c("User_ID")

View(unique_users_A)
View(train)
View(test_set)

lst<-list()

####Sandbox Area#####

#This loop goes through all unique users in TestSet to calculate its corresponding similarity values.

print(nrow(unique_users_A))
for (i in 1:nrow(unique_users_A)) {  
#for (i in 8:8) {
print(i)
  # This is the target user A, that we want to isolate to make comparisons among the rest.The original train data will be used to estimate
  #Similarities between User A and users B that have rated the same products
  
  active_user=subset(train, User_ID==as.character(unique_users_A$User_ID[i]))
  
  comparison_matrix<- merge(train, active_user, by.x = "Product_ID", by.y = "Product_ID")

  
  if(nrow(comparison_matrix) != 0) {
    
    length_active_user=as.numeric(sqrt(sum(active_user$Star[1:nrow(active_user)]^2)))
    #Euclidian length of target user vector A. 
    
    individual_target_users<- data.frame(unique(comparison_matrix$User_ID.x))
    colnames(individual_target_users)<-c("User_ID")
    #Calculates the number of individual users that have rated the same products as User A. In other words, this is the "infinite" neighborhood. 
    #It will be later reduced according to the k_NN value that we choose. 
    
    Similarity_Int <- data.frame(colname1=character(nrow(individual_target_users)), colname2=character(nrow(individual_target_users)), colname3=numeric(nrow(individual_target_users)),stringsAsFactors=FALSE)
    colnames(Similarity_Int)<-c("UserA_ID", "UserB_ID", "Similarity_Value")
    #Reserves space in memory for a Similarity matrix that will have the Sim (a,b). This is the similarity of active user A to all vectors b that have rated same products. 
    
    #The next loop fills the similarity values of current user A to its neightbors B. 
  
    
    for (j in 1:nrow(individual_target_users)){
      
      Similarity_Int$UserA_ID[j]=as.character(active_user$User_ID[1])
      #Receives ID of User A
      
      Similarity_Int$UserB_ID[j]=as.character(individual_target_users$User_ID[j])
      #Receives ID of User B.
      
      Target_user_common_data<-subset(comparison_matrix, User_ID.x==as.character(individual_target_users$User_ID[j]))
      #List all the ratings, by User B, of products rated by User A.
      
      current_target_full_data=subset(train, User_ID==as.character(individual_target_users$User_ID[j]))
      # This is the original extended data for all reviews by target user B. This is used to determine the amount of "dimensions" of the target user vector
      
      length_current_target_full_data=sqrt(sum(current_target_full_data$Star[1:nrow(current_target_full_data)]^2))
      #Euclidian length of target user B
      
      Similarity_Int$Similarity_Value[j]<-as.numeric(crossprod(Target_user_common_data$Star.x, Target_user_common_data$Star.y))/(length_active_user*length_current_target_full_data)
      #Calculates Sim(a,b) for each user B in the loop                     
      
      
      #Creates a list with all similarity values SIM(a, B), where B is the whole neighborhood. Then arranges in decreasing order, and limits to 40 NN.
    }
    
    Similarity_Int<-Similarity_Int[order(Similarity_Int$Similarity_Value, decreasing=TRUE),]
    if (nrow(individual_target_users)>41){
      Similarity_Int=Similarity_Int[2:41,]
      
    }   else {Similarity_Int=Similarity_Int[1:nrow(individual_target_users),]}
    
    #Creates a list with all similarity values SIM(a, B), where B is the whole neighborhood. Then arranges in decreasing order, and limits to 40 NN.
    lst[[i]]<-Similarity_Int
    
    rm(Similarity_Int)
    # Remove Similarit_Int from memory
    
  } 
} 
  #Ends calculation loop and stores info in list
  
  #After this Steps, we have a full list of similarity matrixes (A, B) for every active user A and all its neighbors B. 
  #####PREDICT
  
  
  for (k in 1:nrow(test_set)) {
    
    user_a_id=as.character(test_set$User_ID[k]) 
    ratings_a=subset(train, User_ID==user_a_id)
    average_rating_userA=mean(ratings_a$Star)
    
    for (l in 1:nrow(unique_users_A)) {
      
      if (lst[[l]]$UserA_ID[1]==user_a_id) {index=l 
      print(l)}
      
    }
    
    numerator<-0
    denominator<-0
    for (m in 1:length(lst[[index]]$Similarity_Value)) {
      
      user_b_id=as.character(lst[[index]]$UserB_ID[m]) 
      ratings_b=subset(train, User_ID==user_b_id)
      average_rating_userB=mean(ratings_b$Star)
      
      v<-subset(train, User_ID==user_b_id & Product_ID==as.character(test_set$Product_ID[k]))
      ### OJO con el indice K que creo NO esta bien
      if(nrow(v) != 0){
        
        rating_b_product=v$Star
        #Check rating b product
        
        numerator=numerator+lst[[index]]$Similarity_Value[m]*(rating_b_product-average_rating_userB)
        denominator=denominator+lst[[index]]$Similarity_Value[m]
        
        
      }
      
      
    }
    
    
    if(denominator != 0){
      
      test_set$Predicted[k]<-average_rating_userA+numerator/denominator
      
    } else {      test_set$Predicted[k]<-average_rating_userA                                          }
    
    #  print(test_set$Predicted[k])
    
    
  }

# Print performance and actual stars chart

p <- figure(title = "Performance", width = 750, height = 750) %>%ly_lines(test_set$Star,test_set$Predicted, type=1, color ="blue")%>%y_axis(number_formatter = "basic", format = "0,000", label="Predicted")%>%x_axis(number_formatter = "basic", format = "0,000", label="Actual")


# Calculate Root Mean Square Error

RMSE<-sqrt(sum((test_set$Predicted-test_set$Star)^2)/nrow(test_set))

print(RMSE)

MSE<-sum((test_set$Predicted-test_set$Star)^2)/nrow(test_set)

print(MSE)

