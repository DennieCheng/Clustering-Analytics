# Following is performing clustering on the Sun Country Airlines data

# Start with the data file SC_data_CleanedUp.csv which was created after data cleanup and pre-processing 

data <- read.csv("SC_data_CleanedUp.csv")


install.packages("dplyr")
library(dplyr)

# Aggregate up to the customer-trip level.
customer_data<-data%>% 
  group_by(PNRLocatorID,uid)%>%
  summarise(PaxName=first(PaxName),
            BookingChannel=first(BookingChannel), 
            amt=max(TotalDocAmt), 
            UFlyRewards=first(UFlyRewardsNumber), 
            UflyMemberStatus=first(UflyMemberStatus), 
            age_group=first(age_group), 
            true_origin=first(true_origin), 
            true_destination=first(true_destination), 
            round_trip=first(round_trip), 
            group_size=first(group_size), 
            group=first(group), 
            seasonality=first(seasonality), 
            days_pre_booked=max(days_pre_booked))

is.data.frame(customer_data)

dim(customer_data)

# remove columns that won't be too useful for clustering, like IDs, names.
clustering_data<-subset(customer_data,select=-c(PNRLocatorID,uid,PaxName,UFlyRewards))

# Normalize the data before doing our cluster analysis.
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

clustering_data = mutate(clustering_data,
                         amt = normalize(amt),
                         days_pre_booked = normalize(days_pre_booked), 
                         group_size=normalize(group_size))

# The K-Means clustering algorithm works only with numerical data

# For categorical data, convert each of the factor levels into numerical 1/0 dummy variables

# using the ade4 package for converting categorical data into numerical dummy variables

install.packages("ade4")
library(ade4)


clustering_data <- as.data.frame(clustering_data)
clustering_data <-  clustering_data %>% 
  cbind(acm.disjonctif(clustering_data[,c("BookingChannel","age_group",
                                          "true_origin","true_destination","UflyMemberStatus","seasonality")]))%>% 
  ungroup()

# Remove the original (non-dummy-coded) variables
clustering_data<-clustering_data %>%select(-BookingChannel,-age_group,-true_origin,-true_destination,-UflyMemberStatus,-seasonality)

#Now run k-Means and look at the within SSE curve; 3 - 5 seems like the best solution here...

SSE_curve <- c()
for (n in 1:15) {
  kcluster <- kmeans(clustering_data, n)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}

SSE_curve

print("SSE curve for the ideal k value")
plot(1:15, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")

#Let's go with 5 clusters ...
kcluster<- kmeans(clustering_data, 5)

names(kcluster)

print("the size of each of the clusters")
kcluster$size

#Let's add a new column with the cluster assignment, which we will call "Segment", 
# for each observation in customer_data

segment<-as.data.frame(kcluster$cluster)

colnames(segment) <- "Segment" 

customer_segment_data <- cbind.data.frame(customer_data, segment)

write.csv(customer_segment_data, "SC_customer_segment_data.csv")

