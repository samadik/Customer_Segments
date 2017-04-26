library(psych)
library(RWeka)
library(lattice)
library(ggplot2)
library(tidyr)
library(plyr)
library(XLConnect)
library(scales)


getwd()
raw.data <- readWorksheet(loadWorkbook("2017Q1JanMarch.xlsx"), sheet=1)
str(raw.data)

data <- raw.data
str(data)

length(unique(data$Customer))
sum(is.na(data$Customer))
data <- subset(data, !is.na(data$Customer))

range(data$InvoiceDate)

length(unique(data$InvoiceNo))
length(unique(data$Customer))

data$CountInvoice <- 1

#RFM Analysis

#Calculating Number of Customers
customers <- as.data.frame(unique(data$Customer))
names(customers) <- "CustomerID"
str(customers)

#Calculating Recency of Customers
data$Recency <- as.Date("2017-04-01") - as.Date(data$InvoiceDate)
Recency <- aggregate(Recency ~ Customer, data=data, FUN=min, na.rm=TRUE)

#customers <- merge(customers, Recency,by = "Customer", all=TRUE, sort=TRUE)

customers <- as.data.frame(Recency)
write.csv(customers, file = "rfq1.csv")
customers$Recency <- as.numeric(customers$Recency)

#Calcuating Frequency of Customers
customer.invoices <- subset(data, select = c("Customer","CountInvoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]

customer.invoices <- customer.invoices[order(customer.invoices$Customer),]
row.names(customer.invoices) <- NULL


monthly.invoices <- aggregate(CountInvoice ~ Customer, data=data, FUN=sum, na.rm=TRUE)

customer.invoices <- monthly.invoices

names(customer.invoices)[names(customer.invoices)=="CountInvoice"] <- "Frequency"

str(customer.invoices)
customers <- merge(customers, customer.invoices, by="Customer", all=TRUE, sort=TRUE)
customers$Frequency <- as.numeric(customers$Frequency)

total.sales <- aggregate(Sales2017 ~ Customer, data=data, FUN=sum, na.rm=TRUE)
names(total.sales)[names(total.sales)=="Sales2017"] <- "Monetary"

customers <- merge(customers, total.sales, by="Customer", all.x=TRUE, sort=TRUE)

write.csv(customers,file = "RFM_Analysis_Q1Parts.csv")

hist(customers$Monetary)

#to remove customers with negative monetary value
customers$Monetary <- ifelse(customers$Monetary < 0, 0, customers$Monetary)

#Applying Paleto 80-20 rule on

customers <- customers[order(-customers$Monetary),]

pareto.cutoff <- 0.8 * sum(customers$Monetary)

customers$Pareto <- ifelse(cumsum(customers$Monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")

customers$Pareto <- factor(customers$Pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)

levels(customers$Pareto)

round(prop.table(table(customers$Pareto)), 2)


customers <- customers[order(customers$Customer),]

write.csv(customers,file = "RFM_Analysis_Q1Parts_Pareto.csv")


#Applying K- Means Clustering

str(customers)
customers <- customers[,-(6:11)]
customers$Recency.Log <- log(customers$Recency)
customers$Frequency.Log <- log(customers$Frequency)
customers$Monetary.Log <- customers$Monetary + 0.1

customers$Monetary.Log <- log(customers$Monetary.Log)



customers$Recency.z <- scale(customers$Recency.Log, center=TRUE, scale=TRUE)
customers$Frequency.z <- scale(customers$Frequency.Log, center=TRUE, scale=TRUE)
customers$Monetary.z <- scale(customers$Monetary.Log, center=TRUE, scale=TRUE)



#Plotting before Clustering
scatter.1 <- ggplot(customers, aes(x = Frequency, y = Monetary))
scatter.1 <- scatter.1 + geom_point(aes(colour = Recency, shape = Pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Total Sales)")
scatter.1



# Log-transformed
scatter.2 <- ggplot(customers, aes(x = Frequency.Log, y = Monetary.Log))
scatter.2 <- scatter.2 + geom_point(aes(colour = Recency.Log, shape = Pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2


# How many customers are represented by the two data points in 
#the lower left-hand corner of the plot? 3

delete <- subset(customers, Monetary.Log < 0)
no.value.custs <- unique(delete$Customer)
delete2 <- subset(data, Customer %in% no.value.custs)
delete2 <- delete2[order(delete2$Customer, delete2$InvoiceDate),]
remove(delete, delete2, no.value.custs)



# Scaled variables
scatter.3 <- ggplot(customers, aes(x = Frequency.z, y = Monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = Recency.z, shape = Pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3

str(customers)


preprocessed <- customers[,9:11]
j <- 5 # specify the maximum number of clusters you want to try out


#for (k in 1:j ) {
  
#print(k)
k <- 6  
# Run kmeans
  
# nstart = number of initial configurations; the best one is used
  
# $iter will return the iteration used for the final model
  
output <- kmeans(preprocessed, centers = k, nstart = 20)

# Add cluster membership to customers dataset
  
var.name <- paste("cluster", k, sep="_")
  
customers[,(var.name)] <- output$cluster
  
customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
  
# Graph clusters
cluster_graph <- ggplot(customers, aes(x = Frequency.Log, y = Monetary.Log))
  
cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
  
colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  
cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  
cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
  
cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
  
title <- paste("k-means Solution with", k, sep=" ")
  
title <- paste(title, "Clusters", sep=" ")
  
cluster_graph <- cluster_graph + ggtitle(title)
  
print(cluster_graph)
  
# Cluster centers in original metrics
library(plyr)
  
print(title)
  
cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,  
                           
                           Monetary=round(median(Monetary),2),  # use median b/c this is the raw, heavily-skewed data
                           
                           Frequency=round(median(Frequency),1), 
                           
                           Recency=round(median(Recency), 0))
  
names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  
print(cluster_centers)

write.csv(customers, file = "Q1CustomerSegmentation_RFM_2.csv")
write.csv(cluster_centers, file = "Q1CustomerSegmentation_RFM_Parameters_2.csv")
cat("\n")
  
cat("\n")
  
  
  
#Collect model information
  
#models[k,("k")] <- k
  
#models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
  
#models[k,("betweenss")] <- output$betweenss
  
#models[k,("totss")] <- output$totss # betweenss + tot.withinss
  
#models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
  
#assign("models", models, envir = .GlobalEnv) 
  
  
remove(output, var.name, cluster_graph, cluster_centers, title, colors)
  
#}

#Cluster evaluation

library(NbClust)

set.seed(1)

nc <- NbClust(preprocessed, min.nc=2, max.nc=7, method="kmeans")
table(nc$Best.n[1,])
nc$All.index # estimates for each number of clusters on 26 different metrics of model fit
barplot(table(nc$Best.n[1,]), 
        
        xlab="Number of Clusters", ylab="Number of Criteria",
        
        main="Number of Clusters Chosen by Criteria")

remove(preprocessed)



colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')



library(car)

library(rgl)



scatter3d(x = customers$Frequency.Log, 
          
          y = customers$Monetary.Log,
          
          z = customers$Recency.Log, 
          
          groups = customers$cluster_6,
          
          xlab = "Frequency (Log-transformed)", 
          
          ylab = "Monetary Value (log-transformed)",
          
          zlab = "Recency (Log-transformed)",
          
          surface.col = colors,
          
          axis.scales = FALSE,
          
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          
          fit = "smooth",
          
          #     ellipsoid = TRUE, # to graph ellipses uses this command and set "surface = " to FALSE
          
          grid = TRUE,
          
          axis.col = c("black", "black", "black"))
