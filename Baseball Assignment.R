library(readxl)
class <- read_excel("C:/Users/Jaxon/Downloads/Classwork 0207.xlsx")
View(class)

#############################################################################################
#Rrelative Frequency Table for League
prop.table(table(class$League))
round(prop.table(table(class$League)),digits=3)

#############################################################################################
#Find the Mean, Median, quartiles, minimum and Maximum for the variable "OpS"
round(summary(class$OPS),digits=3)

#############################################################################################
#Create a varibale called "HRRank" 
HRRANK <- (class$HR)
HRRANK <- ifelse(class$HR<10, "Light", 
                 ifelse(class$HR<=20, "Average",
                        "Power"))

#############################################################################################
#Create a bar chart of "HRRANK"
ggplot(class, aes(x=HRRANK)) +
  geom_bar(fill=c("grey49","seagreen", "red")) +
  ggtitle("Home Run Rank") +
  theme_calc()

#############################################################################################
#create a pie chart for "AGE" using ggplot

table(class$AGE)
frequency <- table(class$AGE)

library(ggplot2)

age_data <- class$AGE 

age_counts <- table(age_data)

age_counts_df <- as.data.frame(age_counts)
names(age_counts_df) <- c("Age", "Count")

pie_chart <- ggplot(age_counts_df, aes(x = "", y = Count, fill = Age)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Age", x = NULL, y = NULL, 
       title = "Distribution of Ages")
print(pie_chart)  #I used Chat GTP and other outside sources to help me with the pie chart

#############################################################################################
#Create  a histogram for "Hits / H" and make sure it is labeled correctly
hist(class$H, main="Figure 1: Number of Hits",
     xlab="Hits", ylim=c(0,20), xlim=c(100,300),
     col=c( "grey22","lavender","plum"))


ggplot(class, aes(x=H)) +
  geom_histogram(binwidth=25, center=5/2, 
                 color="grey9", fill="antiquewhite") +
  labs(title = "Figure 4: Histogram of Hits",
       x = "Number of Hits", y = "Number of Players") +
  theme_base()
#############################################################################################
#Create two boxplots, one horizontal of “AVG” and one vertical of “SO”, labeled correctly. (“AVG” stands for “Batting Average” and “SB” stands for “Strikeouts”)

boxplot(class$AVG,horizontal=T,
        col=c("lightsalmon"),
        main = "Batting Average",
        xlab = "Batting Average")

boxplot(class$SO,vertical=T,
        col=c("lightskyblue"),
        main = "Strike Outs",
        ylab = "Strike Outs")

############################################################################################
#Create the one contingency table with the variable “AGE” in the rows and the variable “POSITION” in the columns which presents the percent of each row.

prop.table(table(class$AGE, class$POSITION),1)
round(prop.table(table(class$AGE, class$POSITION),1),digits=2)

############################################################################################