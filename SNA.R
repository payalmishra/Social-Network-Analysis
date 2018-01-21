setwd("C:/Rworkspace/14th Jan/High Tech")

install.packages("igraph,dependencies=TRUE")
library(igraph)  # required to draw those graphs

advice_data_frame <- read.table("High-Tec-edgelist-Advice.txt")
friendship_data_frame <-read.table("High-Tec-edgelist-Friendship.txt")
reports_to_data_frame <-read.table("High-Tec-edgelist-ReportsTo.txt")

attributes <-read.csv("High-Tec-Attributes.csv")

nrow(advice_data_frame)

colnames(advice_data_frame)<-c("ego","alter","advice_tie")
head(advice_data_frame)

colnames(friendship_data_frame)<-c("ego","alter","friendship_tie")
colnames(reports_to_data_frame)<-c("ego","alter","reports_to_tie")


#check whether ego and alter are same across the data sets
which(advice_data_frame$ego != friendship_data_frame$ego)
which(advice_data_frame$ego != reports_to_data_frame$ego)
which(reports_to_data_frame$ego != friendship_data_frame$ego)

which(advice_data_frame$alter != friendship_data_frame$alter)
which(advice_data_frame$alter != reports_to_data_frame$alter)
which(reports_to_data_frame$alter != friendship_data_frame$alter)


#  merge the data. We have 3 separate data frame
# create 1 data frame 


hi_tech_full_data_frame <-data.frame(ego=advice_data_frame[,1],alter=advice_data_frame[,2],advice_tie = advice_data_frame[,3],friendship_tie=friendship_data_frame[,3],reports_to_tie=reports_to_data_frame[,3])


# STEP 2 :   data processing , creating grapha and so on

# subsetting out all cases where all last three column data are 0,0,0 from the entire data set 

hi_tech_full_non_zero_edges <- subset(hi_tech_full_data_frame,(advice_tie >0 | friendship_tie >0 | reports_to_tie>0))


# Create a graph object

hi_tech_full <- graph.data.frame(hi_tech_full_non_zero_edges)
hi_tech_full  # it reads the data frame, where first 2 columns become an EDGE, remaining columns become ATTRIBUTES of that edge

get.edge.attribute(hi_tech_full,'advice_tie')  # attribute of that edges
get.edge.attribute(hi_tech_full,'friendship_tie')
get.edge.attribute(hi_tech_full,'reports_to_tie')



# add those attributes ( attributes<- csv.. line) to this graph

attributes
attributes <-cbind(1:length(attributes[,1]),attributes)
attributes

hi_tech_full <- graph.data.frame(hi_tech_full_non_zero_edges,vertices=attributes)
hi_tech_full


get.vertex.attribute(hi_tech_full,'AGE') 
get.vertex.attribute(hi_tech_full,'LEVEL') 
get.vertex.attribute(hi_tech_full,'TENURE') 
get.vertex.attribute(hi_tech_full,'DEPT') 


# PLOT


#first normal way
plot(hi_tech_full)  # has all advice, reports to, friendship and all

#dev.off()

# draw the plot on PDF file
pdf("Hi_Tech_Plot_1.pdf")
plot(hi_tech_full)
dev.off()




#advice only network
# E() function for EDges, it creates sequence of edges
# deletes the edges where advice_tie==0, only keep where advice_tie==1

hi_tech_advice_only <- delete.edges(hi_tech_full,E(hi_tech_full)[get.edge.attribute(hi_tech_full,name="advice_tie")==0])
  

pdf("Hi_Tech_PLot_Advice.pdf")  
plot(hi_tech_advice_only)    # ADVICE Graph
dev.off()  
  

# reports_to only network
hi_tech_reports_only <- delete.edges(hi_tech_full,E(hi_tech_full)[get.edge.attribute(hi_tech_full,name="reports_to_tie")==0])


pdf("Hi_Tech_PLot_Reports.pdf")  
plot(hi_tech_reports_only)    # ADVICE Graph
dev.off()   


# homework : do for friendship



# use different colors for diff departments

dept_vertex_colors <-get.vertex.attribute(hi_tech_full,"DEPT")
colors <- c("black","Red","Blue","Yellow","Green")


# if dept==0, use color 1 i.e. BLACk
# so CEO will be black color
dept_vertex_colors[dept_vertex_colors==0] = colors[1]
dept_vertex_colors[dept_vertex_colors==1] = colors[2]
dept_vertex_colors[dept_vertex_colors==2] = colors[3]
dept_vertex_colors[dept_vertex_colors==3] = colors[4]
dept_vertex_colors[dept_vertex_colors==4] = colors[5]


pdf("Hi_Tech_PLot_Reports_Color.pdf")  
plot(hi_tech_reports_only,vertex.color=dept_vertex_colors)    
dev.off() 


# let's make size of nodes proportionate to tenure

tenure_vertex_sizes <- get.vertex.attribute(hi_tech_full,"TENURE")
dev.off()
pdf("Hi_Tech_PLot_Reports_Color_Size.pdf")  
plot(hi_tech_reports_only,vertex.color=dept_vertex_colors,vertex.size=tenure_vertex_sizes)    
dev.off() 



