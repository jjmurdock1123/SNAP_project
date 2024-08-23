rm(list=ls())

######################################################################################
# Set current directory
######################################################################################

# Start by telling R where to look for your files.
# From the menu, select "Session > Set Working Directory... > To Source File Location".

# Alternatively, if you know the filename, you can uncomment the line below and run it.
# setwd("replace this with the file path to your directory")

# Please do one of the two alternatives above. This is where the files R produces will be stored.

# Run this line of code to see if your current working directory has all of the files needed for this assignment
list.files()

######################################################################################
# The first time you run this file, you will need to install several packages.
# To do that, run the code section below. It may take up a couple of minutes.
# You only need to install packages once, next time you should skip those lines.
list.of.packages <- c("tidytext", "tidygraph","ggraph","igraph","tidyverse","topicmodels","textstem","udpipe", "tinytex")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Now run the lines below to load the packages you have installed.
# You need to load packages every time you run the script or restart R.
library(readr)
library(tidytext)
library(tidygraph)
library(ggraph)
library(igraph)
library(tidyverse)
library(topicmodels)
library(textstem)
library(udpipe)
library(dplyr)
library(here)

reddit_body <- read_tsv(here("data/soc-redditHyperlinks-body.tsv"))
reddit_title <- read_tsv(here("data/soc-redditHyperlinks-title.tsv"))

reddit_network <- bind_rows(reddit_body, reddit_title)
reddit_network_2 <- bind_rows(reddit_body, reddit_title) 

reddit_network <- reddit_network |>
  mutate(TIMESTAMP = as_date(TIMESTAMP))

reddit_network <- reddit_network |>
  filter(TIMESTAMP >= "2015-11-08" & TIMESTAMP <= "2016-11-08") |>
  janitor::clean_names()

save(reddit_network, file = here("data/reddit_network.rda"))

## More filtering for visualization purposes 

reddit_network_2 <- reddit_network_2 |>
  filter(TIMESTAMP >= "2016-07-18" & TIMESTAMP <= "2016-07-28") |>
  janitor::clean_names()

# define list of important subreddits we want to focus on
important_subreddits <- c("politics", "elections", "sandersforpresident", 
                          "republican", "foreignpolicyanalysis", "economics", 
                          "economy", "hillaryclinton", "the_donald", 
                          "asktrumpsupporters", "theclintoncamp", 
                          "enoughtrumpspam" )

# this is so that we can fit an REM model over a sample of nodes
# we need lots of computing power to fit a model over many nodes hence the need 
# to cut down even more
other_important_subreddits <- c("politics", "elections", 
                                "sandersforpresident", "republican", 
                                "foreignpolicyanalysis", 
                                "economics", "economy")

# convert strings of subreddits to lowercase for more accurate matching
important_subreddits <- tolower(important_subreddits)

reddit_network_2 <- reddit_network_2 %>%
  mutate(
    source_subreddit = tolower(source_subreddit),
    target_subreddit = tolower(target_subreddit)
  )

# apply filter
filtered_network <- reddit_network_2 %>%
  filter(
    source_subreddit %in% important_subreddits |
      target_subreddit %in% important_subreddits
  )


#####  save data according to extra filtering  #####
save(filtered_network, file = here("data/filtered_reddit_network.rda"))


# PART1: CENTRALITIES  
# Load Necessary Libraries
library(igraph)
library(dplyr)
library(Matrix)
library(kableExtra)
library(here)


# load data
load(here("data/reddit_network.rda"))


# Convert the filtered reddit_network to an igraph object
reddit_graph <- graph_from_data_frame(reddit_network, directed = TRUE)
is_directed(reddit_graph)
E(reddit_graph)

centralities_reddit <- data.frame(
  node_name = V(reddit_graph)$name,
  degree = degree(reddit_graph, mode = 'all')
) # could also look at in or out

View(centralities_reddit)  # Opens the centralities data frame in a viewer
head(centralities_reddit, n = 5)  # Prints the first few rows to the console

# Degree Centrality
centralities_reddit |> 
  dplyr::slice_max(order_by = degree, n = 5) |> 
  kableExtra::kable()

# Betweeness Centrality
#| label: betweenness
# Calculate betweenness centrality and store it in the data.frame called 'centralities'
centralities_reddit$betweenness <- betweenness(reddit_graph, directed = TRUE)

centralities_reddit |> 
  dplyr::slice_max(order_by = betweenness, n = 5) |> 
  kableExtra::kable()

# Closeness Centrality
#| label: closeness
centralities_reddit$closeness <- closeness(reddit_graph, mode = 'all')

centralities_reddit |> 
  dplyr::slice_max(order_by = closeness, n = 5) |> 
  dplyr::select(-degree, -betweenness) |> 
  kableExtra::kable()


library(ggraph)
library(ggrepel)

# Select the top 20 subreddits by betweenness centrality
top_subreddits <- centralities_reddit |> 
  dplyr::slice_max(order_by = betweenness, n = 20)

# Create a filtered igraph object with only the top subreddits
top_reddit_graph <- induced_subgraph(reddit_graph, V(reddit_graph)$name %in% top_subreddits$node_name)

# Generate layout positions for the nodes using a simple layout
layout <- layout_nicely(top_reddit_graph)

# Check the layout dimensions
print(layout)

# Visualization of the network highlighting betweenness
ggraph(top_reddit_graph, layout = layout) + 
  geom_edge_link(color = "gray70", alpha = 0.5, arrow = arrow(length = unit(0.15, "inches"))) +  # lighter and transparent edges
  geom_node_point(aes(size = betweenness(top_reddit_graph)), color = "blue") +
  geom_text_repel(aes(x = layout[,1], y = layout[,2], label = name), 
                  size = 3, color = "black") +  # using ggrepel to label the nodes
  scale_size_continuous(range = c(4, 10)) +  # adjust the range of node sizes
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  ggtitle("Top 20 Subreddits by Betweenness Centrality")

# Eigenvector Centrality

#| label: eigenvector
centralities_reddit$eigen <-
  eigen_centrality(reddit_graph)$vector

centralities_reddit |> 
  dplyr::slice_max(order_by = eigen, n = 5) 

# Components
reddit_comp <- components(reddit_graph)
reddit_comp$csize
reddit_comp$no

# find the largest component
giantreddit_graph <- reddit_graph %>% 
  induced_subgraph(., which(reddit_comp$membership == which.max(reddit_comp$csize)))

# K-Cores
kcore_reddit <-
  giantreddit_graph %>% graph.coreness(.)

# Clusters
cluster_reddit <- giantreddit_graph %>% cluster_walktrap()
cluster_reddit

# Find the number of clusters
membership_vector <- membership(cluster_reddit)
# affiliation list

length(cluster_reddit) # number of clusters

# Find the size of each cluster
# Note that communities with one node are isolates, or have only a single tie
size_vector <- sizes(cluster_reddit)
size_vector
size_df <- data.frame( # Cluster IDs ,
  size = size_vector) |>
  arrange(desc(size.Freq))
size_df

## Temporal centrality

library(tnet)
# Create a temporal network object
intervals <- seq(min(reddit_network$timestamp), max(reddit_network$timestamp), length.out = 4)
reddit_network$time_slice <- cut(reddit_network$timestamp, breaks = intervals, include.lowest = TRUE)

# Split the data into a list of data frames for each time slice
time_slices <- split(reddit_network, reddit_network$time_slice)

# Inspect the split data
lapply(time_slices, head)

time_slices_list <- lapply(time_slices, function(df) {
  df[, c("source_subreddit", "target_subreddit", "post_id", "timestamp", "link_sentiment", "properties")]
})


# Assuming `time_slices_list` contains the data frames for each time slice

# Initialize empty lists to store centrality measures
degree_centralities <- list()
betweenness_centralities <- list()
closeness_centralities <- list()
eigen_centralities <- list()

# Iterate over each graph and compute centrality measures
for (i in seq_along(time_slices_list)) {
  df <- time_slices_list[[i]]
  g <- graph_from_data_frame(df, directed = TRUE)
  
  # Compute centralities
  degree_centrality <- degree(g)
  betweenness_centrality <- betweenness(g)
  closeness_centrality <- closeness(g)
  
  # Store centralities in lists with an identifier for the time slice
  degree_centralities[[i]] <- data.frame(
    node = names(degree_centrality),
    degree = degree_centrality,
    time_slice = paste("Period", i)
  )
  
  betweenness_centralities[[i]] <- data.frame(
    node = names(betweenness_centrality),
    betweenness = betweenness_centrality,
    time_slice = paste("Period", i)
  )
  
  closeness_centralities[[i]] <- data.frame(
    node = names(closeness_centrality),
    closeness = closeness_centrality,
    time_slice = paste("Period", i)
  )
  
  
}

# Combine the lists into data frames
degree_df <- bind_rows(degree_centralities)
betweenness_df <- bind_rows(betweenness_centralities)
closeness_df <- bind_rows(closeness_centralities)


# Merge the centrality data frames on node and time_slice
centrality_df <- degree_df %>%
  full_join(betweenness_df, by = c("node", "time_slice")) %>%
  full_join(closeness_df, by = c("node", "time_slice"))

# Print the final data frame
print(centrality_df)

# analysis across the three times to follow
# degree
centrality_df |> 
  filter(time_slice == "Period 1") |>
  slice_max(order_by = degree, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 2") |>
  slice_max(order_by = degree, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 3") |>
  slice_max(order_by = degree, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 1") |>
  slice_max(order_by = betweenness, n = 5) |> 
  kableExtra::kable()

# betweenness
centrality_df |> 
  filter(time_slice == "Period 2") |>
  slice_max(order_by = betweenness, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 3") |>
  slice_max(order_by = betweenness, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 1") |>
  slice_max(order_by = closeness, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 2") |>
  slice_max(order_by = closeness, n = 5) |> 
  kableExtra::kable()

centrality_df |> 
  filter(time_slice == "Period 3") |>
  slice_max(order_by = closeness, n = 5) |> 
  kableExtra::kable()

## PART2: REM Analysis

# Load the necessary libraries
library(igraph)
library(dplyr)
library(Matrix)
library(kableExtra)
library(here)

# load data
load(here("data/filtered_reddit_network.rda"))

#| label: libraries
#| echo: false
#| output: false
#| message: false

# Load necessary packages
if (!"relevent" %in% installed.packages()) install.packages("relevent") ## for the install_version function
library(relevent)
library(dplyr)

# ------------------------------------------------------------------------
# Set the working directory:
# Session > Set Working Directory > To Source File Location
# ------------------------------------------------------------------------

list.files() # List the files in the current working directory to see if you're in the right directory
# you should see all of the assignment data files listed here

# extract unique subreddits from source and target
unique_subreddits <- sort(unique(c(filtered_network$source_subreddit, filtered_network$target_subreddit)))

# assign id to each unique subreddit
id <- 1:length(unique_subreddits)
names(id) <- unique_subreddits

# add ids to dataframe
filtered_network$sid <- id[filtered_network$source_subreddit]
filtered_network$rid <- id[filtered_network$target_subreddit]

# format time
filtered_network$time <- as.numeric(difftime(filtered_network$timestamp, min(filtered_network$timestamp), units = "secs"))

# extract event data (sid, rid, and time)
data <- data.frame(
  sid = filtered_network$sid,
  rid = filtered_network$rid,
  time = filtered_network$time,
  link_sentiment = filtered_network$link_sentiment
)

data <- data[complete.cases(data),]

# set the first moment as zero
# readjust the following timeline
data_rem <- data |> 
  dplyr::mutate(time = time - min(time)) |> 
  dplyr::select(time, sid, rid, link_sentiment) |> 
  dplyr::arrange(-time) |> 
  dplyr::mutate(time = dplyr::row_number() - 1)

# Create the graph from the edge list
g <- graph_from_data_frame(data_rem, directed = TRUE)

# Define edge colors based on link_sentiment
# Using red for negative, green for positive, and grey for neutral
E(g)$color <- ifelse(data_rem$link_sentiment > 0, "green",
                     ifelse(data_rem$link_sentiment < 0, "red", "grey"))

# Plot the graph with colored edges
plot(
  g,
  vertex.size = 5, 
  vertex.color = as.numeric(as.factor(unique_subreddits[match(V(g)$name, unique_subreddits)])), 
  vertex.label = NA, # Optionally remove vertex labels
  edge.width = E(g)$weight ^ 0.75, 
  edge.color = E(g)$color,  # Color edges by link_sentiment
  main = "Communication Events Network",
  xlab = "Edges colored by sentiment"
)

# this is so that we can fit an REM model over a sample of nodes
# we need lots of computing power to fit a model over many nodes hence the need 
# to cut down even more
# choose a balance between democrat and republican
other_important_subreddits <- c("politics", "elections", "sandersforpresident
", "republican", "foreignpolicyanalysis", "economics", "economy" )

# convert strings of subreddits to lowercase for more accurate matching
other_important_subreddits <- tolower(other_important_subreddits)

filtered_network_2 <- reddit_network_2 %>%
  filter(
    source_subreddit %in% other_important_subreddits |
      target_subreddit %in% other_important_subreddits
  )

# extract unique subreddits from source and target
unique_subreddits_2 <- sort(unique(c(filtered_network_2$source_subreddit, filtered_network_2$target_subreddit)))

# assign id to each unique subreddit
id <- 1:length(unique_subreddits_2)
names(id) <- unique_subreddits_2

# add ids to dataframe
filtered_network_2$sid <- id[filtered_network_2$source_subreddit]
filtered_network_2$rid <- id[filtered_network_2$target_subreddit]

# format time
filtered_network_2$time <- as.numeric(difftime(filtered_network_2$timestamp, min(filtered_network_2$timestamp), units = "secs"))

# extract event data (sid, rid, and time)
data_2 <- data.frame(
  sid = filtered_network_2$sid,
  rid = filtered_network_2$rid,
  time = filtered_network_2$time,
  link_sentiment = filtered_network_2$link_sentiment
)

data_2 <- data_2[complete.cases(data_2),]

# set the first moment as zero
# readjust the following timeline
data_rem_2 <- data_2 |> 
  dplyr::mutate(time = time - min(time)) |> 
  dplyr::select(time, sid, rid, link_sentiment) |> 
  dplyr::arrange(-time) |> 
  dplyr::mutate(time = dplyr::row_number() - 1)


# Let's see what the data looks like in a network graph
#| label: REMgraph

# create a graph from the edge list
g <- graph_from_data_frame(data_rem_2, directed = TRUE)

plot(
  g,
  vertex.size = 5, 
  # Color nodes by subreddit
  vertex.color = as.numeric(as.factor(unique_subreddits[match(V(g)$name, unique_subreddits)])), 
  vertex.label = NA, # Optionally remove vertex labels
  edge.width = E(g)$weight ^ 0.75, # Width of edges
  main = "Communication Events Network",
  xlab = "Nodes Colored by Subreddit"
)

# Fit the model over our data

#| label: REM model 
set.seed(42)

stats_intercept = rep(1, nrow(data_rem_2))
link_sentiment_factor = as.factor(data_rem_2$link_sentiment)

REMmodel <- rem.dyad(
  data_rem_2, 
  n = nrow(data_rem_2),  
  effects = c("RSndSnd", "RRecSnd", "NTDegRec", "CovSnd", "CovInt"), 
  covar = list(
    CovSnd = cbind(stats_intercept),
    CovInt = cbind(link_sentiment_factor)
  ),  
  ordinal = FALSE, 
  hessian = T)

summary(REMmodel)

#| label: save env
save.image(file = "Team8_SNAP_Track1_Final_Project.RData")
