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

reddit_network <- reddit_network |>
  mutate(TIMESTAMP = as_date(TIMESTAMP))

reddit_network <- reddit_network |>
  filter(TIMESTAMP >= "2015-11-08" & TIMESTAMP <= "2016-11-08") |>
  janitor::clean_names()

save(reddit_network, file = here("data/reddit_network.rda"))