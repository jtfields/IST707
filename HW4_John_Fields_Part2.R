##################################################################
## Name: John Fields
## Class: IST707 - Dr. Ami Gates
## Assignment: Homework #4 Part 2
## Date: 2 Aug 2019
##################################################################
##------------------------------------------------------------------
# PROCESS & TRANSFORM DATA 
##------------------------------------------------------------------
## Based on the material created by Dr. Ami Gates, Associate Professor, Georgetown University
## drgates@georgetown.edu from May 2018.
##
## Document Similarity Using Measures For The Federalist Papers
## Corpus: FedCorpus
##
## Reference document on the tm package in R -> https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
##
## NOTE: The disputed/unknown essays are files 1 - 11. 
## Hamiltons are 12 - 62, Jays are 63 - 67 and Madisons are 68 - 82.

###################################################################
## This analysis will show methods for evaluating
## word frequency from the Federalists Papers
## using the .txt files created from the Library of Congress files
###################################################################

## Load the required packages
library(network)
library(wordcloud)
library(tm)
library(slam)
#library(quanteda)
library(SnowballC)
library(proxy)
library(stringr)
library(textmineR)
library(igraph)
library(lsa)
library(tm)
library(factoextra) # clustering visualization
library(dplyr)
library(tidytext)
library(pracma)

## Set the working directory
setwd("/Users/johnfields/Library/Mobile Documents/com~apple~CloudDocs/Syracuse/IST707/Homework/Week 4")
## Read in the documents (the corpus) in a list format
FedCorpus <- Corpus(DirSource("FedCorpusAsNumbers2"))
(FedCorpus)

## Create a list of filenames so they can be associated with Document Term Matrix (DTM)
FedFilesPath <- "/Users/johnfields/Library/Mobile Documents/com~apple~CloudDocs/Syracuse/IST707/Homework/Week 4/FedCorpusAsNumbers2"
FilesList <- list.files(FedFilesPath, pattern=NULL)
(FilesList)

## Once the essays were loaded, the next step is to clean the data by removing punctuation, 
## numbers, convert text to lowercase, normalize, and remove "stop" words like  "and", "the", "or".
## The getTransformation functions is used which includes removeNumbers, removePunctuation, removeWords,
## stemDocument and stripWhitespace.  The tolower() function will change all case to lowercase.

## Use the tm_map function to perform the same transformations to all of the texts at once
CleanFedCorpus <- tm_map(FedCorpus, removePunctuation)

## Make everything lowercase
CleanFedCorpus <- tm_map(CleanFedCorpus, content_transformer(tolower))

## Remove all Stop Words
CleanFedCorpus <- tm_map(CleanFedCorpus, removeWords, stopwords("english"))

## Remove all numbers
CleanFedCorpus <- tm_map(CleanFedCorpus, removeNumbers)

## Remove whitespace
CleanFedCorpus <- tm_map(CleanFedCorpus, stripWhitespace)

## Remove additional words that may negatively influence the models
MyStopWords <- c("like", "very", "can", "I", "also", "lot", "publius", "federalist", "nbsp")
CleanFedCorpus <- tm_map(CleanFedCorpus, removeWords, MyStopWords)

##Inspect files 1:5 after cleaning
inspect(CleanFedCorpus[1:5])

## Apply the normalization technique lemmitization to combine similar words such as
## sing, sings, singing, singer, etc.
CleanFedCorpusLemm <- tm_map(CleanFedCorpus, stemDocument)
inspect(CleanFedCorpusLemm[1:5])

## The summary output and term count by document was reviewed to insure the complete data was included
## and look for any additional stop words that may need to be added.

## Create a new data frame with the cleaned data from CleanFedCorpus)
(Feddataframe <- data.frame(text=sapply(CleanFedCorpus, identity), 
                            stringsAsFactors=F))
write.csv(Feddataframe, "CorpusFEDoutput2.csv")

## Create a new data from with the cleaned data from CleanFedCorpusLemm
(FeddataframeLemm <- data.frame(text=sapply(CleanFedCorpusLemm, identity), stringsAsFactors=F))
#View(FeddataframeLemm)

## Output to a csv file
write.csv(FeddataframeLemm, "CorpusFEDoutputLemm2.csv")

## The View function was used to review the data in Feddataframe and FeddataframeLemm.  Text is missing from 
## some files in both data frames. However, when the dataframes are exported to CSV, all text appears.
## During the modeling phase, troubleshooting will contiunue to insure the data from all files is included.

## ------------------------------------------------------------------
## MODELING
##-------------------------------------------------------------------

## View corpus as a document matrix
## TMD stands for Term Document Matrix
(Fed_TermDM <- TermDocumentMatrix(CleanFedCorpusLemm))
inspect(Fed_TermDM)

## NOTE: DocumentTermMatrix vs. TermDocumentMatrix.  TermDocument means that the 
## terms are on the vertical axis and the documents are along the horizontal axis. 
## DocumentTerm is the reverse.

## Before we normalize, we can look at the overall frequencies of words 
## This will find words that occur more than 100 times in the entire corpus
findFreqTerms(Fed_TermDM, 100)
## Find assocations with aselected conf

## Visualize
## Wordclouds
inspect(Fed_TermDM)
m <- as.matrix(Fed_TermDM)
(m)
word.freq <- sort(rowSums(m), decreasing = T)
#wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 100,random.order = F)

## MODEL 1: HIERARCHICAL CLUSTERING ANALYSIS
#FedCleanDF <- as.data.frame(as.matrix(Fed_TermDM)) # reads in all essays
FedCleanDF <- as.data.frame(inspect(Fed_TermDM)) # only reads in 10 essays
(FedCleanDF)
CleanDFScale <- scale(FedCleanDF)
d <- dist(CleanDFScale,method="euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)

## The matrix now allows a visualization of key words of interest and their frequency in each document.
## However, we still need to normalize.  This example is very small and all docs in this example are about the
## same size.  However, this will not always be the case so it is a good practise to always normalize.
(Fed_Doc_TM <- DocumentTermMatrix(CleanFedCorpusLemm))
tm::inspect(Fed_Doc_TM)
#View(as.matrix(Fed_Doc_TM[1:10, 1:5]))
#View(as.matrix(Fed_Doc_TM[48:58, 1:5]))

## Normalize the Term Doc Matrix from above and then visualize it again
NormalizedTermDM <- TermDocumentMatrix(CleanFedCorpusLemm, control = list(weighting = weightTfIdf, stopwords = TRUE))
tm::inspect(NormalizedTermDM)

## Normalize the Document Term Doc Matrix from above and then visualize it again
NormalizedDocTermM <- DocumentTermMatrix(CleanFedCorpusLemm, control = list(weighting = weightTfIdf, stopwords = TRUE))
tm::inspect(NormalizedDocTermM )

## Use HCA to cluster documents and visualize normalized DTM
## The dendrogram:
## Terms higher in the plot appear more frequently within the corpus
## Terms grouped near to each other are more frequently found together
## The tm::inspect will only read in 10 essays to make the dendogram easier to read>>
CleanDF_N <- as.data.frame(inspect(NormalizedTermDM))
CleanDFScale_N <- scale(CleanDF_N )
d <- dist(CleanDFScale_N,method="euclidean")
fit <- hclust(d, method="ward.D2")
rect.hclust(fit, k = 4) # cut tree into 2 clusters 

CleanDF_NDoc_Term <- as.data.frame(as.matrix(NormalizedDocTermM))
CleanDFScale_N_Doc_Term <- scale(CleanDF_NDoc_Term )
d2 <- dist(CleanDFScale_N_Doc_Term,method="euclidean")
hclust(d2, method="ward.D2")
fit2
rect.hclust(fit2, k = 3) # cut tree into 4 clusters 

## Several other distance methods were tested - manhattan and maximum plus the 
## clustering methods single, complete, average and median were also tested with similar results.
## Below are the settings for distance maximum, 2 clusters and complete clustering model.
d3 <- dist(CleanDFScale_N_Doc_Term,method="maximum")
fit3 <- hclust(d3, method="complete")
rect.hclust(fit3, k = 4) # cut tree into 2 clusters 
plot(fit3)

## MODEL 2: K-MEANS CLUSTERING ANALYSIS

## Use kmeans to cluster the documents
ClusterM <- t(m) # transpose the matrix to cluster documents 
k <- 4 # number of clusters
kmeans_FED_Result <- kmeans(ClusterM, k)
(kmeans_FED_Result)

#Use the fviz_cluster function from factoextra to create a visualization of the Kmeans clusters
fviz_cluster(kmeans_FED_Result, data = ClusterM,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

## See the clusters in a table format
(kmeans_FED_Result$cluster)

## JTF use this small sample example for agnes
#Use the agnes function in cluster for single and complete linkages in HAC
(agnes(d,method = "complete"))
(agnes(d,method = "single"))

## COSINE SIMILARITY

## Use cosine similarity to find similarities in our matrix
m_norm_term_DM<-NormalizedTermDM
(tm::inspect(m_norm_term_DM))

FEDcosine_dist_mat <- 
  1 - crossprod_simple_triplet_matrix(m_norm_term_DM)/
  (sqrt(col_sums(m_norm_term_DM^2) %*% t(col_sums(m_norm_term_DM^2))))

(FEDcosine_dist_mat)

## Some people will use 1 - cosine sim to get the nearness - in that case - 1 is nearest.

(FEDcos_sim_matrix <-(1 - FEDcosine_dist_mat))
## Now, larger means closer or more similar

## Compare this to the lsa cosine sim method
LSA_Cos_Method <- lsa::cosine(as.matrix(m_norm_term_DM))
(LSA_Cos_Method)  ## This is the SAME as the FEDcos_sim_matrix above

## MODEL 2: K-MEANS CLUSTERING ON NORMALIZED DATA
## k means clustering for the normalized data
ClusterMN <- t(m_norm_term_DM) # transpose the matrix to cluster documents 
#set.seed(100) # set a fixed random seed
k <- 2 # number of clusters
kmeans_FED_Result2 <- kmeans(ClusterMN, k)

## See the clusters
(kmeans_FED_Result2$cluster)
fviz_cluster(kmeans_FED_Result, data = ClusterM,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

### JTF - use this on federalist papers and update comments
## use PCA (prin comp analysis) to reduce the Dim 
## to view the clusters
clusplot(DataNoLabels, kmeansSmallModel$cluster,color=TRUE,
         shade=TRUE, labels=2, lines=0)


### JTF - new code from Text Mining and NLP to test
## Euclidean
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=4)

## Cosine Similarity
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=4)

## Cosine Similarity for Normalized Matrix
groups_C_n <- hclust(distMatrix_C_norm,method="ward.D")
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=4)

## NETWORK OF THE DATA - DOCUMENT SIMILARITY
ClosenessMatrix <- FEDcos_sim_matrix
ClosenessMatrix[ClosenessMatrix < 0.15] <- 0
ClosenessMatrix[ClosenessMatrix >= 0.15] <- 1
(ClosenessMatrix)
as.network(ClosenessMatrix, directed=FALSE)
g<-as.network.matrix(ClosenessMatrix,matrix.type="adjacency", directed=FALSE)
plot(g)

My_igraph <- graph_from_adjacency_matrix(ClosenessMatrix, mode ="undirected",diag = TRUE,
                                         add.colnames = TRUE)

My_igraph <- simplify(My_igraph, remove.multiple = TRUE, remove.loops = TRUE)

plot(My_igraph, vertex.size=10)
tkplot (My_igraph, vertex.size=8, vertex.color="yellow")

## This is a small example of cosine similarity so you can see how it works
## I will comment it out...
######  m3 <- matrix(1:9, nrow = 3, ncol = 3)
######   (m3)
######   ((crossprod(m3))/(  sqrt(col_sums(m3^2) %*% t(col_sums(m3^2))   )))

#heatmap https://www.rdocumentation.org/packages/stats/versions/3.5.0/topics/heatmap
(heatmap(FEDcos_sim_matrix)
  
  
  