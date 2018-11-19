#> Author:          Arul Mozhivarman Seetharaman
#> Last Modified:   4/17/18

# Required packages -------------------------------------------------------

library(tidyverse)
library(corrplot)
library(reshape2)
library(fpc) # has various methods for clustering
library(factoextra) # cluster and visualize
library(cluster) #clustering
library(HSAUR)#A Handbook of Statistical Analyses Using R
library(rgl)
library(Rtsne)
library(FNN) #Fast nearest neigbhor search algorithm
library(dprep)
library(arules)
library(arulesViz)
library(purrr)
library(rjson) #Convert dataframes to Jason
library(plumber) # Create APIs
library(jsonlite)
library(data.table)
library(Rgraphviz)
library(pastecs)


# Helper Functions --------------------------------------------------------

#1. Function to Choose optimal no. of clusters based on Silhoutte width many k using PAM

opt_clust <- function(distx){
  csc_sil_width <- c(NA)
  
  for(i in 2:10){
    
    CSC_pam_fit <- pam(distx,diss = TRUE, k = i)
    
    csc_sil_width[i] <- CSC_pam_fit$silinfo$avg.width
    
  }
  plot(1:10, csc_sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width", type = "b",col = "blue", main = "Optimal no.of Clusters")
}

#2. Function to Get most similar student  
similar_student <- function(df,id)  {

  student <- id
  clt <- df$cluster[which(df$PERSON_UID == student)]
  #Finding Most similar items
  z1 <- df[df$cluster == clt,]
  idx <- which(z1$PERSON_UID == student)
  z1_sim <- (as.matrix(daisy(z1[,- which(names(z1) %in% c("PERSON_UID", "cluster")) ],metric = "gower")))
  
  z1_df <- as.data.frame(as.table(z1_sim[idx,]) )
  z1_df <- z1_df[z1_df$Freq > 0,]
  z1_df$Var1 <- as.numeric(z1_df$Var1)
  most_similar <- c(z1$PERSON_UID[( z1_df$Var1[z1_df$Freq == min(z1_df$Freq)] )])
  return(most_similar)
}

#3. Function to convert Rules to DF
rule_df <- function(rulez){
  rdf <- data.frame(
    LHS = labels(lhs(rulez)),
    RHS = labels(rhs(rulez)), 
    rulez@quality)
  return(rdf)
}

# Read inputs -------------------------------------------------------------

grades_v4 <- read.csv("FilePath")
students_v4 <- read.csv("FilePath")

# Master student profile --------------------------------------------------

csc_students <- students_v4 %>% select(-c(CATALOG_TERM_CODE,ADMISSION_TYPE_DESC,CITIZENSHIP_DESC,S_ID,COLLEGE_DESC,DEPARTMENT_DESC,MINOR_DESC)) %>% 
  filter(DEGREE_CODE == "BSCS", MAJOR_DESC == "Computer Science") %>% 
  select(-c(DEGREE_CODE,MAJOR_DESC)) %>% 
  group_by(PERSON_UID,TERM_CODE) %>% 
  arrange(PERSON_UID,TERM_CODE) %>% 
  group_by(PERSON_UID) %>% 
  mutate(level_cnt = length(unique(CLASS_AT_START_OF_TERM_CODE)),
         semester = paste0(CLASS_AT_START_OF_TERM_CODE,str_sub(TERM_CODE,-2)),
         actual_year = as.numeric(str_sub(TERM_CODE,1,4)),
         total_duration = (max(actual_year) - min(actual_year) ) ) %>% 
  group_by(PERSON_UID,CLASS_AT_START_OF_TERM_CODE) %>% 
  mutate(level_duration = (max(actual_year) - min(actual_year) ) )



csc_stud_grades <- left_join(csc_students,grades_v4[,-2], by = c("PERSON_UID" = "PERSON_UID", "TERM_CODE" = "TERM_CODE"))

# Missing Values ----------------------------------------------------------

sapply(csc_stud_grades, function(y) sum(length(which(is.na(y)))))

#Convert factors to characters,Replcae spaces and NA's with UNK, TERM_UG_GPA
csc_stud_grades <- csc_stud_grades %>% filter(!(is.na(RETN_UG_GR_GPA))) %>% 
  mutate(COURSE_REFERENCE_NUMBER = as.numeric(as.character(COURSE_REFERENCE_NUMBER)),
         SUBJECT_CODE            = gsub(" ","",SUBJECT_CODE),
         COURSE_NUMBER           = as.numeric(gsub(" ","",COURSE_NUMBER))  ,
         FINAL_GRADE             = (gsub(" ","",FINAL_GRADE)) ) %>% 
 mutate( ENROLLED_MAX_TRAD_TERM_CODE =    ifelse(is.na(ENROLLED_MAX_TRAD_TERM_CODE),0,ENROLLED_MAX_TRAD_TERM_CODE),
         SUBJECT_CODE                =    ifelse( is.na(SUBJECT_CODE),             "UNK",SUBJECT_CODE), 
         COURSE_REFERENCE_NUMBER     =    ifelse( is.na(COURSE_REFERENCE_NUMBER),   0,COURSE_REFERENCE_NUMBER),
         FINAL_GRADE                 =    ifelse( is.na(FINAL_GRADE),              "UNK",FINAL_GRADE),
         COURSE_NUMBER               =    ifelse( is.na(COURSE_NUMBER),             0  ,COURSE_NUMBER),
         COURSE_HOURS_ATTEMPTED      =    ifelse( is.na(COURSE_HOURS_ATTEMPTED),    0  ,COURSE_HOURS_ATTEMPTED),
         TERM_UG_GPA                 =    ifelse( is.na(TERM_UG_GPA),            0  ,TERM_UG_GPA),
         FULL_COURSE                 =    gsub(" ","",paste0(SUBJECT_CODE,COURSE_NUMBER))
         ) %>% 
  mutate(FINAL_GRADE                 =    ifelse( FINAL_GRADE == "",              "UNK",FINAL_GRADE)) %>% 
  group_by(PERSON_UID) %>% 
  mutate(transfer_student = ifelse(sum(FULL_COURSE=="ENGR3511") > 0, TRUE,FALSE))
csc_stud_grades <- csc_stud_grades[,c(1,2,13,14,4,11,15,26,3,16:20,25,5,7,6,8,9,10,12,21:24)]

# Feature Engineering -----------------------------------------------------

#csc_master <- csc_stud_grades  %>% select(-c(S_ID)) %>% 
G <- c("A","B","C","D")
csc_new_feats <-   csc_stud_grades  %>% 
filter(inrange(TERM_CODE, (201710-400),201620)) %>%
#csc_full_feats <-   csc_stud_grades  %>%
  group_by( PERSON_UID) %>% 
  mutate(
    GRADE_VALUE      = as.numeric(case_when(FINAL_GRADE == "A" ~  4,
                                            FINAL_GRADE == "B" ~  3,
                                            FINAL_GRADE == "C" ~  2,
                                            FINAL_GRADE == "D" ~  1,
                                            FINAL_GRADE == "W" ~  -1,
                                                           TRUE~  0)), 
    withdraw_count   = sum(FINAL_GRADE=="W"),
    FR_No_courses    = sum(CLASS_AT_START_OF_TERM_CODE == "FR"),
    SO_No_courses    = sum(CLASS_AT_START_OF_TERM_CODE == "SO"),
    JR_No_courses    = sum(CLASS_AT_START_OF_TERM_CODE == "JR"),
    SR_No_courses    = sum(CLASS_AT_START_OF_TERM_CODE == "SR"),
    total_No_courses = n(),
    FR_No_credits    = sum(COURSE_HOURS_ATTEMPTED[which(CLASS_AT_START_OF_TERM_CODE == "FR")] ),
    SO_No_credits    = sum(COURSE_HOURS_ATTEMPTED[which(CLASS_AT_START_OF_TERM_CODE == "SO")] ),
    JR_No_credits    = sum(COURSE_HOURS_ATTEMPTED[which(CLASS_AT_START_OF_TERM_CODE == "JR")] ), 
    SR_No_credits    = sum(COURSE_HOURS_ATTEMPTED[which(CLASS_AT_START_OF_TERM_CODE == "SR")] ),
    total_No_credits = sum(COURSE_HOURS_ATTEMPTED),
    withdraw_pct     =  (round((withdraw_count/total_No_courses),3 ))
  )
csc_new_feats <- csc_new_feats[,c(1:13,27,14:26,28:39)]
csc_full_feats <- csc_full_feats[,c(1:13,27,14:26,28:39)]


# Descrptive Statistics ---------------------------------------------------

desc_stat <- stat.desc(csc_new_feats[,-c(1,2,6)], basic=F)

# Half Profile ------------------------------------------------------------

#csc_halfProfile <- csc_new_feats %>% select(-c(CLASS_AT_START_OF_TERM_CODE,GRADE_VALUE,CITIZENSHIP_CODE,ADMISSION_TYPE_CODE,ENROLLED_MAX_TRAD_TERM_CODE,TRANSFER_COURSE_IND,TRANSFER_COURSE_INST_CODE,TRANSFER_COURSE_INST_DESC,COUNT_IN_GPA_IND, 
#                                               FR_No_credits,SO_No_credits,JR_No_credits,SR_No_credits,total_duration,level_duration,level_cnt,COURSE_HOURS_ATTEMPTED,FR_No_courses,SO_No_courses,JR_No_courses,SR_No_courses,withdraw_pct,semester,TERM_CODE,actual_year,COURSE_REFERENCE_NUMBER,SUBJECT_CODE,COURSE_NUMBER,FINAL_GRADE,FULL_COURSE)) %>% 
csc_halfProfile <- csc_new_feats %>% select(-c(FR_No_courses,SO_No_courses,JR_No_courses,SR_No_courses,FR_No_credits,SO_No_credits,JR_No_credits,SR_No_credits,total_No_credits, OU_TERM_UG_GPA,CLASS_AT_START_OF_TERM_CODE,GRADE_VALUE,CITIZENSHIP_CODE,ADMISSION_TYPE_CODE,ENROLLED_MAX_TRAD_TERM_CODE,TRANSFER_COURSE_IND,TRANSFER_COURSE_INST_CODE,TRANSFER_COURSE_INST_DESC,COUNT_IN_GPA_IND, 
                                               level_duration,level_cnt,COURSE_HOURS_ATTEMPTED,withdraw_pct,semester,TERM_CODE,actual_year,COURSE_REFERENCE_NUMBER,SUBJECT_CODE,COURSE_NUMBER,FINAL_GRADE,FULL_COURSE)) %>% 
                     distinct(PERSON_UID, .keep_all = TRUE) %>%
                   mutate(HONORS_IND = ifelse(HONORS_IND == "Y",TRUE,FALSE))

# Course Profile ----------------------------------------------------------

csc_best_courses <- csc_new_feats %>% group_by(PERSON_UID,FULL_COURSE) %>% 
                                 slice(which.max(GRADE_VALUE))%>% 
                                 select(PERSON_UID,FULL_COURSE,GRADE_VALUE)  
                                
csc_courses <- spread(csc_best_courses,key = FULL_COURSE,value = GRADE_VALUE,fill = -1)

csc_courses_transp <- t(csc_courses)

#Withdrawn or not taken by none

wdraw_or_ntaken <- colnames(csc_courses)[which(unlist(apply(csc_courses,MARGIN = 2, function(x) all((x <= 0 )))) )] 

csc_courses <- csc_courses[which(!unlist(apply(csc_courses,MARGIN = 2, function(x) all((x <= 0 )) )  ) )]

# Similarity Matrix-Gower's -----------------------------------------------

CSC_similarity_half     <- daisy(csc_halfProfile[,-1],metric ="gower")

CSC_similarity_courses  <- daisy(csc_courses[,-1],metric ="gower")

CSC_similarity_Tcourses <- daisy(csc_courses_transp,metric ="gower") 

# PAM Clustering---------------------------------------------------------------------

#Optimal clusters k for Half Profile: 3 
opt_clust(CSC_similarity_half)

#Optimal clusters k for Course Profile: 2
opt_clust(CSC_similarity_courses)

#k for Transp Course Profile: 
opt_clust(CSC_similarity_Tcourses)

#Run PAM for Half Profile K=3
set.seed(123)
csc_pam_half <- pam(CSC_similarity_half,diss = TRUE,k = 3)
csc_halfProfile$cluster <-  csc_pam_half$clustering

#Run PAM for Course K=2
set.seed(1234)
csc_pam_course <- pam(CSC_similarity_courses,diss = TRUE,k = 2)
csc_courses$cluster     <-  csc_pam_course$clustering

#Run PAM for Transp Course K=2
set.seed(12345)
csc_pam_course_transp <- pam(CSC_similarity_Tcourses,diss = TRUE,k = 2)

# Cluster Summary ---------------------------------------------------------
csc_clusSumry <- csc_halfProfile %>% group_by(cluster) %>% 
  summarise(honors_count               = sum(HONORS_IND),
            no_courses_wdraw           = sum(withdraw_count),
            transfer_count             = sum(transfer_student),
            clust_mean_gpa             = mean(RETN_UG_GR_GPA),
            clust_mean_no_courses      = mean(total_No_courses),
            count                      = n()
  )

#Vizualizing the clusters by reducing dimensions:  t-distributed stochastic neighborhood embedding, or t-SNE.
csc_tsne_obj <- Rtsne(CSC_similarity_courses, is_distance = TRUE)

csc_tsne_data <- csc_tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(csc_pam_course$clustering),
        PERSON_UID = csc_halfProfile$PERSON_UID)

ggplot(aes(x = X, y = Y), data = csc_tsne_data) +
  geom_point(aes(color = cluster)) +
  labs(
    title = "Cluster Formation",
    subtitle = "Clusters from HALF PROFILE",
    caption = "OU UG_CSC",
    x = "Dim1",
    y = "Dim2"
  )

ggplot(aes(x = X, y = Y), data = csc_tsne_data) +
  geom_point(aes(color = cluster)) +
  labs(
    title = "Cluster Formation",
    subtitle = "Clusters from COURSES PROFILE",
    caption = "OU UG_CSC",
    x = "Dim1",
    y = "Dim2"
  )

fviz_dist(CSC_similarity_half,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),order = TRUE,show_labels = F)
fviz_dist(CSC_similarity_courses,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),order = TRUE,show_labels = F)

fviz_dist((get_dist(csc_courses[,-1],method = "euclidean")),gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),order = TRUE,show_labels = F)

# Most Similar Students --------------------------------------------------------

#Finding Most similar students from HALF PROFILE

msh <- similar_student(csc_halfProfile,31541751)

#Finding Most similar students from PROFILE
msc <- similar_student(csc_courses,31541751)

# Courses taken by Most similar students in Spring'17 -----------------------------

csc_fall17 <- csc_new_feats %>% select(PERSON_UID,FULL_COURSE,FINAL_GRADE,GRADE_VALUE,TERM_CODE,CLASS_AT_START_OF_TERM_CODE) %>% 
              filter(TERM_CODE %in% (seq(201320, 201620, by = 100) ),FINAL_GRADE != "UNK" ) %>% 
              select(-TERM_CODE) %>% group_by(PERSON_UID,FULL_COURSE) %>% 
              slice(which.max(GRADE_VALUE)) %>% 
              select(PERSON_UID,FULL_COURSE,FINAL_GRADE)

#To check duplicates courses
csc_fall17 <- csc_fall17[!duplicated(csc_fall17),] %>% 
              mutate(course_value = paste0(FULL_COURSE)) %>% 
              select(-c(FINAL_GRADE))

#Spread the FULL_COURSE column across the table
csc_spread17 <- spread(csc_fall17,key = FULL_COURSE,value = course_value, fill = -1)  

csc_next_similar <- inner_join(csc_halfProfile[,which(names(csc_halfProfile) %in% c("PERSON_UID", "HONORS_IND","transfer_student", "cluster"))],csc_spread17,by = "PERSON_UID")
csc_next_similar <- csc_next_similar %>% mutate(HONORS_IND       = ifelse(HONORS_IND       == TRUE,"H","NH"),
                                                transfer_student = ifelse(transfer_student == TRUE,"T","NT"))


#Fetch courses taken by these most_similar students in Fall'17
ms_mat <- as.matrix(csc_next_similar[csc_next_similar$PERSON_UID %in% msc,-which(names(csc_next_similar) %in% c("cluster"))]) 
ms_list <- lapply(1:nrow(ms_mat), FUN = function(i) (ms_mat[i, ]))  
ms_list <- lapply(ms_list, function(x) x[x!='-1'])


# K-means Clustering ------------------------------------------------------

#Half profile
#Optimal Clausters
csc_kmeans_half <- csc_halfProfile[,-9] 
csc_kmeans_half$HONORS_IND <- as.numeric(csc_kmeans_half$HONORS_IND)
csc_kmeans_half$transfer_student <- as.numeric(csc_kmeans_half$transfer_student)

fviz_nbclust(csc_kmeans_half[,-1], kmeans, method = "wss")

#K-means for 3
csc_khalf <- kmeans(csc_kmeans_half[,-1],3,nstart = 50,iter.max = 15)
csc_kmeans_half$cluster <- csc_khalf$cluster
 #Visualization
fviz_cluster(csc_khalf,csc_kmeans_half[,-1])
clusplot(csc_kmeans_half[,-1],csc_khalf$cluster,color = T,shade = T,labels = 2,lines = 0)



#Course Profile
fviz_nbclust(csc_courses[,-1], kmeans, method = "wss")

#K-means for 4
csc_kcourse <- kmeans(csc_courses[,-1],4,nstart = 50,iter.max = 15)
#Visualization
fviz_cluster(csc_kcourse,csc_courses[,-1])

Kmean_Sumry <- csc_kmeans_half %>% group_by(cluster) %>% 
  summarise(honors_count               = sum(HONORS_IND),
            no_courses_wdraw           = sum(withdraw_count),
            transfer_count             = sum(transfer_student),
            clust_mean_GPAhours        = mean(RETN_UG_GR_GPA_HOURS),
            clust_mean_gpa             = mean(RETN_UG_GR_GPA),
            clust_mean_no_courses      = mean(total_No_courses),
            count                      = n()
  )

# Preparing DF for Transaction --------------------------------------------------
 sid   <- 000000008
# clust <- csc_halfProfile$cluster[which(csc_halfProfile$PERSON_UID == sid)]

csc_sparse <- csc_spread17 %>% 
  filter(PERSON_UID != sid)  

csc_sparse <- csc_sparse[,-1] 

#Convert all columns to factors
csc_sparse[] <- lapply(csc_sparse, factor)

#Convert DF to list of list to easily create transaction type data
spa_mat <- as.matrix(csc_sparse)
spa_list <- lapply(1:nrow(spa_mat), FUN = function(i) (spa_mat[i, ]))  
spa_list <- lapply(spa_list, function(x) x[x!='-1'])

#Transaction
csc_trans <- as(spa_list,"transactions")
inspect(csc_trans)

# Association Rule Mining -------------------------------------------------

freq_items <- eclat(csc_trans,parameter = list(supp = 0.01,maxlen = 10,minlen = 2))
arules::inspect(arules::sort(freq_items, by = "support", decreasing = T)) 

#course frequency plot: top 5 courses- CS3053, CS3323, ENGL3153, MATH4753, MATH3333
itemFrequencyPlot(csc_trans, topN = 30, type = "absolute", main = "Item Frequency")

#Apriori

a_rules17 <-apriori(csc_trans,parameter = list(supp = 0.015,conf = 0.4,minlen = 2))
#appearance = list(rhs = c("CS3053=4") , default = "lhs"),control = list (verbose=F))
inspect(a_rules17)
quality(a_rules17) <- round(quality(a_rules17), digits = 3)
inspect((sort(a_rules17,by = "support", decreasing = T))  )

#Full rules Viz.
plot(a_rules17,method = "graph",control = list(type="items"), main = "All Default Rules ")

#Remove Redundant Rules
csc_rednt <- a_rules17[is.redundant(a_rules17)] 
inspect(csc_rednt) 
unqRules17 <- a_rules17[!(is.redundant(a_rules17))] # remove subsets
inspect((sort(unqRules17,by = "support", decreasing = T))  )
inspect(minlen=4)

#Subset rules
no_core <- sort(subset(unqRules17,
                         !(rhs %in% c("CS4273","MATH4753","COMM2613","CS3053","CS3323","MATH3333",
                                      "CS2413","CS2613","CS2813",
                                      "CS2334","CS1323","CS2603","ENGR3511","PHYS2514","BC2813",
                                      "MATH3113","MATH2924","PSC1113","MATH2934","MATH2443"))),
                       decreasing = TRUE,
                       by = "support")  

inspect(no_core)

#Remove repeating items 
gitems <- (generatingItemsets(a_rules17)) 
impure <- arules::duplicated(gitems)
inspect(no_core[!impure]) 

#Grouped Matrix Viz

plot(a_rules17, method = "grouped matrix")
plot(unqRules17, method = "grouped matrix")
plot(no_core, method = "grouped matrix",
     gp_labels = gpar(col = "blue", cex=1, fontface="italic",fontsize = 13),
     rhs_max = 15
     )







#no_core rules Viz.
plot(no_core,method = "graph",control = list(type="items")
     ,main = "Association Rules: Core and Non-Core Courses ") 

plot(no_core,method = "graph",control = list(type="items"),main = "No_core Rules ",engine="graphviz") 

plotly_arules(no_core, measure = c("support", "lift"), shading = "confidence")
#Unique rules Viz.
plot(unqRules17,method = "graph",control = list(type="items"),main = "Unique Default Rules ",max = 100) 

#Convert Rules as Data Frames
unique_rules <- rule_df(unqRules17)

#Convert Rules DF to Jason fromat
jason_unq <- toJSON(unname(split(unique_rules, 1:nrow(unique_rules))))
cat(jason_unq)

#Api Call
r2 <-plumber::plumb("api_call.R")
r2$run()
# Retrieve student ids for returned students

students3$S_ID[students3$PERSON_UID == 0000000001]
students3$S_ID[students3$PERSON_UID == 0000000008]
