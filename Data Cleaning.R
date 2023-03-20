library(bsrc)
library(dplyr)
library(lubridate)

# load data
load("Data and Samples/Question 1 - Data/Data.RData")

df <- bsrc.findid(df = HAM, id.var = "masterdemoid", idmap = idmap)

# calculate HAM score
# all var starting with "ham_" excluding 3a and 3e

# selecting relevant variables
df %>%
    select(starts_with("ham_")) %>%
    select(-contains(c("_date", "_3a_", "_3b_", "_3c_", "_3d_", "_3e_"))) %>%
    colnames()

# get the sum of hrm scores
df$ham_score = df %>%
    select(starts_with("ham_")) %>%
    select(-contains(c("_date", "_3a_", "_3b_", "_3c_", "_3d_", "_3e_"))) %>%
    rowSums(., na.rm = T)

# identify the repeated masterdemoid column
which(colnames(df) == "masterdemoid")

# exclude the redundant masterdemoid column
df[45] <- NULL

# OPTIONAL
# relabel row names as unique numbers
rownames(df) <- 1:nrow(df)

# exclude rows without a ham_date value
# filter out rows where ifexist value is False
# exclude columns for bprsx and soloffid
# exclude ham_3 sub-items
df2 <- df %>%
    filter(!is.na(ham_date)) %>%
    filter(ifexist == T) %>%
    select(-starts_with("bprsx_"), -soloffid) %>%
    select(-contains(c("_3a_", "_3b_", "_3c_", "_3d_", "_3e_")))

# convert ham_date to POSIX date format
df2$ham_date <- ymd(df2$ham_date)

# get mean score for each masterdemoid
ham_mean <- df2 %>%
    group_by(masterdemoid) %>%
    summarise(ham_mean = mean(ham_score, na.rm = T))

# get latest scores for each masterdemoid
df2 <- df2 %>%
    group_by(masterdemoid) %>%
    slice(which.max(ham_date))

# combine data frame with latest response with mean scores
df3 <- merge(df2, ham_mean, by = "masterdemoid")

# compile final data frame
# reorder columns to have ids at the start, calculated scores, then scores for each items
# excluded the ifexist column
final_df <- df3 %>%
    select(masterdemoid, wpicid, ogid, VisitTimePoint, ham_date, ham_score, ham_mean, everything(), -ifexist)

saveRDS(final_df, file = "output/final_df.rds")