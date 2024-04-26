### Data Preprocessing

# libraries
library(dplyr)

df = read.csv("datasets/prosperLoanData.csv")

dim(df)
names(df)
head(df)
summary(df)


# remove duplicated rows
df = df %>% distinct()

# multiple listings have different ProsperScore but everything else is the same
# cannot be reliable, and can't assume which entry is valid
dupkey = df$ListingKey[duplicated(df$ListingKey)]
df = df %>% filter(!ListingKey %in% dupkey)
nrow(df)

### only include relevant variables

# APR > interest rate for usage, apparently (from Google) - they are highly correlated
# cor(df$BorrowerAPR, df$BorrowerRate, use = "pairwise.complete.obs")

# checking the datatypes of each variable
# apply(df[-1,], 2, typeof)

### Correcting variable types

######## i think this can be removed since we're likely not using them?

# LoanStatus (variable of interest) is a character
table(df$LoanStatus)

# CurrentlyInGroup is a binary variable - recode to 0/1
table(df$CurrentlyInGroup)
df$CurrentlyInGroup = as.integer(as.logical(df$CurrentlyInGroup))

table(df$IncomeVerifiable)
df$IncomeVerifiable = as.integer(as.logical(df$IncomeVerifiable))

table(df$IsBorrowerHomeowner)
df$IsBorrowerHomeowner = as.integer(as.logical(df$IsBorrowerHomeowner))

var = colnames(df)

## We should remove variables that have a lot of NA's
colSums(is.na(df)) / nrow(df)

# As we can see, there are some variables that have like 80% of their values as NA's. As such, 
# they should be removed.
df <- df[,colSums(is.na(df)) / nrow(df) < 0.8]

## We should also only keep information on loans that have actually finished
table(df$LoanStatus)

# As such, the Chargedoff, Completed, and Defaulted loans are the only ones that have finished.
df <- df[df$LoanStatus %in% c("Chargedoff", "Completed", "Defaulted"),]

# Removing identifier columns from predictor space
df <- df[, !names(df) %in% c("ListingKey", "ListingNumber","GroupKey","LoanKey","LoanNumber",'MemberKey')]

# Make date columns into Dates in R

df$ListingCreationDate <- as.Date(df$ListingCreationDate)
df$ClosedDate <- as.Date(df$ClosedDate)
df$DateCreditPulled <- as.Date(df$DateCreditPulled)
df$FirstRecordedCreditLine <- as.Date(df$FirstRecordedCreditLine)
df$LoanOriginationDate <- as.Date(df$LoanOriginationDate)

df$ProsperRatingNew = ifelse(df$CreditGrade!="", df$CreditGrade, df$ProsperRating..Alpha.)
df$ProsperRatingNew[df$ProsperRatingNew==""] = 'NC'
df = df[, !names(df) %in% c("ProsperRating..numeric.", "ProsperRating..Alpha.", "CreditGrade")]

df$EmploymentStatus[df$EmploymentStatus == ""] = "Not available"
df$EmploymentStatus[df$EmploymentStatus == "Retired"] = "Not employed"
df$IncomeRange[df$IncomeRange == "$0"] = "Not employed"

df$IncomeRange = factor(df$IncomeRange, levels = c('Not displayed', 'Not employed', '$1-24,999', '$25,000-49,999', 
                                                   '$50,000-74,999', '$75,000-99,999', '$100,000+'), ordered = TRUE)
df$ProsperRatingNew = factor(df$ProsperRatingNew, levels = c("NC","HR","E","D","C","B","A","AA"), ordered = TRUE)
df$EmploymentStatus = factor(df$EmploymentStatus, levels = c("Not employed", "Not available", "Other","Part-time",
                                                             "Self-employed", "Full-time", "Employed"), ordered = TRUE)

write.csv(df, "datasets/CleanedProsperData.csv")

