### Data Preprocessing

# libraries
library(dplyr)

df = read.csv("prosperLoanData.csv")

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
cor(df$BorrowerAPR, df$BorrowerRate, use = "pairwise.complete.obs")


# checking the datatypes of each variable
apply(df[-1,], 2, typeof)



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

write.csv(df, "CleanedProsperData.csv")
