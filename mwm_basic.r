#!/usr/bin/env r

# Patrick Steadman, Franklandlab 2015

#### 20150817
# unable to get ddply to work when including mouse with the current naming scheme.
# works when mouse not included in melt call

# for watermaze what should be the lm call?

getgroupstat <- function(data, groupvar="genotype", 
        comparisonvar=c("targetzone","nontargetzone")
        ){
    "
    Requires: 
        plyr, reshape2

    Useful to get summarized data for constructing box plots
    
    Inputs:
    groupvar:
        is the variable to contrast experiment data by, often
        this is genotype
    comparisonvar:
        are variables you want to compare between groupvar.
        this could be time in targetzone and nontargetzone in the
        watermaze for example.
    "
    melted<-melt(data[,c(groupvar,
        comparisonvar)], id.vars=groupvar)
    data.sem<-ddply(melted,c(groupvar,"variable"), 
        summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)) )
    data.sem<-transform(data.sem, lower=mean-sem, upper=mean+sem)
    return(data.sem)
    }

addcatvar <- function(df1, df2, catvar="genotype"){
    "
    Add a categorical (factor/level) variable from df1 which is usually probe data  
    to df2 which is usually training data

    I use this to add genotype or condition information from a probe dataframe to
    my training dataframe

    Matches based on both dataframes with column titled 'mouse' - therefore requires 
    a column with this name in df1 and df2

    Inputs:
    df1:
        dataframe extract categorical variable from (e.g. probe test)
    df2:
        dataframe you are adding categorical variable to (e.g. training data) where
        subject occurs many times
    "
    # Tmp make new column all ""
    df2[,catvar] <- as.character("")
    for (ms in levels(df2$mouse)){
        df2[df2$mouse==ms,][catvar] <- as.character(df1[df1$mouse==ms,][catvar])
    }
    return(as.data.frame(df2))
}

# average latency for each mouse, for each day, average time
averagedays <- function(data){
    "
    FIX TO INCLUDE ALL OTHER COLS as well as new day col 
    Inputs:
    data:
        dataframe of your mouse data with columns named
        mouse, day, timetoplatform, trialduration
    "
    out <- NULL
    for (i in levels(data$mouse)){
        for (j in seq(1:max(data$day))){
            tmp<-data.frame(mouse=i,
                genotype=subset(data, mouse==i & day==j & trial==1, genotype),
                dose=subset(data, mouse==i & day==j & trial==1, dose),
                day=j, 
                timetoplatform=mean(subset(data, mouse==i & day==j)$timetoplatform),
                trialduration=mean(subset(data, mouse==i & day==j)$trialduration)
                )
            out <- rbind(out, tmp)
            }
        }
    return(as.data.frame(out))
}