
setwd(".")

#let's read same data from exercise 1

data<-read.csv("data.csv") 

head(data,20)

?subset

numeric_columns<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
int_students=subset(data,finnish=="no",select=numeric_columns)

int_students=subset(data,finnish=="no",select=c(1:10))

int_students=subset(data,finnish=="no",select=c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))

int_students=data[data$finnish=="no",1:10]

int_students



correlation_between_Q1_Q2=cor(int_students$Q1,int_students$Q2)
correlation_between_Q1_Q2

mean_Q6=mean(int_students$Q1)
mean_Q6

standard_deviation <- function(sample){
    xbar<-mean(sample)
    sumXminusXbar_sqrd=0
    for(x in  sample){
        diff_from_mean=x-xbar
        sumXminusXbar_sqrd=sumXminusXbar_sqrd+(diff_from_mean*diff_from_mean)
    }
    denominator=length(sample)-1
    return(sqrt(sumXminusXbar_sqrd/denominator))
}

# Standard Deviation of Q1 responses
sd<-standard_deviation(data$Q1)
sd

sd(data$Q1)

frequency_table <- function(dataframe){
    res <- NULL
    
    for(columnName in names(dataframe)){
        
        sample<-dataframe[,c(columnName)]
        xbar<-mean(sample)
        
        #using our own defined started deviation
        sd<-standard_deviation(sample)
        count<-length(sample)
        
        res <- rbind(res,c(columnName,count,xbar,sd))
        
    }
    colnames(res) <- c("response_for","count","mean","standard_deviation")
    res<-data.frame(res)
    return(res)

}

# selecting or subsetting column 1 to 10 of our entire data

all_questions=data[,1:10]

freq_table<-frequency_table(all_questions)
freq_table



data

levels(data$website) 
factor(data$website)

library("hcitools")
news_score<-function(d){
    res <- NULL
    for (news_firm in levels(d$website)){ 
        for (gender in levels(d$gender)){
            
            subset<-d[d$website==news_firm&d$gender==gender,1:10]
            result=questionnaire.analyse(subset, name="SUS")
            res <- rbind(res,c(news_firm,gender,result))
        }
    }
    colnames(res) <- c("Website","Gender","Sus_score")
    res<-data.frame(res)
    return(res)
}

news_score(data)

?rnorm



generate_data<-function(){
    x<-seq(from=1,to=10)
    y<-rnorm(10,mean=0,sd=1)
    xy<-cbind(x,y)
    #asMatrix<-as.data.frame(xy)
    colnames(xy)<-c("x","y")
    return(data.frame(xy))
}

gen_data1<-generate_data()
gen_data1

gen_data2<-generate_data()
gen_data2



merged_data<-rbind(gen_data1,gen_data2)

merged_data


