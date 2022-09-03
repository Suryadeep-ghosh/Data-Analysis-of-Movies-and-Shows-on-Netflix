data=read.csv("C:\\Users\\LENOVO\\Downloads\\netflix.csv")
View(data)
netflix=na.omit(data[,-10])
View(netflix)
names(netflix)
summary(netflix)
str(netflix)
par(mfrow=c(2,3))
boxplot(netflix$release_year,main="Release Year")
boxplot(netflix$runtime,main="Runtime")
boxplot(netflix$imdb_score,main="IMDB Score")
boxplot(netflix$imdb_votes,main="IMDB Votes")
boxplot(netflix$tmdb_popularity,main="TMDB Popularity")
boxplot(netflix$tmdb_score,main="TMDB Score")
par(mfrow=c(2,2))
plot(netflix$runtime,netflix$imdb_score,main="Runtime Vs. IMDB Score",xlab="Runtime",ylab="IMDB Score")
plot(netflix$imdb_score,netflix$imdb_votes,main="IMDB Votes Vs. IMDB Scores",xlab="IMDB Scores",ylab="IMDB Votes")
plot(netflix$runtime,netflix$tmdb_score,main="Runtime VS. TMDB Score",xlab="Runtime",ylab="TMDB Score")
plot(netflix$tmdb_score,netflix$tmdb_popularity,main="TMDB Score Vs. TMDB Popularity",xlab="TMDB Score",ylab="TMDB Popularity")
par(mfrow=c(2,3))
hist(netflix$release_year,main="Histogram of Release Date",xlab="Release Date")
hist(netflix$runtime,main="Histogram of Runtime",xlab="Runtime")
hist(netflix$imdb_score,main="Histogram of IMDB Score",xlab="IMDB Score")
hist(netflix$imdb_votes,main="Histogram of IMDB Votes",xlab="IMDB Votes")
hist(netflix$tmdb_score,main="Histogram of TMDB Score",xlab="TMDB Score")
hist(netflix$tmdb_popularity,main="Histogram of TMDB Popularity",xlab="TMDB Popularity")

max(netflix$imdb_score)
netflix$title[which.max(netflix$imdb_score)]
netflix$type[which.max(netflix$imdb_score)]
netflix$description[which.max(netflix$imdb_score)]
netflix$release_year[which.max(netflix$imdb_score)]
netflix$age_certification[which.max(netflix$imdb_score)]
netflix$runtime[which.max(netflix$imdb_score)]
netflix$genres[which.max(netflix$imdb_score)]
netflix$production_countries[which.max(netflix$imdb_score)]
netflix$imdb_votes[which.max(netflix$imdb_score)]

min(netflix$imdb_score)
netflix$title[which.min(netflix$imdb_score)]
netflix$type[which.min(netflix$imdb_score)]
netflix$description[which.min(netflix$imdb_score)]
netflix$release_year[which.min(netflix$imdb_score)]
netflix$age_certification[which.min(netflix$imdb_score)]
netflix$runtime[which.min(netflix$imdb_score)]
netflix$genres[which.min(netflix$imdb_score)]
netflix$production_countries[which.min(netflix$imdb_score)]
netflix$imdb_votes[which.min(netflix$imdb_score)]

max(netflix$tmdb_score)
netflix$title[which.max(netflix$tmdb_score)]
netflix$type[which.max(netflix$tmdb_score)]
netflix$description[which.max(netflix$tmdb_score)]
netflix$release_year[which.max(netflix$imdb_score)]
netflix$age_certification[which.max(netflix$tmdb_score)]
netflix$runtime[which.max(netflix$tmdb_score)]
netflix$genres[which.max(netflix$tmdb_score)]
netflix$production_countries[which.max(netflix$tmdb_score)]
netflix$tmdb_popularity[which.max(netflix$tmdb_score)]

min(netflix$tmdb_score)
netflix$title[which.min(netflix$tmdb_score)]
netflix$type[which.min(netflix$tmdb_score)]
netflix$description[which.min(netflix$tmdb_score)]
netflix$release_year[which.min(netflix$imdb_score)]
netflix$age_certification[which.min(netflix$tmdb_score)]
netflix$runtime[which.min(netflix$tmdb_score)]
netflix$genres[which.min(netflix$tmdb_score)]
netflix$production_countries[which.min(netflix$tmdb_score)]
netflix$tmdb_popularity[which.min(netflix$tmdb_score)]


netflix$tmdb_score[which(netflix$title=="Breaking Bad")]
netflix$imdb_score[which(netflix$title=="Pink Zone")]



US=subset(netflix,netflix$production_countries=="['US']")
JP=subset(netflix,netflix$production_countries=="['JP']")
View(US)
View(JP)
dim(US)
dim(JP)
par(mfrow=c(1,2))
boxplot(US$imdb_score,main="US IMDB Score",xlab="US",ylim=c(0,10))
boxplot(JP$imdb_score,main="JP IMDB Score",xlab="JP",ylim=c(0,10))
var.test(US$imdb_score,JP$imdb_score,alternative = "two.sided")
var.test(US$tmdb_score,JP$tmdb_score,alternative = "two.sided")
t.test(US$imdb_score,JP$imdb_score,var.equal=FALSE,alt="less")
t.test(US$tmdb_score,JP$tmdb_score,var.equal=TRUE,alt="less")


movie=subset(netflix,netflix$type=="MOVIE")
show=subset(netflix,netflix$type=="SHOW")
View(movie)
View(show)
dim(movie)
dim(show)
par(mfrow=c(1,2))
boxplot(movie$imdb_score,main="Movie IMDB Score",xlab="Movie",ylim=c(0,10))
boxplot(show$imdb_score,main="Show IMDB Score",xlab="Show",ylim=c(0,10))
var.test(movie$imdb_score,show$imdb_score,alternative = "two.sided")
var.test(movie$tmdb_score,show$tmdb_score,alternative = "two.sided")
t.test(movie$imdb_score,show$imdb_score,var.equal=TRUE,alt="less")
t.test(movie$tmdb_score,show$tmdb_score,var.equal=TRUE,alt="less")

short=subset(netflix,netflix$runtime<=80)
long=subset(netflix,netflix$runtime>80)
View(short)
View(long)
dim(short)
dim(long)
par(mfrow=c(1,2))
boxplot(short$imdb_score,main="Short IMDB Score",xlab="Short",ylim=c(0,10))
boxplot(long$imdb_score,main="Long IMDB Score",xlab="Long",ylim=c(0,10))
var.test(short$imdb_score,long$imdb_score,alternative = "two.sided")
var.test(short$tmdb_score,long$tmdb_score,alternative = "two.sided")
t.test(short$imdb_score,long$imdb_score,var.equal=TRUE,alt="greater")
t.test(short$tmdb_score,long$tmdb_score,var.equal=FALSE,alt="greater")

comedy=subset(netflix,netflix$genres=="['comedy']")
drama=subset(netflix,netflix$genres=="['drama']")
View(comedy)
View(drama)
dim(comedy)
dim(drama)
par(mfrow=c(1,2))
boxplot(comedy$imdb_score,main="Comedy IMDB Score",xlab="Comedy",ylim=c(0,10))
boxplot(drama$imdb_score,main="Drama IMDB Score",xlab="Drama",ylim=c(0,10))
var.test(comedy$imdb_score,drama$imdb_score,alternative = "two.sided")
var.test(comedy$tmdb_score,drama$tmdb_score,alternative = "two.sided")
t.test(comedy$imdb_score,drama$imdb_score,var.equal=FALSE,alt="less")
t.test(comedy$tmdb_score,drama$tmdb_score,var.equal=FALSE,alt="less")


old=subset(netflix,netflix$release_year<=2016)
new=subset(netflix,netflix$release_year>2016)
View(old)
View(new)
dim(old)
dim(new)
par(mfrow=c(1,2))
boxplot(old$imdb_score,main="Old IMDB Score",xlab="Old",ylim=c(0,10))
boxplot(new$imdb_score,main="New IMDB Score",xlab="New",ylim=c(0,10))
var.test(old$imdb_score,new$imdb_score,alternative = "two.sided")
var.test(old$tmdb_score,new$tmdb_score,alternative = "two.sided")
t.test(old$imdb_score,new$imdb_score,var.equal=TRUE,alt="greater")
t.test(old$tmdb_score,new$tmdb_score,var.equal=FALSE,alt="less")

model1=lm(imdb_score~release_year+runtime+imdb_votes+as.factor(type)+tmdb_score+tmdb_popularity,data=netflix)
summary(model1)

model2=lm(tmdb_score~release_year+runtime+tmdb_popularity+as.factor(type)+imdb_score+imdb_votes,data=netflix)
summary(model2)

cov1=data.frame(netflix$release_year,netflix$runtime,netflix$imdb_votes,netflix$tmdb_score,netflix$tmdb_popularity)
y1=netflix$imdb_score
library(lars)
mod.lasso1=lars(as.matrix(cov1),y1,normalize=F)
plot(mod.lasso1,xvar="df") #--plotting-coef-paths--R
coef(mod.lasso1) #--coeffs-for-each-step-in-path
cv.lasso1=cv.lars(as.matrix(cov1),y1,type="lasso") #--cross-validation
limit1=min(cv.lasso1$cv)+cv.lasso1$cv.error[which.min(cv.lasso1$cv)]
s.cv1=cv.lasso1$index[min(which(cv.lasso1$cv<limit1))]
coef(mod.lasso1,s=s.cv1,mode="fraction")

cov2=data.frame(netflix$release_year,netflix$runtime,netflix$imdb_votes,netflix$imdb_score,netflix$tmdb_popularity)
y2=netflix$tmdb_score
library(lars)
mod.lasso2=lars(as.matrix(cov2),y2,normalize=F)
plot(mod.lasso2,xvar="df") #--plotting-coef-paths--R
coef(mod.lasso2) #--coeffs-for-each-step-in-path
cv.lasso2=cv.lars(as.matrix(cov2),y2,type="lasso") #--cross-validation
limit2=min(cv.lasso2$cv)+cv.lasso2$cv.error[which.min(cv.lasso2$cv)]
s.cv2=cv.lasso2$index[min(which(cv.lasso2$cv<limit2))]
coef(mod.lasso2,s=s.cv2,mode="fraction")


