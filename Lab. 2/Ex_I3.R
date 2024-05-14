tablou =  read.csv("life_expect.csv", header=T, sep=',')

male=tablou[['male']]
female=tablou[['female']]

min1=min(male)
max1=max(male)

min2=min(female)
max2=max(female)

interval1 = seq(from=min1, to=max1, length.out=8)
hist(male, breaks=interval1, right=F, freq=F, col="blue")

interval2 = seq(from=min2, to=max2, length.out=8)
hist(female, breaks=interval2, right=T, freq=F, col="red")

