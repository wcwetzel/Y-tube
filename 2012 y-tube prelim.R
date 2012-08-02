### preliminary analysis of Y-tube data
#20 Jul 2012
# wet plants vs. dry plants

library(lme4)
library(rethinking)

#d = read.csv()

wet = scan()
0
0
1
2
2
0
1
1
1
0
0
0
2
1
1
1
1
0
0
0
1
1
1
1
0
1
1
1
0
1
1
0
1
1
1
1
1
1
1
1
1
1
0
0
0
0
0
2



pair = scan()
1
1
1
2
2
2
3
3
3
4
4
4
5
5
5
6
6
6
7
7
7
8
8
8
9
9
9
10
10
10
11
11
11
12
12
12
13
13
13
14
14
14
15
15
15
16
16
16



d = data.frame(wet=wet, pair=pair)
d = d[d$wet<2,]

m0 = glmer(d$wet ~ 1 + (1|d$pair), data=d)

post.m0 = sample.naive.posterior(m0)


par(mfrow=c(1,2))

mean(post.m0[,1])
HPDI(post.m0[,1])

hist(post.m0[,1])
abline(v=mean(post.m0[,1]), col='green')
abline(v=HPDI(post.m0[,1]), col='red')

mean(plogis(post.m0[,1]))
plogis((mean(post.m0[,1])))
HPDI(plogis(post.m0[,1]))
plogis(HPDI(post.m0[,1]))

hist(plogis(post.m0[,1]))
abline(v=mean(plogis(post.m0[,1])), col='green')
abline(v=plogis(HPDI(post.m0[,1])), col='red')
abline(v=HPDI(plogis(post.m0[,1])), col='blue')




wetc = as.numeric(by(d$wet, d$pair, sum))
countc = as.numeric(by(d$wet, d$pair, length))

dryc = countc - wetc
pairc = unique(d$pair)

dc = data.frame(wetc=wetc, dryc=dryc, pairc=pairc)

m0c = glmer(cbind(wetc, dryc) ~ 1 + (1|pairc))
post.m0c = sample.naive.posterior(m0c)
mean(post.m0c[,1])
HPDI(post.m0c[,1])

mean(logistic(post.m0c[,1]))
logistic(mean(post.m0c[,1]))
HPDI(plogis(post.m0c[,1]))


sum(wetc)/sum(countc)

mean(wetc/countc)
