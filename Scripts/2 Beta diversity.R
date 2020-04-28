### Beta Diversity

source(file = "Functions/Functions.R")

# Q1
adonis(sqrt(otu1) ~ factor(Diet), data = env1, method='euc')
pcoa1 <- pcoa(sqrt(otu1))
custom.plot.pcoa(pcoa1,group = factor(env1$Diet), plottype = "t", title = "Q1")

# Q2
adonis(sqrt(otu2) ~ factor(Diet), data = env2, method='euc')
pcoa2 <- pcoa(sqrt(otu2))
custom.plot.pcoa(pcoa2,group = factor(env2$Diet), plottype = "t", title = "Q2")

# Q3
adonis(sqrt(otu3) ~ factor(Diet), data = env3, method='euc')
pcoa3 <- pcoa(sqrt(otu3))
custom.plot.pcoa(pcoa3,group = factor(env3$Diet), plottype = "t", title = "Q3")

# Q4
adonis(sqrt(otu4) ~ factor(Diet) * factor(Time), data = env4, method='euc')
pcoa4 <- pcoa(sqrt(otu4))
env4$Diet.Time = paste(env4$Diet,env4$Time, sep="." )
custom.plot.pcoa(pcoa4,group = factor(env4$Diet.Time), plottype = "t", title = "Q4")

# Q5
adonis(sqrt(otu5) ~ factor(Diet), data = env5, method='euc')
pcoa5 <- pcoa(sqrt(otu5))
custom.plot.pcoa(pcoa5,group = factor(env5$Diet), plottype = "t", title = "Q5")

# Q6
adonis(sqrt(otu6) ~ factor(Diet), data = env6, method='euc')
pcoa6 <- pcoa(sqrt(otu6))
custom.plot.pcoa(pcoa6,group = factor(env6$Diet), plottype = "t", title = "Q6")

# Q7
adonis(sqrt(otu7) ~ factor(Diet) * factor(Time), data = env7, method='euc')
pcoa7 <- pcoa(sqrt(otu7))
env7$Diet.Time = paste(env7$Diet,env7$Time, sep="." )
custom.plot.pcoa(pcoa7,group = factor(env7$Diet.Time), plottype = "t", title = "7")

# Q8
adonis(sqrt(otu8) ~ factor(Cross.foster), data = env8, method='euc')
pcoa8 <- pcoa(sqrt(otu8))
custom.plot.pcoa(pcoa8,group = factor(env8$Cross.foster), plottype = "t", title = "Q8")
