### Alpha diversity

#library(vegan)
library(sciplot)

# Q1
anova(lm(Chao1 ~ factor(Diet), data=env1))
anova(lm(Sobs ~ factor(Diet), data=env1))
#anova(lm(Simp ~ factor(Diet), data=env1))
anova(lm(Shan ~ factor(Diet), data=env1))
anova(lm(PD ~ factor(Diet), data=env1))

# Q2
anova(lm(Chao1 ~ factor(Diet), data=env2))
anova(lm(Sobs ~ factor(Diet), data=env2))
#anova(lm(Simp ~ factor(Diet), data=env2))
anova(lm(Shan ~ factor(Diet), data=env2))
anova(lm(PD ~ factor(Diet), data=env2))

# Q3
anova(lm(Chao1 ~ factor(Diet), data=env3))
anova(lm(Sobs ~ factor(Diet), data=env3))
#anova(lm(Simp ~ factor(Diet), data=env3))
anova(lm(Shan ~ factor(Diet), data=env3))
anova(lm(PD ~ factor(Diet), data=env3))

# Q4
anova(lm(Chao1 ~ factor(Diet) * factor(Time), data=env4))
anova(lm(Sobs ~ factor(Diet) * factor(Time), data=env4))
#anova(lm(Simp ~ factor(Diet) * factor(Time), data=env4))
anova(lm(Shan ~ factor(Diet) * factor(Time), data=env4))
anova(lm(PD ~ factor(Diet) * factor(Time), data=env4))

# Q5
anova(lm(Chao1 ~ factor(Diet), data=env5))
anova(lm(Sobs ~ factor(Diet), data=env5))
#anova(lm(Simp ~ factor(Diet), data=env5))
anova(lm(Shan ~ factor(Diet), data=env5))
anova(lm(PD ~ factor(Diet), data=env5))

# Q6
anova(lm(Chao1 ~ factor(Diet), data=env6))
anova(lm(Sobs ~ factor(Diet), data=env6))
#anova(lm(Simp ~ factor(Diet), data=env6))
anova(lm(Shan ~ factor(Diet), data=env6))
anova(lm(PD ~ factor(Diet), data=env6))

# Q7
anova(lm(Chao1 ~ factor(Diet) * factor(Time), data=env7))
anova(lm(Sobs ~ factor(Diet) * factor(Time), data=env7))
#anova(lm(Simp ~ factor(Diet) * factor(Time), data=env7))
anova(lm(Shan ~ factor(Diet) * factor(Time), data=env7))
anova(lm(PD ~ factor(Diet) * factor(Time), data=env7))

# Q8
anova(lm(Chao1 ~ factor(Cross.foster), data=env8))
anova(lm(Sobs ~ factor(Cross.foster), data=env8))
#anova(lm(Simp ~ factor(Cross.foster), data=env8))
anova(lm(Shan ~ factor(Cross.foster), data=env8))
anova(lm(PD ~ factor(Cross.foster), data=env8))



#Tukey's Honest Significant Difference Post hoc's

# Q4
TukeyHSD(aov(Chao1 ~ factor(Diet) * factor(Time), data=env4))
TukeyHSD(aov(Sobs ~ factor(Diet) * factor(Time), data=env4))
#anova(lm(Simp ~ factor(Diet) * factor(Time), data=env4))
TukeyHSD(aov(Shan ~ factor(Diet) * factor(Time), data=env4))
TukeyHSD(aov(PD ~ factor(Diet) * factor(Time), data=env4))

# Q7
TukeyHSD(aov(Chao1 ~ factor(Diet) * factor(Time), data=env7))
TukeyHSD(aov(Sobs ~ factor(Diet) * factor(Time), data=env7))
#anova(lm(Simp ~ factor(Diet) * factor(Time), data=env7))
TukeyHSD(aov(Shan ~ factor(Diet) * factor(Time), data=env7))
TukeyHSD(aov(PD ~ factor(Diet) * factor(Time), data=env7))

# Q8
TukeyHSD(aov(Chao1 ~ factor(Cross.foster), data=env8))
TukeyHSD(aov(Sobs ~ factor(Cross.foster), data=env8))
#anova(lm(Simp ~ factor(Cross.foster), data=env8))
TukeyHSD(aov(Shan ~ factor(Cross.foster), data=env8))
TukeyHSD(aov(PD ~ factor(Cross.foster), data=env8))


# Plots


# Q1
bargraph.CI(x.factor = factor(Diet), response = Chao1, data=env1, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, data=env1, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, data=env1, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, data=env1, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, data=env1, legend = TRUE)

# Q2
bargraph.CI(x.factor = factor(Diet), response = Chao1, data=env2, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, data=env2, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, data=env2, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, data=env2, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, data=env2, legend = TRUE)

# Q3
bargraph.CI(x.factor = factor(Diet), response = Chao1, data=env3, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, data=env3, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, data=env3, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, data=env3, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, data=env3, legend = TRUE)

# Q4
bargraph.CI(x.factor = factor(Diet), response = Chao1, group = Time, data=env4, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, group = Time, data=env4, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, group = Time, data=env4, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, group = Time, data=env4, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, group = Time, data=env4, legend = TRUE)

# Q5
bargraph.CI(x.factor = factor(Diet), response = Chao1, data=env5, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, data=env5, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, data=env5, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, data=env5, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, data=env5, legend = TRUE)

# Q6
bargraph.CI(x.factor = factor(Diet), response = Chao1, data=env6, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, data=env6, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, data=env6, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, data=env6, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, data=env6, legend = TRUE)

# Q7
bargraph.CI(x.factor = factor(Diet), response = Chao1, group = Time, data=env7, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Sobs, group = Time, data=env7, legend = TRUE)
#bargraph.CI(x.factor = factor(Diet), response = Simp, group = Time, data=env7, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = Shan, group = Time, data=env7, legend = TRUE)
bargraph.CI(x.factor = factor(Diet), response = PD, group = Time, data=env7, legend = TRUE)

# Q8
bargraph.CI(x.factor = factor(Cross.foster), response = Chao1, data=env8, legend = TRUE)
bargraph.CI(x.factor = factor(Cross.foster), response = Sobs, data=env8, legend = TRUE)
#bargraph.CI(x.factor = factor(Cross.foster), response = Simp, data=env8, legend = TRUE)
bargraph.CI(x.factor = factor(Cross.foster), response = Shan, data=env8, legend = TRUE)
bargraph.CI(x.factor = factor(Cross.foster), response = PD, data=env8, legend = TRUE)
