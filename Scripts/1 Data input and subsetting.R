###########################
#____            _ _      
#| __ )  ___   __| (_) ___ 
#|  _ \ / _ \ / _` | |/ _ \
#| |_) | (_) | (_| | |  __/
#|____/ \___/ \__,_|_|\___|
#
############################

# Data input and subsetting

# Get environmental metadata

env <- read.table('../Data/env_with_metadata.csv', header=TRUE, sep=',', row.names=1)

# Get OTU table

otu.tmp <- read.table('../Data/otu_with_tax_1600.csv', header=TRUE, sep=',', row.names=1)
otu <- t(otu.tmp[,-76]/1600) # transpose and divide by 1600 to get relative abundances

taxonomy <- otu.tmp[,76] # Make a list of OTUs with the respective taxonomy
taxonomy <- as.data.frame(taxonomy)
row.names(taxonomy) <- row.names(otu.tmp)

row.names(otu) == row.names(env)

# Subset to questions

env1 <- env[env$Q1 == 1,]
env2 <- env[env$Q2 == 1,]
env3 <- env[env$Q3 == 1,]
env4 <- env[env$Q4 == 1,]
env5 <- env[env$Q5 == 1,]
env6 <- env[env$Q6 == 1,]
env7 <- env[env$Q7 == 1,]
env8 <- env[env$Q8 == 1,]

otu1 <- otu[env$Q1 == 1,]
otu2 <- otu[env$Q2 == 1,]
otu3 <- otu[env$Q3 == 1,]
otu4 <- otu[env$Q4 == 1,]
otu5 <- otu[env$Q5 == 1,]
otu6 <- otu[env$Q6 == 1,]
otu7 <- otu[env$Q7 == 1,]
otu8 <- otu[env$Q8 == 1,]
