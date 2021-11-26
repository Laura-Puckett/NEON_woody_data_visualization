#-------------------------------------------------------
# Description
#-------------------------------------------------------

# Author Laura Puckett
# Date 11/26/2021
# Purpose: Download NEON woody vegetation dataset and save locally. 

#-------------------------------------------------------
# Body
#-------------------------------------------------------

library(neonUtilities); 

# instructions for getting a NEON API token can be found here: https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-tokens-tutorial

# load my neon token as an object called NEON_TOKEN
source('./neon_token.R') 

veglist = loadByProduct(dpID="DP1.10098.001",
                        site = "all",
                        package = "basic",
                        check.size = F,
                        token = NEON_TOKEN)
saveRDS(veglist, './veglist.Rdata')
