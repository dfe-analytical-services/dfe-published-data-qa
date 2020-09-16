# Script to run both testthat and shinytest tests from command line

# paste0("Starting at: ", getwd())
# original_dir <- getwd()
# setwd("c:/R_projects/dfe-published-data-qa") # Need a way to set this as relative to this script somehow?
# paste0("Moved to: ", getwd())
# 
# Sys.unsetenv("http_proxy")
# Sys.unsetenv("https_proxy")
# 
# source("global.R")
# message("================================================================================")
# message("== testthat ====================================================================")
# message("")
# testthat::test_dir("tests/testthat")
# message("")
# message("================================================================================")
# message("== shinytest ===================================================================")
# message("")
# shinytest::testApp(interactive = FALSE)
# message("")
# message("================================================================================")
# 
# setwd(original_dir)
# paste0("Ending at: ", getwd())



## For the shell script

# Rscript -e "originalDir<-getwd();paste0('Starting at: ', getwd());setwd('C:\Agents\RSConnect-Service-01\_work\r99\a\_ees-data-screener-app';paste0('Moved to: '', getwd());source('tests/automatedTests.r);setwd(originalDir);paste0('Ending at: ', getwd())"
# Rscript -e "originalDir<-getwd();paste0('Starting at: ', getwd());setwd('C:\R_projects\dfe-published-data-qa');paste0('Moved to: '', getwd());source('tests/automatedTests.r);setwd(originalDir);paste0('Ending at: ', getwd())"
# C:/Agents/RSConnect-Service-01/_work/r99/a/_ees-data-screener-app/tests/automatedTests.r