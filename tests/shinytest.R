library(shinytest)
expect_pass(testApp("../", compareImages = grepl("^Windows", utils::osVersion)))
