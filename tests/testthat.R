library(TestPackage)
library(testthat)

#test map
expect_that(as.numeric(fars_summarize_years(2014)[1,2]),equals(2168))
