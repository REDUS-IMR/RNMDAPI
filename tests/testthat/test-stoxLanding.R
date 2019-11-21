context("test-stoxLanding test resource file gear")
loc <- readr::locale()
loc$encoding <- "latin1"
gear <- readr::read_delim(system.file("extdata","codeDescriptions", "gear.csv", package="RstoxData"), delim = "\t", locale = loc)
expect_false(any(is.na(gear$gearDescription)))

context("test-stoxLanding test resource file coastal")
coastal <- readr::read_delim(system.file("extdata","codeDescriptions", "coastal.csv", package="RstoxData"), delim = "\t", locale = loc)
expect_false(any(is.na(coastal$coastalDescription)))

context("test-stoxLanding test resource file n62")
n62 <- readr::read_delim(system.file("extdata","codeDescriptions", "n62.csv", package="RstoxData"), delim = "\t", locale = loc)
expect_false(any(is.na(n62$n62English)))

context("test-stoxLanding test resource file n62")
usage <- readr::read_delim(system.file("extdata","codeDescriptions", "usage.csv", package="RstoxData"), delim = "\t", locale = loc)
expect_false(any(is.na(usage$usageDescription)))

#context("test-stoxLanding")
