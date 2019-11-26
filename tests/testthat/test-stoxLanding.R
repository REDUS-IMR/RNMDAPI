context("test-stoxLanding test resource file gear")
gear <- loadResource("gear")
expect_false(any(is.na(gear$gearDescription)))

context("test-stoxLanding test resource file coastal")
coastal <- loadResource("coastal")
expect_false(any(is.na(coastal$coastalDescription)))

context("test-stoxLanding test resource file n62")
n62 <- loadResource("n62")
expect_false(any(is.na(n62$n62Description)))

context("test-stoxLanding test resource file nusage")
usage <- loadResource("usage")
expect_false(any(is.na(usage$usageDescription)))

context("test-stoxLanding")
landingXML <- readXmlFile(system.file("testresources", "landing.xml", package="RstoxData"), stream = T)
flatSL <- extractAggregateLandings(landingXML)
expected_colums <- c("speciesFAOCommercial",
                     "speciesCategoryCommercial",
                     "commonNameCommercial",
                     "year",
                     "catchDate",
                     "gear",
                     "gearDescription",
                     "area",
                     "icesAreaGroup",
                     "coastal",
                     "coastalDescription",
                     "n62Code",
                     "n62Description",
                     "vesselLength",
                     "landingSite",
                     "weight"
                     )
expect_equivalent(expected_colums, names(flatSL))