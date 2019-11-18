context("test-fdirParsing")

context("readLssFile: normal run")
data <- readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxData"))
expect_true("Bruttovekt" %in% names(data))
expect_true("Redskap" %in% names(data))
expect_true("Snurpenot/ringnot" %in% data$Redskap)
expect_true(all(!is.na(data$`Art - FDIR`)))
expect_equal(nrow(data),9)
