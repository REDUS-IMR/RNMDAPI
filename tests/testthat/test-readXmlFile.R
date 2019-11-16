context("test-readXmlFile")

example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")
defaultParse <- readXmlFile(example)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParse)))
expect_equal(nrow(defaultParse$fishstation), 2)