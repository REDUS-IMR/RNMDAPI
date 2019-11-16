context("test-readXmlFile")

example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")
defaultParse <- readXmlFile(example)
expect_true(c("mission", "fishstation", "catchsample", "individual", "age") %in% names(defaultParse))
expect_equal(nrow(defaultParse$fishstation), 2)