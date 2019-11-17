
context("test-readXmlFile: DOM")
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")
defaultParse <- readXmlFile(example, stream = F)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParse)))
expect_equal(nrow(defaultParse$fishstation), 2)


context("test-readXmlFile: stream parse")
streamParse <- readXmlFile(example, stream = T)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(streamParse)))
expect_equal(nrow(defaultParse$fishstation), 2)
