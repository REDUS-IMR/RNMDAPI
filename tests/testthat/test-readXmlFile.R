
context("test-readXmlFile: DOM biotic")
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")
defaultParse <- readXmlFile(example, stream = F)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParse)))
expect_equal(nrow(defaultParse$fishstation), 2)


context("test-readXmlFile: stream parse biotic")
streamParse <- readXmlFile(example, stream = T)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(streamParse)))
expect_equal(nrow(defaultParse$fishstation), 2)

context("test-readXmlFile: stream parse landing")
example <- system.file("testresources","landing.xml", package="RstoxData")
streamParse <- readXmlFile(example, stream = T)
expect_true(all(c("Art", "Dellanding", "Fangstdata", "Landingsdata", "Seddellinje") %in% names(streamParse)))
expect_false(any(is.na(streamParse$Produkt$Rundvekt)))
expect_false(all(is.na(streamParse$Produkt$Registreringsmerke_seddel)))

