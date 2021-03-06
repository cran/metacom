context("BoundaryClump")

test_that("Metacommunity and BoundaryClump agree", {
	set.seed(1000);
	test <- matrix(rbinom(100,1,0.5), ncol=10)
	mc.test <- Metacommunity(test, sims=10, method='swap')
	bc.test <- BoundaryClump(test)
	expect_equal(mc.test$Boundary$stat[1], 
    bc.test$stat[1])
})


test_that("BoundaryClump same as Leibold and Mikkelson 2002", {
  data(TestMatrices)
  bc.test2 <- BoundaryClump(t(TestMatrices[[5]]))
	expect_equal(bc.test2$stat[1], 
    1.684210526)
})


