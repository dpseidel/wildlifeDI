context("Correlation Statistic")

data(deer)
traj1 <- deer[1]
traj2 <- deer[2]
df1 <- ld(traj1)
df2 <- ld(traj2)
sim <- getSim(df1, df2)

test_that("Cr works with ltrajs and dataframes", {
  expect_equal(Cr(traj1, traj2), Cr(sim[[1]], sim[[2]], sim = T))
  }
)
