context("Pull Simultaneous Fixes")
library(purrr)

data(deer)
traj1 <- deer[1]
traj2 <- deer[2]
df1 <- ld(traj1)
df2 <- ld(traj2)

test_that("getSim and GetSimultaneous return matching output", {
  sim <- getSim(df1, df2, tc = 7.5*60)
  Sim <- GetSimultaneous(traj1, traj2, tc = 7.5*60)
  
  expect_equal(map_dbl(sim, nrow), map_dbl(Sim, nrow))
  expect_equal(sim, Sim)
  
  expect_equal(sim[[1]]$date, Sim[[1]]$date)
  expect_equal(sim[[2]]$date, Sim[[2]]$date)
  
  expect_equal(sim[[1]]$dist, Sim[[1]]$dist)
  expect_equal(sim[[2]]$dist, Sim[[2]]$dist)
  
  expect_equal(sim[[1]]$rel.angle, Sim[[1]]$rel.angle)
  expect_equal(sim[[2]]$rel.angle, Sim[[2]]$rel.angle)
  
  expect_equal(sim[[1]]$abs.angle, Sim[[1]]$abs.angle)
  expect_equal(sim[[2]]$abs.angle, Sim[[2]]$abs.angle)
}
)
