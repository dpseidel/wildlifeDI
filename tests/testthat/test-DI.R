context("DI Statistic")

data(deer)
traj1 <- deer[1]
traj2 <- deer[2]
df1 <- ld(traj1)
df2 <- ld(traj2)
sim <- getSim(df1, df2, tc = 0)

test_that("DI works with ltrajs and dataframes", {
  expect_equal(DI(traj1, traj2), DI(sim[1], sim[2], sim = T))
  
  local_test <- function(t1, t2, sim = F){
    set.seed(111)
    DI(t1, t2, sim = sim, local = T)
    }
  
  expect_equal(local_test(traj1, traj2), local_test(sim[1], sim[2], sim = T))
})
