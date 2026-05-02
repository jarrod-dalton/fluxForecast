test_that(".build_run_grid returns stable ordering and stream_id sequence", {
  grid <- fluxForecast:::.build_run_grid(n_entities = 2, n_param_sets = 2, S = 2)

  expect_true(is.data.frame(grid))
  expect_equal(names(grid), c("stream_id", "entity_id", "param_draw_id", "sim_id"))

  expect_equal(grid$stream_id, seq_len(8))
  expect_equal(grid$entity_id, c(1,1,1,1, 2,2,2,2))
  expect_equal(grid$param_draw_id, c(1,1,2,2, 1,1,2,2))
  expect_equal(grid$sim_id, c(1,2,1,2, 1,2,1,2))
})
