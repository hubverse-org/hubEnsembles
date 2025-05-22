# Arg `derived_tasks` is deprecated

    Code
      expect_warning(linear_pool(sample_outputs, weights = NULL, task_id_cols = c(
        "target_date", "target", "horizon", "location"), compound_taskid_set = c(
        "target", "location", "target_date"), derived_tasks = "reference_date"),
      "The `derived_tasks` argument of `linear_pool()` is deprecated as of hubEnsembles 1.0.0.",
      fixed = TRUE)

