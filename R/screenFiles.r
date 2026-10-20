# screenFiles ------------------------------------------------------------
# Thin wrapper around eesyscreener::screen_csv() that normalises the return
# shape for the app.

screenFiles <- function(datapath, metapath, datafilename, metafilename) {
  out <- eesyscreener::screen_csv(
    datapath = datapath,
    metapath = metapath,
    datafilename = datafilename,
    metafilename = metafilename
  )

  stopifnot(
    "eesyscreener::screen_csv()$results_table is missing expected columns" =
      all(screener_result_cols %in% names(out$results_table))
  )

  list(
    results = out$results_table,
    progress_message = out$overall_stage,
    passed = out$passed
  )
}
