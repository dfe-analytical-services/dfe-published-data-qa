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

  list(
    results = out$results_table, # columns: result, message, stage, check, guidance_url
    progress_message = out$overall_stage,
    passed = out$passed,
    api_suitable = out$api_suitable
  )
}
