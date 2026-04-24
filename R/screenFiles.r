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

  # TODO: Laura can making new peas — replace this placeholder with the new
  # per-stage images once they land, and map out$overall_stage to the right file.
  progress_stage <- list(
    src = "www/progress_placeholder.png",
    alt = paste("Stage reached:", out$overall_stage)
  )

  list(
    results = out$results_table, # columns: result, message, stage, check, guidance_url
    progress_stage = progress_stage,
    progress_message = out$overall_stage,
    passed = out$passed,
    api_suitable = out$api_suitable
  )
}
