app <- ShinyDriver$new("../../")
app$snapshotInit("doesItLoad")

Sys.sleep(1)

app$snapshot()
