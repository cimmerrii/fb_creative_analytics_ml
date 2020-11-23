

pacman::p_load(devtools,profvis)

prof_0 <- profvis({
  filtered.results$retargeting.video <- try.master.cvt.run(dimension.start = 99,
                                                           data.update,
                                                           campaign.type = "retargeting",
                                                           analysis.type = "all",
                                                           video.or.all = "video",
                                                           target.account = "",
                                                           date.cutoff,
                                                           dimensions.to.use = "default",
                                                           remove.list = list("spend:format"),
                                                           model,
                                                           model.to.use = "default")
})

prof <- profvis({
  filtered.results$retargeting.video <- try.master.cvt.run(dimension.start = 50,
                                                           data.update,
                                                           campaign.type = "retargeting",
                                                           analysis.type = "all",
                                                           video.or.all = "video",
                                                           target.account = "",
                                                           date.cutoff,
                                                           dimensions.to.use = "default",
                                                           remove.list = list("spend:format"),
                                                           model,
                                                           model.to.use = "default")
})