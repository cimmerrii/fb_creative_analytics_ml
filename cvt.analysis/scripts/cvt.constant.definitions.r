min.value<-0.01
filtered.results<-list()
sample.size<-10000
dimensions<-100
var.one <-("1")

display.min.range <- -1
display.max.range <- 105
label.wrap.width <- 13


get.noise.cvt<-function(study){
  study.noise.vars<-dplyr::select(study,
                                  platform,
                                  account,
                                  promo.account,
                                  campaign.type.account,
                                  campaign.account,
                                  vert.account,
                                  landing.page.name.account,
                                  interest.account,
                                  behavior.account,
                                  demographic.account,
                                  lookalike.size.account,
                                  lookalike.type.account,
                                  lookalike.seed.account,
                                  year.month.account,
                                  year.week.account,
                                  
                                  #utm.spend.bucket,
                                  week.day.account
                                  )
  #study.noise.vars<-cleanup.factors(study.noise.vars)
  study.noise.vars
}


get.universal.cvt<-function(study){
  study.universal.cvt<-dplyr::select(study,
                                     format,
                                     call.to.action,
                                     ad.style,
                                     approach,
                                     ad.content,
                                     camera.angle,
                                     image.overlay,
                                     num.active.ads.by.adset,
                                     #text.overlay,
                                     #has.logo,
                                     seasonal.or.holiday.content,
                                     #sale.or.pricing.in.image,
                                     has.person.or.part.of.person)
  #study.universal.cvt<-cleanup.factors(study.universal.cvt)
  study.universal.cvt
}


get.video.cvt<-function(study){
  study.video.vars<-dplyr::select(study,
                                  video.duration,
                                  video.type,
                                  video.shape,
                                  video.style)
  study.video.vars<-cleanup.factors(study.video.vars)
  study.video.vars
}

var.spend<-clean.strings("spend
                         +spendsquare
                         +spendroot")

var.account<-clean.strings("spend:account
                           +spendsquare:account
                           +spendroot:account")





video.vars<-clean.strings("spend:video.duration
                          +spend:video.shape
                          +spend:video.type")



delivery.vars<-clean.strings("spend
                             +spendsquare:account
                             +spendroot:account
                             +spend:account")

delivery.vars.no.account<-clean.strings("spend")

custom.ink.vars<-clean.strings("spend:custom.ink.t.shirt
                              +spend:custom.ink.multi.product
                              +spend:custom.ink.master.process
                              +spend:custom.ink.name.brand
                              +spend:custom.ink.segment
                              +spend:custom.ink.people
                              +spend:custom.ink.graphic
                              +spend:custom.ink.design
                              +spend:custom.ink.inky")

ebates.activation.vars<-clean.strings("")
ebates.frequency.vars<-clean.strings("")

ebates.vars<-clean.strings("spend:ebates.cashback.size
                           +spend:ebates.cycle
                           +spend:ebates.discount.type
                           +spend:ebates.dollars.off
                           +spend:ebates.product.type
                           +spend:ebates.template
                           +spend:ebates.vendor
                           +spend:ebates.vendor.product.width
                           ")

leesa.vars<-clean.strings("spend:leesa.gender
                          +spend:leesa.has.animal
                          +spend:leesa.has.box
                          +spend:leesa.has.mattress
                          +spend:num.prods
                          +spend:number.people
                          +spend:leesa.product.type
                          ")


bitsbox.vars <- clean.strings("spend:leesa.gender")
keto.vars <- clean.strings("spend:leesa.gender")

purple.carrot.vars<- clean.strings("spend:purple.carrot.food.type
                                   +spend:purple.carrot.ingredients.shown
                                   +spend:purple.carrot.overlay.text.details
                                   +spend:purple.carrot.recipe.name.included
                                   +spend:purple.carrot.recipe.steps.shown")


#add if getting a good results from penrose/has.people
#+spend:penrose.hill.people.gender
#+spend:penrose.hill.people.age
#+spend:penrose.hill.num.people


penrose.hill.vars<-clean.strings("spend:penrose.hill.has.animal
                                 +spend:penrose.hill.has.food.general
                                 +spend:penrose.hill.has.wine.general
                                 +spend:penrose.hill.shows.drinkware
                                 +spend:penrose.hill.shows.awards
                                 +spend:penrose.hill.setting
                                 ")

#for penrose hill detail remove spend:has.person.or.part.of.person
penrose.hill.detail.vars<-clean.strings("spend:penrose.hill.has.animal
                                 +spend:penrose.hill.has.food.detail
                                 +spend:penrose.hill.has.wine.detail
                                 +spend:penrose.hill.shows.drinkware
                                 +spend:penrose.hill.shows.awards
                                 +spend:penrose.hill.setting
                                 +spend:penrose.hill.num.people
                                 +spend:penrose.hill.people.age
                                 +spend:penrose.hill.people.gender
                                 ")

framebridge.vars<-clean.strings("spend:framebridge.product.type
                                +spend:framebridge.product.name
                                +spend:framebridge.campaign.type
                                +spend:framebridge.video.length
                                +spend:framebridge.motion.type
                                +spend:framebridge.audio.sound
                                +spend:framebridge.captions
                                +spend:framebridge.model.human.element
                                +spend:framebridge.asset.environment
                                +spend:framebridge.environment.key
                                +spend:framebridge.asset.background.gradient
                                ")

framebridge.prospecting.video.vars<-clean.strings("spend:framebridge.has.logo
                                +spend:framebridge.environment.key
                                +spend:framebridge.asset.environment
                                +spend:framebridge.asset.background.gradient
                                ")



discarded.framebridge.vars<-"+ spend:framebridge.has.logo"

dimension.cutoff<-95
p.value.cutoff<-0.9
kill.category<-c("spend","spendroot","spendsquare","purchase","null","account","emojis.in.pic.or.vid")
kill.value<-c("none",
              "not_video",
              "video_unknown_type",
              "video unknown type",
              "seemenu",
              "ad_type_video_style_unknown",
              "book_travel",
              "see_menu",
              "book_now",
              "neither/other",
              "get_offer_view",
              "video_unknown_shape",
              "video_unknown_length",
              "other",
              "apply_now",
              "install_app",
              "sign_up",
              "sign.up")

final.kill.value<-c("Fred")
final.kill.category<-c("Emojis in Pic or Vid")




#custom.ink.prospecintg - list("spend:call.to.action","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:has.person.or.part.of.person")
#custom.ink.prospecting.video list("spend:custom.ink.name.brand","spend:format","spend:call.to.action","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:has.person.or.part.of.person")
#custom.ink.retention list("spend:custom.ink.people","spend:call.to.action","spend:ad.style","spend:approach","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image")
#custom.ink.retention.video  list("spend:format","spend:custom.ink.people","spend:call.to.action","spend:ad.style","spend:approach","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image")
#custom.ink.retarg- list("spend:has.logo","spend:has.person.or.part.of.person","spend:call.to.action","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:ad.style","spend:emojis.in.pic.or.vid","spend:approach")
#custom.ink.retarg.video list("spend:custom.ink.name.brand","spend:format","spend:has.logo","spend:has.person.or.part.of.person","spend:call.to.action","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:ad.style","spend:emojis.in.pic.or.vid","spend:approach")
#framebridge.retarg- list("spend:has.logo","spend:approach","spend:framebridge.image.type","spend:framebridge.audio.sound")
#framebridge.retarg.video -list("spend:has.logo","spend:approach","spend:framebridge.image.type","spend:framebridge.audio.sound","spend:format")
#framebridge.prospecting list("spend:has.logo","spend:approach")
#fraemebridge.prospecitng video (buggy)  list("spend:has.logo","spend:approach","spend:framebridge.image.type","spend:framebridge.audio.sound","spend:format")
#ebates activation <-list("spend:has.logo")
#ebates activation.video list("spend:has.logo","spend:format")
#universal prospecting- <-list("bob")
#universal prospecting.video- <-list("spend:format")
#universal retargeting- <-list("bob")
#universal retargeting.video- <-list("spend:format")
#penrose prospecting <-list("spend:has.logo")
#penrose prospecting.video <-list("spend:has.logo","spend:format")
#penrose retargeting.video <-list("spend:has.logo","spend:format","spend:call.to.action","spend:text.overlay","spend:ad.style","spend:approach","spend:ad.content","spend:camera.angle","spend:emojis.in.pic.or.vid","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:penrose.hill.has.animal","spend:penrose.hill.has.wine.detail","spend:penrose.hill.shows.drinkware","spend:penrose.hill.shows.awards","spend:penrose.hill.setting")
#purple carrot prospecting- <-list("bob")
#purple carrot prospecting.video <-list("spend:format")
#purple carrot retargeting video <-list("spend:format","spend:call.to.action","spend:emojis.in.pic.or.vid","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:purple.carrot.recipe.name.included")
