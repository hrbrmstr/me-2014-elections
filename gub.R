library(rgdal)
library(ggplot2)
library(jsonlite)
library(gridExtra)

me <- readOGR("towns", "towns")

me_map <- fortify(me)
me_map <- merge(me_map, data.frame(id=rownames(me@data), TOWN=me@data$TOWN, COUNTY=me@data$COUNTY), all.x=TRUE)

f20646 <- fromJSON("http://election2014.pressherald.com/json2/20646.json")
towns <- data.frame(TOWN=f20646$race$town$name, color="red")

names(f20646[[1]]$candidates) <- f20646[[1]]$town$name
mrg <- do.call(rbind, lapply(f20646[[1]]$candidates, function(x) {
  data.frame(margin=x[x$party=="R",]$percentage - x[x$party=="D",]$percentage,
             indep=x[x$party=="I",]$percentage,
             repub=ifelse((x[x$party=="R",]$percentage-x[x$party=="D",]$percentage)>0, "#b2182b", "#2166ac"))
}))
mrg$TOWN <- rownames(mrg)
rownames(mrg) <- NULL

# Some errant towns/voting townships...i corrected most of them
# [1] "Oxford Cty Townships"     "Penobscot Cty Townships"  "Penobscot Nation Vot Dst" "Owl's Head"               "Old Orchrd Bch"
# [6] "Piscataquis Cty Townshps" "Pleasant Point Votng Dst" "Monhegan Plt."            "Washington Cty Townships" "Verona"
# [11] "Westport"                 "Somerset Cty Townships"   "Aroostook Cty Townships"  "Indian Township Vtng Dst" "Isle Au Haut"
# [16] "Hancock Cty Townships"    "LaGrange"                 "Grand Lake Stream Plt"    "Franklin Cty Townships"   "Maine"

mrg[mrg$TOWN == "Owl's Head",]$TOWN <- "Owls Head"
mrg[mrg$TOWN == "Old Orchrd Bch",]$TOWN <- "Old Orchard Bch"
mrg[mrg$TOWN == "Verona",]$TOWN <- "Verona Island"
mrg[mrg$TOWN == "Monhegan Plt.",]$TOWN <- "Monhegan Island Plt."
mrg[mrg$TOWN == "Westport",]$TOWN <- "Westport Island"
mrg[mrg$TOWN == "Isle Au Haut",]$TOWN <- "Isle au Haut"
mrg[mrg$TOWN == "LaGrange",]$TOWN <- "Lagrange"
mrg[mrg$TOWN == "Grand Lake Stream Plt",]$TOWN <- "Grand Lake Stream Plt."
mrg[mrg$TOWN == "Indian Township Vtng Dst",]$TOWN <- "Indian Twp Res"
mrg[mrg$TOWN == "Hancock Cty Townships",]$TOWN <- "Hancock County Island"
mrg[mrg$TOWN == "Pleasant Point Votng Dst",]$TOWN <- "Pleasant Ridge Plt."

met <- merge(me_map, mrg, all.x=TRUE)

met$repub <- as.character(met$repub)
met$repub[is.na(met$repub)] <- "white"

gg <- ggplot()
gg <- gg + geom_map(data=met, map=me_map,
                    aes(map_id=id, x=long, y=lat, group=group, fill=repub),
                    color="white", size=0.15)
gg <- gg + coord_map()
gg <- gg + labs(x=NULL, y=NULL, title="Red vs Blue")
gg <- gg + scale_fill_identity(na.value="white")
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(plot.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(panel.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(legend.position="none")
gg_rvb <- gg

gg <- ggplot()
gg <- gg + geom_map(data=met, map=me_map,
                    aes(map_id=id, x=long, y=lat, group=group, fill=margin),
                    color="white", size=0.15)
gg <- gg + coord_map()
gg <- gg + labs(x=NULL, y=NULL, title="Fill by vote margin")
gg <- gg + scale_fill_gradient2(low="#2166ac", mid="#d8daeb", high="#b2182b",
                                na.value="white", limits=c(-30, 30), name="Margin %",
                                labels=c("<-30", "-20", "-10", "0", "10", "20", "30+"))
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(plot.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(panel.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(legend.position=c(0.65, 0.1),
                 legend.direction="horizontal")

gg_margin <- gg

gg <- ggplot()
gg <- gg + geom_map(data=met, map=me_map,
                    aes(map_id=id, x=long, y=lat, group=group, fill=indep),
                    color="white", size=0.15)
gg <- gg + coord_map()
gg <- gg + labs(x=NULL, y=NULL, title="Who voted independent?")
gg <- gg + scale_fill_gradient2(name="Independent %", low="#f7fcfd", high="#006d2c", na.value="white")
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(plot.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(panel.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(legend.position=c(0.65, 0.1),
                 legend.direction="horizontal")
gg_ind <- gg


svg(file="gub.svg", width=12, height=6, bg="gray")
grid.arrange(gg_rvb, gg_margin, gg_ind, ncol=3, main="2014 Maine Gubernatorial Race")
dev.off()
