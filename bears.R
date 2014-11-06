library(rgdal)
library(ggplot2)
library(jsonlite)
library(gridExtra)

me <- readOGR("/Users/bob/Desktop/me/towns", "towns")

me_map <- fortify(me)
me_map <- merge(me_map, data.frame(id=rownames(me@data), TOWN=me@data$TOWN, COUNTY=me@data$COUNTY), all.x=TRUE)

f21269 <- fromJSON("http://election2014.pressherald.com/json2/21269.json")
towns <- data.frame(TOWN=f21269$race$town$name, color="red")

names(f21269[[1]]$candidates) <- f21269[[1]]$town$name
mrg <- do.call(rbind, lapply(f21269[[1]]$candidates, function(x) {
  data.frame(yes=x[x$lastname=="Yes",]$votes,
             no=x[x$lastname=="No",]$votes,
             margin=x[x$lastname=="Yes",]$percentage -
                    x[x$lastname=="No",]$percentage,
             yes_pct=x[x$lastname=="Yes",]$percentage,
             no_pct=x[x$lastname=="No",]$percentage)
}))
mrg$TOWN <- rownames(mrg)
rownames(mrg) <- NULL

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

gg <- ggplot()
gg <- gg + geom_map(data=met, map=me_map,
                    aes(map_id=id, x=long, y=lat, group=group, fill=margin),
                    color="white", size=0.15)
gg <- gg + coord_map()
gg <- gg + labs(x=NULL, y=NULL, title="Fill by vote margin (Bear traps)")
gg <- gg + scale_fill_gradient2(low="#762a83", mid="white", high="#1b7837",
                                na.value="white", limits=c(-100, 100), name="Margin %")
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(plot.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(panel.background=element_rect(fill="gray", color="gray"))
gg <- gg + theme(legend.position=c(0.65, 0.1),
                 legend.direction="horizontal",
                 legend.text=element_text(size=8))
gg
gg_margin <- gg


mrg$TOWN <- reorder(mrg$TOWN, mrg$margin, order=TRUE)

g2 <- ggplot(data=mrg, aes(x=TOWN, y=margin))
# g2 <- g2 + geom_bar(stat="identity")
g2 <- g2 + geom_segment(aes(xend=TOWN), yend=0, size=0.25)
g2 <- g2 + labs(x=NULL, y=NULL, title="Vote margin by town (Bear traps)")
g2 <- g2 + theme_bw()
g2 <- g2 + theme(axis.text.x=element_blank())
g2 <- g2 + theme(axis.ticks.x=element_blank())
g2 <- g2 + theme(panel.grid=element_blank())
g2 <- g2 + theme(panel.border=element_blank())
g2 <- g2 + theme(panel.background=element_rect(fill="#636363"))
g2

g3 <- ggplot(mrg, aes(x=1, y=margin)) +
  geom_violin(fill="#762a83") +
  labs(x=NULL, y=NULL, title="Vote margin violin plot (Bear traps)") +
  theme_bw()
g3

gg

grid.arrange(gg_margin, g3, ncol=2)

