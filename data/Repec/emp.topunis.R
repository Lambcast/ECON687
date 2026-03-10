setwd(dir_repec_main)

# load data
topunis <- read.csv("top100unis.csv", header=TRUE)
p <- nrow(topunis)

# compute confidence sets
topunis <- topunis[order(topunis$n, decreasing=TRUE),][1:100,]
topunis$IFrank <- xrank(topunis$IFmean)

CS_marg <- csranks(topunis$IFmean, topunis$IFse, coverage=0.95, stepdown=TRUE, simul=FALSE, R=1000, seed=101)
topunis$IFmargL <- CS_marg$L
topunis$IFmargU <- CS_marg$U

CS_simul <- csranks(topunis$IFmean, topunis$IFse, coverage=0.95, stepdown=TRUE, simul=TRUE, R=1000, seed=101)
topunis$IFsimulL <- CS_simul$L
topunis$IFsimulU <- CS_simul$U

# plot rankings and marginal confidence sets for IF
plotIFall <- ggplot(topunis, aes(x=reorder(name,IFrank),y=IFrank)) + geom_point() + geom_errorbar(aes(ymin=IFmargL,ymax=IFmargU)) + labs(title = "Ranking universities by impact factor", subtitle = "(with 95% marginal confidence sets)") + xlab(NULL) + ylab(NULL) + theme_bw() + coord_flip() + scale_y_continuous(name = "rank", limits = c(1, p), breaks = unique(c(1, seq(5, 100, by = 5))), labels = unique(c(1, seq(5, 100, by = 5))))
ggplot2::ggsave("topunisIFrankingall.pdf", plot=plotIFall, width=8, height=12)

# plot rankings and simultaneous confidence sets for IF
plotIFall <- ggplot(topunis, aes(x=reorder(name,IFrank),y=IFrank)) + geom_point() + geom_errorbar(aes(ymin=IFsimulL,ymax=IFsimulU)) + labs(title = "Ranking universities by impact factor", subtitle = "(with 95% simultaneous confidence sets)") + xlab(NULL) + ylab(NULL) + theme_bw() + coord_flip() + scale_y_continuous(name = "rank", limits = c(1, p), breaks = unique(c(1, seq(5, 100, by = 5))), labels = unique(c(1, seq(5, 100, by = 5))))
ggplot2::ggsave("topunisIFrankingsimulall.pdf", plot=plotIFall, width=8, height=12)

plotIF50.1 <- ggplot(topunis, aes(x=reorder(name,IFrank),y=IFmean)) + geom_point() + geom_errorbar(aes(ymin=IFmean-2*IFse,ymax=IFmean+2*IFse)) + labs(title = "Impact Factor", subtitle = "(with +/- 2*SE)") + xlab("") + ylab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot2::ggsave("topunisIFestim.pdf", plot=plotIF50.1, width=12, height=8)




