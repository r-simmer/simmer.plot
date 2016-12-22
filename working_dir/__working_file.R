library(simmer)

t0 <- trajectory() %>%
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  branch(function() 1, TRUE,
         trajectory() %>%
           seize("doctor", function() 1) %>%
           timeout(function() rnorm(1, 20)) %>%
           release("doctor", function() 1) %>%
           branch(function() 1, TRUE,
                  trajectory() %>%
                    seize("administration", 1) %>%
                    timeout(1) %>%
                    release("administration", 1))) %>%
  rollback(1) %>%
  rollback(1, check = function() FALSE) %>%
  set_attribute("dummy", 1) %>%
  set_attribute("dummy", function() 1)

########################################################

library(ggnetwork)
library(dplyr)

t0$verbose <- TRUE
out <- utils::capture.output(t0)
out <- out[grep("0x", out)]
out <- sub(".*Activity: ", "", out)
activities <- as.data.frame(sub(" .*", "", out))
colnames(activities) <- "name"
out <- sub(".*<- ", "", out)
pointers <- sub(" \\|.*", "", out)
pointers <- t(as.data.frame(strsplit(pointers, " -> ")))
rownames(pointers) <- NULL
colnames(pointers) <- c("id", "to")
dt <- cbind(activities, pointers)
dt$id <- as.character(dt$id)
dt$to <- as.character(dt$to)
dt$to[[grep("0 .*", dt$to)]] <- NA
dt$xend <- dt$x <- runif(nrow(dt))
dt$yend <- dt$y <- nrow(dt):1
connections <- do.call(rbind, lapply(which(!is.na(dt$to)), function(i) {
  row <- dt[i,]
  to <- dt[dt$id==row$to,]
  row$xend <- if_else(row$xend > to$x, to$x + 0.04, to$x - 0.04)
  row$yend <- to$y + 0.4
  row
}))

ggplot(rbind(dt, connections), aes(x=x, y=y, xend=xend, yend=yend, label=name)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  #geom_nodes() +
  geom_nodelabel() +
  theme_blank()
