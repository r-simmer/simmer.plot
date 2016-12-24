library(simmer)

t0 <- trajectory() %>%
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  branch(function() 1, c(T, T),
         trajectory() %>%
           seize("doctor", function() 1) %>%
           timeout(function() rnorm(1, 20)) %>%
           release("doctor", function() 1) %>%
           branch(function() 1, TRUE,
                  trajectory() %>%
                    seize("administration", 1) %>%
                    timeout(1) %>%
                    release("administration", 1)),
         trajectory() %>%
           rollback(1) %>%
           rollback(1, check = function() FALSE) %>%
           set_attribute("dummy", 1) %>%
           set_attribute("dummy", function() 1)) %>%
  timeout(1)

########################################################

library(visNetwork)

t0$verbose <- TRUE
out <- utils::capture.output(t0)
out <- out[grep("0x", out)]

out <- sub(".*Activity: ", "", out)
nodes <- as.data.frame(sub(" .*", "", out), stringsAsFactors=FALSE)
colnames(nodes) <- "label"
nodes$shape <- "box"

out <- sub("[[:alpha:]]*[[:space:]]*\\| ", "", out)
b_edges <- sub(" ->.*", "", out)
b_edges <- t(as.data.frame(strsplit(b_edges, " <- ")))
b_edges <- as.data.frame(b_edges, stringsAsFactors=FALSE)
b_edges <- tail(b_edges, -1)
rownames(b_edges) <- NULL
colnames(b_edges) <- c("from", "to")

out <- sub(".*<- ", "", out)
f_edges <- sub(" \\|.*", "", out)
f_edges <- t(as.data.frame(strsplit(f_edges, " -> ")))
f_edges <- as.data.frame(f_edges, stringsAsFactors=FALSE)
rownames(f_edges) <- NULL
colnames(f_edges) <- c("from", "to")
nodes$id <- f_edges$from
f_edges <- head(f_edges, -1)

forks <- as.data.frame(f_edges != b_edges)
add <- which(forks$to)
dashes <- which(forks$to & !forks$from)

edges <- rbind(f_edges, b_edges[add,])
edges$arrows <- "to"
edges$dashes <- FALSE
edges[dashes,]$dashes <- TRUE

visNetwork(nodes, edges)
