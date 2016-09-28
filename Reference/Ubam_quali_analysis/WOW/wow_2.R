## Dynamic Network
library(networkDynamic)
par(mfrow = c(1, 1))
ub <- dat_31
node <- ub$V8
max(node)
wownet <- network.initialize(164)

di1 = 1
di2 = 2

38 != 38
node[di1] != node[di2]

for(i in 1:length(node)) {
  if (node[di1] != node [di2]) {
    add.edge(wownet, node[di1], node[di2])
  }
  di1 = di1 + 1
  di2 = di2 + 1
}

plot.network(wownet)

