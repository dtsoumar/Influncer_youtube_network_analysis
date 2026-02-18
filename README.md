## YouTube Influencer Network
This project analyzes the YouTube influencer network as a complex social network, focusing on its structural properties and the role of influencers and their followers. The dataset represents the network as an undirected graph consisting of 1,134,890 nodes (vertices) and 2,987,623 edges, where nodes correspond to YouTube users and edges represent mutual connections (subscriptions or interactions). The primary objective of the project is to examine the network’s topology, identify influential nodes, and assess the potential for information spread and network vulnerability.

## Key Components of the Analysis

- Construction and visualization of the network (where computationally feasible)
- Study of fundamental topological properties
- Computation of centrality measures:
  - Degree centrality
  - Average nearest neighbor degree
- Assessment of network structure:
  - Network density
  - Transitivity / Clustering coefficient
  - Assortativity
  - Power-law distribution fit
- Analysis of ego-networks and the impact of removing influencers or follower subgraphs on network cohesion

The analysis was conducted primarily in R, using the igraph package for network modeling, graph-theoretic computations, and visualization.
## Objectives
- Identify the most influential nodes (influencers) and structurally important users
- Examine the network’s vulnerability and robustness
- Assess how the network structure facilitates rapid information diffusion, especially from influencers
- Provide a quantitative basis to evaluate claims about influencer dominance and follower behavior in large-scale social networks

This project demonstrates the application of complex network theory to social media systems, highlighting how graph analytics can provide insights into structural resilience, information flow, and the role of key nodes in extremely large networks.

## Data Source
The dataset corresponds to the YouTube network used in this study and is publicly available from the Stanford Large Network Dataset Collection (SNAP): http://snap.stanford.edu/data/com-Youtube.html 
