module BetweennessCentralityResilience

import MetaGraphsNext: MetaGraph, labels, edge_labels
import Graphs: Graph, dijkstra_shortest_paths
import ArchGDAL as AG
import DataFrames: DataFrame
import Random: rand, Xoshiro


struct VertexID
    id::Int64
end

Base.isless(i1::VertexID, i2::VertexID) = i1.id < i2.id
Base.:+(v::VertexID, i) = VertexID(v.id + i)

struct EdgeData
    length::Float64
end

make_graph() = MetaGraph(
    Graph();
    label_type=VertexID,
    vertex_data_type=NTuple{2, Int64},
    edge_data_type=EdgeData,
    weight_function=e -> e.length
)

"Make a gridded subgraph and return a matrix of VertexIDs"
function make_grid!(G, origin, nrow, ncol; perturb=0.01, seed=nrow * ncol)
    vid = maximum(labels(G); init=VertexID(0))

    vids = Matrix{VertexID}(undef, nrow, ncol)

    for r in 1:nrow, c in 1:ncol
        vid += 1
        vids[r, c] = vid
        G[vid] = tuple((origin .+ [r, c] + rand(2) .* 2 .* perturb .- perturb)...)
    end

    for r in 1:nrow, c in 1:ncol
        if r < nrow
            G[vids[r, c], vids[r + 1, c]] = EdgeData(1)
        end

        if c < ncol
            G[vids[r, c], vids[r, c + 1]] = EdgeData(1)
        end
    end

    return vids
end

function graph_to_gdf(G)
    DataFrame(
        [
            (geom=AG.createlinestring([
                collect(G[e[1]]),
                collect(G[e[2]])
            ]),)
            for e in edge_labels(G)
        ]
    )
end

"""
The betweenness centrality function in Graphs.jl only looks at
nodes, but we want to look at edges. Calculate betweeness centrality
by edges.
"""
function edge_betweenness_centrality(G)
    output = Dict(edge_labels(G) .=> 0)

    for i in 1:nv(G)
        paths = dijkstra_shortest_paths(
    end

    return output
end


export make_graph, make_grid!, graph_to_gdf, EdgeData
end