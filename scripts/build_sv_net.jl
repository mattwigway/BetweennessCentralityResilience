using MissingLinks
import GeoFormatTypes as GFT
using MetaGraphsNext
import ArchGDAL

graph = graph_from_osm("data/southern_village.pbf", TraversalPermissionSettings(walkable_tags=Set(["highway"=>"footway"])), GFT.EPSG(32119))

for (fr, to) in edge_labels(graph)
    ArchGDAL.flattento2d!(graph[fr, to].geom)
end

graph = remove_tiny_islands(graph, 100)

MissingLinks.graph_to_graphml("data/graph.graphml", graph)
MissingLinks.graph_to_gis("data/graph.gpkg", graph)
