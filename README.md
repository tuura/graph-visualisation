# graph-visualisation

## Circular Flat Graphs
The `Visualise.FlatCircle` module can be used to draw a flat graph with each node being at a vertex of a regular polygon with n sides, where n is the number of nodes the graph has. This works best for small graphs.

A circular graph can be drawn using the function `drawFlatCircle` with the type signature `(Show a) => FilePath -> Dimensions -> Graph a -> IO ()`. When supplied with a file path for the output SVG file, an image dimension (in the form of a tuple with the type `(Maybe Double, Maybe Double)`, the second dimension can be calculated from the first) and a graph of the type from Algebra.Graph.

A second version of the draw function also exists, with the accomodation for a Settings type as a parameter in order to alter the visualisation of the graph: `drawFlatCircle'` with the type signature `(Show a) => Settings -> FilePath -> (Maybe Double, Maybe Double) -> Graph a -> IO ()`

The `Settings` type allows the arrows connecting nodes to be customised, with the type being defined as: `Settings { dynamicHead :: Measure Double , dynamicThick :: Measure Double }`. When the standard draw function is called the default settings are used which dynamically change the size of the arrow heads and thickness of the arrow shafts in accordance with the number of nodes.

### Issues
* Does not work for nodes with self-connected loops

# General Flat Graphs
The `Visualise.FlatAdaptive` module tries to represent any flat graph in a readable way.

### Issues


## Directed Acyclic Graphs
Directed graphs with no cycles can be drawn as trees using the `Visualise.DAG` module. This uses the Coffman-Graham algorithm to produce the layout, with the topological ordering of the nodes being achieved by using Kahn's algorithm.

## Hierarchical Graphs
Hierarchical graphs can be drawn using the `Visualise.Hierarchical` module.

The function `drawHierarchical` 

When two adjacent graphs are overlayed they are placed next to each other within a box, and when two adjacent graphs are connected their boxes are placed next to each other and connected with an arrow.

### Issues
* Currently only works with DAGs

## GraphGen
GraphGen is a small Java program which generates a random graph to be used for testing with a given number of layers.