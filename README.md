# graph-visualisation
# README in progress
Each graph drawing module has two drawing functions, one that uses a set of default settings and one that can be supplied with user-defined settings.

Each draw function has the type signature of: 
```Haskell
(Show a) => FilePath -> Dimensions -> Graph a -> IO ()
```
This means that it requires an output file path, a set of dimensions (a tuple of `Maybe Double` values for width and height, but only one is required for a diagram to successfully be scaled) and a graph of the type defined by `Algebra.Graph`. The function will provide an IO action to create the file, which will be in the SVG format. To change the type of file format the code changes would be straightforward.

## Circular Flat Graphs
The `Visualise.FlatCircle` module can be used to draw a flat graph with each node being at a vertex of a regular polygon with n sides, where n is the number of nodes the graph has. This works best for small graphs.

The standard drawing function is `drawFlatCircle`, to provide additional settings parameters the function `drawFlatCircle'` is also provided.

The `Settings` type allows the arrows connecting nodes to be customised, with the type being defined as:
```Haskell
Settings { dynamicHead :: Measure Double , dynamicThick :: Measure Double }
``` 
This enables the arrow shafts and head sizes to be customised. When the standard draw function is called the default settings are used which dynamically change the size of the arrow heads and thickness of the arrow shafts in accordance with the number of nodes.

### Issues
* Currently does not work for nodes with self-connected loops

## General Flat Graphs
The `Visualise.FlatAdaptive` module tries to represent any flat graph in a readable way however needs more work to be fully functioning.
The two drawing functions are `drawFlatAdaptive` and `drawFlatAdaptive'` with the same settings adjustments available as `Visualise.FlatCircle`. Currently this module can work well with some graphs but not with others.

### Issues
* Collisions between nodes
* Arrows crossing each other

## Directed Acyclic Graphs
Directed graphs with no cycles can be drawn as trees using the `Visualise.DAG` module, using the Coffman-Graham algorithm to produce the layout. The indirect dependancies are removed/reduced in order to simplify the graph before topological ordering using Kahn's algorithm is carried out, then the nodes are drawn in layers and connected.

The functions `drawDAG` and `drawDAG'` can be used to draw the graph, with the adjustable settings again being the same as `Visualise.FlatCircle` and `Visualise.FlatAdaptive`.

### Issues
Sometimes arrows can cross nodes

## Hierarchical Graphs
Hierarchical graphs can be drawn using the `Visualise.Hierarchical` module. 

When two adjacent graphs are overlayed they are placed next to each other within a box, and when two adjacent graphs are connected their boxes are placed next to each other and connected with an arrow.

The two drawing functions are `drawHierarchical` and `drawHierarchical'`, but this time more settings can be defined when using the `drawHierarchical'` function. As well as arrow and head sizes being user-defineable, the background opacity for the boxes can also be defined (with the type of `Double`, which can be useful for if the colour for all boxes is the same as it will darken them with each layer) and a background colour choosing function can be supplied. This function is used with each layer to determine the the background colour, and has the type signature `Int -> Colour Double`, where the integer supplied is the depth of the current layer.

The arrow settings defaults are the same as the other drawing modules, the default opacity is `0.7` and the default colour choosing function alternates between blue and red for each layer.

The type signature for `Settings` is given by:
```Haskell
Settings { colF :: Int -> Colour Double
		 , bgOp :: Double
		 , dynamicHead :: Measure Double
		 , dynamicThick :: Measure Double
		 }
```

### Issues
* Currently only works with DAGs

# Testing

## GraphGen
GraphGen is a small Java program which generates a random graph to be used for testing with a given number of layers.

A set of graphs will be drawn by each of the algorithms
## The graphs
As required, the graphs are of the type defined in `Algebra.Graph`:
*
*
*

## Results

<img src="examples/DAG_example_1.svg" width="50%" />