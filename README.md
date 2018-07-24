# graph-visualisation
# README in progress
Provided are different ways to draw an algebraic graph (as defined by `Algebra.Graph`) using the Haskell `Diagrams` library.

Each graph drawing module has two drawing functions, one that uses a set of default settings and one that can be supplied with user-defined settings. These functions produce a diagram of the type defined by the Haskell `Diagrams` library. The function with default settings has the type signature of:
```Haskell
(Show a) => Graph a -> Diagram B
```

Meaning that it requires a graph of the type defined by `Algebra.Graph` and from this produces a `Diagram` from the `Diagrams` library. The corrisponding function which requires an extra settings parameter adds a required function of the type `Graph a -> Settings` to the start of the type signature. The output of these functions is suitable to be given to the following `saveSVG` function.

The `Visualise` module has an impure function named `saveSVG` which can be used to output a `Diagram` to an SVG file. This function has the type signature of: 
```Haskell
(Show a) => FilePath -> Dimensions -> Diagram B -> IO ()
```
This means that it requires an output file path, a set of dimensions (a tuple of `Maybe Double` values for width and height, but only one is required for a diagram to successfully be scaled) and a `Diagram`. The function will provide an IO action to create the file, which will be in the SVG format. To change the type of file format the code changes would be straightforward.

Another useful function in `Visualise` is `dynamicStyle` which produces a `Measure Double` from a default size and the size of a graph:
```Haskell
dynamicStyle :: Measure Double -> Int -> Measure Double
dynamicStyle def graphSize = def * 10/fromIntegral graphSize
```

A graph is defined like so:
```Haskell
data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
```

Where `a` can be of the type `String`, `Char`, `Int` or even `Graph b` (i.e. a vertex can be another graph, this will be discussed in the individual module descriptions).

## Directed Acyclic Graphs
The `Visualise.DAG` module is the most developed module.

Graphs with no cycles can be drawn as trees using the `Visualise.DAG` module. There are two main drawing functions which use default settings and two extra functions which corrispond to the main two but add a settings parameter. `drawDAG` draws the graph will all its connections whereas `drawDAGPartialOrder` uses the Coffman-Graham algorithm to produce the layout, which removes/reduces the indirect dependancies in order to simplify the (partial order) graph. Topological ordering using Kahn's algorithm is carried out for both functions, followed then by the nodes being drawn in layers and then connected.

The two primary functions have the same type signature:
```Haskell
(Show a, Eq a, Countable a) => (a -> Diagram B) -> Graph a -> Diagram B
```

The customisable settings are customisable by giving the secondary drawing functions a function which takes a graph and returns a settings instance:
```Haskell
data Settings = Settings { layerSpacing :: Double
                         , nodeSpacing :: Double
                         , graphPadding :: Double
                         , dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         , directed :: Directed
                         }
```

Which allow the spacing between nodes and layers, the padding around the graph, the arrowhead size/shaft thickness and whether the graph is directed (so if edges should have arrows) to be set. 
The directed type is given by:
```Haskell
data Directed = Directed | Undirected deriving (Eq)
```

The default settings function is:
```Haskell
defaultSettings :: (Countable a) => Graph a -> Settings
defaultSettings g = Settings 0.2 0.3 0.1 (dynamicStyle small $ count g) (dynamicStyle thin $ count g) Directed
```

### Issues
* Sometimes arrows can cross nodes
* Occasionally nodes are placed on the wrong layer

## Circular Flat Graphs
The `Visualise.FlatCircle` module can be used to draw a flat graph with each node being at a vertex of a regular polygon with n sides, where n is the number of nodes the graph has. This works best for small graphs.

The standard drawing function is `drawFlatCircle`, to provide additional settings parameters the function `drawFlatCircle'` is also provided.

The `Settings` type allows the arrows connecting nodes to be customised, with the type being defined as:
```Haskell
data Settings = Settings { dynamicHead :: Measure Double 
                         , dynamicThick :: Measure Double 
                         }
``` 
This enables the arrow shafts and head sizes to be customised. When the standard draw function is called the default settings are used which dynamically change the size of the arrow heads and thickness of the arrow shafts in accordance with the number of nodes, the `defaultSettings` function is defined like so:
```Haskell
defaultSettings :: Graph a -> Settings
defaultSettings g = Settings (dynamicStyle normal $ countVertices g) (dynamicStyle thin $ countVertices g)
```

### Issues
* Currently does not work for nodes with self-loops

## Adaptive Layout Flat Graphs
The `Visualise.FlatAdaptive` module tries to represent any flat graph in a readable way however needs more work to be fully functioning.
The two drawing functions are `drawFlatAdaptive` and `drawFlatAdaptive'` with the same settings adjustments available as `Visualise.FlatCircle` plus an integer (default 1) that determines the layout mode by adjusting the initial placement of the nodes before the algorithm is executed. Currently this module can work well with some graphs but not with others, with some graphs being very spread out.

It works by grouping together connected nodes, going up in group size.

### Issues
* Collisions between nodes
* Arrows crossing each other

## Hierarchical Graphs
Hierarchical graphs can be drawn using the `Visualise.Hierarchical` module. 

When two adjacent graphs are overlayed they are placed next to each other within a box, and when two adjacent graphs are connected their boxes are placed next to each other and connected with an arrow.

The two drawing functions are `drawHierarchical` and `drawHierarchical'`, but this time more settings can be defined when using the `drawHierarchical'` function. As well as arrow and head sizes being user-defineable, the background opacity for the boxes can also be defined (with the type of `Double`, which can be useful for if the colour for all boxes is the same as it will darken them with each layer) and a background colour choosing function can be supplied. This function is used with each layer to determine the the background colour, and has the type signature `Int -> Colour Double`, where the integer supplied is the depth of the current layer.

The arrow settings defaults are the same as the other drawing modules, the default opacity is `1` and the default colour choosing function alternates between blue and red for each layer.

The type signature for `Settings` is given by:
```Haskell
data Settings = Settings { colF :: Int -> Colour Double
                		 , bgOp :: Double
                		 , dynamicHead :: Measure Double
                		 , dynamicThick :: Measure Double
                		 }
```

### Issues
* Currently only works with DAGs

# GraphViz Integration
There is a fourth module which uses `Data.GraphViz` to get the layout of a graph and draws it using `Diagrams` with the help of the `diagrams-graphviz` library. The drawing function is called `drawWithGraphViz` and takes the `GraphvizCommand` to be used (see [here](https://hackage.haskell.org/package/graphviz-2999.20.0.2/docs/Data-GraphViz-Attributes-Complete.html#t:GraphvizCommand)), whether the graph 

# Testing

## GraphGen
GraphGen is a small Java program which generates a random graph to be used for testing with a given number of layers.

## Example Drawings
* The graphs from figure 3 in the [FDL paper](https://github.com/tuura/fdl17-paper) will be drawn using `Visualise.DAG`.
The table below gives the figures from the paper on the left and their visualisations using `graph-visualisation` on the right:

<table>
    <tr>
        <td><img src="examples/fdl-fig-3/fdl_3a_paper.png" /></td>
        <td><img src="examples/fdl-fig-3/fdl_3a_dag.svg" /></td>
    </tr>
    <tr>
        <td><img src="examples/fdl-fig-3/fdl_3b_paper.png" /></td>
        <td><img src="examples/fdl-fig-3/fdl_3b_dag.svg" /></td>
    </tr>
    <tr>
        <td><img src="examples/fdl-fig-3/fdl_3c_paper.png" /></td>
        <td><img src="examples/fdl-fig-3/fdl_3c_dag.svg" /></td>
    </tr>
    <tr>
        <td><img src="examples/fdl-fig-3/fdl_3d_paper.png" /></td>
        <td><img src="examples/fdl-fig-3/fdl_3d_dag.svg" /></td>
    </tr>
</table>

A set of graphs will be drawn by each of the algorithms, as required, the graphs are of the type defined in `Algebra.Graph`:
* Firstly the partial order graph `(1 * ((2 * ((4 * 7) + (5 * 7))) + (3 * (6 * (5 * 7)))))`

### With `Visualise.FlatAdaptive`
<img src="examples/flat_adaptive_example_1.svg" />

### With `Visualise.FlatCircle`
<img src="examples/flat_circle_example_1.svg" />

### With `Visualise.DAG`
#### With `drawDAG'` and `drawDAGPartialOrder` respectively

<table>
	<tr>
		<td><img src="examples/DAG_example_1.svg" /></td>
		<td><img src="examples/DAG_partial_order_example_1.svg" /></td>
	</tr>
</table>

`drawDag'` was used with the following `Settings` function to reduce arrow and node overlaps by reducing the spacing between layers and increasing the spacing within layers, while maintaining default arrow characteristics from `defaultSettings`:
```Haskell
settingsF :: Graph a -> Settings
settingsF g = Settings 0.1 0.5 (dynamicStyle normal $ countVertices g) (dynamicStyle thin $ countVertices g)
```

### With `Visualise.Hierarchical`
<img src="examples/hierarchical_example_1.svg" />