# nmaplots 0.2.0.9000

* Thinner default edges (`edge_width_range = c(0.3, 2)` mm).

# nmaplots 0.2.0

## Data

* `data(mash)`: built-in example, a list of four arm-level data frames from a
  MASH network meta-analysis (fibrosis improvement and MASH resolution,
  highest-dose and pooled-dose), with an illustrative `design` column
  (RCT / PSM) for the ring examples. All examples, guides and the notebook use
  it.

## Plot

* Edges are black; the number of direct studies sits in a white circle with a
  black border on each edge. Circles that would overlap slide apart along
  their edges. Networks where every comparison has the same number of studies
  use the thinnest line.
* Node labels: bold treatment name, then `n = ...`, then `(x% of total)`,
  every row centred, placed straight outward from the centre and kept clear
  of the disc. `label_box` adds a container (TRUE, or a fill colour).
* Outer ring (`ring =`): subgroup composition per treatment, from a long
  data frame or a wide table such as `table(d$treatment, d$design)`.
  Percentages are placed at the point of each segment farthest from any
  edge and from the label. `ring_name` feeds the subtitle and the legend.
* Layouts: `"multi"` (default polygon), `"circle"` (dashed circle guide,
  ordered by number of comparisons), `"star"` (reference in the centre; its
  name, n and share go to a legend section). The polygon radius grows with
  the number of treatments.
* Title centred by default; `outcome` becomes the subtitle; a summary line
  gives studies, treatments and patients (`summary_line`).
* Legend: one boxed panel under the network with centred sections, each with
  a bold name and a descriptor: node size (two stacked discs), circled number,
  centre node (star layout), outer ring (a ringed disc plus colour squares).
* Larger default type; no softened text colours.

## Documentation

* Package page with four guides (workflow, ring, style, moving from
  `netgraph()`), `?mash`.

# nmaplots 0.1.0

* First version: `nmaplot()` draws a `netmeta` network with ggplot2 (node
  size by sample size, edge width and count by number of studies), with
  export to PNG, PDF and TIFF and compatibility with the main `netgraph()`
  arguments.
