# nmaplots 0.3.0.9000

* New `nma_clean()`: converts text columns that hold numbers, such as `TE` and
  `seTE` read back from a `write.csv2()` file (`"-0,684"`), to numeric. It
  reads decimal commas, drops thousands dots in `"1.234,5"`, and accepts the
  Unicode minus and dash characters spreadsheets substitute for `-`. Without
  it, `netmeta()` stops with "Non-numeric value for argument 'TE'".
* `nmaplot()` accepts pairwise data directly (`meta::pairwise()` output or a
  data frame with `treat1`, `treat2` and `studlab`), cleaned with
  `nma_clean()`. Sample sizes and events come from `n1`/`n2` and
  `event1`/`event2`.
* New example file `inst/extdata/recurrence_pairwise.csv`: a pairwise
  recurrence network (14 comparisons, 11 treatments) saved with `;`
  separators and decimal commas.

* `nmaplot()` accepts 'gemtc' objects: `mtc.network`, `mtc.model` and
  `mtc.result`. Arms in `data.ab` and `data.re` become the edges; sample sizes
  (`sampleSize`) and events (`responders`) are used when every arm has them,
  so binary arm-level networks get the event-rate ring too. The treatments'
  `description` column supplies the display names when it differs from `id`,
  which lets names like `Cilofexor + Firsocostat` show despite gemtc's
  letters-digits-underscore rule for ids. `reference` defaults to the most
  connected treatment.

# nmaplots 0.3.0

* Binary networks draw an event-rate ring by default: each node shows events
  over participants, with that one percentage printed outside the ring.
  `ring = FALSE` removes it, any `ring` of your own replaces it, and
  continuous networks are unchanged.
* The summary line under the subtitle ends with the total number of events
  when the network is binary.
* Thinner default edges (`edge_width_range = c(0.3, 2)` mm).
* Node labels put the share on the same row as the count, `n = 374 (23%)`.
* Legend: every section header sits on a shaded band, with a larger bold name
  and the descriptor tucked closer under it. The node-size section now shows a
  single node labelled the way the plot labels them, with a note that the
  percentage is the share of all patients.
* Smaller circled study counts, drawn in `edge_font_family` (`"sans"` by
  default) so the digits stay compact.

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
