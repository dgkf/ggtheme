# ggtheme

An R package for more extensive theming of ggplots

`ggplot2` provides a `theme` layer that can be added to any plot, as well as 
`theme_update` to update default values used when applying a theme. Similarly,
it provides functions `update_geom_defaults` and `update_stat_defaults` to 
update the aesthetic defaults for both of these layer constructors. 

However, there are plenty of knobs that one may still want to tweak when 
building a `ggplot2` theme. Some of the limitations of these built in theming
functions include
  - `scales_*` default color schemes and gradients
  - `geom_*` and `stat_*` non-aesthetic default parameters (experimental)
  
This package aims to provide a convenient mechanism of altering a number of
theme-related settings in one go. 

## Theme options

### 1. Arguments passed on to `ggplot2::theme_update()`

Any argument that can be passed to `ggplot2::theme` can be passed to set a theme
parameter.

### 2. Modifying `ggplot2` default aesthetics

Any aesthetic can be set globally for any layer that has a non-`NA` default
for that aesthetic. Alternatively, arguments can be prefaced with a layer 
name (e.g. `geom_bar.fill`) to set an aesthetic for a specific object.  

`ggplot2::aes` can be used to pass aesthetics, which is convenient for arguments
to `Stat` layers, which often need to pass an expression.

```r
ggtheme(aes(stat_bin.y = stat(count / max(count))))
```

### 3. Modifying `ggplot2` default scales

`ggplot2` will look in its own namespace for `scale_*` functions before looking
at your global environment. To set a default, these functions are modified
within an attached `ggplot2` namespace.

`ggtheme` accepts two arguments to define scales, `discrete_scale_colors` and
`continuous_scale_gradient`. Both accept a vector of hex color strings, and a
gradient can be specified as a list of arguments to any
`ggplot2::scale_*_gradient` function. `ggtheme` will infer which gradient
function you would like to use based on the arguments used.

### 4. Non-aesthetic function defaults

Each layer constructor also comes with preset arguments beyond just the default 
aesthetic values. Currently, these can also be modified, though due to extensive 
modification of the ggplot namespace, this feature is considered experimental. 

## Examples

```r
library(ggplot2)
library(ggtheme)

# "charcoal" theme
ggtheme(
  text = element_text(color = "#666666"),      # any argument passed to theme()
  panel.background = element_rect("#EDEDED"),   
  fill = "#464646",                            # global aesthetic defaults
  color = "#666666",
  geom_bar.fill = "#565656",                   # geom aesthetic overrides
  geom_histogram.fill = "#565656",
  geom_bar.position = "dodge",                 # experimental defaults setting
  geom_histogram.na.rm = TRUE,
  discrete_scale_colors = c(                   # overriding scales
    "#111111", "#333333", "#555555",
    "#777777", "#999999", "#BBBBBB"),
  continuous_scale_gradient = list(
    low = "#111111", 
    high = "#CCCCCC"))
```

## What's Next

So far, this is just a barebones way of itemizing a bunch of different
theme-related parameters in one place and handling some hard-to-alter settings
like scales.

`ggthemr` lays out some great prior art for more convenient syntax for designing
a theme, and some really convenient presets. Eventually something like this
will make its way into this package, but for the time being the goal is just
the underlying functionality. 
