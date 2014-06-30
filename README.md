## Crowd flow analyzer in the Zapopan municipality

In this project we develop a crowd flow analyzer based on the localized publications on `Twitter` and `Foursquare` of people throughout the Zapopan, Guadalajara, Tlaquepaque and Tlajomulco municipalities. 

This project was mainly coded on `R` for the data processing. We used both `Shiny` and `rMaps` to show some interesting interactive results. Basically `rMaps` is still under development and it still doesn't have very good compatibility with the `html` output support from `Shiny`. 

The wrapper of `Datamaps` in `rMaps` library still doesn't have a stable option to include bubbles on a map. That's why in one step of the development you should edit manually in a text editor some `html` configuration code. Basically there are three steps: 

* Replace all strings `"NA: "` with null strings, *i.e.* "". This can be done with regular expressions. 
* Add the bubble configuration variable at the end of the file in order to load bubble's popups. It should look like:
```{bash}
    var bubbles = chartParams.bubbles
    var bubblesconfig = chartParams.bubbleConfig
    mapchart2d4439c77d0e.bubbles(bubbles, bubblesconfig)
```
* Replace the beginning of the bubble's JSON with a bracket. It should be like:
```{bash}
    "bubbles": [ ... ]
```
Instead of being:
```{bash}
    "bubble": { "": ... }
```

Also it should be noted that it was possible to include custom polygons based on the publication of [Fellgernon Bit](http://www.r-bloggers.com/rmaps-mexico-map/)
