# Evaluating the impact of using different tectonic plates motion models in palaeobiogeography

Our goal is to assess the impact of using different rotation models in paleobiogeography. For that, we need to first quantify how different the available paleorotation models. The second part of our project will be devoted to an illustration of these differences by applying the models to true fossil data (corals)

## Assessing the differences between paleorotation models

Our approach consists of:
<ol>
  <li> Creating a 1x1° meshgrid
  <li> Opening it in Gplates
  <li> Merging it with continental polygons associated with a given model
  <li> Applying rotation to the resulting spatial data points
  <li> Extracting their coordinates over time
  <li> Comparing them between models
</ol>

[workflow](workflow.pdf)
  
For the parts 1-5, see *rotating.R* script.

For the part 6, see *data_analysis.R* script. The script first gets rid of the spatial points belonging to the oceanic plates. In the models that only reconstruct continental plates motion, these points may be easily identified, as they haven't been associated with any plate ID (see georeferencing step).
© 2022 GitHub, Inc.
Terms
Priv
