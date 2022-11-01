# Ibex.manuscript
Analyzing single-cell BCR + RNA

### Abstract 

As a primary interface between adaptive and innate immune responses, B cells are a central node in regulating the immune system. 
In turn, B cell response is governed by recognizing an antigen to the B cell receptor (BCR). This recognition and subsequent 
response coordinate a series of transcriptional programs. Single-cell RNA and paired BCR profiling offer insights into numerous 
physiological and pathological processes. Unlike the plethora of single-cell RNA analysis pipelines, computational tools that 
leverage single-cell BCR sequences for further analyses are wanting. Therefore, we developed Ibex, which translates the amino 
acid sequence of the complementarity-determining region 3 (cdr3) of the immunoglobulin heavy and light chains, allowing for unbiased 
dimensional reduction. Ibex is implemented as an R package with integration into both the Seurat and Single-Cell Experiment framework, 
providing ease to the incorporation of many single-cell workflows and multimodal experiments.

### Directory Structure 
```
├── data
│   ├── Ibex_FullExample.rds #All B cells
|   └── Ibex_ClonotypeRep.rds #Reduced to clonotype
├── Output #Visualization in Manuscript
└── Code for Analysis.Rmd
```

*****
### Contact
Questions, comments, suggestions, please feel free to contact Nick Borcherding via this repository, [email](mailto:ncborch@gmail.com), or using [twitter](https://twitter.com/theHumanBorch). 
