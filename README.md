# adgs_report_schneidewind
in this repository I work through the course Applied Geodata Science 1 and here I will submit my graded reports.


## Folders


### The functions folder

The `functions` folder contains R functions, not scripts which are stored there so that they can be accessed
in other documents through realtive paths. 

### The data-raw folder

The `dataraw` folder contains, as the name suggests, raw data and the scripts
to download and pre-process the data. This is data which requires significant
pre-processing to be of use in analysis.

### The data folder

The `data` folder contains analysis ready data. This is data which you can use,
as is. This often contains the output of a `data-raw` pre-processing workflow,
but can also include data which doesn't require any intervention, e.g. a land
cover map which is used as-is. 

### The analysis folder

The `analysis` folder contains, *surprise*, R scripts covering analysis of your
analysis ready data (in the `data` folder). These are R scripts with output
which is limited to numbers, tables and figures. It should not include R
markdown code!

### The vignettes folder

The `vignettes` folder contains dynamic notebooks, i.e. R markdown files. These
might serve a dual use between analysis and manuscript. 
My final reports will be stored here. 
