# samcleanR
This is an R package I created for preparing my data. It scores psychological self-report measures, calculates the percent of items on a subscale that are missing, substitutes person-level means for missing item scores, and centers total scores around their means (both the person mean and the grand mean). 

###Usage
Because I created this to prepare my own data, I did not write the functions to be terribly flexible (though I may rewrite everything when my R skills improve). The most complicated functions, `megaScorer` and `megaMissingness`, read in Excel workbooks that are formatted in a specific way (i.e., with raw item scores from a self-report measure in each sheet, with the sheets named after the measures). The function `getLookup` (upon which those functions depend) reads in a table of information for scoring those measures, also created in Excel and formatted in a specific way. However, if you have a lot of self-report instruments to score, you might find these functions useful and could easily alter the code of these functions to read in data in a different way. The code is heavily commented, which should help. 

The functions `calcPercentMissing`, `calcSubscale`, and `meanSubstitute` are much more flexible because the input and output format is a data frame. The meanSubstitute function may be especially useful to others: you can specify the maximum percent of a subject's items that are allowed to be missing in order to substitute that subject's mean for the missing items.\* 

The function `centerer` is the most flexible and thus also more useful to others. It grand-mean and person-mean centers longitudinal data, as described in chapter 5 of the book *Intensive Longitudinal Methods* by Niall Bolger and Jean-Philippe Laurenceau (a pretty useful book if you're interested in analyzing repeated measures data using multilevel modeling ... and I'm not just saying that because I attended a workshop with J-P and Niall and they are hella chill). The function takes a data frame in either "wide" or "long" format. 

###List of functions
The functions are heavily documented, so calling `help` should give a thorough explanation. 
```
centerer
megaScorer
megaMissingness
meanSubstitute
getLookup
workbookToDfList
calcSubscale
percentMissing
calcPercentMissing
```
###Disclaimer
This code was written by a true novice (as in, I learned R two weeks ago). Use at your own risk. Please contact me with bugs or if the documentation is unclear. 

#####A note to other workshop attendees who may wish to use the centering function (or others), but who have minimal experience with R
You can import data from just about any stats package or file format into a data frame--just search the Web for "import data to R from SAS" or whatever you want. Make sure that missing data in the data frame end up as `NA`. You can also write the resulting data frame to your favorite file format (again, the Internet is full of straightforward explanations). To use my package, you will need to install it ... which requires installing the devtools package first, like so: 
```r
install.packages("devtools")
library(devtools)
install_github("sbernecker/samcleanR")
```
Then, just call `library(samcleanR)` any time you want to use my functions. Alternatively, if you just want one or two functions, you could download the .R file into your working directory and `source` it when you want to use it, e.g., `source("centerer.R")`.

To reshape data from wide to long and vice versa, you can use R's built-in function `reshape`. Other advice: RStudio and Google will hold your hand through this. 
***
\*I've done this because I believe subject-mean substitution is an acceptable way to deal with item-level missing data, if missingness is minimal. After all, it's probably not going to introduce a lot of bias if you substitute a person's mean item score for a single missing item on a 15-item instrument. I wouldn't use multiple imputation to replace item-level missing data because (at least with the procedures I have tried) it places too high a demand on memory to be feasible. My approach is to use item-level mean substitution when a few items are missing, but when several are missing, to use multiple imputation on total scores (or a statistical procedure that is robust to missing data). 


