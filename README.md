# Age-structured Social Learning

This repository contains the scripts to reproduce all results and plots in 

***Deffner D, McElreath R. (2022). When does selection favor learning from the old? Social Learning in age-structured populations. PloS ONE https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0267204***

Description of files:

- "Recursions.r" contains code for numerical simulation of age-structured social learning model. It can be used to get a feeling for how the model behaves and produces output for Figs. 1 and 2.

- "ABM_Simulations.r" contains code for stochastic individual-based simulations for both temporally and spatially variable environments as well as simulations including adult learning. It's advisable to run this on a computer cluster. Produces output for Figs. 4,5,6 S2 and S3.

- "Analysis.nb" is a Wolfram Mathematica notebook used to solve recursions in section 3.3. of the manuscript

- Other files contain plotting code to reproduce all figures in the manuscript. In the beginning of each file, it says how to generate data for respective plots.
