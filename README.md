# IA2BC_LCLMM

In our manuscript titled *"Improved Classification of Alcohol Intake Groups in the Intermittent-Access Two-Bottle Choice Rat Model Using a Latent Class Linear Mixed Model"* [Angeles-Valdez et al., in review]().
The repository provides a graphical user interface (GUI) for running the Linear Latent Class Mixed Model (LCLMM).
Users can execute the analysis by running the ShinyApp file.


## Instalation 
  
 ### 1. R

Choose the appropriate installer for your operating system (Windows, macOS, or Linux) and follow the default installation instructions.

- **Download R**: Visit the official [CRAN (Comprehensive R Archive Network)](https://cran.r-project.org/) page.

 ### 2. RStudio

-  Get the latest version from [RStudio Download](https://www.rstudio.com/products/rstudio/download/).
-  The free version (RStudio Desktop) is sufficient to get started.


## Downloand 

Download the ShinyApp file, [here](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/App_LCLMM.R).

### R packages

We mainly use the following R package:   
lcmm: Extended Mixed Models Using Latent Classes and Latent Processes, [lcmm](https://cran.r-project.org/web/packages/lcmm/index.html)

###



  
## How to use:


 1- Open the Shiny app file in RStudio and click "Run". Your Shiny app should now appear.

 ![Alt text](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/images/1_Load_LCLMM_app.png)

 
 2- Load your data using the graphical interface.
 
 ![Alt text](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/images/2_Load_data.png)
 
 3- Select the input variables necessary for estimating the LCLMM model. Specify the number of classes you wish to estimate and press submit button. 
 
 ![Alt text](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/images/3_select_inputs.png)

 4-  Visualize the results.
 
 **Classification trajectories (two groups)**

![Alt text](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/images/4_two_clases.jpg)

**Please check the dataset oput to see the classification in dataset**

![Alt text](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/images/5_classification.jpg)


**Here you can change the number of classifications to be estimated**

![Alt text](https://github.com/DiegoAngls/IA2BC_LCLMM/blob/main/images/6_more_classification.jpg)

 

**Notes:**
For additional support, please feel free to contact us or consult the documentation of the **lcmm** package in R.

## Remember to always say thank you

More information in :
Proust-Lima C, Philipps V, Liquet B. Estimation of Extended Mixed Models Using Latent Classes and Latent Processes: The R Package lcmm. Journal of Statistical Software, Articles. 2017;78(2):1-56. https://doi.org/10.18637/jss.v078.i02



## Contact me


M. Sc. Diego Angeles-Valdez,   
PhD Student,   
University Medical Center Groningen, Cognitive Neuroscience Center,   
Biomedical Sciences of Cells and Systems (BCSS).   
Twitter: [@diegoangls](https://twitter.com/diegoangls)  
Bluesky:[@dangeles.bsky.social](https://bsky.app/profile/dangeles.bsky.social)   
Email: d.angeles.valdez@rug.nl  | diego.ang.val@gmail.com
Work address: Hanzeplein 1, 9713 GZ Groningen, The Netherlands

