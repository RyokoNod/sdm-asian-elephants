# The codes used in this project

The R codes I used are separated by files. The ```old``` folders that can be found here and in subdirectories contain old versions of code that are not used anymore. Some have mistakes in their implementations so it's better not to touch them. The working codes that are placed directly in this folder are:

* ```utils.R```: This contains helper functions that I used within the project. 
* ```thesis_plots.R```: This is the file I used to create plots that I made specifically for the written report.

Most code files start with the specifications of the feature types. Some variable names will be different from the written report, so I will do a brief explanation here.

* ```feature_type```: "GLM" stands for random-CV features, "SGLM" stands for spatial-CV features.
* ```normalize```: If set to TRUE, uses scaled features.
* ```adapt_d```: The adapt delta parameter in Stan. A larger value makes physics simulation fine-grained.
* ```treedepth```: The maximum tree depth parameter in Stan. A larger value makes the wait time longer until the algorithm stops the NUTS simulation mid-trajectory.
* ```k```: The "basis dimension" parameter that indirectly determines the complexity of splines.

Oh, and one note: I am usually a Python user so I couldn't figure out how to do proper documentation with R. So I did what I can with what I know, which is probably not the style regular R users are used to seeing. So sorry for any inconvenience, I will try to figure it out in my future R projects.

And finally, below is the list of things you can find in the individual subfolders.

## Random_Forest

There is only one file in here, ```standard_RF.R``` that does everything from model building to plotting. This code was created to run in RStudio.

## bayesian_linearGLM (Bayesian logistic regression)

  This folder for Bayesian logistic regression is slightly messy because I kept on having to separate code from the main script every time RStudio was not behaving on Aalto's server. The subfolder ```baseline_priors``` is contains models that I made initially and the subfolder ```adjusted_priors``` contains models after I adjusted the priors using the [priorsense package](https://github.com/n-kall/priorsense). The models in the ```baseline_priors``` folder is further separated into models that have different data types for the response variable (factor or not factor - some plotting functions did not work when the response variable was a factor). Also, ```baseline_priors``` and ```adjusted_priors``` may contain RData files, which are outputs I exported from ShinyStan to create plots in ```codes/thesis_plots.R```.
  
  The scripts ```CV.R``` and ```inference_adjustpriors.R``` are designed to run on a linux terminal. These need arguments that specify the feature type, scaling, adapt delta, and the maximum tree depth. For example, if you are trying to do CV with the model using scaled spatial CV features with adapt delta = 0.99 and maximum tree depth = 10, you need to run the script as below.

  ```
  Rscript CV.R SGLM TRUE 0.99 10
  ```

  Scripts:
  * ```CV.R```: Does only the spatial cross-validation for the initial priors. 
  * ```bayes_linGLM.R```: The main script for Bayesian GLM. Has everything from model building to plotting, but tends to crash on Aalto's RStudio.
  * ```bayes_linGLM_adjustpriors.R```: Exactly the same as ```bayes_linGLM.R```, but has adjusted prior settings in the function definition.
  * ```big_condeff_surface.R```: The plotting script that draws the conditional surface plot beyond the training data's range.
  * ```inference_adjustpriors.R```: The script that creates models with adjusted priors. 
  * ```prior_sensitivity.R```: The script that uses the ```priorsense``` package to check whether there is prior sensitivity.

## bayesian_splineGLM (Bayesian GAM)

  This folder is even more messy than the Bayesian logistic regression folder because this is where the most of the classic, disorganized iterative modeling took place. The models are  placed in the subfolders ```bnorm_sdsnorm```, ```bnorm_sdst```, and ```bunif_sdsnorm```, whose file names indicate the prior sets (bnorm: the intercept and coefficients are normal distributions, bunif: the intercept and coefficients are uniform (flat) distributions, sdsnorm: the non-linearity priors are normal distributions, sdst: the non-linearity priors are Student's t-distributions). These folders are further separated into subfolders that indicate the basis dimension I used (k_default: default basis dimension (-1), k1: basis dimension = 1). Please note that some models created early on in the project do not work with some plotting functions because their response variables were defined as factors.
  
  Aside from ```big_condeff_surface.R```, ```image_outputs.R```, and ```prior_sensitivity.R```, everything is designed to run on a linux terminal. Depending on the script, there are up to 5 required arguments you need to specify (all the arguments shown in the Bayesian GLM section + the argument for the basis dimension). For example, when fitting a Bayesian GAM model using raw random CV features with default basis dimension, adapt delta 0.99, and maximum tree depth 13, you need to run the script as below.

  ```
  Rscript inference.R GLM FALSE 0.99 13 -1
  ```

  **DO NOT** run any of the Bayesian GAM code on a laptop. These contain extremely heavy code that sometimes took more than 3 days even using Aalto's computing cluster.
  
  Scripts:
  * ```CV.R```: Does spatial cross-validation. This one has variations ending in ```_bnorm_sdsnorm``` and ```_bunif_sdsnorm```, but their only difference is the prior settings.
  * ```big_condeff_surface.R```: The plotting script that draws the conditional surface plot beyond the training data's range.
  * ```future_preds.R```: Does predictions using future data.
  * ```image_outputs.R```: Creates plots from outputs of other scripts.
  * ```inference.R```: Builds the models. Also has variations ending in ```_bnorm_sdsnorm``` and ```_bunif_sdsnorm```, each with different prior settings.
  * ```present_preds.R```: Does predictions using present-day data.
  * ```prior_sensitivity.R```: The script that uses the ```priorsense``` package to check whether there is prior sensitivity. However PSIS breaks down on these models so it is not very useful.
  
  ## standard_linearGLM (MLE logistic regression)
  
  There is only one file in here, ```standard_linGLM.R``` that does everything from model building to plotting. This code was created to run in RStudio.
  
