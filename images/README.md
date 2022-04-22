# The images I used for interpretation

Here you can find the images I created in this project. Some were used in the written thesis, but most were only used for interpretation. Some images are old versions that I created while testing the plotting functions, so they will have a different design.

The subfolders here have the most complicated structure of all, so I don't have a way to communicate where everything is (this is the messy aftermath of iterative modeling). So instead I will define the folders and naming conventions of files here so you can check them out yourselves.

Folders:
* ```Bayesian_linearGLM```: Images related to Bayesian GLM.
* ```Bayesian_splineGLM```: Images related to Bayesian GAM.
* ```Standard_RF```: Images related to random forest.
* ```Standard_linearGLM```: Images related to logistic regression.
* ```GIF```: The GIF animations I created for the mind map and their ingredients.
* ```adjusted_priors```: Images from the Bayesian GLM models with adjusted priors.
* ```baseline_priors```: Images from the Bayesian GLM models with initial priors.
* ```bnorm_sdsnorm```: Images from the Bayesian GAM models with normal priors for intercept, coefficients, and non-linearity.
* ```bnorm_sdst```: Images from the Bayesian GAM models with normal priors for intercept and coefficients and Student's t-disttribution priors non-linearity.
* ```bunif_sdst```: Images from the Bayesian GAM models with uniform (flat) priors for intercept and coefficients and normal priors non-linearity.
* ```k1```: Images from the Bayesian GAM models with basis dimension 1.
* ```k_default```: Images from the Bayesian GAM models with default basis dimension.
* ```conditional_contours```: The two-feature interaction effect plots.
* ```conditional_effects```: The conditional effect of one feature on habitat suitability.
* ```conditional_lines```: The conditional effect of one feature on the log-odds (for Bayesian GLM).
* ```conditional_smooths```: The conditional effect of one feature on the log-odds (for Bayesian GAM).
* ```normalized_randCV```: Plots from models fit on scaled random CV features.
* ```normalized_spatialCV```: Plots from models fit on scaled spatial CV features.
* ```unnormalized_randCV```: Plots from models fit on raw random CV features.
* ```unnormalized_spatialCV```: Plots from models fit on raw spatial CV features.
* ```wide_contours```: The two-feature interaction effect plots visualized outside of the training data's range.
* ```ID_to_others```: The two-feature interaction plots with icing days on the x-axis.

File naming conventions:
* ```train```: Plots that show performance for the training data. The files that don't have this prefix are for the validation folds.
* ```blGLM```: Short for Bayesian linear GLM.
* ```bsGLM```: Short for Bayesian spline GLM.
* ```sRF```: Short for standard random forest.
* ```slinGLM```: Short for standard linear GLM.
* ```newcal```: The new calibration plot by Dimitriadis et al.
* ```posteriors```: The posterior distribution plots from ShinyStan. There are CSV files that accompany these plots that have the actual values.
* ```scores```: The training and validation scores.
* ```tradcal```: The traditional calibration plot.
* ```GLM```: Fit on raw random CV features.
* ```SGLM```: Fit on raw spatial CV features.
* ```nGLM```: Fit on scaled random CV features.
* ```nSGLM```: Fit on scaled spatial CV features.
* ```priorsens```: Priorsense outputs.
* ```12244```: The common random seed I used during this project.
* ```\<feature name\>-PA```: The conditional effect of \<feature name\> on habitat suitability.
* ```\<feature name\>-\<feature name\>```: The interaction effect with the first feature on the x-axis and the second feature on the y-axis.

## Animations on the mind map

In case my mind map link is not working, you can find the PDF version ```mind-map.pdf``` here. This is a vector image that lets you read all the text, but it will not show the GIF animations that was in the original mind map. So here is a list of all GIF animations that I created.

* **The difference between Bayesian GLM future predictions when using scaled features and initial priors**
  * [Point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_linearGLM/baseline_priors/GIF/futurepreds_blGLM_SGLM.gif)
  * [IQR](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_linearGLM/baseline_priors/GIF/futureiqr_blGLM_SGLM.gif)
* **The difference between Bayesian GLM future predictions when using scaled features after prior adjustment**
  * [Point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_linearGLM/adjusted_priors/GIF/diff_future_rawscaledSGLM.gif)
  * [IQR](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_linearGLM/adjusted_priors/GIF/diff_futureiqr_rawscaledSGLM.gif)
* **The prior and basis dimension settings I tried in Bayesian GAM**
  * Models fit on raw random CV features
    * Set 2-4
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_GLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_GLM_future.gif)
    * Set 5-6
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_GLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_GLM_future.gif)
  * Models fit on scaled random CV features
    * Set 2-4
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_nGLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_nGLM_future.gif)
    * Set 5-6
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_nGLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_nGLM_future.gif)
  * Models fit on raw spatial CV features
    * Set 2-4
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_SGLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_SGLM_future.gif)
    * Set 5-6
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_GLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_GLM_future.gif)
  * Models fit on scaled spatial CV features
    * Set 2-4
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_nSGLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/bnorm_sdst_changek/bsGLM_nSGLM_future.gif)
    * Set 5-6
      * [Present-day point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_nSGLM_present.gif)
      * [Future point predictions](https://github.com/RyokoNod/sdm-asian-elephants/tree/main/images/Bayesian_splineGLM/GIF/defaultk_changeprior/bsGLM_nSGLM_future.gif)


