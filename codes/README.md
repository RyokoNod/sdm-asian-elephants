# The codes used in this project

The R codes I used are separated by files. The ```old``` folders that can be found here and in subdirectories contain old versions of code that are not used anymore. Some have mistakes in their implementations so it's better not to touch them. The working codes that are placed directly in this folder are:

* ```utils.R```: This contains helper functions that I used within the project. 
* ```thesis_plots.R```: This is the file I used to create plots that I made specifically for the written report.

Most code files start with the specifications of the feature types. Some variable names will be different from the written report, so I will do a brief explanation here.

* ```feature_type```: "GLM" stands for random CV features, "SGLM" stands for spatial CV features.
* ```normalize```: If set to TRUE, uses scaled features.
* ```adapt_d```: The adapt delta parameter in Stan. A larger value makes physics simulation fine-grained.
* ```treedepth```: The maximum tree depth parameter in Stan. A larger value makes the wait time longer until the algorithm stops the NUTS simulation mid-trajectory.
* ```k```: The "basis dimension" parameter that indirectly determines the complexity of splines.

Oh, and one note: I am usually a Python user so I couldn't figure out how to do proper documentation with R. So I did what I can with what I know, which is probably not the style regular R users are used to seeing. So sorry for any inconvenience, I will try to figure it out in my future R projects.

And finally, below is the list of things you can find in the individual subfolders.

## Random_Forest

There is only one file in here, ```standard_RF.R``` that does everything from model building to plotting.

## bayesian_linearGLM
