# Getting started with SUNA data and Jupyter Notebooks

## Create accounts
* Create a github account and send the account name to Jake.
* Create a zotero account and send the account name to Jake.

## Install software
### Install R and RStudio if you have not already.
### Install JupyterLab
* Jupyter notebooks are a branch of the original ipython notebook project (hence the extension .ipynb). Both projects still exist and are compatible. Jupyter is designed to work with not just Python, but also Julia and R (that's where the name comes from).
* You can install either Jupyter Notebooks or JupyterLab. I like the later because it allows you to open and view multiple files at the same time.
  - Make sure you have Python installed. If you have not installed Python, you should do so. If you have a Mac, it should be pre-installed, but there are packages and dependencies that may need to be installed. If you are using Windows, [you can install Anaconda](https://towardsdatascience.com/ideal-python-environment-setup-for-data-science-cdb03a447de8), which will install many of the dependencies you need. In my experience, Anaconda is great on Windows, but it tends to put files in weird places on a Mac.
  - To install JupyterLab go [here](https://jupyterlab.readthedocs.io/en/stable/getting_started/installation.html). I would recommend using pip to install using the instructions in the link. You may need to install pip. You could also try using the conda method, but as I mentioned I don't love how the conda installer organizes things.
  - Now to link Jupyter and R, run the following lines in R:
    ```
    install.packages("devtools")
    devtools::install_github("IRkernel/IRkernel")
    IRkernel::installspec()
    ```
