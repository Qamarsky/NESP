# NESP
NESP R files

This repository is to hold all of the R files that Chris and Qamar will be working on for the NESP project. 

Here is the url on how to use GitHub and RStudio together. Note that you have to open a shell to add in new branches 

http://r-bio.github.io/intro-git-rstudio/

1. Start new project in R from Version Control (git). Enter the repository URL from your copy of Github. Can be found in the fork command. 

2. Open shell (Tools>Shell in R Studio) and give it the upstream address: ```git remote add upstream https://github.com/Qamarsky/NESP.git```. Make sure that it worked by typing git remote -v, it should display 4 lines, 2 that start with origin and the address of your fork, and 2 that start with upstream and the address of the upstream repository. Note that here we used upstream to name the upstream repository but we could have given it another name

3. Create a branch for the changes in R studio through the shell: `git checkout -b proposed-fixes master`. Proposed-fixes is our branch name within this R studio project. 

4. Make whatever changes, then go to the GIT icon in the menu bar and commit changes. 

5. Once changes are finished, open shell and type `git push origin proposed-fixes` (Note that proposed-fixes is the name of the branch you chose when you originated this branch). You may need to put in user name and password for the GitHub. 

6. Now you can log into github and accept the changes made in RStudio.

To get any changes made within GitHub into the RStudio project, use the Pull command in the shell: `git pull <remote> <branch>`
In this case we have set the remote file to be called origin, and the branch we are working on is master, so command is: `git pull origin master`
