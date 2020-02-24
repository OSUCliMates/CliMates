# Workflow

We are using a *Forking Workflow*: Everyone has their own fork of the main repo. Do your work locally on a branch of your fork of the main repo. To get your work into the main repo, you initiate a Pull Request.

Also documented here: https://happygitwithr.com/fork-and-clone.html

## Initial Setup

Fork the [`OSUCliMates/CliMates`](https://github.com/OSUCliMates/CliMates/issues/4) repo.  Clone it locally.  

### Shortcut

Shortcut to fork, clone and set upstream, in R:
```r
usethis::create_from_github("OSUCliMates/CliMates", fork = TRUE, protocol = "https")
```
Do we 

```r
usethis::create_from_github("cwickham/CliMates", protocol = "https")
```



## Doing your own work

Don't make commits to the `master` branch.  Instead create a branch and commit your work there, e.g. to create and checkout a `workflow` branch:

```shell
git branch workflow
git checkout workflow
```

Make changes, commit, push when you want.  This pushes to your fork.

When you are ready to suggest the changes to the main repo.  Start a pull request from your branch in your fork.  

Your PR is merged and you want to do more work? Update the `master` branch of your fork to match the main repo, then create a new branch, check it out and work away.

## Keeping up-to-date

https://happygitwithr.com/upstream-changes.html#upstream-changes

### 1. You need to set the main repo as `upstream`

List your remotes
```shell
git remote -v
```

If you don't have it, add the main repo as `upstream`
```shell
git remote add upstream https://github.com/OSUCliMates/CliMates
```

### 2. Pull in changes to `master`, merge and push

Make sure you are on the master branch 
```shell
git checkout master
```

Grab changes from main repo
```shell
git pull upstream master --ff-only
```

Push to your fork on GitHub
```shell
git push
```


