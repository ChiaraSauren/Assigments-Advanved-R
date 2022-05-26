










### 4.

## a)

$ git rm byeGit.txt  # removing and staging removal of byeGit.txt

$ git add HelloGit.txt  # staging for commit

$ git status  # make sure HelloGit.txt and byeGit.txt are staged

$ git commit -m "Fixes to file"    # HelloGit.txt committed

$ git add assignment_1.sqlite3     # track files for staging
$ git add rent_advertisment.RData  

$ git commit -m "data added"   # committed files


## b)

$ git log  ## search for commit reference

$ git checkout main  # make sure you are in wanted branch

$ git cherry-pick >ref< # ref stands for placeholder of commit reference
  # commit for HelloGit-txt will be applied to 
  #different branch
  