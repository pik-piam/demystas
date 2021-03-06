# Contributing to Demystas Development

In order to contribute to the development of this package, we need to implement two key procedures as described below.

## A. Forking and cloning from source

First we need to fork the original repository and clone it onto your personal workspace.

1. Fork this repository from GitHub onto your personal account

2. Clone this repository onto your personal workspace

```shell
$ git clone https://github.com/<youraccount>/demystas
```

3. Add our original repository as an additional remote URL

```shell
$ git remote add upstream https://github.com/pik-piam/demystas
```

4. Before committing any changes, ensure your forked repository is synced with all the updates from the original repository

```shell
$ git fetch upstream

$ git checkout master

$ git merge upstream/master
```

5. Create your own topical branch and begin committing your changes there. It should be named uniquely from other branches, in this case we used `dev`.

```shell
$ git checkout master

$ git branch dev

$ git checkout dev
```

Now, you can start to make changes to your `dev` branch.

## B. Submitting pull request

Once your changes to the `dev` branch are complete, please add and commit them with concise commit messages.

1. Update your local `master` branch to absorb newly committed changes from the original repository

```shell
$ git fetch upstream

$ git checkout master

$ git merge upstream/master
```

2. If there were new developments to the upstream repository, you can now rebase your `dev` branch based on these.

```shell
$ git checkout dev

$ git rebase master
```

3. Push your local commits to your forked repository's `dev` branch.

```shell
$ git push -u origin dev
```

4. Go to the forked repository on your GitHub page. Navigate to the `dev` branch and click the pull request button. Describe the contents/crux of your pull request. Once all is well, submit the pull request.

5. The proposed changes will be reviewed and we will revert regarding the merging process.
