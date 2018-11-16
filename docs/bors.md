# Deployment Notes for Bors

Bors is deployed as part of the `iohk-infra` deployment.

## How to set up a new repository with Bors

### 1. Bors Admin

Do this from the web interface https://bors-ng.aws.iohkdev.io/

1. Go to the Bors
   [Repositories List](https://bors-ng.aws.iohkdev.io/repositories) then
   [Add/Remove repositories](https://github.com/apps/iohk-bors/installations/new).

   Choose input-output-hk and then your repo.

2. Go to the [Bors admin page](https://bors-ng.aws.iohkdev.io/admin)
   and choose `Sync installations`.

3. Go to back to _Repositories_ and visit the _Settings_ tab of the new repository.
   1. Update the branch names to `bors/staging` and `bors/trying`
   2. Set _Reviewers_ to `Push`, click `Update`.
   3. Set _Members_ to `Push`, click `Update`.


### 2. Hydra jobsets

If there is `ci/hydra` among the required `status` in `bors.toml`,
then you will need to add Hydra jobsets for the `bors/staging` and
`bors/trying` branches.

Look in `iohk-ops/jobsets/default.nix` for examples
(`iohk-bors-{staging,trying}`).


### 3. Repo

Add your `bors.toml` in a PR and check that `bors ping` works.

You should then be able to `bors try` and `bors r+` the PR.

If the jobsets/ci status names are wrong/missing then `bors try` will
just time out after 2 hours.


## How to add admin users to bors

You need to log in to the Bors host and update the PostgreSQL database
which Bors uses. There are instructions in the Bors documentation.
