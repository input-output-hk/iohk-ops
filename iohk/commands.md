* find-installers -r *revision* -c *configfile*

fetches installers from CI and prints various information about them
```
[nix-shell:~/iohk/iohk-ops]$ io find-installers -r ee63efafb3f70048be2bc231e40cf73027ae3e74 -c mainnet.yaml
From https://github.com/input-output-hk/daedalus
 * branch              HEAD       -> FETCH_HEAD
fetched
appveyor URL: https://ci.appveyor.com/api/buildjobs/jte4nscfa4uyq0ak/artifacts/installers/daedalus-win64-1.0.3528.0-installer.exe
downloading ‘https://ci.appveyor.com/api/buildjobs/jte4nscfa4uyq0ak/artifacts/installers/daedalus-win64-1.0.3528.0-installer.exe’... [104608/105866 KiB, 1473.3 KiB/s]
these derivations will be built:
  /nix/store/zdh6a2ivkf4cnsddrb43lfl3qxwvqk58-daedalus-win64-1.0.3528.0-installer.exe.drv
building path(s) ‘/nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe’
'/nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe' -> '/nix/store/v9c4vg7cis4nhscrcirl07f01aq4izfs-daedalus-win64-1.0.3528.0-installer.exe'
/nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe
"other CI status found: continuous-integration/appveyor/pr"
cardano build number: 16374
cardano commit: 47473ec57f7486c5d244f05a3ca3a78f317d5672
travis URL: http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.3378.pkg
Cloning into bare repository '/tmp/gitcache-cardano'...
remote: Counting objects: 143153, done.
remote: Compressing objects: 100% (589/589), done.
remote: Total 143153 (delta 419), reused 431 (delta 212), pack-reused 142299
Receiving objects: 100% (143153/143153), 81.65 MiB | 2.03 MiB/s, done.
Resolving deltas: 100% (103947/103947), done.
From https://github.com/input-output-hk/cardano-sl
 * branch                HEAD       -> FETCH_HEAD
fetched
From https://github.com/input-output-hk/cardano-sl
 * branch                HEAD       -> FETCH_HEAD
fetched
applicationVersion is 3
downloading ‘http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.3378.pkg’... [79846/79882 KiB, 2144.5 KiB/s]
these derivations will be built:
  /nix/store/nfvh46dblaq6d6rd1msang0alm7cd6aw-Daedalus-installer-1.0.3378.pkg.drv
building path(s) ‘/nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg’
'/nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg' -> '/nix/store/17983i9zkb8w9k0519s2h6x8x1af0nyp-Daedalus-installer-1.0.3378.pkg'
/nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg
"other CI status found: continuous-integration/travis-ci/pr"
total 114K
lrwxrwxrwx  1 clever users  83 Nov 16 11:21 daedalus-win64-1.0.3528.0-installer.exe -> /nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe
lrwxrwxrwx  1 clever users  75 Nov 16 11:23 Daedalus-installer-1.0.3378.pkg -> /nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg
drwxrwxrwt 58 root   root  127 Nov 16 11:23 ..
drwx------  2 clever users   4 Nov 16 11:23 .
InstallersResults {travisResult = TravisResult {localPath = "/tmp/iohk-ops22383/Daedalus-installer-1.0.3378.pkg", travisVersion = Version {versionToText = "1.0.3378"}, cardanoCommit = "47473ec57f7486c5d244f05a3ca3a78f317d5672", travisJobNumber = "3378", travisUrl = "https://travis-ci.org/input-output-hk/daedalus/builds/298771279?utm_source=github_status&utm_medium=notification"}, appveyorResult = AppveyorResult {avLocalPath = "/tmp/iohk-ops22383/daedalus-win64-1.0.3528.0-installer.exe", avVersion = Version {versionToText = "1.0.3528"}, avUrl = "https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.3528"}, globalResult = GlobalResults {grCardanoCommit = "47473ec57f7486c5d244f05a3ca3a78f317d5672", grDaedalusCommit = "ee63efafb3f70048be2bc231e40cf73027ae3e74", grApplicationVersion = 3}}
3 | [ee63ef](https://github.com/input-output-hk/daedalus/commit/ee63efafb3f70048be2bc231e40cf73027ae3e74) | [47473e](https://github.com/input-output-hk/cardano-sl/commit/47473ec57f7486c5d244f05a3ca3a78f317d5672) | [3378](https://travis-ci.org/input-output-hk/daedalus/builds/298771279?utm_source=github_status&utm_medium=notification) | [1.0.3528](https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.3528) | DATE
```
it also prints a chunk of markdown, which has been included again outside the quote block

3 | [ee63ef](https://github.com/input-output-hk/daedalus/commit/ee63efafb3f70048be2bc231e40cf73027ae3e74) | [47473e](https://github.com/input-output-hk/cardano-sl/commit/47473ec57f7486c5d244f05a3ca3a78f317d5672) | [3378](https://travis-ci.org/input-output-hk/daedalus/builds/298771279?utm_source=github_status&utm_medium=notification) | [1.0.3528](https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.3528) | DATE

* s3upload -r *revision* -c *configfile*

uploads the installers to the bucket defined in *configfile*

each installer is uploaded twice, once with a human readable name, and once with the hash required by the update system
```
[nix-shell:~/iohk/iohk-ops]$ io s3upload -r ee63efafb3f70048be2bc231e40cf73027ae3e74 -c mainnet.yaml
From https://github.com/input-output-hk/daedalus
 * branch              HEAD       -> FETCH_HEAD
fetched
appveyor URL: https://ci.appveyor.com/api/buildjobs/jte4nscfa4uyq0ak/artifacts/installers/daedalus-win64-1.0.3528.0-installer.exe
/nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe
"other CI status found: continuous-integration/appveyor/pr"
cardano build number: 16374
cardano commit: 47473ec57f7486c5d244f05a3ca3a78f317d5672
travis URL: http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.3378.pkg
From https://github.com/input-output-hk/cardano-sl
 * branch                HEAD       -> FETCH_HEAD
fetched
From https://github.com/input-output-hk/cardano-sl
 * branch                HEAD       -> FETCH_HEAD
fetched
applicationVersion is 3
/nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg
"other CI status found: continuous-integration/travis-ci/pr"
total 114K
drwxrwxrwt 58 root   root  127 Nov 16 11:31 ..
lrwxrwxrwx  1 clever users  83 Nov 16 11:31 daedalus-win64-1.0.3528.0-installer.exe -> /nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe
lrwxrwxrwx  1 clever users  75 Nov 16 11:31 Daedalus-installer-1.0.3378.pkg -> /nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg
drwx------  2 clever users   4 Nov 16 11:31 .
InstallersResults {travisResult = TravisResult {localPath = "/tmp/iohk-ops31006/Daedalus-installer-1.0.3378.pkg", travisVersion = Version {versionToText = "1.0.3378"}, cardanoCommit = "47473ec57f7486c5d244f05a3ca3a78f317d5672", travisJobNumber = "3378", travisUrl = "https://travis-ci.org/input-output-hk/daedalus/builds/298771279?utm_source=github_status&utm_medium=notification"}, appveyorResult = AppveyorResult {avLocalPath = "/tmp/iohk-ops31006/daedalus-win64-1.0.3528.0-installer.exe", avVersion = Version {versionToText = "1.0.3528"}, avUrl = "https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.3528"}, globalResult = GlobalResults {grCardanoCommit = "47473ec57f7486c5d244f05a3ca3a78f317d5672", grDaedalusCommit = "ee63efafb3f70048be2bc231e40cf73027ae3e74", grApplicationVersion = 3}}
uploading things to binary-cache
"/tmp/iohk-ops31006/Daedalus-installer-1.0.3378.pkg"
"Daedalus-installer-1.0.3378.pkg"
darwin installer /tmp/iohk-ops31006/Daedalus-installer-1.0.3378.pkg hash 95d5ef67b069c1a7e585ec098197d11944c477d054f55cec857d725130ac0a6c
"/tmp/iohk-ops31006/daedalus-win64-1.0.3528.0-installer.exe"
"daedalus-win64-1.0.3528.0-installer.exe"
windows installer /tmp/iohk-ops31006/daedalus-win64-1.0.3528.0-installer.exe hash c15d05b178197755c1bf09890b9bd181856434fd528f9a41b005bab33488fc0c
```
the tmp directory in /tmp is automatically deleted when the command exits

* set-version-json -r *revision* -c *configfile*

updates the `daedalus-latest-version.json` file on the bucket defined in <configfile> to point to the version numbers made by the last CI build on *revision*

```
[nix-shell:~/iohk/iohk-ops]$ io set-version-json -r ee63efafb3f70048be2bc231e40cf73027ae3e74 -c mainnet.yaml
From https://github.com/input-output-hk/daedalus
 * branch              HEAD       -> FETCH_HEAD
fetched
appveyor URL: https://ci.appveyor.com/api/buildjobs/jte4nscfa4uyq0ak/artifacts/installers/daedalus-win64-1.0.3528.0-installer.exe
/nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe
"other CI status found: continuous-integration/appveyor/pr"
cardano build number: 16374
cardano commit: 47473ec57f7486c5d244f05a3ca3a78f317d5672
travis URL: http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.3378.pkg
From https://github.com/input-output-hk/cardano-sl
 * branch                HEAD       -> FETCH_HEAD
fetched
From https://github.com/input-output-hk/cardano-sl
 * branch                HEAD       -> FETCH_HEAD
fetched
applicationVersion is 3
/nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg
"other CI status found: continuous-integration/travis-ci/pr"
total 90K
drwxrwxrwt 58 root   root  127 Nov 16 11:35 ..
lrwxrwxrwx  1 clever users  83 Nov 16 11:35 daedalus-win64-1.0.3528.0-installer.exe -> /nix/store/5w59irdlzx9ihnp32lw0c2pqybiy9gcr-daedalus-win64-1.0.3528.0-installer.exe
lrwxrwxrwx  1 clever users  75 Nov 16 11:35 Daedalus-installer-1.0.3378.pkg -> /nix/store/vgx2nbi7cd5r0hpdksfzci9b941k50wy-Daedalus-installer-1.0.3378.pkg
drwx------  2 clever users   4 Nov 16 11:35 .
InstallersResults {travisResult = TravisResult {localPath = "/tmp/iohk-ops2153/Daedalus-installer-1.0.3378.pkg", travisVersion = Version {versionToText = "1.0.3378"}, cardanoCommit = "47473ec57f7486c5d244f05a3ca3a78f317d5672", travisJobNumber = "3378", travisUrl = "https://travis-ci.org/input-output-hk/daedalus/builds/298771279?utm_source=github_status&utm_medium=notification"}, appveyorResult = AppveyorResult {avLocalPath = "/tmp/iohk-ops2153/daedalus-win64-1.0.3528.0-installer.exe", avVersion = Version {versionToText = "1.0.3528"}, avUrl = "https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.3528"}, globalResult = GlobalResults {grCardanoCommit = "47473ec57f7486c5d244f05a3ca3a78f317d5672", grDaedalusCommit = "ee63efafb3f70048be2bc231e40cf73027ae3e74", grApplicationVersion = 3}}
```
