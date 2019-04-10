{ config, resources, pkgs, lib, ... }:

let
  cfg = config.hydra;
in {
  services.hydra.extraConfig = with pkgs; lib.mkIf (cfg.s3Bucket != null) ''
    <runcommand>
      job = *:*:docker-image
      command = ${writeScript "docker-push-s3" ''
        #!${stdenv.shell}

        set -eo pipefail

        PATH=${lib.makeSearchPath "bin" [ awscli coreutils gnutar gzip jq ]}

        # Sample contents of $HYDRA_JSON:
        # {
        #   "drvPath": "/nix/store/24yiwqfanmskmalh25s449nzdnwxaqnf-docker-image-hello-kitty.tar.gz.drv",
        #   "stopTime": 1552617197,
        #   "job": "docker-image",
        #   "buildStatus": 0,
        #   "timestamp": 1552615367,
        #   "outputs": [
        #     {
        #       "path": "/nix/store/1asfwyb9ggjxbxhdwxydv4cx79nha46n-docker-image-hello-kitty.tar.gz",
        #       "name": "out"
        #     }
        #   ],
        #   "finished": 1,
        #   "products": [],
        #   "project": "hello-kitty",
        #   "jobset": "master",
        #   "startTime": 1552617197,
        #   "metrics": [],
        #   "event": "buildFinished",
        #   "build": 7
        # }

        # extract data we care about from $HYDRA_JSON into environment variables
        eval "$(jq -r '@sh "output=\(.outputs[].path) build_status=\(.buildStatus) job=\(.job) jobset=\(.jobset) project=\(.project)"' $HYDRA_JSON)"

        if [[ $build_status != 0 ]]; then
          echo "Build failed; push skipped"
          exit $build_status
        fi

        # $output is `/nix/store/00000000000000000000000000000000-docker-image-packageName.tar.gz`
        # in this case we want it uploaded to S3 as `packageName.tar.gz` because the bucket is dedicated to 
        # docker images (and named accordingly); therefore leaving the `docker-image-` prefix is redundant.
        # Also note that `packageName` refers to the `name` attribute of the package derivation, which includes the
        # version. That's in contrast to the docker image derivation whose version goes in the `tag` attribute.
        target=s3://${cfg.s3Bucket}/$(tar --file $output --gunzip --extract --to-stdout manifest.json | jq -r '.[].RepoTags[]'    | head -n1               | tr : -   ).tar.gz
        #                                                                                                      ^- package:version   ^- only take the first   ^- package-version
        echo "Pushing '$output' to '$target'"
        trap "echo Push failed 🤬" ERR
        aws s3 cp --only-show-errors $output $target
        echo "Push successful"
        ''}
    </runcommand>
  '';
}
