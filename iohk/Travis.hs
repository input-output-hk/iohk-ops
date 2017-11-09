{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Travis where

import           Control.Monad
import           Data.Aeson           (FromJSON, defaultOptions,
                                       genericParseJSON, (.:))
import qualified Data.Aeson           as AE
import           Data.Aeson.Types     (fieldLabelModifier)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HashMap
import           Data.Maybe
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import qualified Data.Yaml            as Y
import           GHC.Generics         (Generic)
import           Utils                (fetchJson)

type BuildId = T.Text
type JobId = Integer
type BuildNumber = Integer
type RepoId = T.Text

data TravisBuild = TravisBuild {
    number   :: T.Text
    , matrix :: [ TravisJobInfo ]
    } deriving (Show, Generic)

data TravisJobInfo = TravisJobInfo {
    tjiId :: JobId
    } deriving (Show, Generic)

data TravisInfo2 = TravisInfo2 {
    ti2state    :: String
    , ti2commit :: T.Text
    } deriving (Show, Generic)

data TravisYaml
  = TravisYaml
  {
    language :: T.Text
    , sudo   :: Bool
    , env    :: TravisYamlEnv
  }
  deriving (Show, Generic)

data TravisYamlEnv
  = TravisYamlEnv
  {
    global :: [ TravisEnvEntry ]
  }
  deriving (Show, Generic)

data TravisEnvEntry
  = TravisEnvPlain T.Text T.Text
  | TravisEnvSecure Y.Value
  deriving (Show, Generic)

instance FromJSON TravisBuild
instance FromJSON TravisJobInfo where
    parseJSON = AE.withObject "TravisJobInfo" $ \v -> TravisJobInfo
        <$> v .: "id"
instance FromJSON TravisInfo2 where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }
instance FromJSON TravisYaml
instance FromJSON TravisYamlEnv
instance FromJSON TravisEnvEntry where
    parseJSON value = case value of
      Y.String txt -> do
        let
          (key, value) = T.breakOn "=" txt
        pure $ TravisEnvPlain key (T.drop 1 value)
      Y.Object obj -> case HashMap.lookup "secure" obj of
        Nothing -> fail "no secure in env"
        Just x  -> pure $ TravisEnvSecure x

lookup' :: T.Text -> [TravisEnvEntry] -> Maybe T.Text
lookup' key = do
  let
    f3 :: T.Text -> TravisEnvEntry -> Maybe T.Text
    f3 needle (TravisEnvPlain key value) = guard (needle == key) *> Just value
    f3 needle _                          = Nothing
  listToMaybe . mapMaybe (f3 key)

fetchTravis2 :: RepoId -> BuildNumber -> IO TravisInfo2
fetchTravis2 repo buildNumber = do
  results <- fetchJson $ "https://api.travis-ci.org/builds?number=" <> (T.pack . show $ buildNumber) <> "&slug=" <> repo
  return $ head results

fetchTravis :: BuildId -> IO TravisBuild
fetchTravis buildId = fetchJson $ "https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/" <> buildId

sampleTravisYml :: LBS.ByteString
sampleTravisYml = "language: nix\nsudo: false\nmatrix:\n  include:\n  - os: linux\n  - os: osx\n    osx_image: xcode8.3\nenv:\n  global:\n    - VERSION=0.6\n    - CARDANO_SL_BRANCH=cardano-sl-0.6\n      # NOTE: when bumping nixpkgs, also update default.nix\n    - NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/48ecdcf5980a6504cd3b884b121e29efb2fb83dc.tar.gz\n    # AWS access key\n    - secure: \"Gvw61r5B2Noo263owgZIAw8Jvl2IU1ZSCsKXbsW9lNjPeFWvoEWxHhmTURO25lIYm3iz5gEkx6AqRB3wRADfBAejizKtP1LHQMJJ8E1VvNUJLr+2pOSrR098k3yO8R5rdxFkzNnMLVbsUKTnpAA9kbCvj0AcnnUOAm0eDkjO/wCfU1Joz5pSVqxJ5xdQUd/hosMoeR/SurjdKYxwjLVNrlZe0IUejuPIPda92511hFpzbjZ4fxObcyPJwVCMgymn46ICSZ4K8WKyZfwkWvBMG6zFJLaaeAb8vxHBwqtpftURD6hiy4zL+94j2yJbyllWA0aeqAB9rVDOnJnyqxdfkeLa4SCte4DyRTyW4CicYHXlLPtdXY/Ek6rIHYuRcSF+S/zEFJj3/V7VZXRHrZGyc4Ua08Hr7bHfmHHtf9xhmayP+FOkrNmfVxzMHQjVc4W/+gD0gzzGGwk7J8wOzRtSwnKoGBGeRCvNXUtK3IwnecY+XD1qoeRL2Yil+8TxyWw0iMigHB+WoBPbloX6W1uIn67MAxvWKpdi8/h9lnC0c00wCW0ZIq9//OSvRWWBGzZ6I5zofzjNBetyIHDz+77k0pkVkdrqaF99wQRaDJBXGZ8DfBWv+XDRLpDPzy1tZwv3Ukh8Cg05f1HODCazT3jY0S7ZDCCcELwIg/0PapYU4Mc=\"\n    # AWS secret key\n    - secure: \"PG0QbtFLEwRLdegn7yrHYhz36UuVm8AwypQ5ai5yvrTkFJQwx0pJoPF0483zLuXouDNMdkGy4w6Fo6o+XAuBylBheduuCYR0s0TmQmsCs5r+Yc+PsNVZVH5YTkgGXXrE5+mtZuUwSmACP6fEM48SBbJd5wXWPphIF6WfxiHxwFS+EhqVDXIFobcyYxgzSulfacB1mEtvsHS+o67HVMifAUJ4qrTRuE085/3oP2e+sdKuZleAI6tEsTQ6q4AkjxMiSYG3YLvNv4ZxMbtrGjPKkXBEG+KUfanCbfj6Lup0oFbfP2QDJAY2qT+1J6o3JZXaBvYJxnFivkv1YQrXCKas8z8DcFG/jdNBtR8JGTw/IB/Q8vHY0+jEPlrfzyPNMsgfiDSw7/WjuqqMjZPaZ8at/XDLmFkZeIcXt0syhqeF6Gc+qYd9F9I+f1HFvf9OePrBYmi/1EXC+vbnwlVGqcYKmXBoVdwQTvxth1MtnHfQCqwJd/3qBdNdT9aeT1/g8cni2s9ZDUV3l1F8MdCg4aJYSvKg9Phgy2lFrpkFOcQykb31yn8NDuyTv+4QXE+f8OMsrMq7UiH7W1WFuv94camff9HW/hZBVcgFzRmoTw/UW3ea4wYvbyJrvRAowuqDBvjRbYC5Ig7dDMg9eDlmISdf7WnQ6XN4wEJLXQd7nsloPyo=\"\n    # mac p12 cert\n    - secure: \"fKdY/PTgvC6sY+bzE8KLs5oDQ5xmsehl6k57BxUkcNZpaOFLttCqRJP3MwTWxywM3bNPN4DgkKnfyuyMZYTJmj51zPz3e6Loxbs55dVHmWrJNeOJfLiJJaWf15561WWSoM4DcDNNIHdpjC5USwWAdqVJFa3WC8nm0o8s+ybd0va520iMWSGpJxGz82WB9gNKyPGK7xAOx0p0dhh7xACgzM3QKulr7XFY1QSpFdWerKjEs4LaDtclvgIV1+NZEygTOdWlPGBXaiGRmnPabyVFl5dgZqCDLAh82FIP6PhbLhNzAqzgrP/ZzBO+n90opAAUzb+j9x/PoT9aI6y+5iFnyi9Xdwrn2Yg6h2IG3QfxSNb25FwKKosyKBYzgxoVet5L2l8PSSyACBvPSYQUHhAV8az9h66y73qjfVRN2oW3SgYQ27IZMLUfT88ttM/ndzgLqnHOAa7BDBm5LOFqyRYmBO7Gm3YUzWrMe1R+HQQYdZC0yy8ALY/wZ4j2CXoE+AitZXPY/KL+pA1ZCyHF3VFx2G/CtuKj3HvFZNUT0tPJR2KqKZ/9hsgF4vDKiMnF+ESTYA5OFB4Eh0DNsSaq7TqDwPyJ95DufiIa+n1kB9lIG2FBC5GHQogpaSPcNNsVXs65mrwQJMJMgdPWlTOUx9KWu9IBOPsTF7MnUT3+RjJKKnA=\"\ninstall:\n- true\nscript:\n- sudo mount -o remount,exec,size=4G,mode=755 /run/user || true    ## Travis-specific hack.\n- nix-env -iA nixpkgs.bashInteractive\n- scripts/build-installer-unix.sh \"$VERSION\" \"$CARDANO_SL_BRANCH\"\n     --build-id       \"$TRAVIS_BUILD_NUMBER\"\n     --travis-pr      \"$TRAVIS_PULL_REQUEST\"\n     --nix-path       \"$NIX_PATH\"\nnotifications:\n  slack:\n    secure: hfzreJ033RxFI2UYtcGJ+MFgQX08UyUnQwhb8X8CuVuIZXDWhxz9JLMEUuL6QqogtffbiqfqIhyDwZ0PsE3aEJaHQAQ7lxJkMjZFU/o+CYvHBQy+WYCa/sFM0C8cEXn3Wb/LmhjvtKAgF302tnD5PAPglZTGtCJHpihKG/Wc+YwF2aFSon0DqoMtVmwVPvhD/WlAHyszARsE9C7jH7jN43gwupAmwj7QLH+kd96HKrrc2ItgMS0f5ucFnpM900c9LwhE5StxVdgTXLnogWQKcd1f4DwTNoXfS3Dy6VJkKj049VScvavcFcVPBL9xU62SMMy/eS/yRcHZkxJehUi16aoir2GXNZOKjQVQ85zEQofQgkR+b4roJJmtWJuCoyE0nptoHgDi4BlLwFuC1g7pkngtLvo3BrG3IjhV+s7QuhxFA9aA8MBQDTRBQNeZdU2I/iwJaYsggGQ2JYA/B2AE4C0sentbegGumsY4fah6HVQGOOZoCl3k9fmMDS42/D55kdteRAQBZBlixLHMXTsMf90jcJJGi/vkzVEnhby63RdTRNDW6MBA6RgvUIpI0VQgAIEM8LFHMNeHMYLoj++NnSI3gCrX8zi8nrnCUJmpCQ89WLDRbVy3rFltgcd7ds8SlRxMtSXCO9lulq/Q6Gk65mRhuq/My0FqBbT+DQUBg04=\ndeploy:\n  provider: s3\n  access_key_id: AKIAJKSJR3FUN3CWKC5Q\n  secret_access_key:\n    secure: KL0IpBTLy54CC6w48E7t19uEME1lcdWLZ1RP0qz7jm9A0J58RnBn4VqQL86vXZZZF3L6s4BM5bsD+mOXQ88ePzL4UNL0swJijFI4jh1cv5RsOpoFJc5ULxQWm/ybGqKt2I9kF0TRwI23ilKkxz531cRBmoIbKSDEbDr8Tn7n1gU9ubhAJTI0/Gy1jnrVwQejoZMTEpJJcti1o046wJd0GEJpA88dXPjvi3WXMBcQ1acRxd5tkEjLooScD7CijdUgHTGZC/HvRLY+6SdI8pHaB8/yhli87MBODqpdMstV9zPZI2bJiUDPNaFL+goGA1+4CQnmfyBNfHijDnuBMNTUcj00WHCMJcuLcXM42fli6gO2iivNEe8pz+3Jvz3142hJL8seHm9//QDsGZg7HUYYiPGbDSmug8knh9Zsk8QZUtxlXBNvQqZe2Q289cGzW9Hoo+tNgCvpaUWkBNe/XUXTI2HoE5XkrziRwyVLaXj5Yz7ncjMkwIljrLCxuDS3dT53lwxK7NkLmWovp8r0gL/8szoqTMdKLPNQXOa9dknlW9L905PKOjON1F5SCHc0JKREnQQFNrbx4X1bjVa++Fv6Ix42XBvn7h2MT3NorjSlvyKwBTUeQf5bYD6603Q9YCqsZgEnG0uB3yWvgFXQ0PKH/5htjBwWWQJ4PxLe+mhZ9Jw=\n  bucket: daedalus-travis\n  region: eu-central-1\n  local_dir: installers/dist\n  skip_cleanup: true\n  acl: public_read\n  on:\n    all_branches: true\n"

parseTravisYaml :: LBS.ByteString -> Either String TravisYaml
parseTravisYaml = Y.decodeEither . LBS.toStrict
