# IOHK GPG signing authority usage guidelines

# Design goals

Establish a GPG authority, to facilitate publicly-verifiable attribution of
artifacts distributed by IOHK. Since compromise of such an authority entails
reputation risks, security is a major consideration.

# Requirements

1.  The solution needs to allow maximum security for the root-of-trust, yet still
    allow for on-line, and potentially automated usage.
1.  Compromised elements of the authority need to be replaceable (this does not
    apply to the root of trust).
1.  Organisational separation of concerns between the **_root-of-trust operator_**
    and **_artifact signing parties_** needs to be supported.

# Design

1.  The requirements can be facilitated with a master/subkey GPG scheme, where the
    master key plays the role of the root of trust, and subkeys can be used for
    signing of the actual artifacts.
1.  The master key is to be held securely off-line, and is only used for subkey
    signing and revocation.
1.  As a consequence tooling needs to support separate GPG keyboxes and external
    storage.
1.  The subkeys can be revoked through publishing of the GPG revocation
    certificate merged with the subkey, on the public GPG keyservers.
1.  All of the keys must be protected with the GPG passphrase encryption.

# Proposed solution

## Prerequisites

1.  `gnupg` v2 installed
1.  The `authority.sh` script ([https://raw.githubusercontent.com/input-output-hk/iohk-ops/serge/authority/scripts/authority.sh](https://raw.githubusercontent.com/input-output-hk/iohk-ops/serge/authority/scripts/authority.sh)).
1.  Two empty, POSIX-FS-formatted (`ext4` for example) USB sticks intended for
    secure off-line storage of the GPG master key.
1.  Strong passwords for the key encryption.

## Tooling

The solution is based on an automation script called `authority.sh`, which is to
be used by both the **_root-of-trust operator_** and the **_artifact signing
parties_**.

The general synopsis of the command is:

    authority.sh [OPTIONS..] <GNUPGHOME> SUBCOMMAND [SUBOPTIONS..]

That is, every `authority.sh` invocation takes the GNUPG "workspace" directory as
an explicit argument.  This is intended to support the explicit separation of GPG
keyboxes.

The `authority.sh --help` carries a helpful summary of commands and options.

## Workflow: root-of-trust operator

### Master key storage

The master GPG key designates the root of trust -- its exposure would enable a
third party to produce subkeys signed with the master key, and therefore to
effectively subvert the signing authority, as these keys would appear to be
trusted by IOHK.  For this reason, the master key is intended for offline use
only.

The GNUPG workspace directory for master key storage (henceforth referred to as
`<MASTER-DIR>`) should be prepared as a USB stick mount (importantly, the USB
stick filesystem should be POSIX-compatible, for example `ext4`). Special caution
is advised not to allow it to touch the filesystem outside that USB stick mount
directory.

At the end of every key management session, the USB stick should be unmounted, and
backed up to the second USB stick (using some disk mirroring tool). The two USB
sticks should then be stored at separate locations.  The loss of the master key
would make it impossible to both produce new subkeys, and to revoke any
compromised subkeys.

### Preparation

Root-of-trust initialisation (`<MASTER-DIR> `needs to reside in a secure
file-system location, such as a directory on a mounted, ext4-formatted USB stick
intended for off-line storage):

    authority.sh [OPTIONS..] <MASTER-DIR> setup-master "Full Name" email@address

The master key parameters can be specified as options to `authority.sh`:

- `--algo` (default is `RSA`)
- `--length` in bits (default is `4096`)
- `--expiration` for the key duration (default is `5y`)

### Subkey operations

1.  Subkey issuance:

	`authority.sh <MASTER-DIR> new-signing-subkey`

    Subkey parameters can be specified the same way

1.  Listing subkeys (with fingerprints):

	`authority.sh <MASTER-DIR> show`

1.  Exporting the subkey into a separate GPG keybox to facilitate handover:

	`authority.sh <MASTER-DIR> export-secret-subkey-to <SUBKEY-DIR> <SUBKEY-FINGERPRINT>`

1.  Due to GNUPG pecularities, subkey revocation is not supported in an automated
    way, so has to be performed manually:
    1.  `GNUPGHOME=<MASTER-DIR> gpg --edit-key email@address` will start an
        interactive environment
    1.  the subkey to be revoked needs to be selected, using the `key` command
        with the subkey index (first subkey has index `1`)
    1.  The subkey is then to be marked for revocation with the `revkey` command.
    1.  Save and exit the key edit dialog with the `save` command.
    1.  You can ensure that the subkey is marked for revocation:

    	`authority.sh <MASTER-DIR> show `will display the subkey status as `revoked`:

    ```
          sub   rsa4096 2018-04-26 [S] [revoked: 2018-04-26]
            A93E 066E EDEB 968A 419D  12A1 0CC3 3F5C 75C5 D216
    ```

    1.  The last step is to announce revocation to the wide world (optionally
        specifying `--keyserver` of preference):

         `GNUPGHOME=<MASTER-DIR> gpg --send-keys`

### Publishing the keys

The public part of the keychain can be exported in two ways -- as a GPG keybox (to
allow convenient handover to a third party), and as an ASCII-armored keychain:

1.  `authority.sh <MASTER-DIR> export-public-keys-to <PUBLIC-DIR> `
1.  `authority.sh <MASTER-DIR> print-public-keys`

### Master key revocation

In the event of master key compromise (the worst case scenario), it needs to be
revoked.  This procedure is not automated, and is as follows:

1. Determine the master key fingerprint: `authority.sh <MASTER-DIR> show`
1. Generate the revocation certificate: `GNUPGHOME=<MASTER-DIR> gpg --gen-revoke <MASTER-FINGERPRINT> --output master-key.rev`
1. Attach the revocation certificate to the master key: `GNUPGHOME=<MASTER-DIR> gpg --import master-key.rev`
1. Publish the revoked key:
    1. To the keyservers: `GNUPGHOME=<MASTER-DIR> gpg --send-key <MASTER-FINGERPRINT>`
    1. Extract the revoked key and send to partners: `GNUPGHOME=<MASTER-DIR> gpg --armor --export <MASTER-FINGERPRINT> --output master-public-revoked.asc`

**NOTE**: an important consequence of the above is that in the event of master key
loss, it becomes impossible to revoke it.  This is one of the chief motivators for
using two replicated USB sticks for master key storage.

## Workflow: artifact signing party

The **_artifact signing party_** deals with signing and signature publishing:

1.  Actual signing: `authority.sh <SUBKEY-DIR> sign-with-subkey <SUBKEY-FINGERPRINT> <ARTIFACT>`
1.  The detached, ASCII-armored signature can then be found in `<ARTIFACT>.asc`

## Testing

The self-test mode of `authority.sh` can be invoked with:

    authority.sh --dry --selftest
