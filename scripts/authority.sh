#!/bin/sh
# shellcheck disable=SC2039,SC2086,SC2155

set -e

# Reference materials:
#
#   https://wiki.debian.org/Subkeys?action=show&redirect=subkeys
#   https://gnupg.org/faq/subkey-cross-certify.html
#   https://gnupg.org/ftp/people/neal/an-advanced-introduction-to-gnupg/an-advanced-introduction-to-gnupg.pdf
#
# Intended usage:
#
#  1. For the master key: prepare a secure location (USB stick?) on the file system.
#     Henceforth referred to as 'master-dir'.  This is intended for offline storage.
#
#  2. Generate the master key:
#
#         authority.sh master-dir setup-master
#
#  3. Add a subkey:
#
#         authority.sh master-dir new-signing-subkey
#
#     You will be able to list the key fingerprints with:
#
#         authority.sh master-dir show
#
#  4. For the signing subkey: prepare another secure location on the file system.
#     Henceforth referred to as 'subkey-dir'.
#
#  5. Export the subkey to the above separate location:
#
#         authority.sh master-dir export-secret-subkey-to subkey-dir SUBKEY-FINGERPRINT
#
#     The 'subkey-dir' will now contain only the subkey, and not the master key.
#     It can now be used for signing in the wild, without putting the master at risk.
#
#  5. Sign a file using the subkey store & verify the signature:
#
#         authority.sh subkey-dir   sign-with-subkey SUBKEY-FINGERPRINT FILE-TO-SIGN
#         authority.sh subkey-dir verify-with-subkey SUBKEY-FINGERPRINT FILE-TO-SIGN FILE-TO-SIGN.asc
#
#  6. Export the public keys into a separate GNUPG store for publishing:
#
#         authority.sh master-dir export-public-keys-to public-dir
#
#     ..or, alternatively, to export as an ASCII-armored key block:
#
#         authority.sh master-dir print-public-keys
#
help() {
        cat >&2 <<EOF
$(basename "$0") -- manage a GPG master/subkey setup
Usage:

     $(basename "$0") OPTIONS.. GNUPGHOME COMMAND

Options:

     --type ALGO              Default: ${ALGO}
     --length BITS            Default: ${LENGTH}
     --expiration TIMESPEC    Default: ${EXPIRATION}

     --dry                    DEBUG: dry-run mode -- '' passphrases
     --trace                  DEBUG: trace command execution
     --selftest               DEBUG: run self-tests and exit

Commands:

     show                     List all keys;  This is the default command
     list-masters             List master key fingerprints
     list-subkeys             List subkey fingerprints

     setup-master FULLNAME EMAIL
                              Setup a single master key;  None should pre-exist
     new-signing-subkey [EMAIL] [FULL-NAME] [COMMENT]
                              Create a signing subkey;  A master key should exist

     print-public-keys        Print ASCII-armored public keys
     export-public-keys-to   GNUPGHOME-TO
                              Export public keys into a new GNUPG key store
     export-secret-subkey-to GNUPGHOME-TO FINGERPRINT
                              Export a subkey into a new GNUPG key store
     sign-with-subkey FINGERPRINT FILE
                              Sign FILE with a subkey denoted by FINGERPRINT
     verify-with-subkey FINGERPRINT FILE ASC
                              Verify FILE's signature against subkey FINGERPRINT

EOF
}
op_selftest() {
        local MASTERDIR="$(mktemp -d)" SUBDIR="$(mktemp -d)" VERDIR="$(mktemp -d)" SIGTGT="$(mktemp)"
        CLEANUP[0]=${MASTERDIR}
        CLEANUP[1]=${SUBDIR}
        CLEANUP[2]=${VERDIR}
        CLEANUP[3]=${SIGTGT}
        trap atexit_rm_CLEANUP EXIT

        banner "Setting up master key:"
        $0 ${DRY:+--dry} "${MASTERDIR}" setup-master "Input Output HK Limited (test)" "info@iohk.io"

        banner "Adding a signing subkey:"
        $0 ${DRY:+--dry} "${MASTERDIR}" new-signing-subkey
        $0 ${DRY:+--dry} "${MASTERDIR}" show
        subfpr=$($0      "${MASTERDIR}" list-subkeys)
        masterfpr=$($0   "${MASTERDIR}" list-masters)

        banner "Exporting subkey to a separate keybox:"
        $0 ${DRY:+--dry} "${MASTERDIR}" export-secret-subkey-to "${SUBDIR}"  "${subfpr}"
        banner "Signing a file using the subkey from separate keybox:"
        $0 ${DRY:+--dry} "${SUBDIR}"    sign-with-subkey                     "${subfpr}" "${SIGTGT}"

        banner "Creating third-party keybox for verification:"
        $0 --dry         "${VERDIR}"    setup-master  "Third Party (test)" "third@party.xyz"
        banner "Exporting public keys to the third-party keybox:"
        $0               "${MASTERDIR}" export-public-keys-to   "${VERDIR}"
        banner "Marking imported keys as trusted:"
        GNUPGHOME="${VERDIR}" gpg --batch --pinentry-mode loopback --passphrase '""' --yes \
                                        --lsign-key "${masterfpr}"
        banner "Verifying signature using the third party:"
        $0 --dry         "${VERDIR}"    verify-with-subkey                   "${subfpr}" "${SIGTGT}" "${SIGTGT}.asc"
        echo
        echo "Self-tests passed OK."
}
CLEANUP=()
atexit_rm_CLEANUP() {
        rm -rf ${CLEANUP[*]}
}
fail() {
        echo "ERROR: $*" >&2
        exit 1
}
banner() {
        cat <<EOF
,____.
|    |
|    |
|    | $*
|    |
|    |
\`^^^^'
EOF
}

##
## Defaults
##
ALGO=RSA
LENGTH=4096
EXPIRATION=5y

##
## Subcommands
##
op_setup_master() {
        local name="$1" email="$2"
        test -n "$name" || \
                fail "setup-master requires identity full name to be specified as first argument"
        test -n "$email" || \
                fail "setup-master requires identity email to be specified as second argument"
        chmod -R go-rwx "${GNUPGHOME}"

        validate_db_nkeys 0 "No master keys are allowed to pre-exist."
        
        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --quick-generate-key "${name} <${email}>" "${ALGO}${LENGTH}" sign "${EXPIRATION}"
            
        status="$?"
        echo "GPG exit status: ${status}"
        return ${status}
}

op_add_sub() {
        validate_db_nkeys 1 "Exactly a single master key is allowed to pre-exist."

        local master_fpr="$(list_master_fprs "$GNUPGHOME")"

        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --quick-add-key "${master_fpr}" "${ALGO}${LENGTH}" sign "${EXPIRATION}"
}

op_revoke_sub() {
        local fpr="$(normalise_fpr "$@")"

        # fail "automated subkey revocation not supported.  Please see the documentation on the --edit-key GNUPG option"
        validate_db_nkeys 1 "Exactly a single master key is allowed to pre-exist."
        validate_sub_exists "${fpr}"

        gpg ${DRY:+--pinentry-mode loopback --passphrase '""'} \
                    --edit-key
}

normalise_fpr() {
        echo "$*" | sed 's/ //g'
}

op_print_public_keys() {
        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --export --armor
        echo
        echo "The above key block contains:"
        op_list_keys ${GNUPGHOME}
}

op_export_public_keys() {
        local target_dir="$1"; shift

        test -d "${target_dir}" || \
                fail "the export target needs to be a pre-existing directory"
        chmod -R go-rwx "${target_dir}"

        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --output "${target_dir}/all-public-keys" \
            --export
        GNUPGHOME=${target_dir} gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --import "${target_dir}/all-public-keys"
        GNUPGHOME=${target_dir} gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --list-keys --fingerprint --fingerprint

        masterfpr="$(list_secret_master_fprs "${GNUPGHOME}")"
        if test -n "$(list_secret_master_fprs "${target_dir}" | grep "${masterfpr}")"
        then fail "INTERNAL ERROR: exported GNUPG keydb carries the secret master key!  Call support."; fi
        if test -n "$({ list_secret_sub_fprs "${target_dir}"; list_secret_sub_fprs "${GNUPGHOME}"; } | sort | uniq -d)"
        then fail "INTERNAL ERROR: exported GNUPG keydb carries the secret subkeys!  Call support."; fi
}

op_export_secret_sub() {
        local target_dir="$1"; shift
        local fpr="$(normalise_fpr "$@")"

        validate_sub_exists "${fpr}"
        test -d "${target_dir}" || \
                fail "the export target needs to be a pre-existing directory"
        chmod -R go-rwx "${target_dir}"

        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --output "${target_dir}/secret-subkey" \
            --export-secret-subkeys "${fpr}!"
        GNUPGHOME=${target_dir} gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --import "${target_dir}/secret-subkey"
        GNUPGHOME=${target_dir} gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --list-keys --fingerprint --fingerprint

        if test -n "$(list_secret_master_fprs "${target_dir}")"
        then fail "INTERNAL ERROR: exported GNUPG keydb carries the secret master key!  Call support."; fi
        if test -z "$(list_secret_sub_fprs "${target_dir}" | grep "${fpr}")"
        then fail "INTERNAL ERROR: exported GNUPG keydb doesn't carry the intended subkey!  Call support."; fi
}

op_sign_with_sub() {
        local fpr="$(normalise_fpr "$1")" file="$2"

        validate_sub_exists "${fpr}"
        test -r "${file}" || \
                fail "the signing target should be a readable file:  ${file}"

        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --default-key "${fpr}" --armor --detach-sign "${file}"
}

op_verify_with_sub() {
        local fpr="$(normalise_fpr "$1")" file="$2" signature="$3"

        validate_sub_exists "${fpr}"
        test -r "${file}" || \
                fail "both the signed file should a be readable file:  ${file}"
        test -r "${signature}" || \
                fail "both the signature should be a readable file:  ${signature}"

        gpg ${DRY:+--batch --pinentry-mode loopback --passphrase '""'} \
            --verify "${signature}" "${file}"
}

list_secret_master_fprs() {
        GNUPGHOME="$1" gpg --list-secret \
                | grep -A1 '^sec ' | grep -v '^sec' | sed 's/ //g'
}

list_secret_sub_fprs() {
        GNUPGHOME="$1" gpg --list-secret --fingerprint --fingerprint \
                | grep -A1 '^ssb ' | grep -v '^ssb' | sed 's/ //g'
}

list_master_fprs() {
        GNUPGHOME="$1" gpg --list-keys \
                | grep -A1 '^pub ' | grep -v '^pub' | sed 's/ //g'
}

list_sub_fprs() {
        GNUPGHOME="$1" gpg --list-keys --fingerprint --fingerprint \
                | grep -A1 '^sub ' | grep -v '^sub' | sed 's/ //g'
}

validate_sub_exists() {
        local fpr="$1"
        if test -z "${fpr}"
        then fail "subkey fingerprint was not specified:  $0 --help"
        elif ! { list_sub_fprs "$GNUPGHOME" | grep "${fpr}" >/dev/null; }
        then fail "subkey doesn't exist: ${fpr}"
        fi
}

validate_db_nkeys() {
        ## The DB should have an expected number of master keys.
        local expected_nkeys="$1" failmsg="$2"
        local actual_nkeys="$(list_master_fprs "$GNUPGHOME" | wc -l)"
        if test "${expected_nkeys}" != "${actual_nkeys}"
        then cat <<EOF
ERROR: the key store was expected to have ${expected_nkeys} keys, but had ${actual_nkeys}:

$(list_master_fprs "$GNUPGHOME")

        ${failmsg}

EOF
             exit 1
        fi
}

list_keys() {
        GNUPGHOME="$1" gpg --list-secret-keys --fingerprint --fingerprint --list-options show-unusable-subkeys
}

op_list_keys() {
        list_keys "$GNUPGHOME"
        if test -n "$(list_sub_fprs "$GNUPGHOME")"
        then cat <<EOF
Cut-paste-friendly subkey FPRs:

$(list_sub_fprs "$GNUPGHOME")

EOF
        fi
}

TMPFILE=
atexit_rm_TMPFILE() {
        rm -f ${TMPFILE}
}
##
## main :: IO ()
##
DRY=
while true
do case "$1" in
           --key-algo )    ALGO="$2";          shift;;
           --key-length )  LENGTH="$2";        shift;;
           --expiration )  EXPIRATION="$2";    shift;;
           ##
           --selftest )    op_selftest; exit $?;;
           --dry )         DRY=yes;;
           --trace )       set -x;;
           --* )         echo "ERROR: unknown option:  $1">&2; help; exit 1;;
           * ) break;; esac; shift; done

GNUPGHOME="$1"; shift
test -d "${GNUPGHOME}" || {
        cat <<EOF
ERROR: the first non-option argument should be an existing directory,
       intended for GNUPG secret storage (permissions should be set accordingly).
EOF
        exit 1
}
export GNUPGHOME

cmd="$1"; shift || true
case "${cmd}" in
        show | "")                               op_list_keys;;
        list-secret-masters | smasters )
                                                 list_secret_master_fprs "$GNUPGHOME";;
        list-secret-subs | ssubs )               list_secret_sub_fprs    "$GNUPGHOME";;
        list-masters | masters )                 list_master_fprs        "$GNUPGHOME";;
        list-subkeys | subs )                    list_sub_fprs           "$GNUPGHOME";;
        setup-master | setup )                   op_setup_master           "$@"; op_list_keys;;
        new-signing-subkey | new-sub )
                                                 op_add_sub                "$@"; op_list_keys;;
        print-public-keys )                      op_print_public_keys;;
        export-public-keys-to )                  op_export_public_keys     "$@";;
        export-secret-subkey-to )                op_export_secret_sub      "$@";;
        sign-with-subkey )                       op_sign_with_sub          "$@";;
        verify-with-subkey )                     op_verify_with_sub        "$@";;
        revoke-subkey )                          op_revoke_sub             "$@";;
        * ) help; exit 1;; esac
