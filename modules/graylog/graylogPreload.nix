{ graylogConfig, ... }:
''
# Shell shebang prepended by nix
set -e

# General Definitions
user="admin"
password="admin"
installSuccess="/var/lib/graylog/.graylogConfigured"
installLog="/var/lib/graylog/.graylogConfigured.log"
graylogApiUrl="http://localhost:9000/api"

# Content Pack Definitions
contentPack="${graylogConfig}"
cpId="$(jq -r .id $contentPack)"
cpRev="$(jq -r .rev $contentPack)"
cpComment="-d '{ \"comment\": \"Default monitoring content pack\" }'"

# Curl Definitions
curlH="curl -s -w \"\\\\napiRc: %{http_code}\" -u $user:$password -H 'X-Requested-By: $user' $graylogApiUrl"
jsonH="-H 'Content-Type: application/json'"
jsonData="-d '@$contentPack'"

# Globals
body=""
code=""
installTotal=""
installIds=""

# Functions
usage () {
  echo -e "Usage:\\n"
  echo "graylogPreload.sh [install | remove]"
  echo "  install = install default monitoring graylog content pack"
  echo "  remove  = remove default monitoring graylog content pack"
  exit 0
}

parseRsp () {
  rsp="$1"
  body="$(echo -e "$rsp" | head -n -1)"
  code="$(echo -e "$rsp" | grep apiRc | cut -f 2 -d ' ')"
}

apiUpload () {
  print "Uploading the default graylog content pack..."
  cmd="$curlH/system/content_packs $jsonH $jsonData"
  parseRsp "$(eval "$cmd")"
  print "  $body"
  print "  ApiRC: $code"
  if [[ $body == *"already found"* ]] || [[ $code == 201 ]]; then
    return 0
  else
    return 1
  fi
}

apiInstallCheck () {
  print "Checking for an installed default monitoring graylog content pack..."
  cmd="$curlH/system/content_packs/$cpId/installations"
  parseRsp "$(eval "$cmd")"
  installTotal=$(echo "$body" | jq -r .total)
  installIds=$(echo "$body" | jq -r '.installations[]._id')
  print "  $body"
  print "  ApiRC: $code"
  print "  Installs: $installTotal"
  print "  Install Ids: $installIds"
 if [[ $(echo "$body" | jq -r .total) == 0 ]] || [[ $code != 200 ]]; then
    return 1
  else
    return 0
  fi
}

apiInstall () {
  print "Installing the default monitoring graylog content pack..."
  cmd="$curlH/system/content_packs/$cpId/$cpRev/installations $jsonH $cpComment"
  parseRsp "$(eval "$cmd")"
  print "  $body"
  print "  ApiRC: $code"
  if [[ $code != 200 ]]; then
    return 1
  else
    echo "$cpId $cpRev" >> $installSuccess
    return 0
  fi
}

apiUninstall () {
  print "Uninstalling the default monitoring graylog content pack...  First checking for installations..."
  if apiInstallCheck; then
          print "  Uninstalling $installTotal installation(s)"
    for i in $installIds; do
      print "  Uninstalling installation: $i"
      cmd="$curlH/system/content_packs/$cpId/installations/$i -XDELETE"
      parseRsp "$(eval "$cmd")"
      print "  $body"
      print "  ApiRC: $code"
      if [[ $code != "200" ]]; then
        return 1
      fi
    done
  else
    print "No install found or API not confirming\\n"
    return 1
  fi
  return 0
}

apiDelete () {
  print "Deleting the default monitoring graylog content pack..."
  cmd="$curlH/system/content_packs/$cpId/$cpRev -XDELETE"
  parseRsp "$(eval "$cmd")"
  print "  $body"
  print "  ApiRC: $code"
  if [[ $code != "204" ]]; then
    return 1
  else
    rm -f $installSuccess
    return 0
  fi
}

print () {
  echo -e "$(date): $1" | tee -a $installLog
}

### MAIN

if [[ $# -ne 1 ]] || { [[ $1 != install ]] && [[ $1 != remove ]]; }; then
  usage
fi

# Install the content pack
if [[ $1 == install ]]; then

  # Check if a previous install succeeded
  if [[ -e $installSuccess ]]; then
    print "This script indicates the content pack was previously installed"
    print "If you wish to override this check, delete the following file:"
    print "  $installSuccess"
    exit 0
  fi

  # Upload the content pack
  if apiUpload; then
    print "Upload succeeded or content pack already uploaded\\n"
  else
    print "Upload failed\\n"
  fi

  # Check for an existing install of the content pack
  if apiInstallCheck; then
    print "The default monitoring content pack is already installed\\n"
    exit 0
  else
    print "No install found or API not confirming\\n"
  fi

  # Install the content pack
  if apiInstall; then
    print "Default content pack installed\\n"
    print "Created success file indicator at $installSuccess"
    exit 0
  else
    print "Unable to install default content pack\\n"
    exit 1
  fi

elif [[ $1 == remove ]]; then

  # Check if a previous install succeeded
  if ! [[ -e $installSuccess ]]; then
    print "This script indicates no content pack was previously installed"
    print "Trying to remove anyway...\\n"
  fi

  # Uninstall the content pack
  if apiUninstall; then
    print "The content pack has been uninstalled from use\\n"
  else
    print "Uninstalling failed\\n"
    exit 1
  fi

  # Delete the content pack
  if apiDelete; then
    print "The default monitoring content pack has been removed\\n"
    print "Deleting success file indicator at $installSuccess"
    exit 0
  else
    print "Unable to delete the default content pack\\n"
    exit 1
  fi
fi
''
