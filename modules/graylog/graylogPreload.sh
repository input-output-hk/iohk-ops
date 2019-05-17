# Shell shebang prepended by nix
set -e

# General Definitions
installSuccess="/var/lib/graylog/.graylogConfigured"
installLog="/var/lib/graylog/.graylogConfigured.log"

# Content Pack Definitions
contentPack=""                         # Content pack file, passed as arg 2
cpName=""                              # Must parse to "monitorContentPack" for proper script logic
cpId=""                                # Content pack name as parsed from the content pack file
cpVer=""                               # Content pack version as parsed from the content pack file
cpRev=""                               # Content pack revision as parsed from the content pack file
cpComment="monitorContentPack"         # Must remain set to "monitorContentPack" for proper script logic
cpVendor="IOHK"                        # Must remain set to "IOHK" for proper script logic

# Curl Definitions
user="@user@"
password="@password@"
graylogApiUrl="http://localhost:9000/api"
curlH="curl -s -w \"\\\\napiRc: %{http_code}\" -u $user:$password -H 'X-Requested-By: $user' $graylogApiUrl"
jsonH="-H 'Content-Type: application/json'"
jsonComment="-d '{ \"comment\": \"$cpComment\" }'"
jsonData=""                            # Set dynamically based on the contentPack variable

# Other globals, used dynamically
cmd=""                                 # Command string used dynamically for evaluations
rsp=""                                 # Generic API body response plus return code
body=""                                # Parsed JSON response body from last API call
code=""                                # HTTP(S) code response from last API call
flagScriptInstalled="false"            # Flag to show if the success file shows the content pack is installed
flagApiLoaded="false"                  # Flag to show if the Api indicates the content pack is loaded
flagApiInstalled="false"               # Flag to show if the Api indicates the content pack is installed
installTotal=""                        # Content pack installed quantity, integer
installIds=""                          # Content pack install _id list, \n separated
preCpName=""                           # Pre-existing default monitoring content pack name, if any
preCpId=""                             # Pre-existing default monitoring content pack id, if any
preCpVer=""                            # Pre-existing default monitoring content pack version, if any
preCpRev=""                            # Pre-existing default monitoring content pack revision, if any
loadedCpNames=""                       # Array with the loaded content pack names
loadedCpVendors=""                     # Array with the loaded content pack vendors
loadedCpIds=""                         # Array with the loaded content pack ids
loadedCpVers=""                        # Array with the loaded content pack versions
loadedCpRevs=""                        # Array with the loaded content pack revisions
i="0"                                  # Looping variable
j="0"                                  # Looping variable
deleteCpId=""                          # Function parameter
uninstallCpId=""                       # Function parameter
uninstallTotal=""                      # Total installations to purge for a given content pack id

# Functions
usage () {
  echo -e "Usage:\\n"
  echo "graylogPreload.sh <install | remove> <content_pack>"
  echo "  install      = install default monitoring graylog content pack"
  echo "  remove       = remove default monitoring graylog content pack"
  echo "  content_pack = path to content pack to install or remove"
  echo ""
  echo "Comments:"
  echo "This script will install or remove one content pack as specified"
  echo "by the content pack definition variables in the script and cli args."
  echo "If the specified content pack if not loaded and installed into"
  echo "graylog, this script will do so.  If the content pack is already"
  echo "loaded into graylog, it will not reload or perform an additional"
  echo "installation.  If the specified content pack is found to already"
  echo "be installed in graylog, but the version, revision, or id of the"
  echo "existing installation do not match the content spec defined in"
  echo "the script, the existing installation will be removed and"
  echo "replaced with the version defined in the script."
  echo ""
  echo "The script determines content pack installation status by"
  echo "searching for a default content pack name, comment and vendor of:"
  echo "name=\"monitorContentPack\", comment=\"monitorContentPack\", vendor=\"IOHK\""
  echo ""
  echo "These key-value definitions must stay static, otherwise, the"
  echo "script logic will no longer work."
  exit 1
}

parseRsp () {
  rsp="$1"
  body="$(echo "$rsp" | head -n -1)"
  code="$(echo "$rsp" | grep apiRc | cut -f 2 -d ' ')"
}

apiCpLoadedCheck () {
  print "Checking for a loaded default monitoring graylog content pack reported by API based on name"
  cmd="$curlH/system/content_packs/"
  parseRsp "$(eval "$cmd")"
  if [[ $code != 200 ]]; then
    print "Failed to retrieve the loaded content packs by API, rc $code"
    return 1
  fi
  loadedTotal="$(echo "$body" | jq -r .total)"
  if [[ $loadedTotal == 0 ]]; then
    print "No content packs are loaded"
    return 0
  fi
  mapfile -t loadedCpNames < <(echo "$body" | jq -r '.content_packs[].name' )
  mapfile -t loadedCpVendors < <(echo "$body" | jq -r '.content_packs[].vendor')
  mapfile -t loadedCpIds < <(echo "$body" | jq -r '.content_packs[].id')
  mapfile -t loadedCpVers < <(echo "$body" | jq -r '.content_packs[].v')
  mapfile -t loadedCpRevs < <(echo "$body" | jq -r '.content_packs[].rev')
  for (( i=0; i < loadedTotal; i++ )) do
    print "  Evaluating content pack: \"${loadedCpNames[$i]}\""
    if [[ ${loadedCpNames[$i]} == "$cpName" ]] && [[ ${loadedCpVendors[$i]} == "$cpVendor" ]]; then
      print "    Found default: \"${loadedCpNames[$i]}\" Vendor: \"${loadedCpVendors[$i]}\" Id: \"${loadedCpIds[$i]}\" Ver: \"${loadedCpVers[$i]}\" Rev: \"${loadedCpRevs[$i]}\""
      if [[ ${loadedCpIds[$i]} == "$cpId" ]] && [[ ${loadedCpVers[$i]} == "$cpVer" ]] && \
        [[ ${loadedCpRevs[$i]} == "$cpRev" ]]; then
        print "    The loaded default monitoring content pack is already the correct spec, Id: \"$cpId\", Ver: \"$cpVer\", Rev: \"$cpRev\""
        flagApiLoaded="true"
      else
        print "    The loaded default monitoring content pack is not the expected:"
        print "      Expected Id: \"$cpId\" Ver: \"$cpVer\" Rev: \"$cpRev\""
        print "      Found Id:    \"${loadedCpIds[$i]}\" Ver: \"${loadedCpVers[$i]}\" Rev: \"${loadedCpRevs[$i]}\""
        print ""
        print "Removing old default monitoring content pack with Id: \"${loadedCpIds[$i]}\" Ver: \"${loadedCpVers[$i]}\" Rev: \"${loadedCpRevs[$i]}\""
        if apiUninstall "${loadedCpIds[$i]}"; then
          apiDelete "${loadedCpIds[$i]}" || return 1
        else
          print "Failed to parse loaded content packs"
          return 1
        fi
      fi
    fi
  done
  print ""
  return 0
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
  print "Checking for an installed version of the specified content pack..."
  cmd="$curlH/system/content_packs/$cpId/installations"
  parseRsp "$(eval "$cmd")"
  installTotal="$(echo "$body" | jq -r .total)"
  if [[ $code != 200 ]]; then
    print "Failed to parse the existing installations for content pack id: $cpId"
    return 1
  elif [[ $installTotal == 0 ]]; then
    print "Existing installation not found for content pack id: $cpId"
  else
    mapfile -t installIds < <(echo "$body" | jq -r '.installations[]._id')
    if [[ $installTotal == 1 ]]; then
      print "Verified one default monitoring content pack installation:"
      print "  Install id: \"${installIds[0]}\""
      flagApiInstalled="true"
    else
      print "Multiple installations, $installTotal, found for the default monitoring content pack:"
      for (( i=0; i < installTotal; i++ )); do
        print "  Install id: \"${installIds[$i]}\""
      done
      print "Uninstalling the installations to ensure the proper install is the one in use"
      apiUninstall "$cpId" || return 1
    fi
  fi
  return 0
}

apiInstall () {
  print "Installing the default monitoring graylog content pack..."
  cmd="$curlH/system/content_packs/$cpId/$cpRev/installations $jsonH $jsonComment"
  parseRsp "$(eval "$cmd")"
  print "  $body"
  print "  ApiRC: $code"
  if [[ $code != 200 ]]; then
    return 1
  else
    echo "$cpName $cpId $cpVer $cpRev" > $installSuccess
    return 0
  fi
}

apiUninstall () {
  uninstallCpId="$1"
  print "Uninstalling content pack with Id: $uninstallCpId"
  cmd="$curlH/system/content_packs/$uninstallCpId/installations"
  parseRsp "$(eval "$cmd")"
  uninstallTotal="$(echo "$body" | jq -r .total)"
  if [[ $code != 200 ]]; then
    print "Error retrieving content pack installations for Id: $uninstallCpId"
    return 1
  elif [[ $uninstallTotal == 0 ]]; then
    print "No content pack installations for Id: $uninstallCpId, proceeding with deletion."
  else
    mapfile -t uninstallIds < <(echo "$body" | jq -r '.installations[]._id')
    for (( j=0; j < uninstallTotal; j++ )); do
      print "Uninstalling content pack Id: $uninstallCpId, install Id: ${uninstallIds[$j]}"
      cmd="$curlH/system/content_packs/$uninstallCpId/installations/${uninstallIds[$j]} -XDELETE"
      parseRsp "$(eval "$cmd")"
      print "  $body"
      print "  ApiRC: $code"
      if [[ $code != "200" ]]; then
        print "Failed uninstall of install Id: ${uninstallIds[$j]}"
        return 1
      fi
    done
  fi
  return 0
}

apiDelete () {
  deleteCpId="$1"
  print "Deleting the content pack with Id: $deleteCpId"
  cmd="$curlH/system/content_packs/$deleteCpId -XDELETE"
  parseRsp "$(eval "$cmd")"
  print "  $body"
  print "  ApiRC: $code"
  if [[ $code != "204" ]]; then
    print "Failed to delete content pack with Id: $deleteCpId"
    return 1
  fi
  return 0
}

print () {
  echo -e "$(date): $1" | tee -a $installLog
}

### MAIN

# Process command cli args and preload content pack required key values
if [[ $# -ne 2 ]] || { [[ $1 != install ]] && [[ $1 != remove ]]; }; then
  usage
elif ! [[ -r $2 ]]; then
  echo "Content pack file specified does not exist or cannot be read: \"$2\""
  exit 1
else
  contentPack="$2"
  # cpName must parse to "monitorContentPack" for proper script logic
  cpName="$(jq -e -r .name "$contentPack")" || { print "Failed to obtain cpName from <content_pack> file"; exit 1; }
  cpId="$(jq -e -r .id "$contentPack")"     || { print "Failed to obtain cpId from <content_pack> file"; exit 1; }
  cpVer="$(jq -e -r .v "$contentPack")"     || { print "Failed to obtain cpVer from <content_pack> file"; exit 1; }
  cpRev="$(jq -e -r .rev "$contentPack")"   || { print "Failed to obtain cpRev from <content_pack> file"; exit 1; }
  jsonData="-d '@$contentPack'"
  print "Specified content pack to install or remove is:"
  print "  Path: \"$contentPack\""
  print "  Name: \"$cpName\", Id: \"$cpId\", Ver: \"$cpVer\", Rev: \"$cpRev\""
  print ""
fi

# Load any pre-existing state info
print "Checking script history for a pre-existing installed default monitoring content pack..."
if [[ -r $installSuccess ]]; then
  flagScriptInstalled="true"
  preCpName="$(cut -f 1 -d ' ' $installSuccess)"
  preCpId="$(cut -f 2 -d ' ' $installSuccess)"
  preCpVer="$(cut -f 3 -d ' ' $installSuccess)"
  preCpRev="$(cut -f 4 -d ' ' $installSuccess)"
  print "  Found, Name: \"$preCpName\", Id: \"$preCpId\", Ver: \"$preCpVer\", Rev: \"$preCpRev\""
else
  print "  No script history for a pre-existing default monitoring content pack was found"
fi
print ""

# Install the content pack
if [[ $1 == install ]]; then

  # Check for the proper definition of the default content monitoring pack
  if [[ $cpName != "monitorContentPack" ]] || [[ $cpComment != "monitorContentPack" ]]; then
    print "Warning: the new content pack or script config does not appear"
    print "to be valid as the content pack JSON name or comment is not as expected:"
    print "Expected content pack name and comment: \"monitorContentPack\""
    print "Actual content pack name: \"$cpName\""
    print "Script defined content pack comment: \"$cpComment\""
    print ""
    print "The default monitoring content pack should utilize the expected name"
    print "and comment above so that this script can differentiate it from other"
    print "user applied content packs which should not be modified by"
    print "this script.  Please correct the comment and re-run this script."
    exit 0
  fi

  # Parse the state info for a pre-existing default content pack installation
  if [[ $flagScriptInstalled == "true" ]] && [[ $preCpId == "$cpId" ]] && [[ $preCpVer == "$cpVer" ]] \
    && [[ $preCpName == "$cpName" ]] && [[ $preCpRev == "$cpRev" ]]; then
    print "This script indicates this content pack was previously installed."
    print "If you wish to re-install, please uninstall the old pack first"
    print "with the \"remove\" script argument."
    exit 0
  elif [[ $flagScriptInstalled == "true" ]] && [[ $preCpName == "$cpName" ]]; then
    print "A different version of the default monitoring content pack is installed:"
    print "Specified New Id: \"$cpId\", Ver: \"$cpVer\", Rev: \"$cpRev\""
    print "Pre-existing Id:  \"$preCpId\", Ver: \"$preCpVer\", Rev: \"$preCpRev\""
    print ""
  fi

  # Process any loaded content packs for possible other version collisions
  # Remove other conflicting content packs as needed
  print "Checking graylog API responses for a pre-existing loaded default monitoring content pack based on name..."
  if ! apiCpLoadedCheck; then
    print "Failed to complete the install request"
    exit 1
  fi

  # Load the content pack if the correct pack is not already present
  if [[ $flagApiLoaded != "true" ]]; then
    if apiUpload; then
      print "Content pack upload succeeded or content pack already uploaded"
    else
      print "Content pack upload failed"
      exit 1
    fi
  fi
  print ""

  # Check for an existing install of the content pack by API
  print "Checking graylog API responses for a pre-existing installed default monitoring content pack..."
  if ! apiInstallCheck; then
    print "Failed to properly check for the correct installed content pack"
    exit 0
  fi

  # Install the content pack
  if [[ $flagApiInstalled != "true" ]]; then
    if apiInstall; then
      print "Default content pack installed\\n"
      print "Created success file indicator at $installSuccess"
      exit 0
    else
      print "Unable to install default content pack\\n"
      exit 1
    fi
  fi
  exit 0

elif [[ $1 == remove ]]; then

  # Check if a previous install succeeded
  if ! [[ -e $installSuccess ]]; then
    print "This script indicates no content pack was previously installed"
    print "Trying to remove anyway...\\n"
  fi

  # Uninstall the content pack
  if ! apiUninstall "$cpId"; then
    print "Uninstalling failed\\n"
    exit 1
  fi

  # Delete the content pack
  if apiDelete "$cpId"; then
    print "The default monitoring content pack has been removed or is not present\\n"
    print "Deleting success file indicator at $installSuccess"
    rm -f $installSuccess
    exit 0
  else
    print "Unable to delete the default content pack\\n"
    exit 1
  fi
fi
