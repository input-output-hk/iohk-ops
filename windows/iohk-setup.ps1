############################################################################
#
# Windows dev environment for cardano-sl
#
# After running this script you will have a VirtualBox machine which
# is be able to compile cardano-sl for Windows using stack.
#
############################################################################
#
#
# Initial VM Setup
# ----------------
#
# Set up windows 10 in VirtualBox.
#
#     - Download the ISO from Microsoft, and install into a new VM
#       https://www.microsoft.com/en-au/software-download/windows10ISO
#       To be safe, just get the US English version.
#     - In the VM disks settings, add a SATA disk which will be E:
#     - In the VM disks settings, add the installer ISO to the CD drive.
#     - Share the directory containing this script as win_shared
#     - Power on the VM and install windows
#     - Enter your product key into Windows
#       (if you don't have one yet, it doesn't need to be done immediately)
#     - Install VirtualBox guest additions,
#     - Use the disk management tool to quick-format E:
#     - Use linked clones in virtualbox so that you can snapshot and
#       revert to working configs.
#
# For the Chocolatey package installs to work, you will need to
# temporarily switch off any Microsoft-blocking software that you have
# enabled (e.g. [WindowsSpyBlocker](https://github.com/crazy-max/WindowsSpyBlocker/tree/master/data/openwrt/spy)).
#
#
# Configuration
# -------------
#
# Edit this script to set the $ghcVer to either 8.2.2 or 8.4.4,
# depending on your branch.
#
# Installation
# ------------
# 
# 1. Click Start
# 2. Type Powershell, but don't press enter
# 3. Right-click on the Powershell item, then
#    right-click and choose "Run as administrator"
# 4. Run `Get-ExecutionPolicy` in the shell
#    If it returns Restricted, then run `Set-ExecutionPolicy Bypass -Scope Process`.
# 5. `cd \\VBOXSRV\win_shared`
# 6 `.\iohk-setup.ps1`
# 7. Go back to step 1 and run the steps a second time.
# 8. You should now be in directory to E:\w ($env:WORK_DIR).
# 9. Run the build. This command is similar to what is run by CI:
#
#      stack.exe install -j 3 --local-bin-path $env:WORK_DIR --flag cardano-sl-tools:for-installer cardano-sl cardano-sl-tools cardano-sl-wallet-new
#
# Note that the cardano-sl clone is shallow.
# If you need to change branch, then do
#   git fetch --unshallow
#
# Related tickets:
#   More details for setting up windows (for a buildkite-agent) are in
#   https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1113
#   https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1168
#
############################################################################

# from here onwards, errors terminate the script
$ErrorActionPreference = "Stop"
Set-ExecutionPolicy Bypass -Scope Process -Force
$drive = "E:"
$downloads = "$drive\Downloads"
$ghcVer = "8.2.2"

# Set-PSDebug -Trace 1

$env:chocoPath = "$drive\chocolatey"
$env:ChocolateyToolsLocation = "$drive\tools"

if (!(Get-Command choco.exe -ErrorAction SilentlyContinue)) {
  Write-Output "Installing choco to $env:chocoPath"

  [System.Environment]::SetEnvironmentVariable("chocoPath", "$env:chocoPath", "Machine")
  [System.Environment]::SetEnvironmentVariable("ChocolateyToolsLocation", "$env:ChocolateyToolsLocation", "Machine")

  iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

  refreshenv

  Write-Output "Chocolately installed. Now close this powershell and start again."
  Write-Output "Remember to run PS as administrator and the Set-ExecutionPolicy thing"
  exit
}

choco install -y 7zip.install
choco install -y curl
choco install -y vcredist140
choco install -y vcredist2013
choco install -y git.install
choco install -y vim
choco install -y msys2

refreshenv

############################################################################
# Patched GHC 8.2.2 or 8.4.4 (with MAX_PATH fix)

New-Item -ItemType Directory -Force -Path "$drive\ghc"

$ghcTar = "$drive\ghc\ghc-$ghcVer.tar.xz"

if (Test-Path $ghcTar) {
  Write-Output "$ghcTar is already downloaded"
} else {
  if ($ghcVer -eq "8.2.2") {
    $ghcUrl = "https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.2.2-x86_64-unknown-mingw32.tar.xz"
  } else {
    $ghcUrl = "https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.4.4-x86_64-unknown-mingw32-20181113-b907eb0f9b.tar.xz"
  }
  Write-Output "Downloading $ghcUrl"
  Invoke-WebRequest $ghcUrl -OutFile $ghcTar -UserAgent "Curl"
}

7z x $ghcTar "-o$drive\ghc" -aos
7z x $drive\ghc\ghc-$ghcVer.tar "-o$drive\ghc" -aos
rm $drive\ghc\ghc-$ghcVer.tar

############################################################################
# OpenSSL, xz

New-Item -ItemType Directory -Force -Path "$downloads"

(New-Object Net.WebClient).DownloadFile('https://slproweb.com/download/Win64OpenSSL-1_0_2q.exe', "$downloads\Win64OpenSSL.exe")
cmd /c start /wait "$downloads\Win64OpenSSL.exe" /silent /verysilent /sp- /suppressmsgboxes /DIR=$drive\OpenSSL-Win64-v102

# Install liblzma/xz
curl.exe -L https://tukaani.org/xz/xz-5.2.3-windows.zip -o "$downloads\xz-5.2.3-windows.zip"
7z x "$downloads\xz-5.2.3-windows.zip" "-o$drive\xz_extracted" -aos

############################################################################
# Install and configure stack

curl.exe -L http://www.stackage.org/stack/windows-x86_64 -o "$downloads\stack.zip"
7z x "$downloads\stack.zip" "-o$drive\stack" -aos

# Avoid long paths on Windows
$env:STACK_ROOT = "$drive\s"
$env:STACK_WORK = ".w"
$env:WORK_DIR = "$drive\w"
# Override the temp directory to avoid sed escaping issues
# See https://github.com/haskell/cabal/issues/5386
$env:TMP = "$drive\tmp"

$env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";$drive\ghc\ghc-$ghcVer\bin;$drive\stack;$Env:Programfiles\7-Zip;$Env:Programfiles\Git\cmd;$Env:Programfiles\chocolatey\bin;$env:WORK_DIR"

[System.Environment]::SetEnvironmentVariable("STACK_ROOT", "$env:STACK_ROOT", "Machine")
[System.Environment]::SetEnvironmentVariable("STACK_WORK", "$env:STACK_WORK", "Machine")
[System.Environment]::SetEnvironmentVariable("WORK_DIR", "$env:WORK_DIR", "Machine")
[System.Environment]::SetEnvironmentVariable("TMP", "$env:TMP", "Machine")
[System.Environment]::SetEnvironmentVariable("STACK_ROOT", "$env:STACK_ROOT", "Machine")
[System.Environment]::SetEnvironmentVariable("PATH", "$env:PATH", "Machine")

New-Item -ItemType Directory -Force -Path $env:STACK_ROOT
New-Item -ItemType Directory -Force -Path $env:TMP

if ($ghcVer -eq "8.2.2") {
  $stackConfigGhc = ""
} else {
  $stackConfigGhc = "ghc-options: `"-copy-libs-when-linking`""
}

$stackConfig = @"
system-ghc: true
local-programs-path: "$drive\\s\\\\programs"
local-bin-path: "$drive\\s\\bin"
extra-include-dirs:
 - "$drive\\OpenSSL-Win64-v102\include"
 - "$drive\\xz_extracted\\include"
 - "$drive\\w\\rocksdb\\include"
extra-lib-dirs:
 - "$drive\\OpenSSL-Win64-v102"
 - "$drive\\xz_extracted\\bin_x86-64"
 - "$drive\\w"
$stackConfigGhc

"@
$stackConfig | Out-File -FilePath "$env:STACK_ROOT\config.yaml" -Encoding ASCII

############################################################################
# git clone

if (Test-Path $env:WORK_DIR) {
  Write-Output "cardano-sl is already cloned"
} else {
  if ($ghcVer -eq "8.2.2") {
    $branch = "release/2.0.0"
  } else {
    $branch = "develop"
  }

  Write-Output "Cloning cardano-sl to $env:WORK_DIR"
  git.exe clone https://github.com/input-output-hk/cardano-sl.git --branch $branch --depth 1 "$env:WORK_DIR"
}

cd "$env:WORK_DIR"

############################################################################
# Rocksdb

# Pre-built rocksdb for Haskell
git.exe clone https://github.com/facebook/rocksdb.git --branch v4.13.5 --depth 1

if (Test-Path "$downloads\rocksdb.zip") {
  Write-Output "rocksdb binaries are already downloads"
} else {
  curl.exe -L 'https://s3.eu-central-1.amazonaws.com/ci-static/serokell-rocksdb-haskell-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip' -o "$downloads\rocksdb.zip"
}
7z x "$downloads\rocksdb.zip" "-o$env:WORK_DIR" -aos

copy rocksdb.dll node
copy rocksdb.dll lib
copy rocksdb.dll wallet-new

############################################################################
# Stack setup

stack.exe path
stack.exe exec -- ghc-pkg recache
stack.exe --verbosity warn setup --no-reinstall

############################################################################
# Finished

Write-Output "Ready to go! Change directory to $env:WORK_DIR and run 'stack build'"
