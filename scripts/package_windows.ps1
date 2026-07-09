param(
  [string]$AppName = "转录组数据综合分析系统 V1.0",
  [switch]$SkipPreparePackages,
  [switch]$NoZip
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Root = Resolve-Path (Join-Path $ScriptDir "..")
$Dist = Join-Path $Root "dist"
$Target = Join-Path $Dist "$AppName-Windows"
$Rscript = Join-Path $Root "R\bin\Rscript.exe"

if (!(Test-Path $Rscript)) {
  throw "Cannot find bundled Rscript: $Rscript"
}

New-Item -ItemType Directory -Force -Path $Dist | Out-Null

if (!$SkipPreparePackages) {
  & $Rscript (Join-Path $ScriptDir "prepare_portable_packages.R")
}

if (Test-Path $Target) {
  $ResolvedDist = (Resolve-Path $Dist).Path
  $ResolvedTarget = (Resolve-Path $Target).Path
  if (!$ResolvedTarget.StartsWith($ResolvedDist, [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Refusing to remove target outside dist: $ResolvedTarget"
  }
  Remove-Item -LiteralPath $ResolvedTarget -Recurse -Force
}

New-Item -ItemType Directory -Force -Path $Target | Out-Null

$Items = @(
  "app.R",
  "global.R",
  "modules",
  "data",
  "assets",
  "R",
  "scripts",
  "start_app.bat",
  "启动软件.bat",
  "WINDOWS_LOCAL_PACKAGE.md"
)

foreach ($Item in $Items) {
  $Source = Join-Path $Root $Item
  if (Test-Path $Source) {
    Copy-Item -LiteralPath $Source -Destination $Target -Recurse -Force
  }
}

New-Item -ItemType Directory -Force -Path (Join-Path $Target "logs") | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $Target "temp") | Out-Null

if (!$NoZip) {
  $ZipPath = Join-Path $Dist "$AppName-Windows.zip"
  if (Test-Path $ZipPath) {
    Remove-Item -LiteralPath $ZipPath -Force
  }
  Compress-Archive -Path (Join-Path $Target "*") -DestinationPath $ZipPath -Force
  Write-Host "Package folder: $Target"
  Write-Host "Zip package: $ZipPath"
} else {
  Write-Host "Package folder: $Target"
}

