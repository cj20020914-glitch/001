param(
  [string]$AppName = "转录组数据综合分析系统 V1.0",
  [string]$InnoCompiler = "C:\Program Files (x86)\Inno Setup 6\ISCC.exe",
  [switch]$SkipPortablePackage
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Root = Resolve-Path (Join-Path $ScriptDir "..")
$PackageDir = Join-Path $Root "dist\$AppName-Windows"
$InnoScript = Join-Path $Root "installer\AgingGeneMLApp.iss"
$Rscript = Join-Path $Root "R\bin\Rscript.exe"

if (!(Test-Path $InnoCompiler)) {
  throw "Cannot find Inno Setup compiler: $InnoCompiler"
}

if (!(Test-Path $Rscript)) {
  throw "Cannot find bundled Rscript: $Rscript"
}

if (!$SkipPortablePackage -or !(Test-Path $PackageDir)) {
  & powershell -ExecutionPolicy Bypass -File (Join-Path $ScriptDir "package_windows.ps1") -AppName $AppName -SkipPreparePackages -NoZip
}

if (!(Test-Path $PackageDir)) {
  throw "Portable package folder does not exist: $PackageDir"
}

New-Item -ItemType Directory -Force -Path (Join-Path $Root "dist\installer") | Out-Null

Push-Location (Join-Path $Root "installer")
try {
  & $InnoCompiler $InnoScript
} finally {
  Pop-Location
}

$SetupExe = Join-Path $Root "dist\installer\$AppName-Setup.exe"
if (!(Test-Path $SetupExe)) {
  throw "Installer was not generated: $SetupExe"
}

Write-Host "Installer generated: $SetupExe"
