# ProgB vs Prog8 Language Test Runner
# Validates that ProgB and Prog8 produce identical assembly output

param(
    [string]$Target = "c64",
    [string]$CompilerPath = "",
    [string]$TestName = "",          # Optional: specific test name to run (without extension)
    [switch]$KeepOutput,
    [switch]$KeepComments,           # Don't strip comments when comparing assembly
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

# Find compiler
if (-not $CompilerPath) {
    $CompilerPath = Join-Path $PSScriptRoot "..\..\compiler\build\install\prog8c\bin\prog8c.bat"
    if (-not (Test-Path $CompilerPath)) {
        $CompilerPath = Join-Path $PSScriptRoot "..\..\build\install\prog8c\bin\prog8c.bat"
    }
}

if (-not (Test-Path $CompilerPath)) {
    Write-Error "Compiler not found. Please build with: .\gradlew.bat installdist"
    exit 1
}

$CompilerPath = Resolve-Path $CompilerPath

Write-Host "Using compiler: $CompilerPath" -ForegroundColor Cyan
Write-Host "Target: $Target" -ForegroundColor Cyan
Write-Host ""

# Paths
$TargetDir = Join-Path $PSScriptRoot $Target
$OutputDir = Join-Path $PSScriptRoot "output"

# Verify target directory exists
if (-not (Test-Path $TargetDir)) {
    Write-Error "Target directory not found: $TargetDir"
    exit 1
}

# Create output directory
if (Test-Path $OutputDir) {
    Remove-Item -Recurse -Force $OutputDir
}
New-Item -ItemType Directory -Path $OutputDir | Out-Null
New-Item -ItemType Directory -Path (Join-Path $OutputDir "progb") | Out-Null
New-Item -ItemType Directory -Path (Join-Path $OutputDir "prog8") | Out-Null

# Function to strip comments and normalize assembly
function Strip-AsmComments {
    param([string]$AsmContent)
    
    $lines = $AsmContent -split "`n"
    $result = @()
    
    foreach ($line in $lines) {
        # Remove full-line comments (lines starting with ; after whitespace)
        if ($line -match '^\s*;') {
            continue
        }
        
        # Remove inline comments (everything after ;)
        $cleaned = $line -replace '\s*;.*$', ''
        
        # Skip empty lines
        if ($cleaned -match '^\s*$') {
            continue
        }
        
        # Normalize whitespace
        $cleaned = $cleaned.TrimEnd()
        
        $result += $cleaned
    }
    
    return ($result -join "`n")
}

# Function to compile a file
function Compile-File {
    param(
        [string]$SourceFile,
        [string]$OutputSubDir
    )
    
    $fileName = [System.IO.Path]::GetFileNameWithoutExtension($SourceFile)
    $workDir = Join-Path $OutputDir $OutputSubDir
    
    # Copy source to work directory
    Copy-Item $SourceFile -Destination $workDir
    
    $sourceInWorkDir = Join-Path $workDir ([System.IO.Path]::GetFileName($SourceFile))
    
    # Compile
    $compileArgs = @("-target", $Target, "-out", $workDir, $sourceInWorkDir)
    
    if ($Verbose) {
        Write-Host "  Compiling: $CompilerPath $($compileArgs -join ' ')" -ForegroundColor Gray
    }
    
    $process = Start-Process -FilePath $CompilerPath -ArgumentList $compileArgs -Wait -PassThru -NoNewWindow -RedirectStandardOutput (Join-Path $workDir "stdout.txt") -RedirectStandardError (Join-Path $workDir "stderr.txt")
    
    if ($process.ExitCode -ne 0) {
        $stderr = Get-Content (Join-Path $workDir "stderr.txt") -Raw -ErrorAction SilentlyContinue
        $stdout = Get-Content (Join-Path $workDir "stdout.txt") -Raw -ErrorAction SilentlyContinue
        Write-Host "  COMPILE ERROR:" -ForegroundColor Red
        if ($stdout) { Write-Host $stdout }
        if ($stderr) { Write-Host $stderr -ForegroundColor Red }
        return $null
    }
    
    # Find generated .asm file matching the source file name
    $asmFile = Join-Path $workDir "$fileName.asm"
    
    if (-not (Test-Path $asmFile)) {
        Write-Host "  ERROR: No .asm file generated (expected $fileName.asm)" -ForegroundColor Red
        return $null
    }
    
    return $asmFile
}

# Function to compare assembly files
function Compare-AsmFiles {
    param(
        [string]$Asm1Path,
        [string]$Asm2Path,
        [bool]$StripComments = $true
    )
    
    $asm1 = Get-Content $Asm1Path -Raw
    $asm2 = Get-Content $Asm2Path -Raw
    
    if ($StripComments) {
        $stripped1 = Strip-AsmComments $asm1
        $stripped2 = Strip-AsmComments $asm2
        
        # Save stripped versions for debugging
        $stripped1 | Out-File -FilePath ($Asm1Path -replace '\.asm$', '.stripped.asm') -Encoding UTF8
        $stripped2 | Out-File -FilePath ($Asm2Path -replace '\.asm$', '.stripped.asm') -Encoding UTF8
    } else {
        $stripped1 = $asm1.TrimEnd()
        $stripped2 = $asm2.TrimEnd()
    }
    
    if ($stripped1 -eq $stripped2) {
        return @{ Match = $true; Diff = $null }
    }
    
    # Find differences
    $lines1 = $stripped1 -split "`n"
    $lines2 = $stripped2 -split "`n"
    
    $maxLines = [Math]::Max($lines1.Count, $lines2.Count)
    $diffs = @()
    
    for ($i = 0; $i -lt $maxLines; $i++) {
        $l1 = if ($i -lt $lines1.Count) { $lines1[$i] } else { "<missing>" }
        $l2 = if ($i -lt $lines2.Count) { $lines2[$i] } else { "<missing>" }
        
        if ($l1 -ne $l2) {
            $diffs += @{
                Line = $i + 1
                ProgB = $l1
                Prog8 = $l2
            }
            
            # Limit diff output
            if ($diffs.Count -ge 10) {
                $diffs += @{ Line = "..."; ProgB = "more differences"; Prog8 = "truncated" }
                break
            }
        }
    }
    
    return @{ Match = $false; Diff = $diffs }
}

# Get test files
$progbFiles = Get-ChildItem -Path $TargetDir -Filter "*.pb" -ErrorAction SilentlyContinue
$prog8Files = Get-ChildItem -Path $TargetDir -Filter "*.p8" -ErrorAction SilentlyContinue

# Filter to specific test if requested
if ($TestName) {
    $progbFiles = $progbFiles | Where-Object { [System.IO.Path]::GetFileNameWithoutExtension($_.Name) -eq $TestName }
    $prog8Files = $prog8Files | Where-Object { [System.IO.Path]::GetFileNameWithoutExtension($_.Name) -eq $TestName }
    
    if (-not $progbFiles -or $progbFiles.Count -eq 0) {
        Write-Host "Test file not found: $TargetDir\$TestName.pb" -ForegroundColor Red
        exit 1
    }
    if (-not $prog8Files -or $prog8Files.Count -eq 0) {
        Write-Host "Test file not found: $TargetDir\$TestName.p8" -ForegroundColor Red
        exit 1
    }
}

if (-not $progbFiles -or $progbFiles.Count -eq 0) {
    Write-Host "No .pb files found in $TargetDir" -ForegroundColor Yellow
    Write-Host "Add test files to the target folder ($Target)." -ForegroundColor Yellow
    exit 0
}

# Match files by name
$testPairs = @()
foreach ($pbFile in $progbFiles) {
    $baseName = [System.IO.Path]::GetFileNameWithoutExtension($pbFile.Name)
    $p8File = $prog8Files | Where-Object { [System.IO.Path]::GetFileNameWithoutExtension($_.Name) -eq $baseName }
    
    if ($p8File) {
        $testPairs += @{
            Name = $baseName
            ProgB = $pbFile.FullName
            Prog8 = $p8File.FullName
        }
    } else {
        Write-Host "WARNING: No matching .p8 file for $($pbFile.Name)" -ForegroundColor Yellow
    }
}

if ($testPairs.Count -eq 0) {
    Write-Host "No matching test pairs found." -ForegroundColor Yellow
    exit 0
}

Write-Host "Found $($testPairs.Count) test pair(s)" -ForegroundColor Cyan
Write-Host ""

# Run tests
$passed = 0
$failed = 0
$errors = 0

foreach ($pair in $testPairs) {
    Write-Host "Testing: $($pair.Name)" -ForegroundColor White
    
    # Compile ProgB version
    Write-Host "  Compiling ProgB..." -NoNewline
    $progbAsm = Compile-File -SourceFile $pair.ProgB -OutputSubDir "progb"
    if (-not $progbAsm) {
        Write-Host " ERROR" -ForegroundColor Red
        $errors++
        continue
    }
    Write-Host " OK" -ForegroundColor Green
    
    # Compile Prog8 version
    Write-Host "  Compiling Prog8..." -NoNewline
    $prog8Asm = Compile-File -SourceFile $pair.Prog8 -OutputSubDir "prog8"
    if (-not $prog8Asm) {
        Write-Host " ERROR" -ForegroundColor Red
        $errors++
        continue
    }
    Write-Host " OK" -ForegroundColor Green
    
    # Compare assembly
    Write-Host "  Comparing assembly..." -NoNewline
    $result = Compare-AsmFiles -Asm1Path $progbAsm -Asm2Path $prog8Asm -StripComments (-not $KeepComments)
    
    if ($result.Match) {
        Write-Host " PASS" -ForegroundColor Green
        $passed++
    } else {
        Write-Host " FAIL" -ForegroundColor Red
        $failed++
        
        Write-Host "  Differences found:" -ForegroundColor Yellow
        foreach ($diff in $result.Diff) {
            Write-Host "    Line $($diff.Line):" -ForegroundColor Yellow
            Write-Host "      ProgB: $($diff.ProgB)" -ForegroundColor Cyan
            Write-Host "      Prog8: $($diff.Prog8)" -ForegroundColor Magenta
        }
    }
    
    Write-Host ""
}

# Summary
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Test Summary" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  Passed: $passed" -ForegroundColor Green
Write-Host "  Failed: $failed" -ForegroundColor $(if ($failed -gt 0) { "Red" } else { "Green" })
Write-Host "  Errors: $errors" -ForegroundColor $(if ($errors -gt 0) { "Red" } else { "Green" })
Write-Host ""

# Cleanup if not keeping output
if (-not $KeepOutput) {
    Remove-Item -Recurse -Force $OutputDir -ErrorAction SilentlyContinue
}

# Exit with appropriate code
if ($failed -gt 0 -or $errors -gt 0) {
    exit 1
}
exit 0
