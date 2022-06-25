param($directory=".\dhall")

Get-ChildItem $directory |
Foreach-Object {
    if ($_ -is [System.IO.DirectoryInfo]) {
        Invoke-Expression -Command ($PSCommandPath + ' -directory ' + $_.FullName)
    } else {
        dhall format $_.FullName
    }
}
