param($command, $directory=".\dhall")

Get-ChildItem $directory |
Foreach-Object {
    if ($_ -is [System.IO.DirectoryInfo]) {
        Invoke-Expression -Command ($PSCommandPath + ' -command ' + $command + ' -directory ' + $_.FullName)
    } else {
        dhall $command $_.FullName
    }
}
