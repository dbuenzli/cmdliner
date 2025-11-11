$Global:_cmdliner_generic = {
    param(
        $wordToComplete,
        $commandAst,
        $cursorPosition
    )

    $exe = $commandAst.CommandElements[0].Extent.Text

    $otherArgs = ($commandAst.CommandElements
        | Select-Object -Skip 1
        | ForEach-Object {
            $_.Extent.Text
        } | Where-Object {
            $_ -ne $wordToComplete
        }) -join " "

    $completions = Invoke-Expression "$exe --__complete $otherArgs --__complete=$wordToComplete"

    $version = $completions[0]
    if ($version -ne "1") {
        throw "Unsupported cmdliner completion protocol: $version"
    }

    $prefix = ""
    $pattern = $wordToComplete
    if ($wordToComplete -match '^\-\-') {
        # take everything before '=' as prefix for long options
        $parts = $wordToComplete -split '=', 2
        $prefix = $parts[0] + '='
        $pattern = $parts[1]
    }
    elseif ($wordToComplete -match '^\-') {
        # take first two characters of wordToComplete as prefix for short options
        $prefix = $wordToComplete.Substring(0, [Math]::Min(2, $wordToComplete.Length))
        $pattern = $wordToComplete.Substring(2)
    }

    $idx = 1
    $group = ""
    while ($idx -lt $completions.Count) {
        $type = $completions[$idx]
        $idx += 1

        switch ($type) {
            "group" {
                $group = $completions[$idx]
                $idx += 1
            }
            "item" {
                $item = $completions[$idx]
                $idx += 1

                $itemDoc = ""
                while ($true) {
                    $line = $completions[$idx]
                    $idx += 1
                    if ($line -eq "item-end" ) {
                        break
                    }
                    if ($itemDoc -ne "") {
                        $itemDoc += "`n"
                    }
                    $itemDoc += $line
                }
                # avoid null tooltip error
                if ($itemDoc -eq "") {
                    $itemDoc = $item
                }

                $completionItem = $item
                # re-add prefix for things like --foo=
                if ($group -eq "Values" ) {
                    $item = $prefix + $item
                }

                [System.Management.Automation.CompletionResult]::new(
                    $completionItem,
                    $item,
                    'ParameterValue',
                    $itemDoc)
            }
            "files" {
                [Management.Automation.CompletionCompleters]::CompleteFilename($pattern)
                | ForEach-Object {
                    [System.Management.Automation.CompletionResult]::new(
                        $prefix + $_.CompletionText,
                        $_.ListItemText,
                        $_.ResultType,
                        $_.ToolTip)
                }
            }
            "dirs" {
                $dirs = [Management.Automation.CompletionCompleters]::CompleteFilename($pattern)
                | Where-Object {
                    Test-Path $_.CompletionText -PathType Container
                } | ForEach-Object {
                    [System.Management.Automation.CompletionResult]::new(
                        $prefix + $_.CompletionText,
                        $_.ListItemText,
                        $_.ResultType,
                        $_.ToolTip)
                }
                if ($dirs.Count -gt 0) {
                    $dirs
                }
                else {
                    # supress default behavior (file completion) when you don't return anything
                    $null
                }
            }
            "restart" {
                [Management.Automation.CompletionCompleters]::CompleteCommand($exe)
            }
            "message" {
                $msg = ""
                while ($true) {
                    $line = $completions[$idx]
                    $idx += 1
                    if ($line -eq "message-end" ) {
                        break
                    }
                    if ($msg -ne "") {
                        $msg += "`n"
                    }
                    $msg += $line
                }
                Write-Output "$msg" | Out-Host
                $null
            }
            default {
                throw "Unknown completion type: $type"
            }
        }
    }
}
