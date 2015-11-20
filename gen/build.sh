#!/usr/bin/env bash

set -eu

readonly -a dependencies=(
    'blaze-html'
    'data-default-class'
    'iso8601-time'
)
readonly mainHs='test-html.hs'
readonly indexFile='index.html'

prepareSandbox()
{
    if [[ ! -e 'cabal.sandbox.config' ]]; then
        cabal sandbox init
    fi

    cabal install "${dependencies[@]}"
}

buildIndex()
{
    local -r packageDb="$(
        sed -nr '/^package-db:/{s/^[^:]+: *//;p}' cabal.sandbox.config
    )"

    runhaskell -package-db="$packageDb" "$mainHs"
}

checkExitCode()
{
    if (( $# )); then
        local -r -i isFinal="$1"; shift
    else
        local -r -i isFinal=0
    fi

    if (( ret )); then
        printf '\n\n*** FAILED: exit code: %d\n' $ret
        exit $ret
    else
        if (( isFinal )); then
            printf '\n*** SUCCESS\n'
        fi
    fi
}

main()
{
    local ret=0

    printf '*** Checking if "%s" is available:\n\n' "$mainHs"
    if [[ -e "$mainHs" ]]; then
        echo "$mainHs: File found."
    else
        ret=1
        echo "$mainHs: File NOT found: Are we running from correct directory?"
    fi
    checkExitCode

    printf '\n*** Preparing cabal sandbox and installing dependencies:\n\n'
    prepareSandbox || ret=$?
    checkExitCode

    printf '\n*** Building new "%s":\n\n' "$indexFile"
    buildIndex || ret=$?
    if (( ret == 0 )); then
        if [[ -e "$indexFile" ]]; then
            echo "$indexFile: New index file. Please move it to correct place."
        else
            echo "Error: $indexFile: Something went wrong and file is missing."
        fi
    fi

    local -r FINAL_CHECK=1
    checkExitCode FINAL_CHECK

    exit $ret
}

main "$@"
