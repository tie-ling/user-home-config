#!/bin/sh
function e () { emacsclient -nw --alternate-editor="" --create-frame "${@}"; }
export EDITOR="emacsclient -nw --alternate-editor="" --create-frame"
