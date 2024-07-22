#!/bin/sh
function e () { emacsclient -nw --alternate-editor="" --create-frame "${@}"; }
export EDITOR="emacsclient -nw --alternate-editor="" --create-frame"
function vpncon ()
{  SSH_AUTH_SOCK= \
  autossh -i ~/.ssh/vpn_key \
          -D localhost:1088 -M 1089 \
          debian@2001:41d0:701:1100::7b17 ;
}
