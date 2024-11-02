mnthtw () {
    doas mount -t davfs  -o uid=1000 -o gid=100 https://cloud.htw-berlin.de/remote.php/dav/files/s0596965/ /home/yc/HTW_Cloud
}

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
}

tab1080 () {
    swaymsg input 9580:110:PenTablet_Pen  map_to_region 950 200 1000 562
}

tablap () {
    swaymsg input 9580:110:PenTablet_Pen map_to_region 0 0 1280 720
}

gitpushall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {} push
}

tom () {
    python ~/tomato/tyc.py
}
export LEDGER_FILE=~/hledger/yc.hledger
