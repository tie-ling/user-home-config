#!/usr/bin/env bash

set -xe

#https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/
#https://f-droid.org/repo/

# mkdir 宇琛2026电子书软件包

DL_LINKS="https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/org.koreader.launcher.fdroid_114502.apk
https://f-droid.org/repo/tw.com.daxia.virtualsoftkeys_10.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.ghostsq.commander_479.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.activitymanager_555.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.termux_1022.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.termux.styling_1000.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/org.pocketworkstation.pckeyboard_1041001.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.adilhanney.saber_1290302.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/org.fdroid.fdroid_1023051.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/org.mozilla.fennec_fdroid_1460120.apk
https://f-droid.org/repo/com.gacode.relaunchx_200106000.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/org.fcitx.fcitx5.android_102.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/juloo.keyboard2_50.apk
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.saha.batchuninstaller_13.apk
https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/emacs-30.2-21-arm64-v8a.apk/download
https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/termux-app_apt-android-7-release_universal.apk/download
https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/org.gnu.emacs_300200003.apk"

# ifytek t2 firmware
# https://iflyink.com//#/download/dxk
# https://download.iflyink.com/lo9ShbLTT57DEdyIi_T0dLa7ZoaG?attname=updateXFDXJ210E0_R_3.2.2.zip
# has wps office built in, can be used to install software
for i in $DL_LINKS; do
    curl -LO $i;
done

# re-sign termux with emacs key
# github.com/johanwiden/termux-for-android-emacs
curl -O https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo/com.termux_1002.apk
curl -O https://cgit.git.savannah.gnu.org/cgit/emacs.git/plain/java/emacs.keystore
nix-shell -p apksigner
apksigner sign --v2-signing-enabled --ks			\
	emacs.keystore -debuggable-apk-permitted	\
	--ks-pass pass:emacs1 com.termux_1002.apk
