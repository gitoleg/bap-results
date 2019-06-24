#!/bin/bash


function add (){
    where=$1
    file=$2/results
    time=$3
#    sed -i "s/$where:/$where:\n Time: $time\n/" $file
}


add Null httpd-2.4.18 01:36:00        v
add Null libbfd-2.31.1
add Null lighttpd-1.4.15 00:15:12     v
add Null nginx-1.7 04:47:01
add Null ntpd-4.2.8p5 01:35:31
add Null ntpdc-4.2.8p5 00:25:22
add Null openssl-1.1.0 00:46:44       v
add Null samba-4.7.6 00:00:40         v
add Null smtpd-5.7.3p2 01:08:20
add Null sqlite3-2.27.2
add Null sshd-7.3.p1 01:53:16
add Null swfc-0.9.2 02:30:08
add Null swfcombine-0.9.2 00:18:13
add Null swfextract-0.9.2 01:30:19
add Null tshark-2.6.0 00:32:20
add Null wav2swf-0.9.2 00:20:07
add Null wpa_cli-2.2 00:12:49
add Null wpa_supplicant-2.2
