#!/bin/bash


function tm (){
    where=$1
    file=$2/results
    time=$3
    sed -i "s/$where:/$where:\n Time: $time\n/" $file
}

# unused
# add Unused lighttpd-1.4.15 01:28:39
# add Unused samba-4.7.6 00:00:40
# add Unused wpa_cli-2.2 00:17:55
# add Unused swfcombine-0.9.2 01:24:01

# primus-checks
# httpd-2.4.18 1:04:41

function sz() {
    file=$1/results
    size=$2
    if [ -f $file ]; then
        sed -i "1s/^/Size: $size\n\n/" $file
    fi
}



sz httpd-2.4.18        873K
sz libbfd-2.31.1       6.0M
sz lighttpd-1.4.15     770K
sz nginx-1.7           859K
sz ntpd-4.2.8p5        3.1M
sz ntpdc-4.2.8p5       1.1M
sz openssl-1.1.0       745K
sz samba-4.7.6         46K
sz smtpd-5.7.3p2       2.2M
sz sqlite3-2.27.2      1.1M
sz sshd-7.3.p1         3.0M
sz swfc-0.9.2          1.4M
sz swfcombine-0.9.2    113K
sz swfextract-0.9.2    1.1M
sz tshark-2.6.0        1.6M
sz wav2swf-0.9.2       241K
sz wpa_cli-2.2         114K
sz wpa_supplicant-2.2  934K
