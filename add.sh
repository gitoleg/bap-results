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
