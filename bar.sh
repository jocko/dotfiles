echo $((($(date +%s) - $(date -d "2016-05-23" +%s)) / 86400))
grep 'starting full system upgrade' /var/log/pacman.log | tail -1 | cut -c2-17
