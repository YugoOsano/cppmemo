#!/bin/bash
# package 'inotify-tools' required

# for inotify-tools, see:
# http://ameblo.jp/itboy/entry-10172179452.html

# monitor creation of files in the current directory
# http://d.hatena.ne.jp/aki-yam/20090401/1238569171

# '.' can be replaced by /home/yugo/cppmemo
out_dir=. 

events=(-e CREATE -e MODIFY)

while inotifywait ${events[@]} $out_dir
do
    #-- sort file by time and then pick the latest --
    newfile=`ls -rt | tail -n1`
    echo "file created or modified: $newfile"
done

# basics for handling an array in bash:
# http://d.hatena.ne.jp/nattou_curry_2/20091104/1257346587
