set term aqua size 1600 1000 font 'Tahoma,16' enhanced
set title 'attempt nagle async:4 kpoll payload:100+'
set termoption dash
set dummy jw,y
set grid xtics nomxtics noytics nomytics noztics nomztics \
 nox2tics nomx2tics y2tics nomy2tics nocbtics nomcbtics
set key inside center top vertical Right noreverse enhanced autotitles nobox
set xtics border out scale 1,0.5 mirror norotate  offset character 0, 0, 0 autojustify
set ytics border out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
set ztics border out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
set y2tics border out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
set y2tics autofreq  norangelimit
set cbtics border out scale 1,0.5 mirror norotate  offset character 0, 0, 0 autojustify
set rtics axis out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
set ylabel "Rate, rps"
set y2label "Delay, ms"
#set logscale y
#set size 1,0.5
set yrange [0:350000]
#set xlabel 'test [№]'
#set ylabel 'time [ms]'
#set format y '%g'
set datafile separator '\t'
set timefmt "%S"
#set xdata time
set grid
set ytic 10000
#set label 'cpp 1bl' at 850, 25 left tc rgb 'dark-green'
#set label 'java 1bl' at 850, 80 left tc rgb 'dark-red'
#set label 'cpp 3bl' at 850, 130 left tc rgb 'green'
#set label 'java 3bl' at 850, 170 left tc rgb 'red'
plot 'attempt_nagle_async:4_kpoll_payload:100+.log' using 1:2 lt 1 lc rgb 'dark-green' lw 2 with lines title 'rate' axes x1y1,\
     'attempt_nagle_async:4_kpoll_payload:100+.log' using 1:3 lt 1 lc rgb 'dark-red' lw 2 with lines title 'delay ms' axes x2y2,\
     'attempt_nagle_async:4_kpoll_payload:100+.log' using 1:($4*5) lt 1 lc rgb 'dark-blue' with lines title 'tx packet rate *5',\
     'attempt_nagle_async:4_kpoll_payload:100+.log' using 1:($5*5) lt 5 lc rgb 'dark-blue' with lines title 'rx packet rate *5',\
     'attempt_nagle_async:4_kpoll_payload:100+.log' using 1:(5*$6/1024) lt 1 lc rgb 'dark-gray' with lines title 'tx kbyte rate *5',\
     'attempt_nagle_async:4_kpoll_payload:100+.log' using 1:(5*$7/1024) lt 5 lc rgb 'dark-gray' with lines title 'rx kbyte rate *5'
