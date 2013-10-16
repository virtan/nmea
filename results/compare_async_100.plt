set term aqua size 1600 1000 font 'Tahoma,16' enhanced
set title 'Delay comparison for different async pools and 100+ packets'
#set termoption dash
#set dummy jw,y
#set grid xtics nomxtics noytics nomytics noztics nomztics \
# nox2tics nomx2tics y2tics nomy2tics nocbtics nomcbtics
#set key inside center top vertical Right noreverse enhanced autotitles nobox
#set xtics border out scale 1,0.5 mirror norotate  offset character 0, 0, 0 autojustify
#set ytics border out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
#set ztics border out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
#set y2tics border out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
#set y2tics autofreq  norangelimit
#set cbtics border out scale 1,0.5 mirror norotate  offset character 0, 0, 0 autojustify
#set rtics axis out scale 1,0.5 nomirror norotate  offset character 0, 0, 0 autojustify
set xlabel "RPS"
set ylabel "Delay, ms"
#set y2label "Delay, ms"
set logscale y
#set size 1,0.5
set yrange [1:10000]
#set xrange [0:400000]
#set xlabel 'test [№]'
#set ylabel 'time [ms]'
#set format y '%g'
set datafile separator '\t'
set timefmt "%S"
#set xdata time
set grid
#set ytic 10000
#set label 'cpp 1bl' at 850, 25 left tc rgb 'dark-green'
#set label 'java 1bl' at 850, 80 left tc rgb 'dark-red'
#set label 'cpp 3bl' at 850, 130 left tc rgb 'green'
#set label 'java 3bl' at 850, 170 left tc rgb 'red'
#set style fill transparent solid 0.5
plot 'attempt_nagle_async:0_kpoll_payload:100+.log' using 2:3 lt 1 lc rgb 'dark-blue' lw 10 with impulses title 'async 0',\
     'attempt_nagle_async:4_kpoll_payload:100+.log' using ($2+2000):3 lt 1 lc rgb 'dark-green' lw 10 with impulses title 'async 4',\
     'attempt_nagle_async:16_kpoll_payload:100+.log' using ($2+4000):3 lt 1 lc rgb 'dark-gray' lw 10 with impulses title 'async 16'
