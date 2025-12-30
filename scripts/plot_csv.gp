set term svg size 800,600
set datafile separator ','
set output 'output/emulator_program_bench.svg'
set title 'Emulator Program Bench Latency'
set key autotitle columnhead
set xlabel 'run'
set ylabel 'latency_s'
plot 'output/emulator_program_bench.csv' using 2:3 with linespoints
