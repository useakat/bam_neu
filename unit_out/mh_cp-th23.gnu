###################### Options ###########################################
#set title "gg -> ng Cross Secti"
#set logscale x
#set logscale y
#set format x "%L"
#set format y "10^{%L}"
#set xtics (2,3,4,5,6)
#set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
#set tics scale 2
#set key at 1.0E3,1.0E7 samplen 2
#set xlabel 'Final gluons' offset 0,0
#set ylabel 'Cross Section (fb)' offset 0,0
#set xrange [1:7]
#set yrange [1E-5:2E8]
####################### Definitions ######################################
file1 = 'mh_cp-th23_t2kk_1000_3.0_0.5_nh.dat'
c1 = 'red'			
c2 = 'blue'
c3 = '#006400' # dark green
c4 = 'purple'
c5 = '#ff33ff'
c6 = '#cc6600' # dark orange
##########################################################################
set terminal postscript eps enhanced "Times-Roman" color 20
set output "mh_cp-th23.eps"
set grid
set key spacing 1.5 samplen 2
#set multiplot

set title "{/=28 Delta chi^2 for rejecting wrong MH}"
set xlabel '{/=24 CP}'
set ylabel '{/=24 th23}' offset 1.5,0
#################### plot ##########################################
splot file1 u 1:2:3 notitle w l lt 1 lw 3
###########################################################################
#set nomultiplot
reset