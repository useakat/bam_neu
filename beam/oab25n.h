/*!
 ---------------------------------------------------------------------\n

 \short off-axis angle のbeam profile (2.5 \f${\nu}\f$ foucus)

 \b 解説

 50MeV刻みのビームのフラックス配列。場所は 1000 km 。
 エネルギーレンジは 0.0GeV - 15GeV
 (index にすると 0-299までの300個)

 ---------------------------------------------------------------------\n

 \author "$Author: okamura $"

 \date "$Date: 2011-01-18 13:45:53 +0900 (Tue, 18 Jan 2011) $" 

 \version "$Name:  $-$Revision: 188 $-$State: Exp $"

 \note "$Source: /home/cvsroot/std.set.T2K4.3/Beam/oab25n.cc,v $"

 ---------------------------------------------------------------------\n

 \b 使用法

 \return 配列要素は
 \f$ E_\nu(GeV),\nu_\mu,\bar{\nu}_{\mu},\nu_e,\bar{\nu}_e \f$

 ---------------------------------------------------------------------\n
*/
/*--------------------------------------------------------
// Neutrino flux table at 1000 km 
// off axcis angle is 25n
// Enu(GeV),   nu_mu,        bar{nu_mu},   nu_e,         bar{nu_e}
---------------------------------------------------------*/
double oab25n[300][5]={
{2.500000e-02, 1.434186e+03, 2.167897e+03, 1.747460e+01, 4.014790e+00},
{7.500000e-02, 7.236736e+03, 6.326079e+03, 1.061127e+02, 1.503936e+01},
{1.250000e-01, 1.515619e+04, 4.255369e+03, 2.528241e+02, 2.078065e+01},
{1.750000e-01, 2.325600e+04, 3.375609e+03, 3.456493e+02, 2.906892e+01},
{2.250000e-01, 3.366126e+04, 3.206102e+03, 4.594271e+02, 3.540972e+01},
{2.750000e-01, 4.733665e+04, 3.204633e+03, 5.305814e+02, 4.360417e+01},
{3.250000e-01, 6.293446e+04, 3.434510e+03, 6.219514e+02, 5.076865e+01},
{3.750000e-01, 7.920606e+04, 3.520469e+03, 6.651778e+02, 5.503530e+01},
{4.250000e-01, 9.629136e+04, 3.654342e+03, 6.481906e+02, 6.285829e+01},
{4.750000e-01, 1.156530e+05, 3.746582e+03, 6.477306e+02, 6.189880e+01},
{5.250000e-01, 1.391863e+05, 3.728948e+03, 6.454885e+02, 4.906061e+01},
{5.750000e-01, 1.670438e+05, 3.817237e+03, 6.290310e+02, 6.002832e+01},
{6.250000e-01, 1.752467e+05, 3.796856e+03, 6.823205e+02, 4.746778e+01},
{6.750000e-01, 1.546190e+05, 3.660235e+03, 5.429421e+02, 5.445596e+01},
{7.250000e-01, 1.166882e+05, 3.472510e+03, 5.546307e+02, 5.950812e+01},
{7.750000e-01, 8.749012e+04, 3.314473e+03, 5.278653e+02, 5.004258e+01},
{8.250000e-01, 6.536657e+04, 3.047895e+03, 4.986443e+02, 5.007863e+01},
{8.750000e-01, 4.839361e+04, 2.524276e+03, 4.782248e+02, 4.750742e+01},
{9.250000e-01, 3.301352e+04, 2.510867e+03, 4.071995e+02, 5.577664e+01},
{9.750000e-01, 2.220859e+04, 2.447028e+03, 3.807341e+02, 4.846210e+01},
{1.025000e+00, 1.568700e+04, 2.350278e+03, 3.530481e+02, 4.190457e+01},
{1.075000e+00, 1.124848e+04, 2.159699e+03, 3.008303e+02, 4.924537e+01},
{1.125000e+00, 9.239037e+03, 2.058710e+03, 3.235368e+02, 4.228462e+01},
{1.175000e+00, 7.649232e+03, 1.964489e+03, 2.725183e+02, 4.763220e+01},
{1.225000e+00, 6.304493e+03, 1.955920e+03, 2.386237e+02, 3.949737e+01},
{1.275000e+00, 5.258847e+03, 1.901365e+03, 2.027156e+02, 4.165605e+01},
{1.325000e+00, 4.391520e+03, 1.451619e+03, 2.032062e+02, 3.686138e+01},
{1.375000e+00, 4.434041e+03, 1.428091e+03, 1.735722e+02, 3.581349e+01},
{1.425000e+00, 4.019271e+03, 1.092162e+03, 1.774362e+02, 3.996293e+01},
{1.475000e+00, 3.341714e+03, 1.276672e+03, 1.887122e+02, 3.813941e+01},
{1.525000e+00, 2.989343e+03, 1.117105e+03, 1.496639e+02, 4.675900e+01},
{1.575000e+00, 2.958553e+03, 1.067795e+03, 1.321938e+02, 2.986273e+01},
{1.625000e+00, 2.555898e+03, 8.641572e+02, 1.721429e+02, 4.005729e+01},
{1.675000e+00, 2.721780e+03, 1.000324e+03, 1.439843e+02, 4.355811e+01},
{1.725000e+00, 2.062053e+03, 9.605686e+02, 1.471556e+02, 3.593362e+01},
{1.775000e+00, 2.008973e+03, 9.153982e+02, 1.393895e+02, 3.982439e+01},
{1.825000e+00, 1.646144e+03, 7.520469e+02, 1.355678e+02, 3.704779e+01},
{1.875000e+00, 1.997428e+03, 1.212901e+03, 1.325174e+02, 3.070743e+01},
{1.925000e+00, 1.667001e+03, 8.994592e+02, 1.270638e+02, 3.006523e+01},
{1.975000e+00, 1.702407e+03, 5.338502e+02, 1.189905e+02, 4.196940e+01},
{2.025000e+00, 1.334236e+03, 5.062849e+02, 1.238560e+02, 3.319871e+01},
{2.075000e+00, 1.422904e+03, 6.512184e+02, 1.280544e+02, 3.375869e+01},
{2.125000e+00, 1.333296e+03, 6.564539e+02, 1.193624e+02, 3.417907e+01},
{2.175000e+00, 1.554069e+03, 4.904087e+02, 1.225375e+02, 2.747216e+01},
{2.225000e+00, 1.778864e+03, 4.474119e+02, 1.118559e+02, 2.570862e+01},
{2.275000e+00, 1.563235e+03, 6.566654e+02, 1.122289e+02, 3.107847e+01},
{2.325000e+00, 1.112116e+03, 5.998626e+02, 1.241896e+02, 3.039250e+01},
{2.375000e+00, 1.218384e+03, 6.617151e+02, 1.148156e+02, 3.382450e+01},
{2.425000e+00, 1.219353e+03, 4.045373e+02, 1.186553e+02, 2.250612e+01},
{2.475000e+00, 1.372292e+03, 9.406781e+02, 9.716207e+01, 1.696185e+01},
{2.525000e+00, 8.880489e+02, 4.619845e+02, 1.032717e+02, 2.447537e+01},
{2.575000e+00, 1.291266e+03, 5.299566e+02, 9.429380e+01, 2.144004e+01},
{2.625000e+00, 1.204605e+03, 5.013594e+02, 9.317632e+01, 1.983722e+01},
{2.675000e+00, 1.135906e+03, 5.203510e+02, 1.003036e+02, 2.368657e+01},
{2.725000e+00, 1.181607e+03, 4.725095e+02, 1.006797e+02, 2.905050e+01},
{2.775000e+00, 1.253456e+03, 4.894509e+02, 9.065331e+01, 2.922389e+01},
{2.825000e+00, 1.084951e+03, 1.568167e+02, 9.623704e+01, 2.639799e+01},
{2.875000e+00, 9.245758e+02, 1.604434e+02, 8.584975e+01, 1.944493e+01},
{2.925000e+00, 1.233066e+03, 4.862765e+02, 9.270602e+01, 3.118077e+01},
{2.975000e+00, 9.580619e+02, 4.830143e+02, 8.669044e+01, 1.903979e+01},
{3.025000e+00, 1.049933e+03, 2.990322e+02, 8.405465e+01, 2.771330e+01},
{3.075000e+00, 9.505761e+02, 5.255851e+02, 8.475372e+01, 5.850975e+01},
{3.125000e+00, 1.050236e+03, 1.765131e+02, 7.628700e+01, 2.357662e+01},
{3.175000e+00, 9.331908e+02, 1.810771e+02, 7.281350e+01, 2.062239e+01},
{3.225000e+00, 1.057196e+03, 1.128383e+02, 7.488331e+01, 5.923293e+01},
{3.275000e+00, 8.058123e+02, 1.064157e+02, 7.361029e+01, 2.078375e+01},
{3.325000e+00, 1.156395e+03, 1.196436e+02, 5.732121e+01, 1.132184e+01},
{3.375000e+00, 8.892021e+02, 2.944615e+02, 6.059272e+01, 1.930268e+01},
{3.425000e+00, 1.176044e+03, 2.062800e+02, 5.640012e+01, 1.203681e+01},
{3.475000e+00, 9.740713e+02, 2.556929e+01, 5.519525e+01, 9.775947e+00},
{3.525000e+00, 1.256568e+03, 1.216400e+02, 5.573486e+01, 1.806428e+01},
{3.575000e+00, 8.366382e+02, 3.287777e+02, 5.867874e+01, 2.339030e+01},
{3.625000e+00, 9.504744e+02, 2.506297e+01, 5.210336e+01, 1.380467e+01},
{3.675000e+00, 1.070149e+03, 2.479627e+02, 4.678022e+01, 1.261074e+01},
{3.725000e+00, 1.081231e+03, 3.430564e+02, 4.679542e+01, 1.413062e+01},
{3.775000e+00, 9.486577e+02, 2.258171e+01, 4.774605e+01, 1.512158e+01},
{3.825000e+00, 1.123187e+03, 2.564079e+02, 4.415610e+01, 1.406049e+01},
{3.875000e+00, 1.133800e+03, 4.631664e+02, 3.659095e+01, 5.786857e+00},
{3.925000e+00, 1.009990e+03, 2.778798e+02, 4.515594e+01, 1.421447e+01},
{3.975000e+00, 1.049969e+03, 2.342704e+01, 5.106963e+01, 2.583815e+01},
{4.025000e+00, 1.112530e+03, 1.421512e+02, 3.402085e+01, 1.085296e+01},
{4.075000e+00, 9.042845e+02, 1.283865e+02, 4.029869e+01, 1.512235e+01},
{4.125000e+00, 1.179284e+03, 1.547194e+02, 2.518435e+01, 6.926402e+00},
{4.175000e+00, 9.626881e+02, 2.580227e+01, 3.593144e+01, 1.228889e+01},
{4.225000e+00, 8.740093e+02, 4.718718e+01, 2.881579e+01, 9.749562e+00},
{4.275000e+00, 1.046060e+03, 1.606701e+02, 3.309790e+01, 1.022170e+01},
{4.325000e+00, 9.892446e+02, 3.169626e+01, 3.408602e+01, 1.353982e+01},
{4.375000e+00, 1.217267e+03, 2.425075e+01, 2.282359e+01, 6.165494e+00},
{4.425000e+00, 1.072546e+03, 1.727997e+02, 2.487283e+01, 5.282816e+00},
{4.475000e+00, 1.103567e+03, 2.332293e+01, 2.369175e+01, 6.940070e+00},
{4.525000e+00, 1.015690e+03, 1.732414e+01, 3.603142e+01, 2.160486e+01},
{4.575000e+00, 8.700350e+02, 2.650527e+01, 1.862901e+01, 4.482354e+00},
{4.625000e+00, 9.480802e+02, 3.269791e+02, 2.084051e+01, 8.106267e+00},
{4.675000e+00, 8.164482e+02, 3.463959e+01, 2.313620e+01, 7.958904e+00},
{4.725000e+00, 8.218867e+02, 4.478470e+01, 1.879421e+01, 5.857650e+00},
{4.775000e+00, 1.331512e+03, 3.141250e+01, 1.998450e+01, 6.520040e+00},
{4.825000e+00, 7.359911e+02, 2.456512e+01, 2.301445e+01, 1.040667e+01},
{4.875000e+00, 7.992869e+02, 2.163909e+02, 1.697480e+01, 3.086004e+00},
{4.925000e+00, 6.463961e+02, 2.810927e+01, 1.463032e+01, 6.225675e+00},
{4.975000e+00, 6.723818e+02, 3.137106e+01, 1.514726e+01, 3.561752e+00},
{5.025000e+00, 7.313856e+02, 2.400146e+01, 2.030498e+01, 1.074243e+01},
{5.075000e+00, 1.080010e+03, 1.628159e+01, 2.215438e+01, 1.453046e+01},
{5.125000e+00, 8.635071e+02, 3.060727e+01, 8.976395e+00, 9.626562e-01},
{5.175000e+00, 5.807874e+02, 2.547431e+01, 1.745228e+01, 6.857524e+00},
{5.225000e+00, 5.541882e+02, 2.315265e+01, 1.777673e+01, 8.861009e+00},
{5.275000e+00, 5.501626e+02, 2.233142e+02, 1.078437e+01, 5.405696e+00},
{5.325000e+00, 6.205468e+02, 1.207043e+01, 1.407842e+01, 6.893035e+00},
{5.375000e+00, 6.046982e+02, 1.832374e+01, 1.292478e+01, 5.771956e+00},
{5.425000e+00, 5.014149e+02, 3.419297e+01, 1.065274e+01, 4.900987e+00},
{5.475000e+00, 4.785383e+02, 1.906809e+01, 1.379179e+01, 5.423766e+00},
{5.525000e+00, 4.776179e+02, 1.614786e+01, 1.709403e+01, 7.973919e+00},
{5.575000e+00, 3.879989e+02, 1.969704e+01, 1.185512e+01, 6.225900e+00},
{5.625000e+00, 3.777920e+02, 1.339647e+01, 6.601067e+00, 0.000000e+00},
{5.675000e+00, 3.613878e+02, 3.070354e+01, 6.650951e+00, 0.000000e+00},
{5.725000e+00, 3.813435e+02, 1.734291e+01, 1.053940e+01, 5.936904e+00},
{5.775000e+00, 4.409370e+02, 1.058793e+01, 5.950630e+00, 1.939632e+00},
{5.825000e+00, 3.767845e+02, 2.789521e+02, 4.759652e+00, 0.000000e+00},
{5.875000e+00, 3.321711e+02, 1.823491e+01, 1.040913e+01, 5.833421e+00},
{5.925000e+00, 6.085138e+02, 1.111117e+01, 1.437761e+01, 9.274747e+00},
{5.975000e+00, 2.836293e+02, 2.264712e+01, 1.022087e+01, 4.814300e+00},
{6.025000e+00, 3.529759e+02, 7.696831e+00, 8.300727e+00, 2.183924e+00},
{6.075000e+00, 3.277713e+02, 2.345944e+01, 5.982004e+00, 2.925532e+00},
{6.125000e+00, 2.655833e+02, 1.588805e+01, 1.739368e+00, 0.000000e+00},
{6.175000e+00, 3.590112e+02, 1.206468e+01, 3.557897e+00, 7.739442e-01},
{6.225000e+00, 2.213085e+02, 2.456513e+01, 4.317775e+00, 0.000000e+00},
{6.275000e+00, 2.874197e+02, 2.086366e+01, 7.949644e+00, 2.438371e+00},
{6.325000e+00, 2.878503e+02, 1.692302e+01, 8.066489e+00, 4.558139e+00},
{6.375000e+00, 2.150004e+02, 2.583395e+01, 6.718651e+00, 3.553431e+00},
{6.425000e+00, 2.794439e+02, 8.745156e+00, 1.222664e+01, 9.637723e+00},
{6.475000e+00, 2.262998e+02, 1.326123e+01, 2.564320e+00, 0.000000e+00},
{6.525000e+00, 1.668357e+02, 1.353473e+01, 1.843371e+00, 0.000000e+00},
{6.575000e+00, 2.332421e+02, 3.475939e+02, 7.783712e+00, 4.148305e+00},
{6.625000e+00, 1.951310e+02, 1.395030e+01, 4.216391e+00, 3.010273e+00},
{6.675000e+00, 2.215962e+02, 2.823812e+01, 1.032788e+01, 8.618814e+00},
{6.725000e+00, 2.007146e+02, 1.440650e+01, 3.582632e+00, 2.407667e+00},
{6.775000e+00, 1.940567e+02, 1.461168e+01, 1.322385e+00, 0.000000e+00},
{6.825000e+00, 1.874410e+02, 1.478813e+01, 2.549671e+00, 0.000000e+00},
{6.875000e+00, 1.599942e+02, 1.495800e+01, 2.043496e+00, 1.743146e+00},
{6.925000e+00, 1.826419e+02, 4.061064e+01, 5.019840e+00, 6.208950e+00},
{6.975000e+00, 1.749722e+02, 5.115057e+00, 1.177350e+00, 0.000000e+00},
{7.025000e+00, 1.826510e+02, 0.000000e+00, 7.017255e+00, 5.306080e+00},
{7.075000e+00, 1.748705e+02, 5.331602e+00, 2.796244e+00, 0.000000e+00},
{7.125000e+00, 1.716685e+02, 2.148760e+01, 0.000000e+00, 0.000000e+00},
{7.175000e+00, 1.523625e+02, 2.715678e+01, 0.000000e+00, 0.000000e+00},
{7.225000e+00, 1.656041e+02, 1.659437e+01, 4.189247e+00, 3.290516e+00},
{7.275000e+00, 1.400355e+02, 1.679393e+01, 4.496347e+00, 3.774671e+00},
{7.325000e+00, 1.304104e+02, 2.265445e+01, 7.162843e-01, 0.000000e+00},
{7.375000e+00, 1.092473e+02, 1.147455e+01, 6.570760e+00, 2.160782e+00},
{7.425000e+00, 1.284059e+02, 5.848128e+00, 2.242981e+00, 0.000000e+00},
{7.475000e+00, 1.477115e+02, 1.181437e+01, 2.532211e+00, 5.777309e-01},
{7.525000e+00, 1.855244e+02, 5.978872e+00, 2.914767e+00, 0.000000e+00},
{7.575000e+00, 1.335655e+02, 1.217846e+01, 1.337670e+00, 6.393792e-01},
{7.625000e+00, 1.044147e+02, 6.138163e+00, 6.279317e-01, 0.000000e+00},
{7.675000e+00, 9.966175e+01, 6.229792e+00, 2.715030e+00, 0.000000e+00},
{7.725000e+00, 8.208863e+01, 6.300229e+00, 0.000000e+00, 8.527163e-01},
{7.775000e+00, 1.404149e+02, 6.364461e+00, 2.845028e+00, 0.000000e+00},
{7.825000e+00, 1.230343e+02, 1.291266e+01, 2.781695e+00, 0.000000e+00},
{7.875000e+00, 8.531365e+01, 0.000000e+00, 6.610001e-01, 0.000000e+00},
{7.925000e+00, 1.329546e+02, 0.000000e+00, 6.521179e-01, 0.000000e+00},
{7.975000e+00, 1.212575e+02, 6.759490e+00, 6.318638e+00, 4.003428e+00},
{8.025000e+00, 8.850423e+01, 6.795382e+00, 1.634501e+00, 0.000000e+00},
{8.075000e+00, 1.034061e+02, 2.060972e+01, 0.000000e+00, 0.000000e+00},
{8.125000e+00, 4.880178e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{8.175000e+00, 9.904517e+01, 1.420044e+01, 0.000000e+00, 0.000000e+00},
{8.225000e+00, 8.583851e+01, 0.000000e+00, 1.661940e+00, 0.000000e+00},
{8.275000e+00, 1.159834e+02, 7.254002e+00, 1.291489e+00, 0.000000e+00},
{8.325000e+00, 1.172336e+02, 0.000000e+00, 7.690906e-01, 0.000000e+00},
{8.375000e+00, 8.172319e+01, 0.000000e+00, 1.630133e+00, 0.000000e+00},
{8.425000e+00, 7.504903e+01, 1.496956e+01, 6.283368e+00, 5.382769e+00},
{8.475000e+00, 5.313242e+01, 2.278965e+01, 7.697818e-01, 0.000000e+00},
{8.525000e+00, 3.846451e+01, 7.712870e+00, 1.111107e+00, 9.288390e-01},
{8.575000e+00, 9.337395e+01, 2.326080e+01, 9.151735e-01, 0.000000e+00},
{8.625000e+00, 7.068460e+01, 7.854920e+00, 0.000000e+00, 0.000000e+00},
{8.675000e+00, 5.577931e+01, 0.000000e+00, 7.610139e-01, 0.000000e+00},
{8.725000e+00, 8.060934e+01, 1.604357e+01, 0.000000e+00, 0.000000e+00},
{8.775000e+00, 4.062701e+01, 0.000000e+00, 1.794202e+00, 0.000000e+00},
{8.825000e+00, 5.766902e+01, 0.000000e+00, 9.754226e-01, 0.000000e+00},
{8.875000e+00, 4.990698e+01, 0.000000e+00, 8.465581e-01, 0.000000e+00},
{8.925000e+00, 8.438300e+01, 1.680266e+01, 0.000000e+00, 0.000000e+00},
{8.975000e+00, 5.958333e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.025000e+00, 5.173059e+01, 8.611444e+00, 0.000000e+00, 0.000000e+00},
{9.075000e+00, 6.105446e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.125000e+00, 8.811529e+01, 8.810255e+00, 1.151821e+00, 0.000000e+00},
{9.175000e+00, 3.563601e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.225000e+00, 2.704024e+01, 9.004684e+00, 5.932886e+00, 5.932886e+00},
{9.275000e+00, 4.542100e+01, 9.067021e+00, 0.000000e+00, 0.000000e+00},
{9.325000e+00, 3.686155e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.375000e+00, 6.502601e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.425000e+00, 5.646580e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.475000e+00, 4.747130e+01, 9.483491e+00, 1.437533e+00, 0.000000e+00},
{9.525000e+00, 1.918147e+01, 1.920280e+01, 1.054786e+00, 0.000000e+00},
{9.575000e+00, 5.816479e+01, 9.730563e+00, 4.253881e+00, 4.253881e+00},
{9.625000e+00, 2.935223e+01, 9.808905e+00, 0.000000e+00, 0.000000e+00},
{9.675000e+00, 2.969899e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.725000e+00, 7.006239e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.775000e+00, 4.054920e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.825000e+00, 4.079818e+01, 1.019776e+01, 0.000000e+00, 0.000000e+00},
{9.875000e+00, 4.121950e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{9.925000e+00, 1.038514e+01, 1.046466e+01, 0.000000e+00, 0.000000e+00},
{9.975000e+00, 3.162039e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.002500e+01, 0.000000e+00, 1.066561e+01, 0.000000e+00, 0.000000e+00},
{1.007500e+01, 3.222197e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.012500e+01, 3.248190e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.017500e+01, 2.193809e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.022500e+01, 3.326794e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.027500e+01, 1.115454e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.032500e+01, 3.381497e+01, 1.123569e+01, 0.000000e+00, 0.000000e+00},
{1.037500e+01, 1.138575e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.042500e+01, 2.296655e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.047500e+01, 1.160957e+01, 0.000000e+00, 6.123737e+00, 6.123737e+00},
{1.052500e+01, 2.340245e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.057500e+01, 3.543910e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.062500e+01, 4.777965e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.067500e+01, 2.410327e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.072500e+01, 2.433559e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.077500e+01, 0.000000e+00, 1.223871e+01, 0.000000e+00, 0.000000e+00},
{1.082500e+01, 2.479269e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.087500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 2.463383e+00},
{1.092500e+01, 3.788641e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.097500e+01, 1.274103e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.102500e+01, 2.573411e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.107500e+01, 1.294304e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.112500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.117500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.122500e+01, 1.337016e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.127500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.132500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.137500e+01, 1.371377e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.142500e+01, 1.376035e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.147500e+01, 1.389823e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.152500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.157500e+01, 2.841869e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.162500e+01, 1.431358e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.167500e+01, 1.447015e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.172500e+01, 1.459055e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.177500e+01, 2.934947e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.182500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.187500e+01, 0.000000e+00, 0.000000e+00, 9.699965e+00, 7.679291e+00},
{1.192500e+01, 1.509502e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.197500e+01, 1.517680e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.202500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.207500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.212500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.217500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.222500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.227500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.232500e+01, 1.611630e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.237500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.242500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.247500e+01, 0.000000e+00, 1.651393e+01, 0.000000e+00, 0.000000e+00},
{1.252500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.257500e+01, 1.679224e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.262500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.267500e+01, 1.701711e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.272500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.277500e+01, 1.731856e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.282500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.287500e+01, 1.757223e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.292500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.297500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.302500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.307500e+01, 1.802092e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.312500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.317500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.322500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.327500e+01, 1.867404e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.332500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.337500e+01, 1.894143e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.342500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.347500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.352500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.357500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.362500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.367500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.372500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.377500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.382500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.387500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.392500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.397500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.402500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.407500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.412500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.417500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.422500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.427500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.432500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.437500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.442500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.447500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.452500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.457500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.462500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.467500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.472500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.477500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.482500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.487500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.492500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.497500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00}
};
