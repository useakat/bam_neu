/*!
 ---------------------------------------------------------------------\n

 \short off-axis angle のbeam profile (0.0 \f${\nu}\f$ foucus)

 \b 解説

 50MeV刻みのビームのフラックス配列。場所は 1000 km 。
 エネルギーレンジは 0.0GeV - 15GeV
 (index にすると 0-299までの300個)

 ---------------------------------------------------------------------\n

 \author "$Author: okamura $"

 \date "$Date: 2011-01-18 13:45:53 +0900 (Tue, 18 Jan 2011) $" 

 \version "$Name:  $-$Revision: 188 $-$State: Exp $"

 \note "$Source: /home/cvsroot/std.set.T2K4.3/Beam/oab00n.cc,v $"

 ---------------------------------------------------------------------\n

 \b 使用法

 \return 配列要素は
 \f$ E_\nu(GeV),\nu_\mu,\bar{\nu}_{\mu},\nu_e,\bar{\nu}_e \f$

 ---------------------------------------------------------------------\n
*/
/*--------------------------------------------------------
// Neutrino flux table at 1000 km 
// off axcis angle is 00n
// Enu(GeV),   nu_mu,        bar{nu_mu},   nu_e,         bar{nu_e}
---------------------------------------------------------*/
double oab00n[300][5]={
{2.500000e-02, 1.426105e+03, 2.117698e+03, 1.752012e+01, 3.549832e+00},
{7.500000e-02, 7.180795e+03, 6.722104e+03, 1.032965e+02, 1.372092e+01},
{1.250000e-01, 1.491096e+04, 3.700187e+03, 2.301362e+02, 2.264097e+01},
{1.750000e-01, 2.161415e+04, 2.997707e+03, 3.474821e+02, 2.581479e+01},
{2.250000e-01, 3.005732e+04, 2.772859e+03, 3.829022e+02, 3.542567e+01},
{2.750000e-01, 3.962913e+04, 2.853957e+03, 4.576131e+02, 3.846221e+01},
{3.250000e-01, 5.023843e+04, 3.050746e+03, 5.308084e+02, 4.264121e+01},
{3.750000e-01, 5.929475e+04, 2.976591e+03, 6.040116e+02, 5.187093e+01},
{4.250000e-01, 6.783075e+04, 3.172131e+03, 6.493191e+02, 5.317921e+01},
{4.750000e-01, 7.489746e+04, 3.158574e+03, 6.211424e+02, 4.948524e+01},
{5.250000e-01, 8.256432e+04, 3.461464e+03, 6.844996e+02, 5.803163e+01},
{5.750000e-01, 8.946080e+04, 3.853965e+03, 6.416525e+02, 5.007985e+01},
{6.250000e-01, 9.560317e+04, 3.903296e+03, 7.415673e+02, 5.433311e+01},
{6.750000e-01, 9.823842e+04, 4.030817e+03, 6.185793e+02, 5.583009e+01},
{7.250000e-01, 1.006390e+05, 3.794860e+03, 6.463874e+02, 6.828545e+01},
{7.750000e-01, 1.021158e+05, 3.808868e+03, 5.676068e+02, 6.340274e+01},
{8.250000e-01, 1.064450e+05, 3.848740e+03, 5.895861e+02, 4.915441e+01},
{8.750000e-01, 1.094614e+05, 3.873778e+03, 5.899774e+02, 5.916065e+01},
{9.250000e-01, 1.122099e+05, 4.219126e+03, 5.431951e+02, 6.313212e+01},
{9.750000e-01, 1.176245e+05, 3.970915e+03, 5.854676e+02, 4.710146e+01},
{1.025000e+00, 1.189644e+05, 4.544965e+03, 4.801114e+02, 4.613862e+01},
{1.075000e+00, 1.206597e+05, 4.562729e+03, 5.752102e+02, 5.785637e+01},
{1.125000e+00, 1.211842e+05, 4.630341e+03, 6.592584e+02, 4.595771e+01},
{1.175000e+00, 1.211520e+05, 4.646760e+03, 5.609600e+02, 4.603525e+01},
{1.225000e+00, 1.227845e+05, 5.054849e+03, 4.850657e+02, 3.982697e+01},
{1.275000e+00, 1.209643e+05, 4.528534e+03, 4.880475e+02, 4.698907e+01},
{1.325000e+00, 1.155540e+05, 4.349668e+03, 4.646054e+02, 5.158870e+01},
{1.375000e+00, 1.132256e+05, 4.784448e+03, 5.113431e+02, 4.375662e+01},
{1.425000e+00, 1.112804e+05, 4.090814e+03, 4.259785e+02, 4.554941e+01},
{1.475000e+00, 1.073834e+05, 4.253418e+03, 5.062170e+02, 4.309884e+01},
{1.525000e+00, 1.042335e+05, 4.408322e+03, 3.899403e+02, 3.421151e+01},
{1.575000e+00, 1.002864e+05, 3.842984e+03, 4.410349e+02, 4.121026e+01},
{1.625000e+00, 1.034292e+05, 4.164009e+03, 3.566596e+02, 3.005466e+01},
{1.675000e+00, 9.979750e+04, 3.456470e+03, 4.277100e+02, 3.613551e+01},
{1.725000e+00, 9.626789e+04, 4.145944e+03, 3.793271e+02, 3.909068e+01},
{1.775000e+00, 9.799159e+04, 3.681552e+03, 3.374257e+02, 4.362546e+01},
{1.825000e+00, 9.277737e+04, 3.583777e+03, 4.092570e+02, 3.418267e+01},
{1.875000e+00, 9.362887e+04, 3.471804e+03, 3.897814e+02, 3.786842e+01},
{1.925000e+00, 8.968647e+04, 3.601135e+03, 3.738465e+02, 4.435750e+01},
{1.975000e+00, 9.053383e+04, 3.115931e+03, 2.889146e+02, 3.983417e+01},
{2.025000e+00, 8.836852e+04, 3.268199e+03, 3.590314e+02, 3.615021e+01},
{2.075000e+00, 8.650435e+04, 3.074431e+03, 3.223420e+02, 3.055509e+01},
{2.125000e+00, 8.202543e+04, 2.806622e+03, 2.479358e+02, 3.057994e+01},
{2.175000e+00, 7.874777e+04, 2.717395e+03, 2.899620e+02, 3.049200e+01},
{2.225000e+00, 7.809295e+04, 2.857054e+03, 3.091345e+02, 2.617111e+01},
{2.275000e+00, 7.229225e+04, 2.741254e+03, 2.029171e+02, 2.721885e+01},
{2.325000e+00, 7.235070e+04, 2.524228e+03, 2.927295e+02, 2.861141e+01},
{2.375000e+00, 6.587733e+04, 3.555872e+03, 2.384099e+02, 3.967125e+01},
{2.425000e+00, 6.589644e+04, 2.138509e+03, 1.885346e+02, 2.367155e+01},
{2.475000e+00, 5.835548e+04, 2.034759e+03, 3.688933e+02, 3.466182e+01},
{2.525000e+00, 5.601993e+04, 2.267788e+03, 2.023541e+02, 3.008160e+01},
{2.575000e+00, 5.503808e+04, 2.859894e+03, 1.821552e+02, 2.773873e+01},
{2.625000e+00, 5.075045e+04, 2.683754e+03, 2.279164e+02, 3.220313e+01},
{2.675000e+00, 5.102620e+04, 2.876317e+03, 2.091108e+02, 3.536959e+01},
{2.725000e+00, 4.697609e+04, 2.483252e+03, 1.694572e+02, 2.608778e+01},
{2.775000e+00, 4.595750e+04, 2.331443e+03, 1.655830e+02, 2.809551e+01},
{2.825000e+00, 3.885841e+04, 1.913924e+03, 1.843920e+02, 2.759493e+01},
{2.875000e+00, 4.033482e+04, 2.177190e+03, 2.345350e+02, 3.947590e+01},
{2.925000e+00, 3.646274e+04, 1.982257e+03, 2.141708e+02, 3.516772e+01},
{2.975000e+00, 3.525966e+04, 2.283365e+03, 1.377807e+02, 2.369408e+01},
{3.025000e+00, 3.484082e+04, 2.117496e+03, 2.934663e+02, 2.854701e+01},
{3.075000e+00, 3.162868e+04, 1.560991e+03, 1.459072e+02, 2.505508e+01},
{3.125000e+00, 3.103701e+04, 1.547286e+03, 1.844746e+02, 2.658617e+01},
{3.175000e+00, 3.055020e+04, 1.273185e+03, 3.284943e+02, 2.918038e+01},
{3.225000e+00, 2.388432e+04, 1.832331e+03, 1.572401e+02, 2.743420e+01},
{3.275000e+00, 2.594204e+04, 1.094671e+03, 1.610070e+02, 1.504941e+01},
{3.325000e+00, 2.558212e+04, 1.788422e+03, 1.847847e+02, 2.037139e+01},
{3.375000e+00, 2.352124e+04, 1.696984e+03, 1.504724e+02, 2.785846e+01},
{3.425000e+00, 2.300254e+04, 1.779823e+03, 1.281083e+02, 2.589741e+01},
{3.475000e+00, 1.919947e+04, 2.398301e+03, 1.231954e+02, 2.903565e+01},
{3.525000e+00, 2.212283e+04, 1.537682e+03, 9.736511e+01, 1.853329e+01},
{3.575000e+00, 1.916016e+04, 1.011875e+03, 1.026355e+02, 2.620048e+01},
{3.625000e+00, 1.833307e+04, 1.340613e+03, 1.328411e+02, 2.329394e+01},
{3.675000e+00, 1.726702e+04, 1.370821e+03, 8.788000e+01, 1.403942e+01},
{3.725000e+00, 1.587804e+04, 1.929739e+03, 9.056228e+01, 1.823174e+01},
{3.775000e+00, 1.694577e+04, 1.355269e+03, 1.010744e+02, 2.578131e+01},
{3.825000e+00, 1.548353e+04, 1.546183e+03, 1.260755e+02, 1.708937e+01},
{3.875000e+00, 1.486659e+04, 1.101267e+03, 1.487139e+02, 2.099697e+01},
{3.925000e+00, 1.134328e+04, 6.106194e+02, 1.001638e+02, 2.378283e+01},
{3.975000e+00, 1.395588e+04, 1.411929e+03, 1.415243e+02, 2.130045e+01},
{4.025000e+00, 1.349156e+04, 1.966549e+03, 9.006801e+01, 1.482487e+01},
{4.075000e+00, 1.252467e+04, 1.400939e+03, 8.917651e+01, 1.921251e+01},
{4.125000e+00, 9.245510e+03, 1.533857e+03, 1.692246e+02, 1.339060e+01},
{4.175000e+00, 1.066685e+04, 1.320977e+03, 8.060383e+01, 1.586708e+01},
{4.225000e+00, 1.012843e+04, 1.095982e+03, 1.570388e+02, 2.799123e+01},
{4.275000e+00, 8.627640e+03, 8.929389e+02, 9.698533e+01, 2.200373e+01},
{4.325000e+00, 8.414979e+03, 8.966005e+02, 7.497457e+01, 1.216412e+01},
{4.375000e+00, 6.749234e+03, 1.043810e+03, 6.465861e+01, 1.109240e+01},
{4.425000e+00, 7.793058e+03, 3.259356e+02, 7.453339e+01, 1.971331e+01},
{4.475000e+00, 6.900975e+03, 7.550613e+02, 1.649707e+02, 1.492702e+01},
{4.525000e+00, 5.523882e+03, 9.364663e+02, 1.198057e+02, 2.909309e+01},
{4.575000e+00, 6.436607e+03, 1.584636e+03, 2.216128e+02, 1.712922e+01},
{4.625000e+00, 5.928472e+03, 1.284079e+03, 7.200126e+01, 2.360170e+01},
{4.675000e+00, 4.877054e+03, 1.485800e+03, 6.599231e+01, 1.257288e+01},
{4.725000e+00, 6.465397e+03, 1.229424e+03, 1.413973e+02, 2.987851e+01},
{4.775000e+00, 4.832394e+03, 7.313843e+02, 7.626188e+01, 1.823265e+01},
{4.825000e+00, 5.854798e+03, 1.232488e+03, 5.234122e+01, 1.242894e+01},
{4.875000e+00, 3.719017e+03, 5.429653e+02, 6.964379e+01, 1.291541e+01},
{4.925000e+00, 4.341517e+03, 2.057182e+02, 1.327792e+02, 8.884104e+00},
{4.975000e+00, 3.291844e+03, 4.034839e+02, 6.745713e+01, 1.376097e+01},
{5.025000e+00, 3.018168e+03, 7.673661e+02, 6.734524e+01, 7.843733e+00},
{5.075000e+00, 1.959890e+03, 6.095916e+02, 4.801644e+01, 1.129203e+01},
{5.125000e+00, 3.618818e+03, 9.962431e+02, 7.794364e+01, 1.839625e+01},
{5.175000e+00, 2.238198e+03, 4.399045e+02, 6.485794e+01, 2.225645e+01},
{5.225000e+00, 4.301119e+03, 1.041377e+03, 6.779303e+01, 1.095819e+01},
{5.275000e+00, 2.573057e+03, 1.449066e+03, 6.911891e+01, 2.163881e+01},
{5.325000e+00, 2.785915e+03, 1.058023e+03, 5.066934e+01, 7.244183e+00},
{5.375000e+00, 4.064610e+03, 1.099446e+03, 5.729937e+01, 9.508949e+00},
{5.425000e+00, 3.777305e+03, 2.805117e+01, 5.469790e+01, 7.523653e+00},
{5.475000e+00, 2.372205e+03, 4.593941e+02, 6.196703e+01, 1.578164e+01},
{5.525000e+00, 3.630741e+03, 1.825321e+03, 6.080151e+01, 8.600564e+00},
{5.575000e+00, 2.487829e+03, 9.557956e+02, 4.855778e+01, 4.579547e+00},
{5.625000e+00, 3.090218e+03, 6.140407e+01, 6.228247e+01, 1.820707e+01},
{5.675000e+00, 2.928088e+03, 7.271900e+02, 5.781636e+01, 7.825139e+00},
{5.725000e+00, 1.241007e+03, 3.114209e+02, 5.360651e+01, 1.185963e+01},
{5.775000e+00, 2.611723e+03, 3.040053e+02, 5.291731e+01, 7.288390e+00},
{5.825000e+00, 1.681717e+03, 1.024302e+03, 5.032822e+01, 1.286200e+01},
{5.875000e+00, 1.950914e+03, 1.457848e+01, 6.183108e+01, 1.761097e+01},
{5.925000e+00, 2.692086e+03, 2.602458e+01, 5.241359e+01, 3.431225e+00},
{5.975000e+00, 1.982095e+03, 5.436852e+02, 2.046530e+02, 1.021391e+01},
{6.025000e+00, 1.153545e+03, 8.163908e+02, 6.069859e+01, 1.547394e+01},
{6.075000e+00, 2.040140e+03, 1.953328e+01, 5.687274e+01, 4.147234e+00},
{6.125000e+00, 2.617538e+03, 5.670158e+02, 4.307689e+01, 4.547263e+00},
{6.175000e+00, 3.042624e+03, 3.060248e+02, 5.434922e+01, 1.440785e+01},
{6.225000e+00, 2.153593e+03, 8.742362e+02, 4.722197e+01, 7.328705e+00},
{6.275000e+00, 1.249836e+03, 2.931610e+01, 4.506956e+01, 9.151576e+00},
{6.325000e+00, 1.504826e+03, 3.139510e+02, 4.114405e+01, 1.203773e+01},
{6.375000e+00, 1.877395e+03, 1.520275e+03, 4.021871e+01, 5.164902e+00},
{6.425000e+00, 1.231204e+03, 8.717113e+01, 5.055222e+01, 1.981379e+01},
{6.475000e+00, 1.526173e+03, 4.010765e+02, 3.950686e+01, 7.055418e+00},
{6.525000e+00, 1.328663e+03, 5.403686e+01, 3.756578e+01, 4.839901e+00},
{6.575000e+00, 2.241772e+03, 3.362652e+02, 6.011583e+01, 1.434050e+01},
{6.625000e+00, 1.576580e+03, 3.475657e+02, 4.080908e+01, 8.200981e+00},
{6.675000e+00, 1.941508e+03, 6.823339e+02, 2.943385e+01, 8.234124e-01},
{6.725000e+00, 1.697813e+03, 6.934465e+02, 3.827869e+01, 6.417133e+00},
{6.775000e+00, 1.004415e+03, 3.703389e+02, 2.485402e+01, 2.599138e+00},
{6.825000e+00, 1.365533e+03, 3.733725e+02, 4.687823e+01, 8.216550e+00},
{6.875000e+00, 2.678415e+03, 2.502571e+01, 4.032418e+01, 1.377806e+01},
{6.925000e+00, 1.686573e+03, 3.665573e+02, 3.883537e+01, 0.000000e+00},
{6.975000e+00, 6.893508e+02, 1.541667e+01, 3.004082e+01, 5.931632e+00},
{7.025000e+00, 1.812971e+03, 3.941838e+02, 3.248461e+01, 6.975180e+00},
{7.075000e+00, 9.666604e+02, 1.057072e+01, 2.388778e+01, 0.000000e+00},
{7.125000e+00, 1.441978e+03, 3.225666e+01, 2.012868e+01, 0.000000e+00},
{7.175000e+00, 1.442106e+03, 3.989403e+02, 1.792060e+02, 7.360614e+00},
{7.225000e+00, 7.452432e+02, 1.655339e+01, 5.647770e+01, 3.038451e+01},
{7.275000e+00, 2.130511e+03, 1.676941e+01, 3.279528e+01, 1.828856e+01},
{7.325000e+00, 9.958749e+02, 3.418308e+01, 3.184184e+01, 5.166592e-01},
{7.375000e+00, 1.806901e+03, 4.131495e+02, 3.294077e+01, 1.208334e+01},
{7.425000e+00, 4.484340e+02, 4.237824e+02, 3.694163e+01, 6.716911e-01},
{7.475000e+00, 1.008741e+03, 8.541498e+02, 4.334962e+01, 1.150204e+01},
{7.525000e+00, 1.457814e+03, 2.386162e+01, 3.649506e+01, 1.469925e+01},
{7.575000e+00, 1.032297e+03, 6.041566e+00, 3.513241e+01, 9.552881e+00},
{7.625000e+00, 6.334366e+02, 4.554688e+02, 1.744352e+02, 2.613764e+00},
{7.675000e+00, 1.544243e+03, 3.117757e+01, 2.990226e+01, 6.210993e+00},
{7.725000e+00, 1.544149e+03, 4.732539e+02, 3.363072e+01, 1.056791e+01},
{7.775000e+00, 8.718473e+02, 4.724020e+02, 2.247762e+01, 0.000000e+00},
{7.825000e+00, 1.401242e+03, 9.217763e+02, 2.345374e+01, 1.667682e+00},
{7.875000e+00, 6.166798e+02, 3.285874e+01, 2.450856e+01, 0.000000e+00},
{7.925000e+00, 1.446843e+03, 7.809404e+01, 2.043155e+01, 0.000000e+00},
{7.975000e+00, 1.461677e+03, 1.342844e+01, 2.722642e+01, 0.000000e+00},
{8.025000e+00, 9.634534e+02, 2.041615e+01, 3.487486e+01, 8.625995e+00},
{8.075000e+00, 1.018970e+03, 1.377926e+01, 1.606025e+01, 1.901274e+00},
{8.125000e+00, 6.286197e+02, 3.498509e+01, 3.868963e+01, 9.692703e+00},
{8.175000e+00, 5.579690e+02, 2.833088e+01, 2.759706e+01, 1.037959e+01},
{8.225000e+00, 1.042881e+03, 2.149611e+01, 1.048939e+01, 9.612880e-01},
{8.275000e+00, 4.999731e+02, 0.000000e+00, 2.008661e+01, 9.905859e-01},
{8.325000e+00, 9.595461e+02, 1.462276e+01, 2.663889e+01, 4.579930e+00},
{8.375000e+00, 5.341095e+02, 1.481693e+01, 2.566095e+01, 0.000000e+00},
{8.425000e+00, 1.042223e+03, 5.671462e+02, 2.963541e+01, 9.577203e+00},
{8.475000e+00, 5.851836e+02, 2.271544e+01, 1.605777e+01, 4.167512e+00},
{8.525000e+00, 4.687212e+02, 1.536544e+01, 2.707677e+01, 0.000000e+00},
{8.575000e+00, 4.201002e+02, 7.764734e+00, 1.902323e+01, 1.434910e+00},
{8.625000e+00, 1.062823e+03, 7.837018e+00, 1.796505e+01, 5.248849e+00},
{8.675000e+00, 3.981273e+02, 2.376728e+01, 2.964846e+01, 1.469838e+01},
{8.725000e+00, 4.107833e+02, 3.223620e+01, 2.728440e+01, 4.274544e+00},
{8.775000e+00, 4.643330e+02, 5.949476e+02, 9.078579e+00, 0.000000e+00},
{8.825000e+00, 4.859237e+02, 1.647849e+01, 2.637703e+01, 1.421446e+01},
{8.875000e+00, 1.165727e+03, 1.666480e+01, 2.353765e+01, 6.454166e+00},
{8.925000e+00, 5.392480e+02, 0.000000e+00, 2.048477e+01, 0.000000e+00},
{8.975000e+00, 6.051116e+02, 8.552061e+00, 1.284706e+01, 3.356814e+00},
{9.025000e+00, 1.100209e+03, 0.000000e+00, 1.144129e+01, 5.643359e+00},
{9.075000e+00, 3.483931e+02, 6.199032e+02, 1.564450e+01, 0.000000e+00},
{9.125000e+00, 1.096206e+03, 1.868168e+03, 1.978681e+01, 4.783290e+00},
{9.175000e+00, 3.471044e+02, 0.000000e+00, 2.375155e+01, 5.051271e+00},
{9.225000e+00, 5.490322e+02, 9.035939e+00, 2.270545e+01, 9.364542e+00},
{9.275000e+00, 4.368919e+02, 6.418812e+02, 2.197133e+01, 1.520850e+01},
{9.325000e+00, 5.239343e+02, 9.178243e+00, 2.061416e+01, 0.000000e+00},
{9.375000e+00, 5.112241e+02, 0.000000e+00, 1.449719e+01, 0.000000e+00},
{9.425000e+00, 4.224113e+02, 0.000000e+00, 2.489488e+00, 0.000000e+00},
{9.475000e+00, 1.087148e+03, 1.895575e+01, 2.059799e+01, 0.000000e+00},
{9.525000e+00, 4.509091e+02, 0.000000e+00, 2.772419e+01, 1.683443e+01},
{9.575000e+00, 5.528755e+02, 2.916290e+01, 1.236160e+01, 0.000000e+00},
{9.625000e+00, 4.311641e+02, 9.753806e+00, 9.077798e+00, 0.000000e+00},
{9.675000e+00, 3.764127e+02, 6.969901e+02, 1.175944e+01, 0.000000e+00},
{9.725000e+00, 4.301817e+02, 1.000080e+01, 1.431876e+01, 6.415326e+00},
{9.775000e+00, 4.645098e+02, 0.000000e+00, 9.509453e+00, 0.000000e+00},
{9.825000e+00, 4.083815e+02, 1.019710e+01, 3.932057e+00, 0.000000e+00},
{9.875000e+00, 3.195635e+02, 1.034145e+01, 1.126641e+01, 0.000000e+00},
{9.925000e+00, 4.792439e+02, 7.498265e+02, 5.110629e+00, 0.000000e+00},
{9.975000e+00, 4.414995e+02, 1.052062e+01, 1.031084e+01, 1.187582e+01},
{1.002500e+01, 1.218385e+03, 2.130703e+01, 1.808703e+01, 7.813909e+00},
{1.007500e+01, 4.294678e+02, 1.075697e+01, 1.106106e+01, 6.076981e+00},
{1.012500e+01, 4.882096e+02, 3.252542e+01, 1.355636e+01, 4.982478e+00},
{1.017500e+01, 5.145547e+02, 2.199741e+01, 1.079274e+01, 0.000000e+00},
{1.022500e+01, 4.421722e+02, 2.210653e+01, 1.389709e+01, 5.392297e+00},
{1.027500e+01, 4.690695e+02, 2.241595e+01, 6.042808e+00, 0.000000e+00},
{1.032500e+01, 3.159600e+02, 3.388959e+01, 2.073850e+01, 1.449323e+01},
{1.037500e+01, 2.959643e+02, 1.140126e+01, 5.918345e+00, 1.200767e+00},
{1.042500e+01, 4.135468e+02, 1.152644e+01, 1.054785e+01, 4.639777e+00},
{1.047500e+01, 3.830427e+02, 2.320504e+01, 8.031515e+00, 1.814138e+00},
{1.052500e+01, 3.749177e+02, 0.000000e+00, 8.032806e+00, 0.000000e+00},
{1.057500e+01, 4.609975e+02, 1.179302e+01, 9.690483e+00, 0.000000e+00},
{1.062500e+01, 4.060364e+02, 0.000000e+00, 8.755623e+00, 1.523381e+00},
{1.067500e+01, 3.134221e+02, 1.209863e+01, 8.040954e+00, 0.000000e+00},
{1.072500e+01, 4.139009e+02, 0.000000e+00, 1.365329e+01, 0.000000e+00},
{1.077500e+01, 3.805567e+02, 0.000000e+00, 5.410119e+00, 0.000000e+00},
{1.082500e+01, 4.340079e+02, 0.000000e+00, 1.141477e+01, 8.497312e+00},
{1.087500e+01, 1.270806e+03, 0.000000e+00, 8.348315e+00, 0.000000e+00},
{1.092500e+01, 2.397451e+02, 3.793497e+01, 7.333538e+00, 0.000000e+00},
{1.097500e+01, 3.183529e+02, 0.000000e+00, 3.992140e+00, 0.000000e+00},
{1.102500e+01, 3.988047e+02, 1.285907e+01, 1.535263e+01, 9.535044e+00},
{1.107500e+01, 3.761675e+02, 3.899487e+01, 6.962712e+00, 0.000000e+00},
{1.112500e+01, 2.488961e+02, 1.305231e+01, 1.409529e+01, 5.045637e+00},
{1.117500e+01, 3.300260e+02, 1.315548e+01, 1.017363e+01, 0.000000e+00},
{1.122500e+01, 2.665364e+02, 2.668649e+01, 4.135292e+00, 0.000000e+00},
{1.127500e+01, 2.418265e+02, 0.000000e+00, 6.148813e+00, 0.000000e+00},
{1.132500e+01, 3.663171e+02, 0.000000e+00, 6.482019e+00, 0.000000e+00},
{1.137500e+01, 3.966392e+02, 2.735126e+01, 9.884412e+00, 0.000000e+00},
{1.142500e+01, 4.142885e+02, 0.000000e+00, 6.359705e+00, 0.000000e+00},
{1.147500e+01, 3.342944e+02, 0.000000e+00, 6.200901e+00, 0.000000e+00},
{1.152500e+01, 3.933546e+02, 0.000000e+00, 3.828131e+00, 0.000000e+00},
{1.157500e+01, 2.551383e+02, 2.832630e+01, 0.000000e+00, 0.000000e+00},
{1.162500e+01, 2.859927e+02, 0.000000e+00, 2.108262e+01, 1.101694e+01},
{1.167500e+01, 3.316023e+02, 0.000000e+00, 7.576418e+00, 0.000000e+00},
{1.172500e+01, 1.599113e+02, 0.000000e+00, 4.611788e+00, 0.000000e+00},
{1.177500e+01, 2.051157e+02, 1.461636e+01, 8.887583e+00, 0.000000e+00},
{1.182500e+01, 1.921208e+02, 0.000000e+00, 2.187464e+00, 0.000000e+00},
{1.187500e+01, 3.131800e+02, 0.000000e+00, 3.359077e+00, 0.000000e+00},
{1.192500e+01, 2.707970e+02, 1.497785e+01, 1.447279e+00, 0.000000e+00},
{1.197500e+01, 2.877241e+02, 0.000000e+00, 7.321613e+00, 0.000000e+00},
{1.202500e+01, 4.435341e+02, 0.000000e+00, 2.389311e+00, 0.000000e+00},
{1.207500e+01, 4.008434e+02, 0.000000e+00, 3.546417e+00, 0.000000e+00},
{1.212500e+01, 2.642922e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.217500e+01, 3.762467e+02, 0.000000e+00, 7.547314e+00, 0.000000e+00},
{1.222500e+01, 2.528915e+02, 1.582173e+01, 3.453191e+00, 0.000000e+00},
{1.227500e+01, 3.027184e+02, 0.000000e+00, 1.056483e+01, 0.000000e+00},
{1.232500e+01, 2.248910e+02, 1.606896e+01, 0.000000e+00, 0.000000e+00},
{1.237500e+01, 3.237269e+02, 0.000000e+00, 5.687717e+00, 0.000000e+00},
{1.242500e+01, 2.449626e+02, 0.000000e+00, 6.600531e+00, 0.000000e+00},
{1.247500e+01, 1.646878e+02, 0.000000e+00, 1.149696e+01, 0.000000e+00},
{1.252500e+01, 2.652741e+02, 0.000000e+00, 2.942624e+00, 0.000000e+00},
{1.257500e+01, 3.178325e+02, 1.675066e+01, 4.138028e+00, 0.000000e+00},
{1.262500e+01, 2.193159e+02, 0.000000e+00, 3.224763e+00, 0.000000e+00},
{1.267500e+01, 2.891198e+02, 0.000000e+00, 2.260606e+00, 0.000000e+00},
{1.272500e+01, 2.396771e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.277500e+01, 2.762951e+02, 0.000000e+00, 6.939776e+00, 6.939776e+00},
{1.282500e+01, 1.914338e+02, 0.000000e+00, 4.805463e+00, 0.000000e+00},
{1.287500e+01, 2.453744e+02, 0.000000e+00, 4.364903e+00, 0.000000e+00},
{1.292500e+01, 3.358981e+02, 0.000000e+00, 8.657945e+00, 0.000000e+00},
{1.297500e+01, 2.137590e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.302500e+01, 3.049878e+02, 0.000000e+00, 4.214303e+00, 0.000000e+00},
{1.307500e+01, 9.028616e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.312500e+01, 2.550586e+02, 0.000000e+00, 9.817044e+00, 0.000000e+00},
{1.317500e+01, 2.201426e+02, 0.000000e+00, 3.739234e+00, 0.000000e+00},
{1.322500e+01, 2.035316e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.327500e+01, 1.865565e+02, 0.000000e+00, 2.116148e+00, 0.000000e+00},
{1.332500e+01, 5.630102e+01, 0.000000e+00, 5.034748e+00, 0.000000e+00},
{1.337500e+01, 2.271292e+02, 1.890688e+01, 3.877279e+00, 0.000000e+00},
{1.342500e+01, 1.905735e+02, 0.000000e+00, 3.733505e+00, 0.000000e+00},
{1.347500e+01, 1.920621e+02, 0.000000e+00, 1.955131e+00, 0.000000e+00},
{1.352500e+01, 3.096037e+02, 0.000000e+00, 4.408436e+00, 0.000000e+00},
{1.357500e+01, 2.534634e+02, 0.000000e+00, 2.027411e+00, 0.000000e+00},
{1.362500e+01, 1.767053e+02, 0.000000e+00, 2.758025e+00, 0.000000e+00},
{1.367500e+01, 1.979985e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.372500e+01, 1.194917e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.377500e+01, 1.805403e+02, 2.007259e+01, 0.000000e+00, 0.000000e+00},
{1.382500e+01, 1.012007e+02, 0.000000e+00, 3.782456e+00, 0.000000e+00},
{1.387500e+01, 8.127997e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.392500e+01, 1.229497e+02, 2.045930e+01, 0.000000e+00, 0.000000e+00},
{1.397500e+01, 1.858014e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.402500e+01, 1.040068e+02, 0.000000e+00, 1.995841e+00, 0.000000e+00},
{1.407500e+01, 8.388003e+01, 0.000000e+00, 2.533309e+00, 0.000000e+00},
{1.412500e+01, 1.900217e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.417500e+01, 2.124914e+02, 0.000000e+00, 4.212828e+00, 0.000000e+00},
{1.422500e+01, 1.068463e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.427500e+01, 1.508286e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.432500e+01, 1.300741e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.437500e+01, 1.966490e+02, 0.000000e+00, 2.576558e+00, 0.000000e+00},
{1.442500e+01, 1.760947e+02, 0.000000e+00, 4.420022e+00, 0.000000e+00},
{1.447500e+01, 1.328840e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.452500e+01, 1.339233e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.457500e+01, 1.796381e+02, 0.000000e+00, 3.273504e+00, 0.000000e+00},
{1.462500e+01, 6.782783e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.467500e+01, 2.734324e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.472500e+01, 1.375944e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.477500e+01, 2.307676e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.482500e+01, 1.858620e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.487500e+01, 1.639275e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.492500e+01, 7.062778e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.497500e+01, 9.493300e+01, 2.364419e+01, 0.000000e+00, 0.000000e+00}
};
