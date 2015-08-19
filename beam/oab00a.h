/*!
 ---------------------------------------------------------------------\n

 \short off-axis angle ��beam profile (0.0 \f$\bar{\nu}\f$ foucus)

 \b ����

 50MeV��ߤΥӡ���Υե�å������󡣾��� 1000 km ��
 ���ͥ륮����󥸤� 0.0GeV - 15GeV
 (index �ˤ���� 0-299�ޤǤ�300��)

 ---------------------------------------------------------------------\n

 \author "$Author: okamura $"

 \date "$Date: 2011-01-18 13:45:53 +0900 (Tue, 18 Jan 2011) $" 

 \version "$Name:  $-$Revision: 188 $-$State: Exp $"

 \note "$Source: /home/cvsroot/std.set.T2K4.3/Beam/oab00a.cc,v $"

 ---------------------------------------------------------------------\n

 \b ����ˡ

 \return �������Ǥ�
 \f$ E_\nu(GeV),\nu_\mu,\bar{\nu}_{\mu},\nu_e,\bar{\nu}_e \f$

 ---------------------------------------------------------------------\n
*/
/*--------------------------------------------------------
// Neutrino flux table at 1000 km 
// off axcis angle is 00a
// Enu(GeV),   nu_mu,        bar{nu_mu},   nu_e,         bar{nu_e}
---------------------------------------------------------*/
double oab00a[300][5]={
{2.500000e-02, 2.221282e+03, 1.423369e+03, 4.248235e+00, 1.744267e+01},
{7.500000e-02, 7.106440e+03, 7.333557e+03, 1.580052e+01, 1.054028e+02},
{1.250000e-01, 3.898924e+03, 1.523187e+04, 2.768612e+01, 2.161661e+02},
{1.750000e-01, 3.174997e+03, 2.182388e+04, 3.855134e+01, 3.059538e+02},
{2.250000e-01, 2.939490e+03, 2.990293e+04, 5.239819e+01, 4.101578e+02},
{2.750000e-01, 3.020429e+03, 3.898214e+04, 6.014172e+01, 5.070305e+02},
{3.250000e-01, 3.273850e+03, 4.844442e+04, 6.142891e+01, 5.048774e+02},
{3.750000e-01, 3.398612e+03, 5.655109e+04, 6.430245e+01, 5.616005e+02},
{4.250000e-01, 3.641704e+03, 6.271069e+04, 7.618181e+01, 6.111378e+02},
{4.750000e-01, 3.925748e+03, 6.873476e+04, 7.462188e+01, 5.916333e+02},
{5.250000e-01, 4.067401e+03, 7.483501e+04, 6.492367e+01, 5.394050e+02},
{5.750000e-01, 4.146952e+03, 7.960439e+04, 7.824856e+01, 5.141372e+02},
{6.250000e-01, 4.342814e+03, 8.415069e+04, 7.595488e+01, 6.029974e+02},
{6.750000e-01, 4.624324e+03, 8.718306e+04, 6.849376e+01, 6.038481e+02},
{7.250000e-01, 4.425392e+03, 8.807359e+04, 6.386639e+01, 5.362406e+02},
{7.750000e-01, 4.846750e+03, 8.906811e+04, 6.156985e+01, 5.244658e+02},
{8.250000e-01, 4.857878e+03, 9.020346e+04, 6.485171e+01, 4.821289e+02},
{8.750000e-01, 4.789427e+03, 9.492644e+04, 6.428228e+01, 4.411714e+02},
{9.250000e-01, 4.925959e+03, 9.677213e+04, 6.221357e+01, 4.726156e+02},
{9.750000e-01, 5.023223e+03, 9.893817e+04, 5.527375e+01, 5.210124e+02},
{1.025000e+00, 5.270268e+03, 9.767885e+04, 6.969922e+01, 3.951642e+02},
{1.075000e+00, 5.322161e+03, 1.010560e+05, 6.249765e+01, 4.225526e+02},
{1.125000e+00, 5.966925e+03, 1.003115e+05, 6.677156e+01, 4.815891e+02},
{1.175000e+00, 5.815669e+03, 9.947010e+04, 5.166764e+01, 4.229476e+02},
{1.225000e+00, 6.076867e+03, 1.008867e+05, 5.782951e+01, 2.812414e+02},
{1.275000e+00, 5.878352e+03, 9.494655e+04, 7.089502e+01, 3.706899e+02},
{1.325000e+00, 5.838437e+03, 9.288425e+04, 5.216071e+01, 3.295947e+02},
{1.375000e+00, 6.151589e+03, 8.992052e+04, 6.119872e+01, 3.834861e+02},
{1.425000e+00, 5.684958e+03, 8.593685e+04, 6.102475e+01, 2.884753e+02},
{1.475000e+00, 6.258865e+03, 8.602191e+04, 6.332711e+01, 3.595452e+02},
{1.525000e+00, 5.895723e+03, 8.223445e+04, 4.845467e+01, 3.368035e+02},
{1.575000e+00, 5.474670e+03, 7.966153e+04, 5.868179e+01, 3.035237e+02},
{1.625000e+00, 5.698402e+03, 7.902007e+04, 4.423934e+01, 2.563360e+02},
{1.675000e+00, 5.442645e+03, 7.669699e+04, 6.444508e+01, 3.868336e+02},
{1.725000e+00, 5.994804e+03, 7.777047e+04, 5.295805e+01, 1.730683e+02},
{1.775000e+00, 5.770473e+03, 7.573072e+04, 4.127125e+01, 2.039467e+02},
{1.825000e+00, 5.021404e+03, 7.333324e+04, 4.452018e+01, 2.849943e+02},
{1.875000e+00, 5.388929e+03, 7.127872e+04, 4.093877e+01, 2.294208e+02},
{1.925000e+00, 5.091112e+03, 7.053190e+04, 6.473863e+01, 1.460196e+02},
{1.975000e+00, 5.710486e+03, 6.517600e+04, 4.911990e+01, 1.590236e+02},
{2.025000e+00, 4.777991e+03, 6.749318e+04, 5.256143e+01, 1.855254e+02},
{2.075000e+00, 4.582575e+03, 6.468863e+04, 3.912506e+01, 1.834238e+02},
{2.125000e+00, 5.042923e+03, 6.324761e+04, 5.469961e+01, 1.762048e+02},
{2.175000e+00, 5.010334e+03, 5.802023e+04, 6.994124e+01, 2.216404e+02},
{2.225000e+00, 4.257977e+03, 5.518536e+04, 5.071089e+01, 2.096355e+02},
{2.275000e+00, 4.329762e+03, 5.517190e+04, 6.578006e+01, 1.255444e+02},
{2.325000e+00, 4.621674e+03, 5.123764e+04, 4.519088e+01, 1.661632e+02},
{2.375000e+00, 4.684585e+03, 4.863872e+04, 4.543084e+01, 1.822379e+02},
{2.425000e+00, 4.585439e+03, 4.491926e+04, 4.433491e+01, 1.511173e+02},
{2.475000e+00, 4.035633e+03, 4.276722e+04, 3.733868e+01, 2.192401e+02},
{2.525000e+00, 3.903739e+03, 4.036904e+04, 3.821892e+01, 2.214525e+02},
{2.575000e+00, 3.657277e+03, 3.721010e+04, 3.725602e+01, 7.187624e+01},
{2.625000e+00, 3.850446e+03, 3.540410e+04, 4.237678e+01, 1.673645e+02},
{2.675000e+00, 3.625230e+03, 3.255944e+04, 4.105319e+01, 1.336749e+02},
{2.725000e+00, 3.796399e+03, 3.134126e+04, 5.283538e+01, 1.567344e+02},
{2.775000e+00, 4.179434e+03, 2.996602e+04, 3.536729e+01, 1.050439e+02},
{2.825000e+00, 3.107744e+03, 2.856300e+04, 4.466301e+01, 1.026193e+02},
{2.875000e+00, 3.475428e+03, 2.721364e+04, 4.257225e+01, 9.284450e+01},
{2.925000e+00, 3.271926e+03, 2.514847e+04, 3.584465e+01, 1.294318e+02},
{2.975000e+00, 3.176550e+03, 2.389497e+04, 3.558495e+01, 5.783460e+01},
{3.025000e+00, 3.374065e+03, 2.250340e+04, 4.231619e+01, 9.739912e+01},
{3.075000e+00, 3.256494e+03, 2.143340e+04, 3.168091e+01, 9.468646e+01},
{3.125000e+00, 2.599742e+03, 2.068903e+04, 4.341272e+01, 1.491723e+02},
{3.175000e+00, 3.321724e+03, 1.782153e+04, 3.736132e+01, 9.015866e+01},
{3.225000e+00, 2.695829e+03, 1.694252e+04, 3.714059e+01, 1.109993e+02},
{3.275000e+00, 2.294746e+03, 1.558730e+04, 4.093225e+01, 1.017044e+02},
{3.325000e+00, 2.416552e+03, 1.766930e+04, 2.488556e+01, 4.143190e+01},
{3.375000e+00, 2.694902e+03, 1.525663e+04, 3.880255e+01, 8.962592e+01},
{3.425000e+00, 2.664682e+03, 1.342990e+04, 3.290309e+01, 4.994125e+01},
{3.475000e+00, 2.984908e+03, 1.348218e+04, 3.966405e+01, 1.907241e+02},
{3.525000e+00, 2.957103e+03, 1.366695e+04, 3.863897e+01, 8.981259e+01},
{3.575000e+00, 3.042766e+03, 1.209699e+04, 3.636642e+01, 4.897702e+01},
{3.625000e+00, 3.496912e+03, 1.205262e+04, 3.704351e+01, 5.637323e+01},
{3.675000e+00, 2.618952e+03, 9.464289e+03, 4.151198e+01, 8.230341e+01},
{3.725000e+00, 2.529300e+03, 1.100992e+04, 3.559195e+01, 1.019945e+02},
{3.775000e+00, 2.831734e+03, 1.001449e+04, 3.278013e+01, 4.407133e+01},
{3.825000e+00, 2.353832e+03, 1.095861e+04, 2.790647e+01, 1.165486e+02},
{3.875000e+00, 3.003724e+03, 7.761404e+03, 2.834785e+01, 4.147466e+01},
{3.925000e+00, 2.547062e+03, 9.714633e+03, 2.621509e+01, 8.696322e+01},
{3.975000e+00, 3.951483e+03, 7.312913e+03, 2.504263e+01, 3.686909e+01},
{4.025000e+00, 3.067916e+03, 7.036417e+03, 2.186416e+01, 9.380115e+01},
{4.075000e+00, 1.680602e+03, 6.909635e+03, 2.898272e+01, 3.636616e+01},
{4.125000e+00, 2.453233e+03, 6.058520e+03, 4.995900e+01, 7.342192e+01},
{4.175000e+00, 3.042798e+03, 5.725355e+03, 2.265748e+01, 6.659837e+01},
{4.225000e+00, 2.231970e+03, 4.625481e+03, 3.688428e+01, 3.858655e+01},
{4.275000e+00, 2.970568e+03, 4.837519e+03, 2.319300e+01, 1.093543e+02},
{4.325000e+00, 2.857740e+03, 4.532299e+03, 3.050010e+01, 4.136330e+01},
{4.375000e+00, 2.840424e+03, 4.269753e+03, 4.441393e+01, 4.909871e+01},
{4.425000e+00, 2.314973e+03, 4.323444e+03, 2.087739e+01, 2.426333e+01},
{4.475000e+00, 2.314872e+03, 3.015119e+03, 3.377377e+01, 9.207776e+01},
{4.525000e+00, 2.508069e+03, 3.878246e+03, 3.570948e+01, 4.102157e+01},
{4.575000e+00, 2.565585e+03, 3.397125e+03, 2.367981e+01, 3.277791e+01},
{4.625000e+00, 2.292914e+03, 2.665242e+03, 3.631335e+01, 3.543353e+01},
{4.675000e+00, 2.366881e+03, 3.221114e+03, 2.279860e+01, 2.823682e+01},
{4.725000e+00, 1.137125e+03, 2.925504e+03, 2.986370e+01, 3.485944e+01},
{4.775000e+00, 3.114969e+03, 2.803679e+03, 3.230715e+01, 3.636952e+01},
{4.825000e+00, 7.720587e+02, 1.487006e+03, 2.647988e+01, 1.064335e+02},
{4.875000e+00, 1.997773e+03, 2.055654e+03, 2.408177e+01, 2.513822e+01},
{4.925000e+00, 2.233639e+03, 2.434000e+03, 2.010803e+01, 2.555786e+01},
{4.975000e+00, 2.120623e+03, 1.413687e+03, 3.326137e+01, 3.025335e+01},
{5.025000e+00, 1.787748e+03, 2.162794e+03, 2.170669e+01, 2.563823e+01},
{5.075000e+00, 1.069188e+03, 1.391732e+03, 3.691932e+01, 3.728402e+01},
{5.125000e+00, 1.458834e+03, 2.835406e+03, 2.879661e+01, 3.028401e+01},
{5.175000e+00, 8.701462e+02, 2.058921e+03, 2.886906e+01, 2.689944e+01},
{5.225000e+00, 9.435196e+02, 1.713543e+03, 1.957092e+01, 1.771005e+01},
{5.275000e+00, 1.095665e+03, 1.497017e+03, 2.365895e+01, 1.960428e+01},
{5.325000e+00, 1.763522e+03, 1.532955e+03, 1.758038e+01, 1.464644e+01},
{5.375000e+00, 2.327114e+03, 7.499700e+02, 2.538988e+01, 2.686680e+01},
{5.425000e+00, 3.195857e+03, 1.385962e+03, 2.350256e+01, 1.167648e+02},
{5.475000e+00, 7.639732e+02, 1.129419e+03, 2.827118e+01, 2.451102e+01},
{5.525000e+00, 1.950926e+03, 1.180942e+03, 2.908081e+01, 2.467571e+01},
{5.575000e+00, 1.268744e+03, 1.912035e+03, 1.963293e+01, 1.266435e+01},
{5.625000e+00, 7.990440e+02, 6.915995e+02, 1.646531e+01, 1.315201e+01},
{5.675000e+00, 1.305457e+03, 9.517420e+02, 1.031876e+01, 1.595855e+01},
{5.725000e+00, 1.311043e+03, 9.817582e+02, 2.294559e+01, 2.486198e+01},
{5.775000e+00, 1.088982e+03, 1.274309e+03, 2.244840e+01, 1.834645e+01},
{5.825000e+00, 1.358224e+03, 7.292325e+02, 2.192700e+01, 2.279985e+01},
{5.875000e+00, 1.081145e+03, 1.012239e+03, 1.726065e+01, 1.152607e+02},
{5.925000e+00, 1.171187e+03, 9.963598e+02, 5.295802e+01, 4.766486e+01},
{5.975000e+00, 2.223087e+03, 1.275438e+03, 3.268916e+01, 3.240528e+01},
{6.025000e+00, 1.170681e+03, 8.287210e+02, 2.492578e+01, 1.553732e+01},
{6.075000e+00, 1.011712e+03, 5.318206e+02, 2.333789e+01, 1.967081e+01},
{6.125000e+00, 1.788941e+03, 8.247836e+02, 2.531519e+01, 2.351760e+01},
{6.175000e+00, 2.653663e+03, 2.300444e+02, 2.425762e+01, 4.886782e+01},
{6.225000e+00, 1.238268e+03, 1.395539e+03, 1.462859e+01, 1.279277e+01},
{6.275000e+00, 6.832314e+02, 6.209265e+02, 1.299557e+01, 1.143704e+01},
{6.325000e+00, 1.909840e+03, 5.440094e+02, 1.784009e+01, 1.697053e+01},
{6.375000e+00, 9.884401e+01, 5.424630e+02, 3.340039e+01, 1.391046e+02},
{6.425000e+00, 7.031236e+02, 2.573183e+02, 3.556097e+01, 3.237837e+01},
{6.475000e+00, 4.196003e+02, 1.189283e+03, 1.274255e+01, 1.684355e+01},
{6.525000e+00, 1.067902e+03, 5.693266e+02, 1.525074e+01, 1.208551e+01},
{6.575000e+00, 7.472021e+02, 9.197338e+02, 1.541994e+01, 1.339014e+01},
{6.625000e+00, 1.095982e+03, 8.831151e+02, 1.420049e+01, 1.466435e+01},
{6.675000e+00, 4.441145e+02, 6.166981e+02, 8.676879e+00, 9.162763e+00},
{6.725000e+00, 4.463663e+02, 1.200932e+03, 1.098189e+01, 2.177580e+00},
{6.775000e+00, 1.122934e+03, 1.601203e+02, 2.335593e+01, 2.290158e+01},
{6.825000e+00, 4.319798e+02, 5.747794e+02, 1.249579e+01, 9.400306e+00},
{6.875000e+00, 7.958055e+02, 2.249636e+02, 1.450445e+01, 1.233333e+01},
{6.925000e+00, 9.636795e+01, 5.556607e+02, 1.503565e+01, 9.236797e+00},
{6.975000e+00, 9.779365e+01, 1.851973e+02, 1.292555e+01, 6.751275e+00},
{7.025000e+00, 1.600979e+03, 6.154237e+02, 1.670500e+01, 1.474402e+01},
{7.075000e+00, 1.579932e+03, 1.957056e+02, 2.736736e+01, 1.497056e+01},
{7.125000e+00, 1.201752e+03, 1.303572e+03, 9.612177e+00, 7.757759e+00},
{7.175000e+00, 9.257560e+01, 5.459505e+02, 6.029712e+00, 3.313802e+00},
{7.225000e+00, 1.645430e+03, 2.373821e+02, 1.725224e+01, 1.156441e+01},
{7.275000e+00, 9.031176e+02, 3.022480e+02, 9.111512e+00, 4.840479e+00},
{7.325000e+00, 5.245906e+02, 5.621942e+02, 1.938458e+01, 1.532852e+01},
{7.375000e+00, 4.746998e+02, 1.464851e+03, 1.672532e+01, 1.119729e+01},
{7.425000e+00, 1.107997e+02, 6.324813e+02, 4.456300e+00, 4.033108e+00},
{7.475000e+00, 5.536260e+02, 5.819264e+02, 2.330643e+01, 2.066251e+01},
{7.525000e+00, 1.303279e+03, 1.855092e+02, 8.103111e+00, 2.965570e+00},
{7.575000e+00, 5.460713e+02, 1.698463e+02, 1.206509e+01, 3.400505e+00},
{7.625000e+00, 5.253746e+02, 2.275169e+02, 1.536381e+01, 8.647777e+00},
{7.675000e+00, 5.487290e+02, 5.799706e+02, 8.140736e+00, 1.307138e+00},
{7.725000e+00, 1.324888e+02, 2.147239e+02, 7.597306e+00, 3.695687e+00},
{7.775000e+00, 5.506525e+02, 1.342639e+02, 8.130243e+00, 4.383871e+00},
{7.825000e+00, 5.892041e+02, 1.813196e+02, 1.116613e+01, 7.149690e+00},
{7.875000e+00, 1.049712e+03, 1.704371e+02, 8.047993e+00, 6.212082e+00},
{7.925000e+00, 5.564352e+02, 1.662036e+02, 1.586367e+01, 2.309864e+00},
{7.975000e+00, 5.838420e+02, 1.345576e+02, 3.032527e+00, 3.852467e+00},
{8.025000e+00, 5.968340e+02, 1.906756e+02, 6.203017e+00, 1.597809e+00},
{8.075000e+00, 6.037743e+02, 1.447683e+02, 1.114983e+01, 5.357573e+00},
{8.125000e+00, 1.397863e+02, 6.050291e+02, 7.248503e+00, 1.049103e+01},
{8.175000e+00, 5.916928e+02, 1.697927e+02, 1.132060e+01, 1.060493e+01},
{8.225000e+00, 1.360374e+02, 1.146625e+03, 4.241725e+00, 8.310912e-01},
{8.275000e+00, 9.419329e+01, 1.521521e+02, 1.137937e+01, 6.826365e+00},
{8.325000e+00, 6.071899e+02, 1.906113e+02, 4.917574e+00, 3.083679e+00},
{8.375000e+00, 1.484053e+02, 1.853866e+02, 1.201454e+01, 9.068627e+00},
{8.425000e+00, 1.425914e+02, 7.186896e+02, 1.839174e+01, 8.751696e+00},
{8.475000e+00, 9.883191e+01, 1.215699e+02, 7.453494e+00, 2.555228e+00},
{8.525000e+00, 1.168453e+03, 1.767000e+02, 4.850553e+00, 0.000000e+00},
{8.575000e+00, 8.548637e+01, 5.447283e+01, 6.463156e+00, 1.821443e+00},
{8.625000e+00, 1.258045e+02, 1.495248e+02, 8.375118e+00, 6.315691e+00},
{8.675000e+00, 1.114098e+02, 1.592084e+02, 7.820538e+00, 1.684655e+00},
{8.725000e+00, 1.046384e+02, 1.449486e+02, 4.705591e+00, 2.877626e+00},
{8.775000e+00, 6.513846e+01, 1.140007e+02, 4.009547e+00, 0.000000e+00},
{8.825000e+00, 6.615163e+02, 1.398806e+02, 1.004609e+01, 6.253827e+00},
{8.875000e+00, 1.082401e+02, 1.416153e+02, 2.013188e+01, 1.336808e+01},
{8.925000e+00, 1.517888e+02, 1.600431e+02, 1.129169e+01, 5.671163e+00},
{8.975000e+00, 1.533524e+02, 7.307191e+02, 8.432091e-01, 8.489559e-01},
{9.025000e+00, 6.750955e+02, 1.033446e+02, 9.823789e+00, 1.030168e+01},
{9.075000e+00, 6.973296e+01, 1.565136e+02, 6.994918e+00, 8.747278e-01},
{9.125000e+00, 9.682318e+01, 1.232960e+02, 1.185486e+01, 1.049584e+01},
{9.175000e+00, 8.016393e+01, 1.292170e+03, 2.742319e+00, 1.012293e+00},
{9.225000e+00, 8.113565e+01, 7.198628e+01, 1.137207e+00, 8.289503e-01},
{9.275000e+00, 7.817437e+02, 1.001588e+02, 3.101088e+00, 1.015177e+00},
{9.325000e+00, 1.195984e+02, 8.264757e+01, 2.881412e+00, 0.000000e+00},
{9.375000e+00, 8.080153e+02, 7.430542e+01, 2.705077e+00, 0.000000e+00},
{9.425000e+00, 9.402878e+01, 8.451553e+01, 2.998072e+00, 1.111233e+00},
{9.475000e+00, 1.043749e+02, 1.329266e+02, 8.915679e+00, 8.824920e+00},
{9.525000e+00, 9.598842e+01, 1.630961e+02, 6.436805e+00, 0.000000e+00},
{9.575000e+00, 1.162539e+02, 7.755951e+01, 6.653079e+00, 4.507665e+00},
{9.625000e+00, 7.773459e+02, 7.842912e+01, 0.000000e+00, 1.122666e+00},
{9.675000e+00, 4.945165e+01, 1.433849e+03, 1.891393e+01, 1.228514e+01},
{9.725000e+00, 1.101976e+02, 8.002999e+01, 5.212760e+00, 1.244615e+00},
{9.775000e+00, 1.009408e+02, 6.061621e+01, 2.270313e+01, 1.470721e+01},
{9.825000e+00, 1.122157e+02, 1.124204e+02, 6.290203e+00, 0.000000e+00},
{9.875000e+00, 9.284498e+01, 1.030802e+02, 4.310982e+00, 0.000000e+00},
{9.925000e+00, 7.282382e+01, 4.168572e+01, 1.023153e+01, 5.591016e+00},
{9.975000e+00, 1.472934e+02, 1.052085e+02, 2.662278e+00, 0.000000e+00},
{1.002500e+01, 7.438229e+01, 5.316583e+01, 7.873160e+00, 7.162654e+00},
{1.007500e+01, 8.581836e+01, 7.515992e+01, 0.000000e+00, 0.000000e+00},
{1.012500e+01, 1.084714e+02, 6.499778e+01, 2.289824e+00, 1.079910e+00},
{1.017500e+01, 4.371701e+01, 8.751650e+01, 1.057439e+01, 7.106572e+00},
{1.022500e+01, 6.643569e+01, 2.220654e+01, 1.395659e+00, 0.000000e+00},
{1.027500e+01, 2.009936e+02, 1.228687e+02, 2.992798e+00, 0.000000e+00},
{1.032500e+01, 1.241479e+02, 1.014607e+02, 1.091002e+00, 0.000000e+00},
{1.037500e+01, 1.024656e+02, 6.830322e+01, 5.972561e+00, 0.000000e+00},
{1.042500e+01, 9.197658e+01, 9.203891e+01, 1.554926e+01, 1.235488e+01},
{1.047500e+01, 6.968906e+01, 3.476217e+01, 1.225128e+01, 8.574091e+00},
{1.052500e+01, 8.205778e+01, 2.338111e+01, 6.474019e+00, 4.832406e+00},
{1.057500e+01, 4.738197e+01, 9.456539e+01, 3.482257e+00, 0.000000e+00},
{1.062500e+01, 7.165480e+01, 3.596620e+01, 0.000000e+00, 0.000000e+00},
{1.067500e+01, 7.244279e+01, 4.822518e+01, 0.000000e+00, 1.321101e+00},
{1.072500e+01, 7.291329e+01, 4.861239e+01, 2.500519e+00, 0.000000e+00},
{1.077500e+01, 1.596310e+02, 7.368264e+01, 1.276106e+01, 6.684208e+00},
{1.082500e+01, 1.242191e+02, 4.955931e+01, 2.825674e+00, 1.316428e+00},
{1.087500e+01, 3.752611e+01, 5.008540e+01, 7.274762e+00, 0.000000e+00},
{1.092500e+01, 8.837742e+01, 5.064174e+01, 0.000000e+00, 0.000000e+00},
{1.097500e+01, 1.148506e+02, 1.271959e+01, 6.721069e+00, 5.331740e+00},
{1.102500e+01, 1.415536e+02, 3.858592e+01, 2.586468e+00, 0.000000e+00},
{1.107500e+01, 9.084592e+01, 0.000000e+00, 5.506414e+00, 5.506414e+00},
{1.112500e+01, 1.045044e+02, 1.312452e+01, 8.239835e+00, 0.000000e+00},
{1.117500e+01, 1.847855e+02, 0.000000e+00, 8.930796e+00, 6.093599e+00},
{1.122500e+01, 3.995524e+01, 5.338390e+01, 2.338779e+01, 1.318778e+01},
{1.127500e+01, 1.075874e+02, 5.368725e+01, 1.422742e+00, 0.000000e+00},
{1.132500e+01, 8.146106e+01, 6.780785e+01, 1.089062e+01, 1.270771e+01},
{1.137500e+01, 9.574360e+01, 2.738592e+01, 1.210867e+01, 0.000000e+00},
{1.142500e+01, 1.105138e+02, 2.757090e+01, 3.547185e+00, 0.000000e+00},
{1.147500e+01, 9.752275e+01, 8.367462e+01, 3.830290e+00, 0.000000e+00},
{1.152500e+01, 8.420971e+01, 2.810552e+01, 0.000000e+00, 0.000000e+00},
{1.157500e+01, 7.093094e+01, 2.840414e+01, 1.514461e+01, 1.056504e+01},
{1.162500e+01, 5.717873e+01, 4.284497e+01, 9.274229e+00, 1.590706e+00},
{1.167500e+01, 1.297722e+02, 1.436267e+01, 3.666959e+00, 0.000000e+00},
{1.172500e+01, 4.359367e+01, 2.910105e+01, 0.000000e+00, 0.000000e+00},
{1.177500e+01, 1.612985e+02, 4.406983e+01, 3.143242e+00, 0.000000e+00},
{1.182500e+01, 7.381780e+01, 1.479263e+01, 6.180080e+00, 0.000000e+00},
{1.187500e+01, 1.344384e+02, 4.471783e+01, 1.002606e+01, 7.934931e+00},
{1.192500e+01, 1.052102e+02, 1.502813e+01, 2.283597e+00, 0.000000e+00},
{1.197500e+01, 1.364932e+02, 3.039685e+01, 4.460954e+00, 0.000000e+00},
{1.202500e+01, 3.067520e+01, 3.051112e+01, 0.000000e+00, 0.000000e+00},
{1.207500e+01, 9.248454e+01, 3.083566e+01, 0.000000e+00, 0.000000e+00},
{1.212500e+01, 4.663518e+01, 1.558744e+01, 7.914763e+00, 6.587637e+00},
{1.217500e+01, 9.404887e+01, 3.141780e+01, 1.686903e+00, 0.000000e+00},
{1.222500e+01, 3.154284e+01, 1.583314e+01, 0.000000e+00, 0.000000e+00},
{1.227500e+01, 7.978778e+01, 3.183886e+01, 1.612629e+00, 0.000000e+00},
{1.232500e+01, 6.427538e+01, 3.213742e+01, 1.946468e+00, 0.000000e+00},
{1.237500e+01, 3.230815e+01, 0.000000e+00, 2.279196e+01, 1.889375e+01},
{1.242500e+01, 8.162761e+01, 3.264362e+01, 3.338977e+00, 0.000000e+00},
{1.247500e+01, 1.152133e+02, 3.284912e+01, 2.083978e+00, 0.000000e+00},
{1.252500e+01, 9.960681e+01, 3.312372e+01, 9.095160e+00, 9.095160e+00},
{1.257500e+01, 1.170195e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.262500e+01, 6.745669e+01, 1.690926e+01, 2.186515e+00, 0.000000e+00},
{1.267500e+01, 1.528290e+02, 5.093672e+01, 0.000000e+00, 0.000000e+00},
{1.272500e+01, 1.710194e+01, 3.430756e+01, 0.000000e+00, 0.000000e+00},
{1.277500e+01, 5.188032e+01, 1.732440e+01, 2.801618e+00, 2.421725e+00},
{1.282500e+01, 1.736243e+01, 3.475300e+01, 0.000000e+00, 0.000000e+00},
{1.287500e+01, 5.252709e+01, 5.248410e+01, 2.938847e+00, 0.000000e+00},
{1.292500e+01, 7.057411e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.297500e+01, 7.127647e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.302500e+01, 3.588837e+01, 1.797295e+01, 0.000000e+00, 0.000000e+00},
{1.307500e+01, 1.808714e+01, 1.809838e+01, 0.000000e+00, 0.000000e+00},
{1.312500e+01, 7.301440e+01, 0.000000e+00, 2.409366e+00, 0.000000e+00},
{1.317500e+01, 7.351189e+01, 0.000000e+00, 5.754300e+00, 0.000000e+00},
{1.322500e+01, 7.414985e+01, 1.850103e+01, 0.000000e+00, 0.000000e+00},
{1.327500e+01, 1.860720e+01, 1.862226e+01, 0.000000e+00, 0.000000e+00},
{1.332500e+01, 1.503219e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.337500e+01, 3.791406e+01, 0.000000e+00, 1.756339e+00, 0.000000e+00},
{1.342500e+01, 3.817912e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.347500e+01, 5.757467e+01, 0.000000e+00, 1.696560e+00, 0.000000e+00},
{1.352500e+01, 5.791640e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.357500e+01, 7.797285e+01, 1.946514e+01, 0.000000e+00, 0.000000e+00},
{1.362500e+01, 1.963021e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.367500e+01, 3.956637e+01, 0.000000e+00, 2.078986e+00, 0.000000e+00},
{1.372500e+01, 5.975898e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.377500e+01, 2.001246e+01, 0.000000e+00, 2.046384e+00, 0.000000e+00},
{1.382500e+01, 2.023780e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.387500e+01, 8.154860e+01, 0.000000e+00, 2.874513e+00, 0.000000e+00},
{1.392500e+01, 1.230092e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.397500e+01, 6.182263e+01, 0.000000e+00, 3.595243e+00, 0.000000e+00},
{1.402500e+01, 4.151594e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.407500e+01, 8.370809e+01, 4.191947e+01, 0.000000e+00, 0.000000e+00},
{1.412500e+01, 4.206212e+01, 2.105329e+01, 0.000000e+00, 0.000000e+00},
{1.417500e+01, 8.495273e+01, 2.119450e+01, 0.000000e+00, 0.000000e+00},
{1.422500e+01, 4.288229e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.427500e+01, 6.475775e+01, 0.000000e+00, 5.066434e+00, 0.000000e+00},
{1.432500e+01, 4.327745e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.437500e+01, 8.762298e+01, 0.000000e+00, 1.600776e+01, 1.340147e+01},
{1.442500e+01, 1.539141e+02, 0.000000e+00, 2.774218e+00, 0.000000e+00},
{1.447500e+01, 8.866240e+01, 2.219761e+01, 0.000000e+00, 0.000000e+00},
{1.452500e+01, 4.470425e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.457500e+01, 6.747609e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.462500e+01, 1.130738e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.467500e+01, 2.271421e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.472500e+01, 6.880920e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.477500e+01, 6.933845e+01, 0.000000e+00, 0.000000e+00, 2.922838e+00},
{1.482500e+01, 4.636144e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.487500e+01, 4.686012e+01, 0.000000e+00, 2.622114e+00, 0.000000e+00},
{1.492500e+01, 4.717109e+01, 0.000000e+00, 1.435166e+01, 1.435166e+01},
{1.497500e+01, 4.735700e+01, 0.000000e+00, 1.684642e+01, 8.909766e+00}
};