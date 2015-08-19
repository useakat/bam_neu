/*!
 ---------------------------------------------------------------------\n

 \short off-axis angle のbeam profile (0.5 \f$\bar{\nu}\f$ foucus)

 \b 解説

 50MeV刻みのビームのフラックス配列。場所は 1000 km 。
 エネルギーレンジは 0.0GeV - 15GeV
 (index にすると 0-299までの300個)

 ---------------------------------------------------------------------\n

 \author "$Author: okamura $"

 \date "$Date: 2011-01-18 13:45:53 +0900 (Tue, 18 Jan 2011) $" 

 \version "$Name:  $-$Revision: 188 $-$State: Exp $"

 \note "$Source: /home/cvsroot/std.set.T2K4.3/Beam/oab05a.cc,v $"

 ---------------------------------------------------------------------\n

 \b 使用法

 \return 配列要素は
 \f$ E_\nu(GeV),\nu_\mu,\bar{\nu}_{\mu},\nu_e,\bar{\nu}_e \f$

 ---------------------------------------------------------------------\n
*/
/*--------------------------------------------------------
// Neutrino flux table at 1000 km 
// off axcis angle is 05a
// Enu(GeV),   nu_mu,        bar{nu_mu},   nu_e,         bar{nu_e}
---------------------------------------------------------*/
double oab05a[300][5]={
{2.500000e-02, 2.221872e+03, 1.417079e+03, 3.981981e+00, 1.712520e+01},
{7.500000e-02, 7.100675e+03, 7.321388e+03, 1.628222e+01, 9.570461e+01},
{1.250000e-01, 3.874929e+03, 1.521179e+04, 2.881150e+01, 2.132157e+02},
{1.750000e-01, 3.211536e+03, 2.174174e+04, 4.113088e+01, 3.198799e+02},
{2.250000e-01, 3.049004e+03, 2.999833e+04, 5.053928e+01, 3.672786e+02},
{2.750000e-01, 3.053015e+03, 3.898659e+04, 5.922797e+01, 4.553214e+02},
{3.250000e-01, 3.202721e+03, 4.853711e+04, 6.271783e+01, 4.744196e+02},
{3.750000e-01, 3.469040e+03, 5.627788e+04, 6.918617e+01, 5.575254e+02},
{4.250000e-01, 3.727844e+03, 6.290681e+04, 6.871985e+01, 5.910622e+02},
{4.750000e-01, 4.038002e+03, 7.022272e+04, 7.503901e+01, 5.635657e+02},
{5.250000e-01, 4.153532e+03, 7.603130e+04, 7.888475e+01, 5.754217e+02},
{5.750000e-01, 4.251382e+03, 8.101303e+04, 7.704841e+01, 5.881705e+02},
{6.250000e-01, 4.539191e+03, 8.459265e+04, 7.253996e+01, 5.997513e+02},
{6.750000e-01, 4.645907e+03, 8.685943e+04, 7.917074e+01, 5.797926e+02},
{7.250000e-01, 4.605715e+03, 8.905843e+04, 6.860276e+01, 4.805881e+02},
{7.750000e-01, 4.749591e+03, 9.219165e+04, 6.226240e+01, 5.462907e+02},
{8.250000e-01, 5.220753e+03, 9.457511e+04, 5.999420e+01, 4.739813e+02},
{8.750000e-01, 5.359677e+03, 9.928647e+04, 6.649635e+01, 4.635309e+02},
{9.250000e-01, 5.259470e+03, 9.758845e+04, 7.096823e+01, 5.237505e+02},
{9.750000e-01, 5.291058e+03, 1.020828e+05, 6.535614e+01, 4.688814e+02},
{1.025000e+00, 5.288886e+03, 1.026633e+05, 6.661881e+01, 4.756698e+02},
{1.075000e+00, 5.593650e+03, 1.045275e+05, 7.493921e+01, 4.405080e+02},
{1.125000e+00, 5.398639e+03, 1.006392e+05, 6.566237e+01, 3.766989e+02},
{1.175000e+00, 5.531689e+03, 9.882820e+04, 6.779096e+01, 3.795008e+02},
{1.225000e+00, 5.931800e+03, 9.574952e+04, 6.116997e+01, 3.506087e+02},
{1.275000e+00, 5.318413e+03, 9.155056e+04, 5.381542e+01, 2.620196e+02},
{1.325000e+00, 5.424983e+03, 9.155963e+04, 5.835981e+01, 4.086612e+02},
{1.375000e+00, 5.369983e+03, 9.101693e+04, 5.618541e+01, 3.304227e+02},
{1.425000e+00, 5.591249e+03, 8.601413e+04, 5.600728e+01, 4.177821e+02},
{1.475000e+00, 5.444059e+03, 8.392439e+04, 4.504365e+01, 2.815886e+02},
{1.525000e+00, 5.787243e+03, 8.499598e+04, 5.414448e+01, 3.077054e+02},
{1.575000e+00, 5.214154e+03, 8.013533e+04, 4.888039e+01, 2.403394e+02},
{1.625000e+00, 5.358966e+03, 7.773300e+04, 5.294482e+01, 1.753173e+02},
{1.675000e+00, 4.640614e+03, 7.760671e+04, 4.493588e+01, 2.386403e+02},
{1.725000e+00, 4.892123e+03, 7.248351e+04, 4.570308e+01, 3.535373e+02},
{1.775000e+00, 4.729101e+03, 7.010851e+04, 4.519659e+01, 1.823777e+02},
{1.825000e+00, 4.472420e+03, 6.858439e+04, 5.645254e+01, 2.282673e+02},
{1.875000e+00, 5.226204e+03, 6.466575e+04, 3.826770e+01, 2.210128e+02},
{1.925000e+00, 5.919354e+03, 6.019133e+04, 6.176017e+01, 1.802163e+02},
{1.975000e+00, 4.767862e+03, 5.776814e+04, 5.592209e+01, 2.400967e+02},
{2.025000e+00, 4.919620e+03, 5.502510e+04, 3.551924e+01, 1.985278e+02},
{2.075000e+00, 5.589363e+03, 5.048074e+04, 3.803180e+01, 2.214311e+02},
{2.125000e+00, 4.369410e+03, 4.793989e+04, 4.008129e+01, 1.704011e+02},
{2.175000e+00, 5.016224e+03, 4.633087e+04, 3.987171e+01, 1.450466e+02},
{2.225000e+00, 4.977032e+03, 4.222169e+04, 4.375611e+01, 2.045795e+02},
{2.275000e+00, 3.942814e+03, 3.985659e+04, 4.187968e+01, 1.555519e+02},
{2.325000e+00, 4.343570e+03, 3.544859e+04, 5.159030e+01, 1.653097e+02},
{2.375000e+00, 4.715291e+03, 3.681061e+04, 3.743805e+01, 1.104838e+02},
{2.425000e+00, 4.435747e+03, 3.513992e+04, 4.470417e+01, 1.709604e+02},
{2.475000e+00, 4.185655e+03, 3.155970e+04, 5.601072e+01, 9.677795e+01},
{2.525000e+00, 3.566928e+03, 2.767036e+04, 3.647180e+01, 1.819435e+02},
{2.575000e+00, 3.691081e+03, 2.666595e+04, 3.926174e+01, 1.504767e+02},
{2.625000e+00, 4.056585e+03, 2.511166e+04, 4.628179e+01, 8.592706e+01},
{2.675000e+00, 4.382902e+03, 2.251248e+04, 4.798680e+01, 8.423131e+01},
{2.725000e+00, 3.880940e+03, 2.367748e+04, 4.259541e+01, 7.258766e+01},
{2.775000e+00, 3.423797e+03, 2.084103e+04, 5.295400e+01, 1.308232e+02},
{2.825000e+00, 3.771039e+03, 1.767491e+04, 3.943446e+01, 1.019462e+02},
{2.875000e+00, 3.980471e+03, 1.967832e+04, 4.175618e+01, 1.420798e+02},
{2.925000e+00, 3.820352e+03, 1.815426e+04, 3.536386e+01, 9.808354e+01},
{2.975000e+00, 3.433944e+03, 1.342160e+04, 4.428076e+01, 1.155598e+02},
{3.025000e+00, 3.712240e+03, 1.330412e+04, 3.202564e+01, 7.296837e+01},
{3.075000e+00, 3.171192e+03, 1.281855e+04, 3.679243e+01, 5.034902e+01},
{3.125000e+00, 3.723436e+03, 1.228671e+04, 2.877616e+01, 5.056831e+01},
{3.175000e+00, 3.203176e+03, 1.301879e+04, 3.838739e+01, 6.695081e+01},
{3.225000e+00, 3.773368e+03, 1.302975e+04, 4.017454e+01, 7.876934e+01},
{3.275000e+00, 2.944981e+03, 9.872734e+03, 2.401673e+01, 8.378858e+01},
{3.325000e+00, 3.814494e+03, 9.900800e+03, 5.222365e+01, 7.763615e+01},
{3.375000e+00, 2.652750e+03, 9.550487e+03, 3.054151e+01, 9.707550e+01},
{3.425000e+00, 2.744181e+03, 8.508849e+03, 3.857106e+01, 5.312224e+01},
{3.475000e+00, 2.688035e+03, 9.009551e+03, 3.087306e+01, 4.495412e+01},
{3.525000e+00, 2.836672e+03, 7.266071e+03, 3.039312e+01, 1.250991e+02},
{3.575000e+00, 2.056521e+03, 8.510269e+03, 3.333892e+01, 4.747528e+01},
{3.625000e+00, 2.659761e+03, 6.870170e+03, 2.935868e+01, 4.812383e+01},
{3.675000e+00, 2.759230e+03, 6.609895e+03, 3.850138e+01, 6.240382e+01},
{3.725000e+00, 1.523363e+03, 6.462658e+03, 3.571061e+01, 4.840589e+01},
{3.775000e+00, 2.421172e+03, 5.285067e+03, 4.905582e+01, 5.464491e+01},
{3.825000e+00, 2.786973e+03, 6.054258e+03, 4.438510e+01, 5.524086e+01},
{3.875000e+00, 2.079477e+03, 5.411027e+03, 3.998587e+01, 7.316630e+01},
{3.925000e+00, 1.999666e+03, 4.619729e+03, 2.158018e+01, 3.002105e+01},
{3.975000e+00, 2.220221e+03, 4.747409e+03, 2.729016e+01, 4.131115e+01},
{4.025000e+00, 2.055996e+03, 4.906235e+03, 5.158993e+01, 5.689324e+01},
{4.075000e+00, 2.414889e+03, 4.858009e+03, 1.795513e+02, 3.457491e+01},
{4.125000e+00, 2.724802e+03, 2.845350e+03, 3.452776e+01, 3.576858e+01},
{4.175000e+00, 2.772795e+03, 4.549026e+03, 3.561267e+01, 4.410049e+01},
{4.225000e+00, 1.283750e+03, 5.056936e+03, 2.334788e+01, 3.017785e+01},
{4.275000e+00, 1.706629e+03, 3.810818e+03, 4.150431e+01, 1.027274e+02},
{4.325000e+00, 2.015985e+03, 4.444059e+03, 2.973577e+01, 3.679394e+01},
{4.375000e+00, 2.919550e+03, 5.093028e+03, 2.647874e+01, 3.876062e+01},
{4.425000e+00, 1.678903e+03, 2.591944e+03, 3.466600e+01, 4.150721e+01},
{4.475000e+00, 1.881497e+03, 3.413971e+03, 2.564293e+01, 3.619462e+01},
{4.525000e+00, 1.626948e+03, 2.425396e+03, 3.818204e+01, 3.882162e+01},
{4.575000e+00, 1.324724e+03, 3.580768e+03, 2.695505e+01, 2.632619e+01},
{4.625000e+00, 1.666996e+03, 3.472985e+03, 2.347464e+01, 3.038254e+01},
{4.675000e+00, 2.046018e+03, 2.798125e+03, 2.161285e+01, 3.889294e+01},
{4.725000e+00, 9.234966e+02, 2.254432e+03, 2.579431e+01, 2.912253e+01},
{4.775000e+00, 1.926352e+03, 2.652867e+03, 3.436601e+01, 3.330669e+01},
{4.825000e+00, 1.631355e+03, 3.044189e+03, 2.681182e+01, 2.726022e+01},
{4.875000e+00, 1.384970e+03, 2.068434e+03, 3.199705e+01, 3.438035e+01},
{4.925000e+00, 9.836247e+02, 2.587987e+03, 1.982808e+01, 2.381202e+01},
{4.975000e+00, 1.937556e+03, 1.593032e+03, 3.866478e+01, 3.771946e+01},
{5.025000e+00, 1.613726e+03, 2.487102e+03, 2.122950e+01, 2.830026e+01},
{5.075000e+00, 1.663143e+03, 3.095592e+03, 2.247775e+01, 2.179306e+01},
{5.125000e+00, 1.064696e+03, 1.459353e+03, 2.589314e+01, 2.686605e+01},
{5.175000e+00, 1.486777e+03, 1.672212e+03, 2.389011e+01, 2.766197e+01},
{5.225000e+00, 1.351744e+03, 2.506643e+03, 1.577787e+01, 2.536585e+01},
{5.275000e+00, 1.110768e+03, 1.500362e+03, 2.170872e+01, 2.194140e+01},
{5.325000e+00, 1.182534e+03, 1.534817e+03, 3.146359e+01, 3.546482e+01},
{5.375000e+00, 1.184570e+03, 1.592413e+03, 4.815733e+01, 4.248229e+01},
{5.425000e+00, 7.491780e+02, 2.061771e+03, 2.693366e+01, 2.056764e+01},
{5.475000e+00, 1.658706e+03, 2.015242e+03, 2.047691e+01, 2.048729e+01},
{5.525000e+00, 1.002915e+03, 1.869103e+03, 2.235426e+01, 2.562126e+01},
{5.575000e+00, 7.669760e+02, 1.183102e+03, 2.333713e+01, 1.904987e+01},
{5.625000e+00, 1.234782e+03, 1.916811e+03, 1.311019e+01, 1.493601e+01},
{5.675000e+00, 1.096793e+03, 1.461995e+03, 2.596419e+01, 2.670084e+01},
{5.725000e+00, 5.581248e+02, 1.724908e+03, 2.674267e+01, 2.676314e+01},
{5.775000e+00, 3.438043e+02, 1.797241e+03, 2.095773e+01, 2.083463e+01},
{5.825000e+00, 8.741643e+02, 1.504413e+03, 2.346772e+01, 9.032748e+00},
{5.875000e+00, 1.382787e+03, 5.110433e+02, 2.552417e+01, 2.522144e+01},
{5.925000e+00, 6.683914e+02, 7.364479e+02, 2.780143e+01, 2.140557e+01},
{5.975000e+00, 1.931191e+03, 1.257174e+03, 2.375484e+01, 1.806998e+01},
{6.025000e+00, 6.254767e+02, 1.600443e+03, 1.261921e+01, 1.576851e+01},
{6.075000e+00, 9.300176e+02, 1.915197e+03, 2.153248e+01, 1.317683e+01},
{6.125000e+00, 6.976305e+02, 2.974668e+02, 1.732935e+01, 1.380861e+01},
{6.175000e+00, 1.200162e+03, 1.994711e+03, 2.872793e+01, 2.435459e+01},
{6.225000e+00, 6.677945e+02, 8.256791e+02, 1.706783e+01, 1.571067e+01},
{6.275000e+00, 1.124879e+02, 5.215333e+02, 1.168133e+01, 9.721953e+00},
{6.325000e+00, 4.154867e+02, 5.775716e+02, 1.477858e+01, 1.433033e+01},
{6.375000e+00, 1.031710e+03, 5.868319e+02, 4.199979e+01, 3.612778e+01},
{6.425000e+00, 4.301223e+02, 5.566851e+02, 2.097247e+01, 2.501222e+01},
{6.475000e+00, 4.010162e+02, 1.465394e+03, 1.949265e+01, 1.685863e+02},
{6.525000e+00, 1.093600e+03, 1.180180e+03, 2.234782e+01, 1.541033e+01},
{6.575000e+00, 7.845982e+02, 1.493961e+03, 1.529048e+01, 1.759211e+01},
{6.625000e+00, 7.738869e+02, 1.202923e+03, 2.368743e+01, 2.123219e+01},
{6.675000e+00, 4.103032e+02, 1.236104e+03, 2.178994e+01, 1.399137e+01},
{6.725000e+00, 1.003443e+02, 1.257164e+03, 3.299955e+01, 3.029017e+01},
{6.775000e+00, 1.456166e+02, 2.232352e+02, 1.733011e+02, 1.218137e+01},
{6.825000e+00, 8.153803e+02, 5.513169e+02, 9.933996e+00, 9.443279e+00},
{6.875000e+00, 7.621113e+02, 1.229312e+03, 6.463867e+00, 3.862780e+00},
{6.925000e+00, 1.014827e+02, 6.152702e+02, 2.801653e+01, 2.351161e+01},
{6.975000e+00, 1.236706e+02, 2.263220e+02, 5.377040e+00, 5.454992e+00},
{7.025000e+00, 1.253377e+02, 2.191587e+02, 2.256044e+01, 1.654858e+02},
{7.075000e+00, 8.997247e+02, 6.415308e+02, 1.713062e+01, 1.033772e+01},
{7.125000e+00, 5.267348e+02, 2.095411e+02, 3.811594e+01, 3.621660e+01},
{7.175000e+00, 8.461737e+02, 5.573485e+02, 1.143527e+01, 4.305874e+00},
{7.225000e+00, 4.598972e+02, 9.772215e+02, 2.298239e+01, 1.890209e+01},
{7.275000e+00, 1.367180e+02, 5.802162e+02, 1.248164e+01, 1.444029e+01},
{7.325000e+00, 4.679847e+02, 9.484772e+02, 3.106246e+01, 2.826025e+01},
{7.375000e+00, 1.092556e+02, 5.844116e+02, 4.705780e+00, 1.823454e+00},
{7.425000e+00, 8.749421e+01, 1.406399e+03, 1.358980e+01, 7.908565e+00},
{7.475000e+00, 4.729483e+02, 1.770924e+02, 2.399065e+01, 1.821415e+01},
{7.525000e+00, 9.969732e+02, 1.046450e+03, 1.123108e+01, 1.363858e+00},
{7.575000e+00, 8.502185e+01, 1.700590e+02, 1.299628e+00, 9.139017e-01},
{7.625000e+00, 1.105609e+02, 9.288094e+02, 1.469919e+01, 3.790758e+00},
{7.675000e+00, 9.991033e+02, 1.868356e+02, 4.520501e+00, 3.391669e+00},
{7.725000e+00, 7.566915e+01, 6.309438e+02, 6.629176e+00, 3.298578e+00},
{7.775000e+00, 8.949254e+01, 1.983876e+02, 1.761698e+01, 1.369077e+01},
{7.825000e+00, 8.427656e+01, 1.748526e+02, 1.120785e+00, 1.386514e+00},
{7.875000e+00, 5.798799e+02, 2.033344e+02, 3.213949e+00, 2.328217e+00},
{7.925000e+00, 1.062821e+03, 1.396632e+02, 5.525297e+00, 7.624900e-01},
{7.975000e+00, 1.211986e+02, 1.546711e+02, 1.512977e+01, 1.194746e+01},
{8.025000e+00, 5.842912e+02, 1.226884e+02, 1.373463e+01, 2.970740e+00},
{8.075000e+00, 5.763107e+02, 1.790948e+02, 1.872003e+01, 1.581830e+01},
{8.125000e+00, 1.101329e+03, 6.553995e+02, 4.383724e+00, 2.270209e+00},
{8.175000e+00, 9.888445e+01, 6.269403e+02, 1.231762e+01, 1.069268e+01},
{8.225000e+00, 7.881731e+01, 6.200741e+02, 6.336234e+00, 5.980833e+00},
{8.275000e+00, 1.012698e+02, 1.735260e+02, 6.539957e+00, 4.333736e+00},
{8.325000e+00, 9.535577e+01, 1.611431e+02, 6.318971e+00, 1.023704e+00},
{8.375000e+00, 5.846685e+02, 1.409950e+02, 5.642606e+00, 9.430322e-01},
{8.425000e+00, 1.049933e+02, 1.227886e+03, 3.762815e+00, 0.000000e+00},
{8.475000e+00, 1.213700e+02, 1.292440e+02, 5.398109e+00, 2.723114e+00},
{8.525000e+00, 1.153790e+02, 1.074919e+02, 9.778634e+00, 9.954882e+00},
{8.575000e+00, 1.243640e+02, 6.808966e+02, 1.633543e+00, 7.964998e-01},
{8.625000e+00, 1.021501e+02, 7.877834e+01, 1.517469e+01, 4.549114e+00},
{8.675000e+00, 1.193360e+02, 1.991383e+02, 1.125197e+01, 0.000000e+00},
{8.725000e+00, 1.126559e+02, 1.288844e+02, 6.033433e+00, 1.082620e+00},
{8.775000e+00, 8.963871e+01, 1.140925e+02, 4.188679e+00, 7.234310e-01},
{8.825000e+00, 9.884289e+01, 1.237104e+02, 1.039032e+01, 6.848693e+00},
{8.875000e+00, 1.333159e+02, 9.162759e+01, 7.915906e+00, 3.166619e+00},
{8.925000e+00, 1.262581e+02, 1.600956e+02, 3.843465e+00, 1.039820e+00},
{8.975000e+00, 1.107515e+02, 9.374002e+01, 1.566117e+01, 1.705439e+01},
{9.025000e+00, 9.461532e+01, 6.889219e+01, 4.785563e+00, 0.000000e+00},
{9.075000e+00, 8.703826e+01, 1.363637e+03, 2.244301e+01, 1.241376e+01},
{9.125000e+00, 6.992330e+02, 7.917778e+01, 2.432304e+00, 0.000000e+00},
{9.175000e+00, 1.513259e+02, 7.027864e+02, 2.168032e+00, 0.000000e+00},
{9.225000e+00, 1.620014e+02, 1.168476e+02, 5.169251e+00, 0.000000e+00},
{9.275000e+00, 9.993685e+01, 8.187665e+01, 1.154787e+01, 8.463570e+00},
{9.325000e+00, 6.439960e+01, 1.010860e+02, 2.643542e+01, 1.146585e+01},
{9.375000e+00, 1.207966e+02, 1.301435e+02, 1.547362e+01, 5.815341e+00},
{9.425000e+00, 7.316963e+02, 7.489055e+02, 9.083548e+00, 1.499813e+00},
{9.475000e+00, 9.495337e+01, 7.750993e+02, 1.137102e+00, 0.000000e+00},
{9.525000e+00, 9.590287e+01, 7.173299e+02, 3.883755e+00, 3.167585e+00},
{9.575000e+00, 1.066070e+02, 4.851287e+01, 7.900212e+00, 7.438322e+00},
{9.625000e+00, 1.176259e+02, 3.930334e+01, 6.189837e+00, 5.091225e+00},
{9.675000e+00, 8.916136e+01, 6.940901e+01, 5.255647e+00, 0.000000e+00},
{9.725000e+00, 1.699736e+02, 7.014444e+01, 2.020849e+01, 1.767334e+01},
{9.775000e+00, 1.009144e+02, 1.110660e+02, 4.321157e+00, 9.724891e-01},
{9.825000e+00, 1.019414e+02, 1.483175e+03, 1.687686e+01, 9.548394e+00},
{9.875000e+00, 9.291602e+01, 8.256863e+01, 1.274907e+00, 0.000000e+00},
{9.925000e+00, 1.041478e+02, 7.290810e+01, 3.611396e+00, 0.000000e+00},
{9.975000e+00, 8.417755e+01, 5.255431e+01, 8.907113e+00, 9.219550e+00},
{1.002500e+01, 8.510492e+01, 3.192855e+01, 6.423615e+00, 2.638213e+00},
{1.007500e+01, 5.361318e+01, 4.300270e+01, 1.657743e+00, 0.000000e+00},
{1.012500e+01, 8.296636e+02, 3.253242e+01, 1.300772e+01, 6.829026e+00},
{1.017500e+01, 7.668030e+01, 4.378258e+01, 0.000000e+00, 0.000000e+00},
{1.022500e+01, 1.215202e+02, 7.738736e+01, 2.873371e+00, 1.326802e+00},
{1.027500e+01, 5.584953e+01, 8.928265e+01, 3.793301e+00, 0.000000e+00},
{1.032500e+01, 1.015114e+02, 6.755878e+01, 4.403062e+00, 0.000000e+00},
{1.037500e+01, 1.024159e+02, 3.414169e+01, 6.057474e+00, 6.057474e+00},
{1.042500e+01, 8.031250e+01, 9.186968e+01, 1.398068e+00, 0.000000e+00},
{1.047500e+01, 6.962720e+01, 5.793681e+01, 6.652308e+00, 7.301792e+00},
{1.052500e+01, 4.693518e+01, 3.513337e+01, 2.309555e+00, 0.000000e+00},
{1.057500e+01, 4.743136e+01, 2.373618e+01, 2.567116e+00, 3.128901e+00},
{1.062500e+01, 5.959028e+01, 3.573833e+01, 1.275695e+00, 0.000000e+00},
{1.067500e+01, 6.017075e+01, 2.413485e+01, 2.148557e+00, 1.226601e+00},
{1.072500e+01, 1.821194e+02, 2.430372e+01, 4.133084e+00, 0.000000e+00},
{1.077500e+01, 1.473635e+02, 6.144266e+01, 3.820920e+00, 0.000000e+00},
{1.082500e+01, 1.241172e+01, 3.722777e+01, 0.000000e+00, 0.000000e+00},
{1.087500e+01, 8.744929e+01, 6.255774e+01, 3.445076e+00, 0.000000e+00},
{1.092500e+01, 8.829575e+01, 1.259474e+01, 8.933004e+00, 5.150008e+00},
{1.097500e+01, 1.399402e+02, 5.095447e+01, 1.134907e+01, 8.297559e+00},
{1.102500e+01, 1.027012e+02, 6.423016e+01, 6.144981e+00, 0.000000e+00},
{1.107500e+01, 1.166173e+02, 2.600219e+01, 3.695568e+00, 0.000000e+00},
{1.112500e+01, 6.536301e+01, 2.618592e+01, 4.578246e+00, 0.000000e+00},
{1.117500e+01, 6.605490e+01, 3.963247e+01, 7.289906e+00, 7.289906e+00},
{1.122500e+01, 1.200180e+02, 2.665154e+01, 0.000000e+00, 0.000000e+00},
{1.127500e+01, 1.346208e+02, 2.692182e+01, 3.254250e+00, 0.000000e+00},
{1.132500e+01, 6.786483e+01, 2.709943e+01, 5.657142e+00, 0.000000e+00},
{1.137500e+01, 5.474303e+01, 4.113889e+01, 3.212234e+00, 1.351222e+00},
{1.142500e+01, 6.894869e+01, 5.513933e+01, 8.921223e+00, 0.000000e+00},
{1.147500e+01, 4.175228e+01, 0.000000e+00, 1.553905e+00, 0.000000e+00},
{1.152500e+01, 5.628995e+01, 1.405889e+01, 0.000000e+00, 0.000000e+00},
{1.157500e+01, 8.504616e+01, 1.413590e+01, 3.228265e+00, 0.000000e+00},
{1.162500e+01, 2.852857e+01, 5.710127e+01, 1.805794e+00, 0.000000e+00},
{1.167500e+01, 7.219744e+01, 1.442627e+01, 5.414716e+00, 0.000000e+00},
{1.172500e+01, 5.810875e+01, 0.000000e+00, 9.484035e+00, 9.484035e+00},
{1.177500e+01, 8.795174e+01, 1.466504e+01, 3.667952e+00, 0.000000e+00},
{1.182500e+01, 7.395154e+01, 5.918180e+01, 3.796566e+00, 0.000000e+00},
{1.187500e+01, 5.970622e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.192500e+01, 6.013473e+01, 1.507032e+01, 1.392308e+00, 0.000000e+00},
{1.197500e+01, 6.062218e+01, 6.064368e+01, 1.600753e+01, 1.005670e+01},
{1.202500e+01, 7.646561e+01, 3.057328e+01, 0.000000e+00, 0.000000e+00},
{1.207500e+01, 1.119814e+03, 4.621575e+01, 0.000000e+00, 0.000000e+00},
{1.212500e+01, 9.340534e+01, 1.560463e+01, 0.000000e+00, 0.000000e+00},
{1.217500e+01, 1.097141e+02, 1.564864e+01, 3.187705e+00, 1.879850e+00},
{1.222500e+01, 7.907951e+01, 6.315118e+01, 0.000000e+00, 0.000000e+00},
{1.227500e+01, 1.115803e+02, 1.593236e+01, 1.541810e+00, 1.697452e+00},
{1.232500e+01, 8.046260e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.237500e+01, 8.092674e+01, 3.239927e+01, 9.680964e+00, 7.816033e+00},
{1.242500e+01, 3.259529e+01, 1.627098e+01, 9.150494e+00, 8.964908e+00},
{1.247500e+01, 4.934049e+01, 1.650686e+01, 6.390350e+00, 0.000000e+00},
{1.252500e+01, 9.965947e+01, 0.000000e+00, 7.049957e+00, 0.000000e+00},
{1.257500e+01, 5.021517e+01, 1.667627e+01, 1.658746e+00, 0.000000e+00},
{1.262500e+01, 1.346751e+02, 0.000000e+00, 6.541479e+00, 6.541479e+00},
{1.267500e+01, 1.019109e+02, 0.000000e+00, 5.244647e+00, 0.000000e+00},
{1.272500e+01, 6.857545e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.277500e+01, 6.909667e+01, 0.000000e+00, 2.939227e+00, 0.000000e+00},
{1.282500e+01, 1.219077e+02, 0.000000e+00, 2.505096e+00, 0.000000e+00},
{1.287500e+01, 5.265908e+01, 1.748240e+01, 0.000000e+00, 0.000000e+00},
{1.292500e+01, 1.238188e+02, 0.000000e+00, 1.337693e+01, 1.110277e+01},
{1.297500e+01, 5.338241e+01, 5.333263e+01, 0.000000e+00, 0.000000e+00},
{1.302500e+01, 1.791061e+01, 1.793263e+01, 0.000000e+00, 0.000000e+00},
{1.307500e+01, 5.411719e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.312500e+01, 7.304748e+01, 0.000000e+00, 8.699417e+00, 8.699417e+00},
{1.317500e+01, 1.103048e+02, 1.835743e+01, 5.670916e+00, 0.000000e+00},
{1.322500e+01, 0.000000e+00, 0.000000e+00, 3.129805e+00, 0.000000e+00},
{1.327500e+01, 5.590381e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.332500e+01, 3.744843e+01, 0.000000e+00, 2.595269e+00, 0.000000e+00},
{1.337500e+01, 3.784348e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.342500e+01, 5.731723e+01, 3.823041e+01, 0.000000e+00, 0.000000e+00},
{1.347500e+01, 5.760011e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.352500e+01, 3.865755e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.357500e+01, 7.784771e+01, 0.000000e+00, 8.614241e+00, 0.000000e+00},
{1.362500e+01, 3.932152e+01, 1.970284e+01, 0.000000e+00, 0.000000e+00},
{1.367500e+01, 9.904152e+01, 1.976207e+01, 0.000000e+00, 0.000000e+00},
{1.372500e+01, 3.978469e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.377500e+01, 8.039990e+01, 2.011169e+01, 0.000000e+00, 0.000000e+00},
{1.382500e+01, 6.068226e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.387500e+01, 1.627492e+02, 0.000000e+00, 4.523967e+00, 0.000000e+00},
{1.392500e+01, 4.102042e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.397500e+01, 4.132969e+01, 2.065496e+01, 0.000000e+00, 0.000000e+00},
{1.402500e+01, 6.248886e+01, 2.075521e+01, 0.000000e+00, 0.000000e+00},
{1.407500e+01, 4.188749e+01, 0.000000e+00, 4.831941e+00, 0.000000e+00},
{1.412500e+01, 4.220189e+01, 2.105785e+01, 8.529945e+00, 8.529945e+00},
{1.417500e+01, 8.476416e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.422500e+01, 6.423297e+01, 2.133839e+01, 0.000000e+00, 0.000000e+00},
{1.427500e+01, 2.153416e+01, 0.000000e+00, 3.191145e+00, 0.000000e+00},
{1.432500e+01, 2.172084e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.437500e+01, 8.737607e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.442500e+01, 6.615776e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.447500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.452500e+01, 6.692332e+01, 2.236622e+01, 0.000000e+00, 0.000000e+00},
{1.457500e+01, 4.498376e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.462500e+01, 4.521755e+01, 2.257941e+01, 0.000000e+00, 0.000000e+00},
{1.467500e+01, 6.835033e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.472500e+01, 2.289832e+01, 0.000000e+00, 2.627026e+00, 0.000000e+00},
{1.477500e+01, 4.606183e+01, 0.000000e+00, 3.929014e+00, 0.000000e+00},
{1.482500e+01, 4.648302e+01, 0.000000e+00, 2.914217e+00, 0.000000e+00},
{1.487500e+01, 7.008126e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.492500e+01, 2.353782e+01, 0.000000e+00, 6.440142e+00, 0.000000e+00},
{1.497500e+01, 9.505770e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00}
};
