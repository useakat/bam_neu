/*!
 ---------------------------------------------------------------------\n

 \short off-axis angle ��beam profile (1.0 \f$\bar{\nu}\f$ foucus)

 \b ����

 50MeV��ߤΥӡ���Υե�å������󡣾��� 1000 km ��
 ���ͥ륮����󥸤� 0.0GeV - 15GeV
 (index �ˤ���� 0-299�ޤǤ�300��)

 ---------------------------------------------------------------------\n

 \author "$Author: okamura $"

 \date "$Date: 2011-01-18 13:45:53 +0900 (Tue, 18 Jan 2011) $" 

 \version "$Name:  $-$Revision: 188 $-$State: Exp $"

 \note "$Source: /home/cvsroot/std.set.T2K4.3/Beam/oab10a.cc,v $"

 ---------------------------------------------------------------------\n

 \b ����ˡ

 \return �������Ǥ�
 \f$ E_\nu(GeV),\nu_\mu,\bar{\nu}_{\mu},\nu_e,\bar{\nu}_e \f$

 ---------------------------------------------------------------------\n
*/
/*--------------------------------------------------------
// Neutrino flux table at 1000 km 
// off axcis angle is 10a
// Enu(GeV),   nu_mu,        bar{nu_mu},   nu_e,         bar{nu_e}
---------------------------------------------------------*/
double oab10a[300][5]={
{2.500000e-02, 2.234262e+03, 1.420318e+03, 4.106149e+00, 2.358346e+01},
{7.500000e-02, 7.027779e+03, 7.323197e+03, 1.650333e+01, 9.218467e+01},
{1.250000e-01, 3.940503e+03, 1.522412e+04, 2.780645e+01, 2.142891e+02},
{1.750000e-01, 3.270387e+03, 2.191328e+04, 4.247172e+01, 3.079052e+02},
{2.250000e-01, 3.123726e+03, 3.004466e+04, 5.529902e+01, 3.910532e+02},
{2.750000e-01, 3.254838e+03, 3.973155e+04, 5.561372e+01, 4.978507e+02},
{3.250000e-01, 3.279563e+03, 4.919213e+04, 6.440048e+01, 4.845135e+02},
{3.750000e-01, 3.564654e+03, 5.795611e+04, 6.840618e+01, 5.060800e+02},
{4.250000e-01, 3.820926e+03, 6.555259e+04, 6.489158e+01, 6.508434e+02},
{4.750000e-01, 3.917649e+03, 7.215273e+04, 6.660310e+01, 5.103976e+02},
{5.250000e-01, 4.289420e+03, 7.887767e+04, 7.422500e+01, 5.363638e+02},
{5.750000e-01, 4.368367e+03, 8.376692e+04, 8.248532e+01, 6.212031e+02},
{6.250000e-01, 4.536031e+03, 8.830739e+04, 6.539502e+01, 5.687814e+02},
{6.750000e-01, 4.762636e+03, 9.191252e+04, 6.105119e+01, 5.889768e+02},
{7.250000e-01, 5.111219e+03, 9.621382e+04, 6.361727e+01, 4.950871e+02},
{7.750000e-01, 4.990435e+03, 1.025330e+05, 6.658611e+01, 4.863176e+02},
{8.250000e-01, 5.026721e+03, 1.054094e+05, 6.294976e+01, 5.240937e+02},
{8.750000e-01, 5.112280e+03, 1.104111e+05, 5.659243e+01, 4.628380e+02},
{9.250000e-01, 5.377285e+03, 1.125715e+05, 7.161567e+01, 4.435201e+02},
{9.750000e-01, 5.148543e+03, 1.100784e+05, 5.778388e+01, 5.510970e+02},
{1.025000e+00, 5.255953e+03, 1.070998e+05, 5.649690e+01, 4.033161e+02},
{1.075000e+00, 5.184560e+03, 1.036054e+05, 6.754127e+01, 4.504429e+02},
{1.125000e+00, 5.203818e+03, 1.002068e+05, 6.611344e+01, 3.227487e+02},
{1.175000e+00, 5.117699e+03, 9.915105e+04, 7.029449e+01, 4.006948e+02},
{1.225000e+00, 5.297998e+03, 9.750579e+04, 6.416108e+01, 3.273447e+02},
{1.275000e+00, 4.972323e+03, 9.380319e+04, 6.922059e+01, 2.662056e+02},
{1.325000e+00, 5.275350e+03, 8.956611e+04, 5.463877e+01, 3.486886e+02},
{1.375000e+00, 4.945331e+03, 8.430268e+04, 4.422344e+01, 3.105659e+02},
{1.425000e+00, 4.863335e+03, 7.838054e+04, 6.936750e+01, 2.808004e+02},
{1.475000e+00, 4.948640e+03, 7.226888e+04, 5.777302e+01, 2.813245e+02},
{1.525000e+00, 5.180028e+03, 6.749496e+04, 8.002918e+01, 2.149850e+02},
{1.575000e+00, 4.643792e+03, 5.918589e+04, 5.233211e+01, 3.324319e+02},
{1.625000e+00, 5.221324e+03, 5.396509e+04, 4.825490e+01, 1.825084e+02},
{1.675000e+00, 4.429950e+03, 4.962132e+04, 5.840925e+01, 2.642039e+02},
{1.725000e+00, 4.273650e+03, 4.317012e+04, 4.551539e+01, 2.823695e+02},
{1.775000e+00, 3.982850e+03, 3.848653e+04, 4.486130e+01, 1.430089e+02},
{1.825000e+00, 4.379147e+03, 3.405850e+04, 3.679193e+01, 2.013934e+02},
{1.875000e+00, 4.272567e+03, 3.072350e+04, 4.835818e+01, 1.858020e+02},
{1.925000e+00, 3.891973e+03, 2.824965e+04, 5.325949e+01, 1.768357e+02},
{1.975000e+00, 4.095343e+03, 2.553728e+04, 3.600332e+01, 1.628977e+02},
{2.025000e+00, 3.759360e+03, 2.199408e+04, 4.398289e+01, 1.643422e+02},
{2.075000e+00, 3.756187e+03, 2.030238e+04, 4.600373e+01, 1.172535e+02},
{2.125000e+00, 3.623860e+03, 2.052945e+04, 6.864806e+01, 1.912408e+02},
{2.175000e+00, 3.693296e+03, 1.657496e+04, 3.910807e+01, 1.175689e+02},
{2.225000e+00, 3.329296e+03, 1.624685e+04, 3.722037e+01, 1.076202e+02},
{2.275000e+00, 3.730993e+03, 1.597372e+04, 3.353476e+01, 7.227837e+01},
{2.325000e+00, 3.214675e+03, 1.246226e+04, 3.690108e+01, 1.118773e+02},
{2.375000e+00, 3.112866e+03, 1.255884e+04, 5.255009e+01, 1.473638e+02},
{2.425000e+00, 3.065742e+03, 1.358555e+04, 4.298902e+01, 9.594361e+01},
{2.475000e+00, 2.824179e+03, 1.256015e+04, 5.008117e+01, 1.055856e+02},
{2.525000e+00, 2.959463e+03, 1.132820e+04, 4.895518e+01, 2.009957e+02},
{2.575000e+00, 2.597751e+03, 9.349844e+03, 5.305559e+01, 9.650161e+01},
{2.625000e+00, 2.886201e+03, 8.864711e+03, 3.224806e+01, 4.842669e+01},
{2.675000e+00, 2.415579e+03, 7.987485e+03, 4.179178e+01, 1.391124e+02},
{2.725000e+00, 2.432189e+03, 8.707182e+03, 3.932223e+01, 1.496957e+02},
{2.775000e+00, 2.544382e+03, 8.182547e+03, 4.816891e+01, 8.662380e+01},
{2.825000e+00, 2.114689e+03, 6.190811e+03, 4.014001e+01, 5.087666e+01},
{2.875000e+00, 3.219649e+03, 6.632940e+03, 5.531072e+01, 6.730474e+01},
{2.925000e+00, 3.188409e+03, 6.538741e+03, 3.276746e+01, 7.619438e+01},
{2.975000e+00, 2.463449e+03, 5.745229e+03, 3.188522e+01, 9.497103e+01},
{3.025000e+00, 1.806514e+03, 5.433579e+03, 5.792990e+01, 7.077276e+01},
{3.075000e+00, 1.524683e+03, 4.365754e+03, 4.301336e+01, 5.048979e+01},
{3.125000e+00, 2.866047e+03, 4.949539e+03, 5.903230e+01, 7.679694e+01},
{3.175000e+00, 2.710486e+03, 5.538075e+03, 4.953382e+01, 1.085806e+02},
{3.225000e+00, 2.385396e+03, 4.099739e+03, 3.948580e+01, 1.930562e+02},
{3.275000e+00, 1.602400e+03, 4.899595e+03, 3.821905e+01, 6.859505e+01},
{3.325000e+00, 2.210069e+03, 4.378354e+03, 3.139335e+01, 4.669486e+01},
{3.375000e+00, 2.373778e+03, 4.722604e+03, 3.940508e+01, 4.889166e+01},
{3.425000e+00, 2.370695e+03, 4.973593e+03, 4.263912e+01, 5.127592e+01},
{3.475000e+00, 1.368469e+03, 4.127687e+03, 6.940209e+01, 5.353953e+01},
{3.525000e+00, 2.026259e+03, 4.263232e+03, 4.329374e+01, 5.747373e+01},
{3.575000e+00, 1.595762e+03, 4.575480e+03, 3.603439e+01, 4.273041e+01},
{3.625000e+00, 1.572987e+03, 3.482911e+03, 4.421536e+01, 5.564379e+01},
{3.675000e+00, 2.139275e+03, 4.292381e+03, 3.492030e+01, 4.249308e+01},
{3.725000e+00, 2.356890e+03, 3.089890e+03, 3.577095e+01, 4.865949e+01},
{3.775000e+00, 2.144051e+03, 3.464292e+03, 3.046489e+01, 7.295947e+01},
{3.825000e+00, 1.280116e+03, 3.162276e+03, 3.027183e+01, 2.893482e+01},
{3.875000e+00, 1.402446e+03, 3.733617e+03, 3.209077e+01, 9.395620e+01},
{3.925000e+00, 2.152779e+03, 3.828493e+03, 2.618646e+01, 4.145059e+01},
{3.975000e+00, 1.751394e+03, 2.806776e+03, 2.476590e+01, 3.269666e+01},
{4.025000e+00, 1.647334e+03, 2.562580e+03, 2.932552e+01, 3.803970e+01},
{4.075000e+00, 1.091056e+03, 2.137212e+03, 2.410874e+01, 3.314473e+01},
{4.125000e+00, 1.880928e+03, 2.704029e+03, 2.453410e+01, 3.317791e+01},
{4.175000e+00, 1.912230e+03, 2.482757e+03, 2.167225e+01, 2.773289e+01},
{4.225000e+00, 1.040267e+03, 1.858410e+03, 2.400482e+01, 2.500788e+01},
{4.275000e+00, 1.702135e+03, 3.274898e+03, 2.650055e+01, 2.670071e+01},
{4.325000e+00, 7.934038e+02, 2.237300e+03, 1.666297e+01, 2.695595e+01},
{4.375000e+00, 9.402441e+02, 2.406674e+03, 2.390649e+01, 3.359740e+01},
{4.425000e+00, 1.131132e+03, 1.701947e+03, 2.143371e+01, 2.645462e+01},
{4.475000e+00, 1.140169e+03, 1.755372e+03, 2.418738e+01, 3.067125e+01},
{4.525000e+00, 1.327166e+03, 1.988234e+03, 3.516372e+01, 3.702944e+01},
{4.575000e+00, 7.160208e+02, 1.082036e+03, 2.299185e+01, 2.821434e+01},
{4.625000e+00, 1.544912e+03, 1.893744e+03, 3.288800e+01, 2.858323e+01},
{4.675000e+00, 9.293489e+02, 1.628680e+03, 2.194600e+01, 1.794929e+01},
{4.725000e+00, 1.272754e+03, 2.627311e+03, 2.919157e+01, 3.353546e+01},
{4.775000e+00, 2.142317e+03, 1.638622e+03, 1.798894e+01, 3.365608e+01},
{4.825000e+00, 1.108128e+03, 8.511763e+02, 2.849559e+01, 2.302425e+01},
{4.875000e+00, 1.518359e+03, 1.251574e+03, 2.089907e+01, 2.454762e+01},
{4.925000e+00, 6.418806e+02, 1.211949e+03, 2.852225e+01, 2.322118e+01},
{4.975000e+00, 1.748663e+03, 2.306865e+03, 2.413221e+01, 3.111558e+01},
{5.025000e+00, 8.436855e+02, 1.202705e+03, 2.566037e+01, 2.480401e+01},
{5.075000e+00, 8.702391e+02, 1.413726e+03, 2.228465e+01, 2.395835e+01},
{5.125000e+00, 1.281938e+03, 1.079740e+03, 2.229030e+01, 2.113203e+01},
{5.175000e+00, 1.903075e+03, 6.719944e+02, 2.960239e+01, 2.418635e+01},
{5.225000e+00, 1.309573e+03, 8.494624e+02, 1.862533e+01, 1.512681e+01},
{5.275000e+00, 5.263134e+02, 7.030744e+02, 2.617693e+01, 1.251386e+02},
{5.325000e+00, 7.457245e+02, 1.103303e+03, 3.132922e+01, 3.160182e+01},
{5.375000e+00, 5.350901e+02, 1.566924e+03, 1.670037e+01, 1.596574e+01},
{5.425000e+00, 9.765556e+02, 2.011108e+03, 2.571208e+01, 2.574824e+01},
{5.475000e+00, 7.761735e+02, 7.155841e+02, 1.154974e+01, 1.480719e+01},
{5.525000e+00, 5.721605e+02, 1.607511e+03, 2.402855e+01, 1.686991e+01},
{5.575000e+00, 2.925538e+02, 2.695317e+02, 1.820372e+01, 2.079604e+01},
{5.625000e+00, 1.765344e+03, 9.472237e+02, 1.944080e+01, 1.826776e+01},
{5.675000e+00, 9.865097e+01, 7.610553e+02, 2.969959e+01, 2.569343e+01},
{5.725000e+00, 1.576521e+03, 1.246502e+03, 1.835391e+01, 1.139400e+01},
{5.775000e+00, 3.412519e+02, 1.508029e+03, 9.712137e+00, 5.588587e+00},
{5.825000e+00, 1.327761e+02, 9.930355e+02, 1.632466e+01, 1.367068e+01},
{5.875000e+00, 6.562656e+02, 1.035177e+03, 2.178709e+01, 1.807822e+01},
{5.925000e+00, 6.246444e+02, 1.314308e+03, 2.535336e+01, 2.237706e+01},
{5.975000e+00, 1.154288e+03, 5.054997e+02, 9.563629e+00, 8.773258e+00},
{6.025000e+00, 3.793385e+02, 1.276323e+03, 1.845392e+01, 1.399995e+01},
{6.075000e+00, 4.021768e+02, 5.121132e+02, 1.884151e+01, 1.463257e+01},
{6.125000e+00, 9.300037e+02, 1.328605e+03, 1.395716e+01, 1.757827e+01},
{6.175000e+00, 3.975021e+02, 2.660606e+02, 1.876685e+01, 1.604910e+01},
{6.225000e+00, 9.993079e+02, 7.391015e+02, 2.230244e+01, 1.964892e+01},
{6.275000e+00, 7.532108e+02, 1.119545e+03, 2.439766e+01, 1.832764e+01},
{6.325000e+00, 6.989265e+02, 1.725532e+03, 1.094444e+01, 9.101701e+00},
{6.375000e+00, 1.302645e+03, 8.565120e+02, 2.447586e+01, 1.830351e+01},
{6.425000e+00, 7.012179e+02, 2.315794e+02, 1.903944e+01, 1.751208e+01},
{6.475000e+00, 1.286165e+02, 1.122844e+03, 1.363000e+01, 1.184996e+01},
{6.525000e+00, 7.755920e+02, 5.591131e+02, 8.209298e+00, 7.376628e+00},
{6.575000e+00, 1.095621e+03, 6.195360e+02, 1.265350e+01, 9.111847e+00},
{6.625000e+00, 1.069981e+03, 5.396921e+02, 1.330835e+01, 6.337715e+00},
{6.675000e+00, 4.210199e+02, 9.173175e+02, 1.126356e+01, 6.989345e+00},
{6.725000e+00, 7.269217e+02, 2.057533e+02, 2.178808e+01, 1.490651e+01},
{6.775000e+00, 7.882405e+02, 2.184575e+02, 1.195725e+01, 6.018448e+00},
{6.825000e+00, 1.035169e+02, 2.117252e+02, 1.471398e+01, 1.384838e+01},
{6.875000e+00, 4.752372e+02, 9.123369e+02, 1.504266e+01, 8.325591e+00},
{6.925000e+00, 4.755789e+02, 5.472671e+02, 8.727230e+00, 8.978583e+00},
{6.975000e+00, 8.217407e+02, 2.214393e+02, 7.878145e+00, 9.515668e+00},
{7.025000e+00, 9.392758e+01, 8.876133e+02, 3.334355e+01, 3.004661e+01},
{7.075000e+00, 8.444688e+02, 5.239153e+02, 9.400610e+00, 4.171066e+00},
{7.125000e+00, 4.698753e+02, 1.234736e+02, 7.454053e+00, 5.326849e+00},
{7.175000e+00, 4.934580e+02, 5.649360e+02, 8.953995e+00, 6.618731e+00},
{7.225000e+00, 8.647508e+02, 2.486922e+02, 8.708005e+00, 6.138135e+00},
{7.275000e+00, 1.007609e+02, 2.684605e+02, 7.451348e+00, 2.658923e+00},
{7.325000e+00, 5.288150e+02, 1.871000e+02, 5.626294e+00, 8.083114e+00},
{7.375000e+00, 6.328783e+01, 1.954013e+02, 1.203730e+01, 7.929876e+00},
{7.425000e+00, 5.286398e+02, 1.516083e+02, 1.745761e+01, 8.697247e+00},
{7.475000e+00, 9.152570e+02, 1.773075e+02, 8.562899e+00, 1.655956e+00},
{7.525000e+00, 1.198812e+02, 1.495901e+02, 1.081218e+01, 1.088360e+01},
{7.575000e+00, 7.295661e+01, 5.611544e+02, 1.589621e+01, 6.398217e+00},
{7.625000e+00, 5.370841e+02, 1.045553e+02, 1.506368e+01, 1.410078e+01},
{7.675000e+00, 5.238738e+02, 9.969095e+01, 1.544741e+01, 1.214489e+01},
{7.725000e+00, 6.946659e+01, 5.961414e+02, 1.718542e+01, 9.527207e+00},
{7.775000e+00, 1.023438e+02, 1.405657e+02, 8.210964e+00, 5.784781e+00},
{7.825000e+00, 8.417206e+01, 1.685712e+02, 1.537611e+01, 5.497319e+00},
{7.875000e+00, 9.856638e+01, 1.246933e+02, 7.110223e+00, 6.516110e+00},
{7.925000e+00, 5.989089e+01, 1.196075e+02, 1.543934e+01, 1.178144e+01},
{7.975000e+00, 1.345664e+02, 1.480416e+02, 9.950874e+00, 6.549954e+00},
{8.025000e+00, 7.490541e+01, 1.839577e+02, 2.139564e+01, 1.941596e+01},
{8.075000e+00, 1.035483e+02, 1.376631e+02, 9.648158e+00, 4.828068e+00},
{8.125000e+00, 5.879105e+02, 8.352113e+01, 1.489130e+00, 0.000000e+00},
{8.175000e+00, 1.202138e+02, 1.836193e+02, 1.950426e+01, 1.601830e+01},
{8.225000e+00, 6.150748e+02, 1.358699e+02, 9.639088e+00, 4.689903e+00},
{8.275000e+00, 1.080897e+03, 1.158915e+02, 9.225087e+00, 5.362927e+00},
{8.325000e+00, 6.277416e+02, 8.794671e+01, 4.841617e+00, 0.000000e+00},
{8.375000e+00, 1.140589e+03, 8.169014e+01, 1.652232e+01, 1.533682e+01},
{8.425000e+00, 1.275047e+02, 6.011172e+01, 4.284906e+00, 9.773969e-01},
{8.475000e+00, 6.831114e+01, 9.872187e+01, 2.997239e+00, 9.913683e-01},
{8.525000e+00, 7.688066e+01, 1.075277e+02, 4.876560e+00, 7.643936e-01},
{8.575000e+00, 5.437423e+01, 6.334854e+02, 6.222581e+00, 0.000000e+00},
{8.625000e+00, 9.428838e+01, 9.451054e+01, 1.178625e+01, 9.414602e+00},
{8.675000e+00, 6.314101e+02, 6.163412e+02, 3.287850e+00, 2.081502e+00},
{8.725000e+00, 1.127606e+02, 9.668410e+01, 3.925537e+00, 3.124900e+00},
{8.775000e+00, 9.780927e+01, 1.464086e+02, 1.021183e+01, 7.062878e+00},
{8.825000e+00, 9.059587e+01, 1.073150e+02, 2.669702e+01, 2.523847e+01},
{8.875000e+00, 9.985953e+01, 5.835436e+01, 5.021935e+00, 0.000000e+00},
{8.925000e+00, 1.264689e+02, 6.735296e+01, 8.515180e+00, 5.967381e+00},
{8.975000e+00, 9.366490e+01, 6.816866e+01, 9.973605e+00, 9.973605e+00},
{9.025000e+00, 6.890453e+02, 9.475301e+01, 1.187118e+01, 6.280627e+00},
{9.075000e+00, 8.702561e+01, 6.955978e+02, 1.146822e+00, 0.000000e+00},
{9.125000e+00, 7.927139e+01, 2.639994e+01, 7.408879e+00, 5.347443e+00},
{9.175000e+00, 9.787163e+01, 6.901426e+02, 6.058342e+00, 1.016225e+00},
{9.225000e+00, 5.404344e+01, 2.709408e+01, 7.382523e+00, 3.917923e+00},
{9.275000e+00, 6.382459e+01, 6.357497e+01, 5.423555e+00, 0.000000e+00},
{9.325000e+00, 9.214756e+01, 9.206461e+01, 2.320453e+00, 1.782701e+00},
{9.375000e+00, 6.517277e+01, 4.656146e+01, 1.419706e+01, 8.104143e+00},
{9.425000e+00, 1.315710e+02, 6.594911e+01, 7.064664e+00, 5.019017e+00},
{9.475000e+00, 2.847914e+01, 7.118682e+02, 4.162214e+00, 0.000000e+00},
{9.525000e+00, 1.056519e+02, 4.795991e+01, 3.939212e+00, 9.068942e-01},
{9.575000e+00, 7.607390e+02, 7.048514e+02, 1.951552e+01, 1.563448e+01},
{9.625000e+00, 7.802523e+02, 4.906958e+01, 2.319852e+00, 0.000000e+00},
{9.675000e+00, 4.940454e+01, 4.952661e+01, 3.015947e+00, 0.000000e+00},
{9.725000e+00, 3.999958e+01, 3.991386e+01, 1.016023e+01, 1.016023e+01},
{9.775000e+00, 9.085782e+01, 7.524177e+02, 1.100670e+01, 1.100670e+01},
{9.825000e+00, 8.562193e+02, 4.078584e+01, 3.605815e+00, 2.487884e+00},
{9.875000e+00, 9.274168e+01, 6.197779e+01, 6.832087e+00, 1.350725e+00},
{9.925000e+00, 8.679677e+02, 9.374555e+01, 9.484204e-01, 0.000000e+00},
{9.975000e+00, 1.578546e+02, 3.160086e+01, 5.154089e+00, 3.736521e+00},
{1.002500e+01, 1.169454e+02, 2.124152e+01, 3.775214e+00, 0.000000e+00},
{1.007500e+01, 3.221936e+01, 3.228075e+01, 9.185436e+00, 6.290472e+00},
{1.012500e+01, 7.579540e+01, 4.349527e+01, 1.260969e+01, 6.981411e+00},
{1.017500e+01, 8.149617e+02, 5.465584e+01, 5.103284e+00, 5.103284e+00},
{1.022500e+01, 7.724231e+01, 3.320416e+01, 3.004098e+00, 1.121873e+00},
{1.027500e+01, 1.451777e+02, 3.344923e+01, 2.509482e+00, 2.071507e+00},
{1.032500e+01, 4.508835e+01, 3.381125e+01, 5.501094e+00, 0.000000e+00},
{1.037500e+01, 6.824026e+01, 3.423097e+01, 1.344214e+00, 0.000000e+00},
{1.042500e+01, 5.763036e+01, 0.000000e+00, 1.555605e+00, 0.000000e+00},
{1.047500e+01, 1.046024e+02, 0.000000e+00, 4.449011e+00, 0.000000e+00},
{1.052500e+01, 8.186397e+01, 4.692301e+01, 2.764462e+00, 0.000000e+00},
{1.057500e+01, 9.469420e+01, 2.368812e+01, 2.472939e+00, 0.000000e+00},
{1.062500e+01, 4.773047e+01, 3.580923e+01, 4.484579e+00, 0.000000e+00},
{1.067500e+01, 1.084063e+02, 4.818778e+01, 1.318415e+00, 0.000000e+00},
{1.072500e+01, 7.285718e+01, 3.650513e+01, 1.646774e+00, 2.976159e+00},
{1.077500e+01, 8.594010e+01, 4.907161e+01, 1.212485e+01, 1.081568e+01},
{1.082500e+01, 8.661029e+01, 8.741313e+02, 7.621012e+00, 5.949197e+00},
{1.087500e+01, 1.000989e+02, 5.007865e+01, 1.572873e+00, 1.352372e+00},
{1.092500e+01, 8.843555e+01, 3.794237e+01, 1.634411e+00, 0.000000e+00},
{1.097500e+01, 1.148650e+02, 1.271682e+01, 7.339546e+00, 5.788168e+00},
{1.102500e+01, 3.854352e+01, 6.436632e+01, 5.527597e+00, 0.000000e+00},
{1.107500e+01, 3.891053e+01, 0.000000e+00, 0.000000e+00, 1.547823e+00},
{1.112500e+01, 5.227014e+01, 3.916769e+01, 0.000000e+00, 1.307200e+00},
{1.117500e+01, 3.968377e+01, 1.323726e+01, 0.000000e+00, 0.000000e+00},
{1.122500e+01, 7.986861e+01, 0.000000e+00, 1.944177e+00, 0.000000e+00},
{1.127500e+01, 1.076483e+02, 4.035008e+01, 1.650849e+00, 0.000000e+00},
{1.132500e+01, 9.484984e+01, 0.000000e+00, 3.952165e+00, 0.000000e+00},
{1.137500e+01, 5.468222e+01, 1.366986e+01, 1.934680e+00, 0.000000e+00},
{1.142500e+01, 6.896443e+01, 4.143773e+01, 0.000000e+00, 0.000000e+00},
{1.147500e+01, 5.561333e+01, 0.000000e+00, 3.266902e+00, 0.000000e+00},
{1.152500e+01, 4.216326e+01, 1.409977e+01, 5.119435e+00, 0.000000e+00},
{1.157500e+01, 1.133498e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.162500e+01, 2.853278e+01, 1.430880e+01, 0.000000e+00, 0.000000e+00},
{1.167500e+01, 1.154682e+02, 0.000000e+00, 7.818054e+00, 7.818054e+00},
{1.172500e+01, 7.269569e+01, 0.000000e+00, 1.734707e+00, 0.000000e+00},
{1.177500e+01, 5.871039e+01, 1.467542e+01, 0.000000e+00, 0.000000e+00},
{1.182500e+01, 2.960894e+01, 1.478219e+01, 0.000000e+00, 0.000000e+00},
{1.187500e+01, 7.450646e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.192500e+01, 4.510394e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.197500e+01, 6.064178e+01, 3.037324e+01, 2.345385e+00, 0.000000e+00},
{1.202500e+01, 9.194589e+01, 0.000000e+00, 3.433858e+00, 0.000000e+00},
{1.207500e+01, 7.705618e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.212500e+01, 3.107934e+01, 3.110147e+01, 0.000000e+00, 0.000000e+00},
{1.217500e+01, 1.098483e+02, 1.566866e+01, 0.000000e+00, 0.000000e+00},
{1.222500e+01, 1.422746e+02, 0.000000e+00, 1.706155e+00, 0.000000e+00},
{1.227500e+01, 1.598174e+01, 1.592497e+01, 2.105426e+01, 2.105426e+01},
{1.232500e+01, 3.215646e+01, 1.602083e+01, 0.000000e+00, 0.000000e+00},
{1.237500e+01, 6.466353e+01, 1.621814e+01, 0.000000e+00, 0.000000e+00},
{1.242500e+01, 6.537087e+01, 1.632943e+01, 0.000000e+00, 0.000000e+00},
{1.247500e+01, 4.948765e+01, 4.936761e+01, 8.788560e+00, 8.788560e+00},
{1.252500e+01, 3.315252e+01, 0.000000e+00, 1.682130e+00, 0.000000e+00},
{1.257500e+01, 6.698575e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.262500e+01, 3.375153e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.267500e+01, 5.100304e+01, 0.000000e+00, 1.101611e+01, 6.920232e+00},
{1.272500e+01, 1.028649e+02, 1.719061e+01, 2.369199e+00, 0.000000e+00},
{1.277500e+01, 8.627736e+01, 3.445362e+01, 1.663605e+00, 0.000000e+00},
{1.282500e+01, 1.737189e+01, 1.746189e+01, 0.000000e+00, 0.000000e+00},
{1.287500e+01, 3.512167e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.292500e+01, 8.833518e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.297500e+01, 3.551580e+01, 3.561695e+01, 2.229691e+00, 0.000000e+00},
{1.302500e+01, 3.585667e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.307500e+01, 3.607371e+01, 1.809404e+01, 2.628084e+00, 0.000000e+00},
{1.312500e+01, 1.818846e+01, 0.000000e+00, 1.936700e+00, 0.000000e+00},
{1.317500e+01, 9.176747e+01, 0.000000e+00, 2.270890e+00, 0.000000e+00},
{1.322500e+01, 5.548852e+01, 0.000000e+00, 8.613230e+00, 8.613230e+00},
{1.327500e+01, 1.866335e+01, 0.000000e+00, 3.037795e+00, 0.000000e+00},
{1.332500e+01, 3.748080e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.337500e+01, 7.567198e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.342500e+01, 1.900612e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.347500e+01, 3.848288e+01, 1.922039e+01, 0.000000e+00, 0.000000e+00},
{1.352500e+01, 3.864629e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.357500e+01, 9.758862e+01, 0.000000e+00, 1.855466e+00, 0.000000e+00},
{1.362500e+01, 5.891360e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.367500e+01, 1.977589e+01, 1.984177e+01, 2.375026e+00, 0.000000e+00},
{1.372500e+01, 1.991764e+01, 0.000000e+00, 2.431844e+00, 0.000000e+00},
{1.377500e+01, 2.004106e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.382500e+01, 6.063590e+01, 2.020979e+01, 0.000000e+00, 0.000000e+00},
{1.387500e+01, 4.071814e+01, 2.039466e+01, 1.537052e+01, 1.537052e+01},
{1.392500e+01, 6.139623e+01, 2.044892e+01, 0.000000e+00, 0.000000e+00},
{1.397500e+01, 6.212739e+01, 0.000000e+00, 1.986720e+00, 0.000000e+00},
{1.402500e+01, 4.161388e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.407500e+01, 2.092501e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.412500e+01, 4.217402e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.417500e+01, 2.129976e+01, 2.124700e+01, 0.000000e+00, 0.000000e+00},
{1.422500e+01, 8.563279e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.427500e+01, 8.617455e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.432500e+01, 1.084044e+02, 2.171948e+01, 0.000000e+00, 0.000000e+00},
{1.437500e+01, 6.566414e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.442500e+01, 4.400866e+01, 4.405865e+01, 0.000000e+00, 0.000000e+00},
{1.447500e+01, 8.859414e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.452500e+01, 4.459221e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.457500e+01, 4.491276e+01, 0.000000e+00, 2.171806e+00, 0.000000e+00},
{1.462500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.467500e+01, 4.553398e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.472500e+01, 4.579529e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.477500e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.482500e+01, 2.326441e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.487500e+01, 2.340535e+01, 2.346149e+01, 0.000000e+00, 0.000000e+00},
{1.492500e+01, 2.360945e+01, 0.000000e+00, 0.000000e+00, 0.000000e+00},
{1.497500e+01, 7.116539e+01, 0.000000e+00, 2.588541e+00, 0.000000e+00}
};