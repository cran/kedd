#include <Rcpp.h>
using namespace Rcpp;
#include <stdio.h>  
#include <stdlib.h> 
#include <math.h>   
#include <string.h> 
#include <Rmath.h>
#ifdef _OPENMP
#include <omp.h>
#endif

/* Number of OpenMP threads for one call: nthreads > 0 is used as given,
 * nthreads <= 0 means all processors; both capped by OMP_THREAD_LIMIT.
 * It is passed to each region with num_threads(), never with the global
 * omp_set_num_threads(), so a call never changes the threads of later calls. */
static inline int kedd_threads(int nthreads) {
#ifdef _OPENMP
    int nt = (nthreads > 0) ? nthreads : omp_get_num_procs();
    int lim = omp_get_thread_limit();
    if (lim > 0 && nt > lim) nt = lim;
    return nt < 1 ? 1 : nt;
#else
    (void) nthreads;
    return 1;
#endif
}

/* Raw function-pointer type — eliminates std::function heap overhead */
typedef double (*KernelFuncPtr)(double, int);

#include <vector>
#include <atomic>

/* 16-point Gauss-Legendre rule on [-1,1]: positive nodes and their weights */
static const double GL16_N[8] = {
    0.09501250983763744, 0.28160355077925891, 0.45801677765722739,
    0.61787624440264375, 0.75540440835500303, 0.86563120238783174,
    0.94457502307323258, 0.98940093499164993
};
static const double GL16_W[8] = {
    0.18945061045506849, 0.18260341504492359, 0.16915651939500254,
    0.14959598881657673, 0.12462897125553388, 0.09515851168249279,
    0.06225352393864789, 0.02715245941175409
};

/*
 * Pow(s,t) kernel family:  K(x) = c_{s,t} (1 - |x|^s)^t 1{|x| <= 1},
 *   c_{s,t} = s / (2 B(1/s, t+1)),  s > 0, t >= 0.
 * Selected by the kernel name "pow(s,t)"; uniform = pow(s,0),
 * epanechnikov = pow(2,1), biweight = pow(2,2), triweight = pow(2,3),
 * triangular = pow(1,1), tricube = pow(3,3).
 *
 * For x > 0, K^{(r)}(x) = sum_k coef_k x^{p_k} (1 - x^s)^{q_k}:
 *   - integer t <= 10: binomial expansion, q_k = 0 (no cancellation near |x| = 1);
 *   - otherwise: p_j = j s - r, q_j = t - j, coefficients by recurrence on r.
 * Parameters live in globals set while resolving the kernel name, which always
 * happens outside OpenMP regions; parallel code only reads them.
 */
#define POW_MAXR 16
#define POW_NTAB 4001

struct PowTerm { double coef, p, q; };

static double g_pow_s = -1.0, g_pow_t = -1.0, g_pow_c = 0.0;
static bool   g_pow_binom = false;
static std::vector<PowTerm> g_pow_terms[POW_MAXR + 1];
static std::vector<double>  g_pow_tab[POW_MAXR + 1];
static std::atomic<bool>    g_pow_ready[POW_MAXR + 1];

static void pow_set(double s, double t) {
    g_pow_s = s;
    g_pow_t = t;
    g_pow_c = s / (2.0 * R::beta(1.0 / s, t + 1.0));
    double tr = std::floor(t + 0.5);
    g_pow_binom = (std::fabs(t - tr) < 1e-12 && tr <= 10.0);
    for (int r = 0; r <= POW_MAXR; ++r) {
        g_pow_terms[r].clear();
        g_pow_ready[r].store(false);
    }
    if (g_pow_binom) {
        int T = (int) tr;
        for (int k = 0; k <= T; ++k) {
            double base = g_pow_c * R::choose((double) T, (double) k) * ((k % 2) ? -1.0 : 1.0);
            double e = k * s, ff = 1.0;
            for (int r = 0; r <= POW_MAXR; ++r) {
                if (r > 0) ff *= (e - (r - 1));
                if (base * ff != 0.0) g_pow_terms[r].push_back({base * ff, e - r, 0.0});
            }
        }
    } else {
        std::vector<double> a(1, g_pow_c), b;
        for (int r = 0; r <= POW_MAXR; ++r) {
            for (int j = 0; j <= r; ++j)
                if (a[j] != 0.0) g_pow_terms[r].push_back({a[j], j * s - r, t - j});
            b.assign(r + 2, 0.0);
            for (int j = 0; j <= r; ++j) {
                b[j]     += a[j] * (j * s - r);
                b[j + 1] -= a[j] * (t - j) * s;
            }
            a.swap(b);
        }
    }
}

static bool pow_select(const char* kernel) {
    double s, t;
    if (strncmp(kernel, "pow(", 4) != 0) return false;
    if (sscanf(kernel, "pow(%lf,%lf)", &s, &t) != 2)
        Rcpp::stop("Pow kernel must be written \"pow(s,t)\", e.g. \"pow(2,3)\".");
    if (!(s > 0.0) || !(t >= 0.0))
        Rcpp::stop("Pow kernel requires s > 0 and t >= 0.");
    if (s != g_pow_s || t != g_pow_t) pow_set(s, t);
    return true;
}

double pow_kernel(double u, int deriv_order) {
    if (deriv_order < 0 || deriv_order > POW_MAXR) return 0.0;
    double ax = std::fabs(u);
    if (ax > 1.0) return 0.0;                          /* closed support, as the named kernels */
    if (ax == 0.0 && (deriv_order & 1)) return 0.0;   /* odd derivative of an even kernel */
    double w = 1.0 - std::pow(ax, g_pow_s), v = 0.0;
    const std::vector<PowTerm>& T = g_pow_terms[deriv_order];
    for (size_t k = 0; k < T.size(); ++k) {
        double xp;
        if (T[k].p == 0.0)   xp = 1.0;
        else if (ax == 0.0) continue;   /* x^p -> 0 (p > 0); p < 0 is a genuine singularity, set to 0 */
        else                 xp = std::pow(ax, T[k].p);
        if (T[k].q > 0.0 && w <= 0.0) continue;
        v += T[k].coef * xp * (T[k].q == 0.0 ? 1.0 : std::pow(w, T[k].q));
    }
    return (u < 0.0 && (deriv_order & 1)) ? -v : v;
}

/* (K^(r) * K^(r))(u) for u >= 0 by quadrature over [u-1, 1], split at 0 and u */
static double pow_conv_direct(double u, int r) {
    if (u >= 2.0) return 0.0;
    double cuts[4];
    int nc = 0;
    cuts[nc++] = u - 1.0;
    if (u < 1.0) cuts[nc++] = 0.0;
    if (u > 0.0 && u < 1.0) cuts[nc++] = u;
    cuts[nc++] = 1.0;
    double total = 0.0;
    for (int c = 0; c + 1 < nc; ++c) {
        double step = 0.5 * (cuts[c + 1] - cuts[c]);
        for (int m = 0; m < 2; ++m) {
            double lo = cuts[c] + m * step, half = 0.5 * step, mid = lo + half, sum = 0.0;
            for (int i = 0; i < 8; ++i) {
                double x1 = mid + half * GL16_N[i], x2 = mid - half * GL16_N[i];
                sum += GL16_W[i] * (pow_kernel(x1, r) * pow_kernel(u - x1, r) +
                                    pow_kernel(x2, r) * pow_kernel(u - x2, r));
            }
            total += half * sum;
        }
    }
    return total;
}

/* Convolution of a function with itself is even: tabulate on [0,2] once per (s,t,r) */
double pow_kernel_conv(double u, int deriv_order) {
    if (deriv_order < 0 || deriv_order > POW_MAXR) return 0.0;
    double au = std::fabs(u);
    if (au >= 2.0) return 0.0;
    if (!g_pow_ready[deriv_order].load(std::memory_order_acquire)) {
        #pragma omp critical(kedd_pow_conv_table)
        {
            if (!g_pow_ready[deriv_order].load(std::memory_order_relaxed)) {
                std::vector<double>& tab = g_pow_tab[deriv_order];
                tab.assign(POW_NTAB, 0.0);
                for (int i = 0; i < POW_NTAB; ++i)
                    tab[i] = pow_conv_direct(2.0 * i / (POW_NTAB - 1), deriv_order);
                g_pow_ready[deriv_order].store(true, std::memory_order_release);
            }
        }
    }
    const std::vector<double>& tab = g_pow_tab[deriv_order];
    double pos = au * (POW_NTAB - 1) / 2.0;
    int i = (int) pos;
    if (i >= POW_NTAB - 1) i = POW_NTAB - 2;
    double f = pos - i;
    return tab[i] + f * (tab[i + 1] - tab[i]);
}

/* even moments  int x^k K(x) dx = B((k+1)/s, t+1) / B(1/s, t+1) */
static double pow_moment(int k) {
    return R::beta((k + 1.0) / g_pow_s, g_pow_t + 1.0) / R::beta(1.0 / g_pow_s, g_pow_t + 1.0);
}

/* R(K^(r)) = int K^(r)(x)^2 dx */
static double pow_Rr(int r) {
    if (r < 0 || r > POW_MAXR) return 0.0;
    const std::vector<PowTerm>& T = g_pow_terms[r];
    if (r == 0)
        return g_pow_s * R::beta(1.0 / g_pow_s, 2.0 * g_pow_t + 1.0) /
               (2.0 * std::pow(R::beta(1.0 / g_pow_s, g_pow_t + 1.0), 2));
    if (g_pow_binom) {                  /* exact: 2 sum_kl b_k b_l / (p_k + p_l + 1) */
        double acc = 0.0;
        for (size_t k = 0; k < T.size(); ++k)
            for (size_t l = 0; l < T.size(); ++l) {
                double e = T[k].p + T[l].p + 1.0;
                if (e <= 0.0) return R_PosInf;
                acc += T[k].coef * T[l].coef / e;
            }
        return 2.0 * acc;
    }
    static const double br[] = {0.0, 1e-6, 1e-4, 1e-3, 1e-2, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                                0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999, 0.9999, 1.0 - 1e-6, 1.0};
    const int nb = sizeof(br) / sizeof(br[0]);
    double total = 0.0;
    for (int b = 0; b + 1 < nb; ++b) {
        double half = 0.5 * (br[b + 1] - br[b]), mid = br[b] + half, sum = 0.0;
        for (int i = 0; i < 8; ++i) {
            double f1 = pow_kernel(mid + half * GL16_N[i], r);
            double f2 = pow_kernel(mid - half * GL16_N[i], r);
            sum += GL16_W[i] * (f1 * f1 + f2 * f2);
        }
        total += half * sum;
    }
    return 2.0 * total;
}

/*
 * Logistic kernel K = sigma' with sigma(x) = 1/(1+e^{-x}).  Since sigma' = y(1-y), y = sigma(x),
 *   K^(r)(x) = y (1-y) Q_{r+1}(y),  Q_1 = 1,  Q_{n+1} = (1-2y) Q_n + (y-y^2) Q_n'.
 * LOGIS_Q[r][k] holds the coefficients of Q_{r+1}; built once at load time.
 */
#define LOGIS_MAXR 20
struct LogisticPoly {
    double q[LOGIS_MAXR + 1][LOGIS_MAXR + 2];
    LogisticPoly() {
        memset(q, 0, sizeof(q));
        q[0][0] = 1.0;
        for (int r = 0; r < LOGIS_MAXR; ++r)
            for (int k = 0; k <= r; ++k) {
                double a = q[r][k];
                q[r + 1][k]     += a * (1.0 + k);
                q[r + 1][k + 1] -= a * (2.0 + k);
            }
    }
};
static const LogisticPoly LOGIS_Q;

/* R(K^(r)) = |B_{2r+2}| (Bernoulli numbers), from the characteristic function
 * pi w / sinh(pi w) and Parseval's identity; exact rationals, r = 0..20. */
static double logistic_Rr(int r) {
    static const double absB[LOGIS_MAXR + 1] = {
        1.0 / 6.0,                                   /* |B_2|  */
        1.0 / 30.0,                                  /* |B_4|  */
        1.0 / 42.0,                                  /* |B_6|  */
        1.0 / 30.0,                                  /* |B_8|  */
        5.0 / 66.0,                                  /* |B_10| */
        691.0 / 2730.0,                              /* |B_12| */
        7.0 / 6.0,                                   /* |B_14| */
        3617.0 / 510.0,                              /* |B_16| */
        43867.0 / 798.0,                             /* |B_18| */
        174611.0 / 330.0,                            /* |B_20| */
        854513.0 / 138.0,                            /* |B_22| */
        236364091.0 / 2730.0,                        /* |B_24| */
        8553103.0 / 6.0,                             /* |B_26| */
        23749461029.0 / 870.0,                       /* |B_28| */
        8615841276005.0 / 14322.0,                   /* |B_30| */
        7709321041217.0 / 510.0,                     /* |B_32| */
        2577687858367.0 / 6.0,                       /* |B_34| */
        26315271553053477373.0 / 1919190.0,          /* |B_36| */
        2929993913841559.0 / 6.0,                    /* |B_38| */
        261082718496449122051.0 / 13530.0,           /* |B_40| */
        1520097643918070802691.0 / 1806.0            /* |B_42| */
    };
    if (r < 0 || r > LOGIS_MAXR) return 0.0;
    return absB[r];
}

double A2_kM(const char* kernel);
double A3_kMr(const char* kernel, int deriv_order);
double A5_kM(const char* kernel);

/* Kernel constants used by the selectors: normalising constant c (pow kernels
 * only, NA otherwise), mu2 = int x^2 K, mu4 = int x^4 K, R = int (K^(r))^2. */
//[[Rcpp::export]]
NumericVector Cpp_kernel_constants(CharacterVector kernel, int deriv_order) {
    std::string k = Rcpp::as<std::string>(kernel[0]);
    double R  = A3_kMr(k.c_str(), deriv_order);
    double m2 = A2_kM(k.c_str()), m4 = A5_kM(k.c_str());
    double c  = pow_select(k.c_str()) ? g_pow_c : NA_REAL;
    return NumericVector::create(Named("c") = c, Named("mu2") = m2,
                                 Named("mu4") = m4, Named("R") = R);
}


/*
 * \int x^2 K(x) dx
 */

double A2_kM(const char* kernel) {
    if (pow_select(kernel)) return pow_moment(2);
    double xKr = 0.0;
    if (strcmp(kernel, "gaussian") == 0) {
        xKr = 1;
    } else if (strcmp(kernel, "silverman") == 0) {
        xKr = 0;
    } else if (strcmp(kernel, "epanechnikov") == 0) {
        xKr = 1 / 5.0;
    } else if (strcmp(kernel, "uniform") == 0) {
        xKr = 1 / 3.0;
    } else if (strcmp(kernel, "triangular") == 0) {
        xKr = 1 / 6.0;
    } else if (strcmp(kernel, "triweight") == 0) {
        xKr = 1 / 9.0;
    } else if (strcmp(kernel, "tricube") == 0) {
        xKr = 35 / 243.0;
    } else if (strcmp(kernel, "biweight") == 0) {
        xKr = 1 / 7.0;
    } else if (strcmp(kernel, "cosine") == 0) {
        xKr = (-8 + M_PI * M_PI) / (M_PI * M_PI);
    } else if (strcmp(kernel, "logistic") == 0) {
        xKr = M_PI * M_PI / 3.0;
    }
    return xKr;
}


/*
 * \int K(x,r)^2 dx
 */
 
 
double DoubleFactorial(int n) {
    if (n <= 1) return 1;
    double result = 1;
    for (int i = n; i > 0; i -= 2) {
        result *= i;
    }
    return result;
}

double A3_kMr(const char* kernel, int deriv_order) {
    if (pow_select(kernel)) return pow_Rr(deriv_order);
    double xKr = 0;
    if (strcmp(kernel, "gaussian") == 0) {
        xKr = (DoubleFactorial(2 * (deriv_order - 1) + 1) * sqrt(M_PI)) / (pow(2, deriv_order + 1) * M_PI);
    } else if (strcmp(kernel, "silverman") == 0) {
        deriv_order = deriv_order % 4;
        switch (deriv_order) {
            case 0:
                xKr = (3 * sqrt(2)) / 16;
                break;
			case 1:
			    xKr = sqrt(2) / 16;
                break;
			case 2:
			    xKr = sqrt(2) / 16;
                break;
            case 3:
                xKr = (3 * sqrt(2)) / 16;
                break;
            default:
                xKr = 0;
                break;
        }
    } else if (strcmp(kernel, "epanechnikov") == 0) {
        switch (deriv_order) {
            case 0:
                xKr = 3.0 / 5;
                break;
            case 1:
                xKr = 3.0 / 2;
                break;
            case 2:
                xKr = 9.0 / 2;
                break;
            default:
                xKr = 0;
                break;
        }
    } else if (strcmp(kernel, "uniform") == 0) {
        if (deriv_order == 0) xKr = 1.0 / 2;
        else xKr = 0;
    } else if (strcmp(kernel, "triangular") == 0) {
        if (deriv_order == 0) xKr = 2.0 / 3;
        else if (deriv_order == 1) xKr = 2;
        else xKr = 0;
    } else if (strcmp(kernel, "triweight") == 0) {
        switch (deriv_order) {
            case 0:
                xKr = 350.0 / 429;
                break;
            case 1:
                xKr = 35.0 / 11;
                break;
            case 2:
                xKr = 35.0;
                break;
            case 3:
                xKr = 1575.0 / 2;
                break;
            case 4:
                xKr = 33075.0;
                break;
            case 5:
                xKr = 826875.0 / 2;
                break;
            case 6:
                xKr = 2480625.0 / 2;
                break;
            default:
                xKr = 0;
                break;
        }
    } else if (strcmp(kernel, "tricube") == 0) {
        switch (deriv_order) {
            case 0:
                xKr = 175.0 /247;
                break;
            case 1:
                xKr = 420.0 /187;
                break;
            case 2:
                xKr = 7840.0 / 243;
                break;
            case 3:
                xKr = 28000.0 / 13;
                break;
            case 4:
                xKr = 4076800.0 / 11;
                break;
            case 5:
                xKr = 1944320000.0 / 81;
                break;
            case 6:
                xKr = 689920000.0;
                break;
            case 7:
                xKr = 9834496000.0;
                break;
            case 8:
                xKr = 196689920000.0 / 3;
                break;
            case 9:
                xKr = 196689920000.0;
                break;
            default:
                xKr = 0;
                break;
        }
    } else if (strcmp(kernel, "biweight") == 0) {
        switch (deriv_order) {
            case 0:
                xKr = 5.0 / 7;
                break;
            case 1:
                xKr = 15.0 / 7;
                break;
            case 2:
                xKr = 45.0 / 2;
                break;
            case 3:
                xKr = 675.0 / 2;
                break;
            case 4:
                xKr = 2025.0 / 2;
                break;
            default:
                xKr = 0;
                break;
        }
    } else if (strcmp(kernel, "cosine") == 0) {
        xKr = 0.25 * (pow(M_PI, (2 * deriv_order + 1)) / pow(2, (2 * deriv_order + 2))) * (2 * pow(-1, deriv_order) * cos(M_PI / 2) * sin(M_PI / 2) + M_PI);
    } else if (strcmp(kernel, "logistic") == 0) {
        xKr = logistic_Rr(deriv_order);   /* 1/6, 1/30, 1/42, 1/30, 5/66, ... */
    } else {
        Rcpp::stop(std::string("Unsupported kernel type: ") + kernel);
    }
    return xKr;
}


/*
 * \int x^4 * K(x) dx
 */

double A5_kM(const char* kernel) {
    if (pow_select(kernel)) return pow_moment(4);
    double xKr = 0.0;
    if (strcmp(kernel, "gaussian") == 0) {
        xKr = 3;
    } else if (strcmp(kernel, "silverman") == 0) {
        xKr = -24;
    } else if (strcmp(kernel, "epanechnikov") == 0) {
        xKr = 3 / 35.0;
    } else if (strcmp(kernel, "uniform") == 0) {
        xKr = 1 / 5.0;
    } else if (strcmp(kernel, "triangular") == 0) {
        xKr = 1 / 15.0;
    } else if (strcmp(kernel, "triweight") == 0) {
        xKr = 1 / 33.0;
    } else if (strcmp(kernel, "tricube") == 0) {
        xKr = 1 / 22.0;
    } else if (strcmp(kernel, "biweight") == 0) {
        xKr = 1 / 21.0;
    } else if (strcmp(kernel, "cosine") == 0) {
        xKr = (pow(M_PI, 4) - 48 * pow(M_PI, 2) + 384) / pow(M_PI, 4);
    } else if (strcmp(kernel, "logistic") == 0) {
        xKr = 7.0 * pow(M_PI, 4) / 15.0;   /* 4th moment of Logistic(0,1) */
    }
    return xKr;
}


/*
*  hos (Over-smoothing)
*/

//[[Rcpp::export]]
double Cpp_hos(CharacterVector kernel, int deriv_order, int n) {
    std::string kernel_str = Rcpp::as<std::string>(kernel);
    const char* c_kernel = kernel_str.c_str();	
    double A3KM = A3_kMr(c_kernel, deriv_order);
    double A2KM = A2_kM(c_kernel);
    double hos = pow((243 * (2 * deriv_order + 1) * A3KM) / (35 * pow(A2KM, 2)), 1.0 / (2 * deriv_order + 5)) * pow(n, -1.0 / (2 * deriv_order + 5));
    return hos;
}

/*
 * r(th) derivative of Kernel functions K(x,r)
 */

// Fonction pour calculer la n-ième dérivée du noyau gaussien
// Fonction pour calculer les polynômes H(x,n)

double hermite_polynomial(int n, double u) {
    if (n == 0) return 1.0;
    if (n == 1) return u;
    double h0 = 1.0, h1 = u, hk = 0.0;
    for (int k = 2; k <= n; ++k) {
        hk = u * h1 - (k - 1) * h0;
        h0 = h1; h1 = hk;
    }
    return h1;
}


double gaussian_kernel(double u, int deriv_order) {
    double hermite = hermite_polynomial(deriv_order, u);
    double gaussianPart = exp(-u * u / 2) / sqrt(2 * M_PI);
    return pow(-1.0, deriv_order) * hermite * gaussianPart;
}

// Fonction pour calculer la n-ième dérivée du noyau epanechnikov

double epanechnikov_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0; 
    switch (deriv_order) {
        case 0:
            return (3.0 / 4.0) * (1 - u * u);
        case 1:
            return -3.0 / 2.0 * u;
        case 2:
            return -3.0 / 2.0;
        default:
            return 0.0;
    }
}

// Fonction pour calculer la n-ième dérivée du noyau triangular

double triangular_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0; 
    switch (deriv_order) {
        case 0:
            return 1 - fabs(u); 
        case 1:
            if (u < 0) return 1; 
            else if (u > 0) return -1; 
            else return 0; 
        default:
            return 0.0; 
    }
}


// Fonction pour calculer la n-ième dérivée du noyau biweight

double biweight_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0;  
    switch (deriv_order) {
        case 0:
            return (15.0/16.0) * pow(1 - u * u, 2);
        case 1:
            return (15.0/4.0) * u * (u * u - 1);
        case 2:
            return (15.0/4.0) * (3 * u * u - 1);
        case 3:
            return (45.0/2.0) * u;
        case 4:
            return (45.0/2.0);
        default:
            return 0.0;
    }
}

// Fonction pour calculer la n-ième dérivée du noyau uniform

double uniform_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0;
    switch (deriv_order) {
        case 0:
            return 0.5;
        default:
            return 0.0;
    }
}

// Fonction pour calculer la n-ième dérivée du noyau triweight

double triweight_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0;  
    switch (deriv_order) {
        case 0:
            return (35.0 / 32.0) * pow((1 - u * u), 3);
        case 1:
            return -(105.0 / 16.0) * u * pow((1 - u * u), 2);
        case 2:
            return (105.0 / 16.0) * (1 - u * u) * (5 * u * u - 1);
        case 3:
            return (105.0 / 4.0) * u * (3 - 5 * u * u);
        case 4:
            return (315 / 4.0) * (1 - 5 * u * u);
        case 5:
            return -(1575 / 2.0) * u;
        case 6:
            return -(1575 / 2.0);
        default:
            return 0.0;
    }
}

// Fonction pour calculer la n-ième dérivée du noyau tricube

double tricube_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0;
    double result = 0.0;
    switch (deriv_order) {
        case 0:
            result = (70.0 / 81.0) * pow((1 - pow(fabs(u),3)), 3);
            break;
        case 1:
            result = (u < 0 && u >= -1) ? (70.0/9.0) * pow((u*u*u + 1), 2) * u*u :
                      (u >= 0 && u <= 1) ? -(70.0/9.0) * pow((u*u*u - 1), 2) * u*u : 0.0;
            break;
        case 2:
            result = (u < 0 && u >= -1) ? (560.0/9.0) * pow(u, 7) + (700.0/9.0) * pow(u, 4) + (140.0/9.0) * u :
                      (u >= 0 && u <= 1) ? -(560.0/9.0) * pow(u, 7) + (700.0/9.0) * pow(u, 4) - (140.0/9.0) * u : 0.0;
            break;
        case 3:
            result = (u < 0 && u >= -1) ? (3920.0/9.0) * pow(u, 6) + (2800.0/9.0) * pow(u, 3) + (140.0/9.0) :
                      (u >= 0 && u <= 1) ? -(3920.0/9.0) * pow(u, 6) + (2800.0/9.0) * pow(u, 3) - (140.0/9.0) : 0.0;
            break;
        case 4:
            result = (u < 0 && u >= -1) ? (7840.0/3.0) * pow(u, 5) + (2800.0/3.0) * pow(u, 2) :
                      (u >= 0 && u <= 1) ? -(7840.0/3.0) * pow(u, 5) + (2800.0/3.0) * pow(u, 2) : 0.0;
            break;
        case 5:
            result = (u < 0 && u >= -1) ? (39200.0/3.0) * pow(u, 4) + (5600.0/3.0) * u :
                      (u >= 0 && u <= 1) ? -(39200.0/3.0) * pow(u, 4) + (5600.0/3.0) * u : 0.0;
            break;
        case 6:
            result = (u < 0 && u >= -1) ? (156800.0/3.0) * pow(u, 3) + (5600.0/3.0) :
                      (u >= 0 && u <= 1) ? -(156800.0/3.0) * pow(u, 3) + (5600.0/3.0) : 0.0;
            break;
        case 7:
            result = (u < 0 && u >= -1) ? 156800.0 * pow(u, 2) :
                      (u >= 0 && u <= 1) ? -156800.0 * pow(u, 2) : 0.0;
            break;
        case 8:
            result = (u < 0 && u >= -1) ? 313600.0 * u :
                      (u >= 0 && u <= 1) ? -313600.0 * u : 0.0;
            break;
        case 9:
            result = (u < 0 && u >= -1) ? 313600.0 :
                      (u >= 0 && u <= 1) ? -313600.0 : 0.0;
            break;
        default:
            result = 0.0; 
    }
    return result;
}

// Fonction pour calculer la n-ième dérivée du noyau cosine

double cosine_kernel(double u, int deriv_order) {
    if (fabs(u) > 1) return 0.0; 
    double result = 0.0;
    int r_div_2_is_even = (deriv_order / 2) % 2 == 0;
    int r_is_even = deriv_order % 2 == 0;
    if (r_is_even) {
        if (r_div_2_is_even) {
            result = pow(-1, deriv_order) / pow(2, deriv_order + 2) * pow(M_PI, deriv_order + 1) * cos(0.5 * M_PI * u);
        } else {
            result = -pow(-1, deriv_order) / pow(2, deriv_order + 2) * pow(M_PI, deriv_order + 1) * cos(0.5 * M_PI * u);
        }
    } else {
        if (r_div_2_is_even) {
            result = pow(-1, deriv_order) / pow(2, deriv_order + 2) * pow(M_PI, deriv_order + 1) * sin(0.5 * M_PI * u);
        } else {
            result = -pow(-1, deriv_order) / pow(2, deriv_order + 2) * pow(M_PI, deriv_order + 1) * sin(0.5 * M_PI * u);
        }
    }
    return result;
}

// Fonction pour calculer la n-ième dérivée du noyau silverman

double silverman_kernel(double u, int deriv_order) {
    double result = 0.0;
    double sqrt2 = sqrt(2.0);
    double pi = M_PI;
    deriv_order = deriv_order % 8;
    switch (deriv_order) {
        case 0:
            result = 0.5 * exp(-fabs(u) / sqrt2) * sin((fabs(u) / sqrt2) + 0.25 * pi);
            break;
        case 1:
            result = (u < 0) ? (1.0 / 4.0) * sqrt2 * exp((u * sqrt2) / 2.0) * (sin((-u * sqrt2) / 2.0 + (pi / 4.0)) - cos((-u * sqrt2) / 2.0 + (pi / 4.0))) :
                      -(1.0 / 4.0) * sqrt2 * exp(-(u * sqrt2) / 2.0) * (sin((u * sqrt2) / 2.0 + (pi / 4.0)) - cos((u * sqrt2) / 2.0 + (pi / 4.0)));
            break;
        case 2:
            result = (u < 0) ? -(1.0 / 2.0) * exp((u * sqrt2) / 2.0) * cos((-u * sqrt2) / 2.0 + (pi / 4.0)) :
                      -(1.0 / 2.0) * exp(-(u * sqrt2) / 2.0) * cos((u * sqrt2) / 2.0 + (pi / 4.0));
            break;
        case 3:
            result = (u < 0) ? -(1.0 / 4.0) * sqrt2 * exp((u * sqrt2) / 2.0) * (cos((-u * sqrt2) / 2.0 + (pi / 4.0)) + sin((-u * sqrt2) / 2.0 + (pi / 4.0))) :
                      (1.0 / 4.0) * sqrt2 * exp(-(u * sqrt2) / 2.0) * (cos((u * sqrt2) / 2.0 + (pi / 4.0)) + sin((u * sqrt2) / 2.0 + (pi / 4.0)));
            break;
        case 4:
            result = (u < 0) ? -(1.0 / 2.0) * exp((u * sqrt2) / 2.0) * sin((-u * sqrt2) / 2.0 + (pi / 4.0)) :
                      -(1.0 / 2.0) * exp(-(u * sqrt2) / 2.0) * sin((u * sqrt2) / 2.0 + (pi / 4.0));
            break;
        case 5:
            result = (u < 0) ? -(1.0 / 4.0) * sqrt2 * exp((u * sqrt2) / 2.0) * (sin((-u * sqrt2) / 2.0 + (pi / 4.0)) - cos((-u * sqrt2) / 2.0 + (pi / 4.0))) :
                      (1.0 / 4.0) * sqrt2 * exp(-(u * sqrt2) / 2.0) * (sin((u * sqrt2) / 2.0 + (pi / 4.0)) - cos((u * sqrt2) / 2.0 + (pi / 4.0)));
            break;
        case 6:
            result = (u < 0) ? (1.0 / 2.0) * exp((u * sqrt2) / 2.0) * cos((-u * sqrt2) / 2.0 + (pi / 4.0)) :
                      (1.0 / 2.0) * exp(-(u * sqrt2) / 2.0) * cos((u * sqrt2) / 2.0 + (pi / 4.0));
            break;
        case 7:
            result = (u < 0) ? (1.0 / 4.0) * sqrt2 * exp((u * sqrt2) / 2.0) * (cos((-u * sqrt2) / 2.0 + (pi / 4.0)) + sin((-u * sqrt2) / 2.0 + (pi / 4.0))) :
                      -(1.0 / 4.0) * sqrt2 * exp(-(u * sqrt2) / 2.0) * (cos((u * sqrt2) / 2.0 + (pi / 4.0)) + sin((u * sqrt2) / 2.0 + (pi / 4.0)));
            break;
        default:
            result = 0.0; 
    }
    return result;
}


/* Logistic kernel: K(x) = e^{-x}/(1+e^{-x})^2 = sech^2(x/2)/4,
 * K^(r)(x) = y (1-y) Q_{r+1}(y), y = 1/(1+e^{-x})  (see LOGIS_Q), r <= LOGIS_MAXR.
 * Evaluated at |x| (K^(r)(-x) = (-1)^r K^(r)(x)) so exp() never overflows.
 */
double logistic_kernel(double u, int deriv_order) {
    if (deriv_order < 0 || deriv_order > LOGIS_MAXR) return 0.0;
    double e = exp(-fabs(u));
    double y = 1.0 / (1.0 + e), w = e / (1.0 + e);
    double Q = 0.0;
    for (int k = deriv_order; k >= 0; --k) Q = Q * y + LOGIS_Q.q[deriv_order][k];
    double v = y * w * Q;
    return (u < 0.0 && (deriv_order & 1)) ? -v : v;
}


static inline KernelFuncPtr get_kernel_deriv_ptr(const std::string& s) {
    if (s == "gaussian")     return &gaussian_kernel;
    if (s == "epanechnikov") return &epanechnikov_kernel;
    if (s == "triangular")   return &triangular_kernel;
    if (s == "biweight")     return &biweight_kernel;
    if (s == "uniform")      return &uniform_kernel;
    if (s == "triweight")    return &triweight_kernel;
    if (s == "tricube")      return &tricube_kernel;
    if (s == "cosine")       return &cosine_kernel;
    if (s == "silverman")    return &silverman_kernel;
    if (s == "logistic")     return &logistic_kernel;
    if (pow_select(s.c_str())) return &pow_kernel;
    Rcpp::stop("Unsupported kernel type: " + s);
    return nullptr;
}

/* La fonction principale appelée depuis R
 * kernel_function_deriv
 */
/* Apply a kernel routine elementwise to a vector or matrix (keeps the dim). */
static SEXP kedd_apply(KernelFuncPtr kfn, NumericVector u, int deriv_order, int nthreads) {
    const int nt = kedd_threads(nthreads);
    const R_xlen_t m = u.size();
    NumericVector res(m);
    const double* pu = u.begin();
    double* pr = res.begin();
    #pragma omp parallel for num_threads(nt) schedule(static)
    for (R_xlen_t i = 0; i < m; ++i)
        pr[i] = kfn(pu[i], deriv_order);
    if (u.hasAttribute("dim")) res.attr("dim") = u.attr("dim");
    return res;
}

// [[Rcpp::export]]
SEXP Cpp_kernel_deriv(CharacterVector kernel, NumericVector u, int deriv_order, int nthreads = 0) {
    std::string kernel_str = Rcpp::as<std::string>(kernel[0]);
    return kedd_apply(get_kernel_deriv_ptr(kernel_str), u, deriv_order, nthreads);
}

/* Number of threads a parallel region actually gets for a given nthreads. */
// [[Rcpp::export]]
int Cpp_omp_threads(int nthreads = 0) {
    int used = 1;
    const int nt = kedd_threads(nthreads);
    #pragma omp parallel num_threads(nt)
    {
#ifdef _OPENMP
        if (omp_get_thread_num() == 0) used = omp_get_num_threads();
#endif
    }
    (void) nt;
    return used;
}

/*
 * Convolution of r(th) derivative of Kernel functions K(x,r) * K(x,r)
 */
 
// Convolution of n-ième dérivée du noyau gaussien
// Fonction pour calculer les polynômes G(x,n)

double guidoum_polynomial(int n, double u) {
    if (n == 0) return 1.0;
    if (n == 1) return u * u - 2.0;
    double g0 = 1.0, g1 = u * u - 2.0, gk = 0.0;
    for (int k = 2; k <= n; ++k) {
        gk = (u * u - 8 * k + 6) * g1 - 8.0 * (k - 1) * (2 * k - 3) * g0;
        g0 = g1; g1 = gk;
    }
    return g1;
}


double gaussian_kernel_conv(double u, int deriv_order) {
    double guidoum = guidoum_polynomial(deriv_order, u);
    double gaussianPart = 0.5 * exp(-u * u / 4) / sqrt(M_PI);
    return pow(4.0, -deriv_order) * guidoum * gaussianPart;
}

 
// Convolution of n-ième dérivée du noyau epanechnikov
 
double epanechnikov_kernel_conv(double u, int deriv_order) {
    double fx = 0.0; 
    if(fabs(u) <= 2) {
        switch(deriv_order) {
            case 0:
                fx = (3.0/5.0) - (3.0/160.0) * pow(fabs(u), 5) + (3.0/8.0) * pow(fabs(u), 3) - (3.0/4.0) * pow(u, 2);
                break;
            case 1:
                fx = -(3.0/2.0) - (3.0/8.0) * pow(fabs(u), 3) + (9.0/4.0) * fabs(u);
                break;
            case 2:
                fx = (9.0/2.0) - (9.0/4.0) * fabs(u);
                break;
            default:
                fx = 0.0;
                break;
        }
    }
    return fx;
}
 
// Convolution of n-ième dérivée du noyau uniform

double uniform_kernel_conv(double u, int deriv_order) {
    double fx = 0.0; 
    if(fabs(u) <= 2) {
        switch(deriv_order) {
            case 0:
                fx = 0.5 - 0.25 * fabs(u);
                break;
            default:
                fx = 0.0;
                break;
        }
    }
    return fx;
}

// Convolution of n-ième dérivée du noyau triangular

double triangular_kernel_conv(double u, int deriv_order) {
    double fx = 0.0; 
    if (deriv_order == 0) {
        if (u <= -1 && u > -2) fx = (1.0/6.0) * pow((u + 2), 3);
        else if (u <= 0 && u > -1) fx = (-(1.0/2.0) * pow(u, 3)) - pow(u, 2) + (2.0/3.0);
        else if (u <= 1 && u > 0) fx = ((2.0/3.0) + (1.0/2.0) * pow(u, 3)) - pow(u, 2);
        else if (u <= 2 && u > 1) fx = -(1.0/6.0) * pow((u - 2), 3);
    } else if (deriv_order == 1) {
        if (u <= -1 && u >= -2) fx = (u + 2);
        else if (u <= 0 && u > -1) fx = (-3 * u - 2);
        else if (u <= 1 && u > 0) fx = (3 * u - 2);
        else if (u <= 2 && u > 1) fx = (2 - u);
    } else {
        fx = 0.0;
    }
    return fx;
}

// Convolution of n-ième dérivée du noyau cosine

double cosine_kernel_conv(double u, int deriv_order) {
    double fx = 0.0; 
    if (fabs(u) <= 2) {
        double term1 = pow(-1, deriv_order + 1) * (pow(M_PI, 2 * deriv_order + 2) / pow(2, 2 * deriv_order + 5)) * cos(0.5 * M_PI * fabs(u)) * fabs(u);
        double term2 = pow(-1, deriv_order) * (pow(M_PI, 2 * deriv_order + 2) / pow(2, 2 * deriv_order + 4)) * cos(0.5 * M_PI * fabs(u));
        double term3 = (pow(M_PI, 2 * deriv_order + 1) / pow(2, 2 * deriv_order + 4)) * sin(0.5 * M_PI * fabs(u));
        fx = term1 + term2 + term3;
    }
    return fx;
}

// Convolution of n-ième dérivée du noyau biweight

double biweight_kernel_conv(double u, int deriv_order) {
    double fx = 0.0;
    if (fabs(u) <= 2) {
        switch(deriv_order) {
            case 0:
                fx = -(15.0/32.0) * pow(fabs(u), 5) - (15.0/14.0) * pow(u, 2) - (5.0/3584.0) * pow(fabs(u), 9) +
                     (15.0/448.0) * pow(fabs(u), 7) + (15.0/16.0) * pow(u, 4) + (5.0/7.0);
                break;
            case 1:
                fx = (45.0/32.0) * pow(fabs(u), 5) - (75.0/8.0) * pow(fabs(u), 3) + (45.0/4.0) * pow(u, 2) -
                     (45.0/448.0) * pow(fabs(u), 7) - (15.0/7.0);
                break;
            case 2:
                fx = (45.0/2.0) - (135.0/32.0) * pow(fabs(u), 5) + (225.0/8.0) * pow(fabs(u), 3) -
                     (225.0/4.0) * fabs(u);
                break;
            case 3:
                fx = -(675.0/2.0) - (675.0/8.0) * pow(fabs(u), 3) + (2025.0/4.0) * fabs(u);
                break;
            case 4:
                fx = (2025.0/2.0) - (2025.0/4.0) * fabs(u);
                break;
            default:
                fx = 0.0;
                break;
        }
    }
    return fx;
}

// Convolution of n-ième dérivée du noyau triweight

double triweight_kernel_conv(double u, int deriv_order) {
    double fx = 0.0;
    if (fabs(u) <= 2) {
        switch(deriv_order) {
            case 0:
                fx = (-(35.0/22.0)*pow(u, 2) + (350.0/429.0) + (35.0/11264.0)*pow(fabs(u), 11) - 
                      (35.0/768.0)*pow(fabs(u), 9) + (35.0/64.0)*pow(fabs(u), 7) - 
                      (35.0/32.0)*pow(u, 6) + (35.0/24.0)*pow(u, 4) - 
                      (175.0/1757184.0)*pow(fabs(u), 13));
                break;
            case 1:
                fx = (-(35.0/11.0) + (735.0/32.0)*pow(fabs(u), 5) + (35.0/2.0)*pow(u, 2) - 
                      (175.0/11264.0)*pow(fabs(u), 11) + (175.0/512.0)*pow(fabs(u), 9) - 
                      (105.0/32.0)*pow(fabs(u), 7) - (525.0/16.0)*pow(u, 4));
                break;
            case 2:
                fx = (35.0 - (2205.0/16.0)*pow(fabs(u), 5) + (3675.0/8.0)*pow(fabs(u), 3) - 
                      (1575.0/4.0)*pow(u, 2) - (875.0/512.0)*pow(fabs(u), 9) + 
                      (1575.0/64.0)*pow(fabs(u), 7));
                break;
            case 3:
                fx = ((11025.0/4.0)*fabs(u) + (33075.0/32.0)*pow(fabs(u), 5) - 
                      (11025.0/4.0)*pow(fabs(u), 3) - (7875.0/64.0)*pow(fabs(u), 7) - 
                      (1575.0/2.0));
                break;
            case 4:
                fx = (33075.0 - (165375.0/32.0)*pow(fabs(u), 5) + (165375.0/8.0)*pow(fabs(u), 3) + 
                      (165375.0/4.0)*pow(u, 2) - 99225.0*fabs(u));
                break;
            case 5:
                fx = (-(826875.0/2.0) - (826875.0/8.0)*pow(fabs(u), 3) + (2480625.0/4.0)*fabs(u));
                break;
            case 6:
                fx = ((2480625.0/2.0) - (2480625.0/4.0)*fabs(u));
                break;
            default:
                fx = 0.0;
                break;
        }
    }
    return fx;
}

// Convolution of n-ième dérivée du noyau tricube

double tricube_kernel_conv(double u, int deriv_order) {
    double fx = 0.0;
    if (fabs(u) > 2) {
        return fx;
    }
    switch(deriv_order) {
        case 0:
         if (u <= -1 && u > -2) {
             fx = ((2870.0/351.0)*pow(u, 6) + (21560.0/2187.0)*pow(u, 4) + (7840.0/1683.0)*pow(u, 2) +
              (22400.0/20007.0) + (70.0/81.0)*pow(u, 9) + (15680.0/6561.0)*u + (11305.0/2187.0)*pow(u, 7) +
              (980.0/99.0)*pow(u, 5) + (2800.0/351.0)*pow(u, 3) + (35.0/625482.0)*pow(u, 16) +
              (1085.0/6561.0)*pow(u, 10) + (245.0/303046029.0)*pow(u, 19) + (665.0/312741.0)*pow(u, 13) +
              (245.0/99.0)*pow(u, 8));
         } else if (u <= 0 && u > -1) {
             fx = (-(350.0/117.0)*pow(u, 6) + (980.0/729.0)*pow(u, 4) - (210.0/187.0)*pow(u, 2) + (175.0/247.0) -
              (70.0/81.0)*pow(u, 9) - (2905.0/729.0)*pow(u, 7) - (35.0/625482.0)*pow(u, 16) -
              (1085.0/6561.0)*pow(u, 10) - (245.0/101015343.0)*pow(u, 19) - (1295.0/312741.0)*pow(u, 13) -
              (245.0/99.0)*pow(u, 8));
         } else if (u > 0 && u <= 1) {
             fx = (-(350.0/117.0)*pow(u, 6) + (980.0/729.0)*pow(u, 4) - (210.0/187.0)*pow(u, 2) + (175.0/247.0) +
              (70.0/81.0)*pow(u, 9) + (2905.0/729.0)*pow(u, 7) - (35.0/625482.0)*pow(u, 16) -
              (1085.0/6561.0)*pow(u, 10) + (245.0/101015343.0)*pow(u, 19) + (1295.0/312741.0)*pow(u, 13) -
              (245.0/99.0)*pow(u, 8));
          } else if (u > 1 && u < 2) {
             fx = ((2870.0/351.0)*pow(u, 6) + (21560.0/2187.0)*pow(u, 4) + (7840.0/1683.0)*pow(u, 2) +
              (22400.0/20007.0) - (15680.0/6561.0)*u - (70.0/81.0)*pow(u, 9) + (245.0/99.0)*pow(u, 8) -
              (11305.0/2187.0)*pow(u, 7) - (980.0/99.0)*pow(u, 5) - (2800.0/351.0)*pow(u, 3) -
              (245.0/303046029.0)*pow(u, 19) - (665.0/312741.0)*pow(u, 13) + (35.0/625482.0)*pow(u, 16) +
              (1085.0/6561.0)*pow(u, 10));
        }
            break;
        case 1:
         if (u <= -1 && u > -2) {
             fx = ((86240.0/729.0)*pow(u, 2) + (28700.0/117.0)*pow(u, 4) + (15680.0/1683.0) +
              (560.0/9.0)*pow(u, 7) + (10850.0/729.0)*pow(u, 8) + (1400.0/104247.0)*pow(u, 14) +
              (13720.0/99.0)*pow(u, 6) + (490.0/1772199.0)*pow(u, 17) +
              (2660.0/8019.0)*pow(u, 11) + (158270.0/729.0)*pow(u, 5) +
              (19600.0/99.0)*pow(u, 3) + (5600.0/117.0)*u);
         } else if (u <= 0 && u > -1) {
             fx = ((3920.0/243.0)*pow(u, 2) - (3500.0/39.0)*pow(u, 4) - (420.0/187.0) -
              (560.0/9.0)*pow(u, 7) - (10850.0/729.0)*pow(u, 8) - (1400.0/104247.0)*pow(u, 14) -
              (13720.0/99.0)*pow(u, 6) - (490.0/590733.0)*pow(u, 17) -
              (5180.0/8019.0)*pow(u, 11) - (40670.0/243.0)*pow(u, 5));
         } else if (u > 0 && u <= 1) {
             fx = ((3920.0/243.0)*pow(u, 2) - (3500.0/39.0)*pow(u, 4) - (420.0/187.0) +
              (560.0/9.0)*pow(u, 7) - (10850.0/729.0)*pow(u, 8) - (1400.0/104247.0)*pow(u, 14) -
              (13720.0/99.0)*pow(u, 6) + (490.0/590733.0)*pow(u, 17) +
              (5180.0/8019.0)*pow(u, 11) + (40670.0/243.0)*pow(u, 5));
         } else if (u > 1 && u < 2) {
             fx = ((86240.0/729.0)*pow(u, 2) + (28700.0/117.0)*pow(u, 4) + (15680.0/1683.0) -
              (560.0/9.0)*pow(u, 7) + (10850.0/729.0)*pow(u, 8) - (158270.0/729.0)*pow(u, 5) +
              (13720.0/99.0)*pow(u, 6) - (19600.0/99.0)*pow(u, 3) - (5600.0/117.0)*u +
              (1400.0/104247.0)*pow(u, 14) - (2660.0/8019.0)*pow(u, 11) -
              (490.0/1772199.0)*pow(u, 17));
         }
            break;
        case 2:
         if (u <= -1 && u > -2) {
             fx = ((114800.0/39.0)*pow(u, 2) + (172480.0/729.0) + (3165400.0/729.0)*pow(u, 3) +
              (607600.0/729.0)*pow(u, 6) + (26600.0/729.0)*pow(u, 9) + (7840.0/104247.0)*pow(u, 15) +
              (7840.0/3.0)*pow(u, 5) + (137200.0/33.0)*pow(u, 4) + (39200.0/33.0)*u +
              (19600.0/8019.0)*pow(u, 12));
         } else if (u <= 0 && u > -1) {
             fx = (-(14000.0/13.0)*pow(u, 2) + (7840.0/243.0) - (813400.0/243.0)*pow(u, 3) -
              (607600.0/729.0)*pow(u, 6) - (51800.0/729.0)*pow(u, 9) - (7840.0/34749.0)*pow(u, 15) -
              (7840.0/3.0)*pow(u, 5) - (137200.0/33.0)*pow(u, 4) - (19600.0/8019.0)*pow(u, 12));
         } else if (u > 0 && u <= 1) {
             fx = (-(14000.0/13.0)*pow(u, 2) + (7840.0/243.0) + (813400.0/243.0)*pow(u, 3) +
              (51800.0/729.0)*pow(u, 9) - (607600.0/729.0)*pow(u, 6) + (7840.0/34749.0)*pow(u, 15) +
              (7840.0/3.0)*pow(u, 5) - (137200.0/33.0)*pow(u, 4) - (19600.0/8019.0)*pow(u, 12));
         } else if (u > 1 && u < 2) {
             fx = ((114800.0/39.0)*pow(u, 2) + (172480.0/729.0) - (3165400.0/729.0)*pow(u, 3) +
              (19600.0/8019.0)*pow(u, 12) - (26600.0/729.0)*pow(u, 9) + (607600.0/729.0)*pow(u, 6) -
              (7840.0/3.0)*pow(u, 5) - (39200.0/33.0)*u + (137200.0/33.0)*pow(u, 4) - 
              (7840.0/104247.0)*pow(u, 15));
         }
            break;
        case 3:
         if (u <= -1 && u > -2) {
             fx = ((229600.0/39.0) + (212800.0/81.0)*pow(u, 7) + (78400.0/243.0)*pow(u, 10) +
              (548800.0/34749.0)*pow(u, 13) + (6076000.0/243.0)*pow(u, 4) + 
              (6330800.0/243.0)*u + (156800.0/3.0)*pow(u, 3) + (548800.0/11.0)*pow(u, 2));
         } else if (u <= 0 && u > -1) {
             fx = (-(28000.0/13.0) - (414400.0/81.0)*pow(u, 7) - (548800.0/11583.0)*pow(u, 13) -
              (6076000.0/243.0)*pow(u, 4) - (1626800.0/81.0)*u - (78400.0/243.0)*pow(u, 10) -
              (156800.0/3.0)*pow(u, 3) - (548800.0/11.0)*pow(u, 2));
         } else if (u > 0 && u <= 1) {
             fx = (-(28000.0/13.0) + (414400.0/81.0)*pow(u, 7) + (548800.0/11583.0)*pow(u, 13) +
              (1626800.0/81.0)*u - (6076000.0/243.0)*pow(u, 4) - (78400.0/243.0)*pow(u, 10) +
              (156800.0/3.0)*pow(u, 3) - (548800.0/11.0)*pow(u, 2));
         } else if (u > 1 && u < 2) {
             fx = ((229600.0/39.0) + (78400.0/243.0)*pow(u, 10) - (6330800.0/243.0)*u +
              (6076000.0/243.0)*pow(u, 4) - (548800.0/34749.0)*pow(u, 13) -
              (212800.0/81.0)*pow(u, 7) - (156800.0/3.0)*pow(u, 3) + (548800.0/11.0)*pow(u, 2));
         }
            break;
        case 4:
         if (u <= -1 && u > -2) {
             fx = (-(589568000.0/81.0)*pow(u, 2) - (29478400.0/33.0) - (16777600.0/27.0)*pow(u, 5) -
              (11603200.0/3.0)*u + (2195200.0/891.0)*pow(u, 11) + (784000.0/27.0)*pow(u, 8) -
              (21952000.0/3.0)*pow(u, 3) - (10976000.0/3.0)*pow(u, 4));
         } else if (u <= 0 && u > -1) {
             fx = ((551936000.0/81.0)*pow(u, 2) + (4076800.0/11.0) + (2038400.0/3.0)*pow(u, 5) +
              2822400.0*u - (2195200.0/297.0)*pow(u, 11) - (784000.0/27.0)*pow(u, 8) +
              (21952000.0/3.0)*pow(u, 3) + (10976000.0/3.0)*pow(u, 4));
         } else if (u > 0 && u <= 1) {
             fx = ((551936000.0/81.0)*pow(u, 2) + (4076800.0/11.0) - (2038400.0/3.0)*pow(u, 5) -
              2822400.0*u - (784000.0/27.0)*pow(u, 8) + (2195200.0/297.0)*pow(u, 11) -
              (21952000.0/3.0)*pow(u, 3) + (10976000.0/3.0)*pow(u, 4));
         } else if (u > 1 && u < 2) {
             fx = (-(589568000.0/81.0)*pow(u, 2) - (29478400.0/33.0) + (16777600.0/27.0)*pow(u, 5) +
              (11603200.0/3.0)*u + (784000.0/27.0)*pow(u, 8) - (2195200.0/891.0)*pow(u, 11) -
              (10976000.0/3.0)*pow(u, 4) + (21952000.0/3.0)*pow(u, 3));
         }
            break;
        case 5:
         if (u <= -1 && u > -2) {
             fx = ((2885120000.0/81.0) + (4406080000.0/27.0)*pow(u, 3) + 219520000.0*pow(u, 2) +
              (43904000.0/27.0)*pow(u, 6) + 43904000.0*pow(u, 4) + 137984000.0*u +
              (21952000.0/81.0)*pow(u, 9));
         } else if (u <= 0 && u > -1) {
             fx = (-(1944320000.0/81.0) - (486080000.0/3.0)*pow(u, 3) - 219520000.0*pow(u, 2) -
              (43904000.0/27.0)*pow(u, 6) - 43904000.0*pow(u, 4) - 125440000.0*u -
              (21952000.0/27.0)*pow(u, 9));
         } else if (u > 0 && u <= 1) {
             fx = (-(1944320000.0/81.0) + (486080000.0/3.0)*pow(u, 3) - 219520000.0*pow(u, 2) -
              (43904000.0/27.0)*pow(u, 6) - 43904000.0*pow(u, 4) + 125440000.0*u +
              (21952000.0/27.0)*pow(u, 9));
         } else if (u > 1 && u < 2) {
             fx = ((2885120000.0/81.0) + 43904000.0*pow(u, 4) + 219520000.0*pow(u, 2) - 137984000.0*u -
              (4406080000.0/27.0)*pow(u, 3) - (21952000.0/81.0)*pow(u, 9) +
              (43904000.0/27.0)*pow(u, 6));
         }
            break;
        case 6:
         if (u <= -1 && u > -2) {
             fx = ((-2320640000.0/3.0) - (3512320000.0/3.0)*pow(u, 3) - (22798720000.0/9.0)*u +
              (175616000.0/9.0)*pow(u, 7) - 2985472000.0*pow(u, 2) + (439040000.0/9.0)*pow(u, 4));
         } else if (u <= 0 && u > -1) {
             fx = (689920000.0 + (3512320000.0/3.0)*pow(u, 3) + 2540160000.0*u - 
              (175616000.0/3.0)*pow(u, 7) + 2985472000.0*pow(u, 2) - (439040000.0/9.0)*pow(u, 4));
         } else if (u > 0 && u <= 1) {
             fx = (689920000.0 - (3512320000.0/3.0)*pow(u, 3) - 2540160000.0*u + 
              (175616000.0/3.0)*pow(u, 7) + 2985472000.0*pow(u, 2) - (439040000.0/9.0)*pow(u, 4));
         } else if (u > 1 && u < 2) {
             fx = ((-2320640000.0/3.0) + (3512320000.0/3.0)*pow(u, 3) + (22798720000.0/9.0)*u - 
              (175616000.0/9.0)*pow(u, 7) - 2985472000.0*pow(u, 2) + (439040000.0/9.0)*pow(u, 4));
         }
            break;
        case 7:
         if (u <= -1 && u > -2) {
             fx = (9834496000.0 + 24586240000.0*u + (2458624000.0/3.0)*pow(u, 5) +
              (49172480000.0/3.0)*pow(u, 2));
         } else if (u <= 0 && u > -1) {
             fx = (-9834496000.0 - 24586240000.0*u - 2458624000.0*pow(u, 5) -
              (49172480000.0/3.0)*pow(u, 2));
         } else if (u > 0 && u <= 1) {
             fx = (-9834496000.0 + 24586240000.0*u + 2458624000.0*pow(u, 5) -
              (49172480000.0/3.0)*pow(u, 2));
         } else if (u > 1 && u < 2) {
             fx = (9834496000.0 - 24586240000.0*u - (2458624000.0/3.0)*pow(u, 5) +
              (49172480000.0/3.0)*pow(u, 2));
         }
            break;
        case 8:
         if (u <= -1 && u > -2) {
             fx = (-98344960000.0*u + (49172480000.0/3.0)*pow(u, 3) - (196689920000.0/3.0));
         } else if (u <= 0 && u > -1) {
             fx = (98344960000.0*u - 49172480000.0*pow(u, 3) + (196689920000.0/3.0));
         } else if (u > 0 && u <= 1) {
             fx = (49172480000.0*pow(u, 3) - 98344960000.0*u + (196689920000.0/3.0));
         } else if (u > 1 && u < 2) {
             fx = (98344960000.0*u - (49172480000.0/3.0)*pow(u, 3) - (196689920000.0/3.0));
         }
            break;
        case 9:
         if (u <= -1 && u > -2) {
             fx = 98344960000.0 * u + 196689920000.0;
         } else if (u <= 0 && u > -1) {
             fx = -295034880000.0 * u - 196689920000.0;
         } else if (u > 0 && u <= 1) {
             fx = 295034880000.0 * u - 196689920000.0;
         } else if (u > 1 && u < 2) {
             fx = -98344960000.0 * u + 196689920000.0;
         }
            break;
        default:
            fx = 0.0;
            break;
    }
    return fx;
}

// Convolution of n-ième dérivée du noyau silverman

double silverman_kernel_conv(double u, int deriv_order) {
    double fx = 0.0;
    deriv_order = deriv_order % 4; 
    double sqrt2 = sqrt(2.0);
    if (deriv_order == 0) {
        if (u < 0) {
            fx = (1.0 / 16.0) * exp(0.5 * u * sqrt2) * (3.0 * sqrt2 * cos(0.5 * u * sqrt2) - 
                  3.0 * sqrt2 * sin(0.5 * u * sqrt2) + 2.0 * sin(0.5 * u * sqrt2) * u);
        } else {
            fx = (1.0 / 16.0) * exp(-0.5 * u * sqrt2) * (3.0 * sqrt2 * cos(0.5 * u * sqrt2) + 
                  3.0 * sqrt2 * sin(0.5 * u * sqrt2) + 2.0 * sin(0.5 * u * sqrt2) * u);
        }
    } else if (deriv_order == 1) {
        if (u < 0) {
            fx = -(1.0 / 16.0) * exp(0.5 * u * sqrt2) * (sqrt2 * cos(0.5 * u * sqrt2) + 
                  sqrt2 * sin(0.5 * u * sqrt2) - 2.0 * cos(0.5 * u * sqrt2) * u);
        } else {
            fx = -(1.0 / 16.0) * exp(-0.5 * u * sqrt2) * (sqrt2 * cos(0.5 * u * sqrt2) + 
                  2.0 * cos(0.5 * u * sqrt2) * u - sqrt2 * sin(0.5 * u * sqrt2));
        }
    } else if (deriv_order == 2) {
        if (u < 0) {
            fx = (1.0 / 16.0) * exp(0.5 * u * sqrt2) * (sqrt2 * cos(0.5 * u * sqrt2) - 
                  sqrt2 * sin(0.5 * u * sqrt2) - 2.0 * sin(0.5 * u * sqrt2) * u);
        } else {
            fx = (1.0 / 16.0) * exp(-0.5 * u * sqrt2) * (sqrt2 * cos(0.5 * u * sqrt2) + 
                  sqrt2 * sin(0.5 * u * sqrt2) - 2.0 * sin(0.5 * u * sqrt2) * u);
        }
    } else if (deriv_order == 3) {
        if (u < 0) {
            fx = -(1.0 / 16.0) * exp(0.5 * u * sqrt2) * (3.0 * sqrt2 * cos(0.5 * u * sqrt2) + 
                  3.0 * sqrt2 * sin(0.5 * u * sqrt2) + 2.0 * cos(0.5 * u * sqrt2) * u);
        } else {
            fx = -(1.0 / 16.0) * exp(-0.5 * u * sqrt2) * (3.0 * sqrt2 * cos(0.5 * u * sqrt2) - 
                  3.0 * sqrt2 * sin(0.5 * u * sqrt2) - 2.0 * cos(0.5 * u * sqrt2) * u);
        }
    }
    return fx;
}


/* Logistic convolution (K^(r) * K^(r))(u) (no closed form), by quadrature:
 * composite 16-pt GL on 5 panels centred at x = u/2, where the integrand peaks;
 * panels cover u/2 +- 14, beyond which the integrand is below 1e-12.
 */
static double logistic_conv_direct(double u, int deriv_order) {
    static const double off[6] = { -14.0, -4.0, -1.5, 1.5, 4.0, 14.0 };
    double c0 = 0.5 * u, total = 0.0;
    for (int p = 0; p < 5; ++p) {
        double half = 0.5 * (off[p + 1] - off[p]), mid = c0 + off[p] + half, sum = 0.0;
        for (int i = 0; i < 8; ++i) {
            double x1 = mid + half * GL16_N[i], x2 = mid - half * GL16_N[i];
            sum += GL16_W[i] * (logistic_kernel(x1, deriv_order) * logistic_kernel(u - x1, deriv_order) +
                                logistic_kernel(x2, deriv_order) * logistic_kernel(u - x2, deriv_order));
        }
        total += half * sum;
    }
    return total;
}

/* The convolution is even and below 1e-15 for |u| >= 40: it is tabulated once per r
 * on [0, 40] (step 0.0025) and read with 4-point Lagrange (cubic) interpolation,
 * instead of 80 quadrature nodes for every pair of observations. */
#define LOGIS_TAB_U 40.0
#define LOGIS_TAB_N 16001
static std::vector<double> g_logis_tab[LOGIS_MAXR + 1];
static std::atomic<bool>    g_logis_ready[LOGIS_MAXR + 1];

double logistic_kernel_conv(double u, int deriv_order) {
    if (deriv_order < 0 || deriv_order > LOGIS_MAXR) return 0.0;
    double au = fabs(u);
    if (au >= LOGIS_TAB_U) return 0.0;
    const double step = LOGIS_TAB_U / (LOGIS_TAB_N - 1);
    if (!g_logis_ready[deriv_order].load(std::memory_order_acquire)) {
        #pragma omp critical(kedd_logis_conv_table)
        {
            if (!g_logis_ready[deriv_order].load(std::memory_order_relaxed)) {
                std::vector<double>& tab = g_logis_tab[deriv_order];
                tab.assign(LOGIS_TAB_N + 2, 0.0);      /* two extra nodes for the stencil */
                for (int i = 0; i < LOGIS_TAB_N + 2; ++i)
                    tab[i] = logistic_conv_direct(i * step, deriv_order);
                g_logis_ready[deriv_order].store(true, std::memory_order_release);
            }
        }
    }
    const std::vector<double>& tab = g_logis_tab[deriv_order];
    double pos = au / step;
    int i = (int) pos;
    double f = pos - i;
    double ym1 = (i == 0) ? tab[1] : tab[i - 1];   /* even function: value at -step */
    double y0 = tab[i], y1 = tab[i + 1], y2 = tab[i + 2];
    return ym1 * (-f * (f - 1.0) * (f - 2.0) / 6.0)
         + y0  * ((f + 1.0) * (f - 1.0) * (f - 2.0) / 2.0)
         + y1  * (-(f + 1.0) * f * (f - 2.0) / 2.0)
         + y2  * ((f + 1.0) * f * (f - 1.0) / 6.0);
}


static inline KernelFuncPtr get_kernel_conv_ptr(const std::string& s) {
    if (s == "gaussian")     return &gaussian_kernel_conv;
    if (s == "epanechnikov") return &epanechnikov_kernel_conv;
    if (s == "triangular")   return &triangular_kernel_conv;
    if (s == "biweight")     return &biweight_kernel_conv;
    if (s == "uniform")      return &uniform_kernel_conv;
    if (s == "triweight")    return &triweight_kernel_conv;
    if (s == "tricube")      return &tricube_kernel_conv;
    if (s == "cosine")       return &cosine_kernel_conv;
    if (s == "silverman")    return &silverman_kernel_conv;
    if (s == "logistic")     return &logistic_kernel_conv;
    if (pow_select(s.c_str())) return &pow_kernel_conv;
    Rcpp::stop("Unsupported kernel type: " + s);
    return nullptr;
}

/* La fonction principale appelée depuis R
 * kernel_function_conv
 */

// [[Rcpp::export]]
SEXP Cpp_kernel_conv(CharacterVector kernel, NumericVector u, int deriv_order, int nthreads = 0) {
    std::string kernel_str = Rcpp::as<std::string>(kernel[0]);
    return kedd_apply(get_kernel_conv_ptr(kernel_str), u, deriv_order, nthreads);
}


/*
// UCV
*/

//[[Rcpp::export]]
NumericVector Cpp_h_ucv(double R_h, CharacterVector R_kernel, int R_r, NumericVector R_x, int nthreads) {
    int n = R_x.size();
    double h = R_h;
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    KernelFuncPtr kderiv = get_kernel_deriv_ptr(kernel_str);
    KernelFuncPtr kconv  = get_kernel_conv_ptr(kernel_str);
    const int nt = kedd_threads(nthreads);
    const int o1 = 2 * R_r;
    /* both kernels have parity (-1)^{2r} = +1 in u: sum over i < j and double */
    double sumD1 = 0.0, sumD2 = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumD1,sumD2) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) {
            double u = (R_x[j] - R_x[i]) / h;
            sumD1 += 2.0 * kderiv(u, o1);
            sumD2 += 2.0 * kconv(u, R_r);
        }
    double meanD1 = sumD1 / (n - 1);
    double meanD2 = sumD2 / (n - 1);
    double RKr = A3_kMr(kernel_str.c_str(), R_r);
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double denom = n * std::pow(h, 2 * R_r + 1);
    double result_val = RKr / denom + (sign / denom) * (meanD2 - 2.0 * meanD1);
    return NumericVector::create(result_val);
}




/*
// BCV 1  : MISE approx with conv K^{(r+2)}
*/

// [[Rcpp::export]]
NumericVector Cpp_h_bcv_1(double R_h, CharacterVector R_kernel, int R_r, NumericVector R_x, int nthreads) {
    int n = R_x.size();
    double h = R_h;
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    KernelFuncPtr kconv = get_kernel_conv_ptr(kernel_str);
    int conv_order = R_r + 2;
    const int nt = kedd_threads(nthreads);
    /* K_conv is symmetric: K*(u) = K*(-u), exploit to halve kernel evals */
    double sumD1 = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumD1) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j)
            sumD1 += 2.0 * kconv((R_x[j] - R_x[i]) / h, conv_order);
    double meanD1 = sumD1 / (n - 1);
    double RKr = A3_kMr(kernel_str.c_str(), R_r), mu2 = A2_kM(kernel_str.c_str());
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double denom = n * std::pow(h, 2 * R_r + 1);
    double result_val = RKr / denom + 0.25 * mu2 * mu2 * sign / denom * meanD1;
    return NumericVector::create(result_val);
}


/*
// BCV 2  : MISE approx with deriv K^{(2r+4)}
*/

// [[Rcpp::export]]
NumericVector Cpp_h_bcv_2(double R_h, CharacterVector R_kernel, int R_r, NumericVector R_x, int nthreads) {
    int n = R_x.size();
    double h = R_h;
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    KernelFuncPtr kderiv = get_kernel_deriv_ptr(kernel_str);
    int deriv_order = 2 * R_r + 4;  /* always even → K^{2r+4}(-u) = K^{2r+4}(u) symmetric */
    const int nt = kedd_threads(nthreads);
    double sumD1 = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumD1) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j)
            sumD1 += 2.0 * kderiv((R_x[j] - R_x[i]) / h, deriv_order);
    double meanD1 = sumD1 / (n - 1);
    double RKr = A3_kMr(kernel_str.c_str(), R_r), mu2 = A2_kM(kernel_str.c_str());
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double denom = n * std::pow(h, 2 * R_r + 1);
    double result_val = RKr / denom + 0.25 * mu2 * mu2 * sign / denom * meanD1;
    return NumericVector::create(result_val);
}


/*
// CCV  : complete cross-validation (4 kernel evaluations)
*/

// [[Rcpp::export]]
NumericVector Cpp_h_ccv(double R_h, CharacterVector R_kernel, int R_r, NumericVector R_x, int nthreads) {
    int n = R_x.size();
    double h = R_h;
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    KernelFuncPtr kconv  = get_kernel_conv_ptr(kernel_str);
    KernelFuncPtr kderiv = get_kernel_deriv_ptr(kernel_str);
    int o1 = R_r, o2 = 2*R_r, o3 = 2*(R_r+1), o4 = 2*(R_r+2);
    const int nt = kedd_threads(nthreads);
    double sumD1 = 0.0, sumD2 = 0.0, sumD3 = 0.0, sumD4 = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumD1,sumD2,sumD3,sumD4) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) {
            double u = (R_x[j] - R_x[i]) / h;
            sumD1 += 2.0 * kconv(u, o1);
            sumD2 += 2.0 * kderiv(u, o2);
            sumD3 += 2.0 * kderiv(u, o3);
            sumD4 += 2.0 * kderiv(u, o4);
        }
    double RKr = A3_kMr(kernel_str.c_str(), R_r), mu2 = A2_kM(kernel_str.c_str()), delta = A5_kM(kernel_str.c_str());
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double sign1 = (((R_r+1) % 2) == 0) ? 1.0 : -1.0;
    double sign2 = (((R_r+2) % 2) == 0) ? 1.0 : -1.0;
    double denom = n * std::pow(h, 2*R_r+1);
    double meanD1 = sumD1 / (n-1), meanD2 = sumD2 / (n-1);
    double meanD3 = (sign1 / (n * std::pow(h, 2*R_r+3))) * (sumD3 / (n-1));
    double meanD4 = (sign2 / (n * std::pow(h, 2*R_r+5))) * (sumD4 / (n-1));
    double result_val = (1.0 / denom) * (RKr + sign * (meanD1 - meanD2))
                      + 0.5 * mu2 * h * h * meanD3
                      + (h*h*h*h / 24.0) * (6.0 * mu2*mu2 - delta) * meanD4;
    return NumericVector::create(result_val);
}


/*
// MCV  : modified cross-validation
*/

// [[Rcpp::export]]
NumericVector Cpp_h_mcv(double R_h, CharacterVector R_kernel, int R_r, NumericVector R_x, int nthreads) {
    int n = R_x.size();
    double h = R_h;
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    KernelFuncPtr kconv  = get_kernel_conv_ptr(kernel_str);
    KernelFuncPtr kderiv = get_kernel_deriv_ptr(kernel_str);
    int o2 = 2*R_r, o3 = 2*R_r+2;
    const int nt = kedd_threads(nthreads);
    double sumD1 = 0.0, sumD2 = 0.0, sumD3 = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumD1,sumD2,sumD3) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) {
            double u = (R_x[j] - R_x[i]) / h;
            sumD1 += 2.0 * kconv(u, R_r);
            sumD2 += 2.0 * kderiv(u, o2);
            sumD3 += 2.0 * kderiv(u, o3);
        }
    double meanD1 = sumD1 / (n-1), meanD2 = sumD2 / (n-1), meanD3 = sumD3 / (n-1);
    double RKr = A3_kMr(kernel_str.c_str(), R_r), mu2 = A2_kM(kernel_str.c_str());
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double denom = n * std::pow(h, 2*R_r+1);
    double result_val = RKr / denom
                      + (sign / denom) * (meanD1 - meanD2 - 0.5 * mu2 * meanD3);
    return NumericVector::create(result_val);
}


/*
// TCV  : trimmed cross-validation
*/

// [[Rcpp::export]]
NumericVector Cpp_h_tcv(double R_h, CharacterVector R_kernel, int R_r, NumericVector R_x, int nthreads) {
    int n = R_x.size();
    double h = R_h;
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    KernelFuncPtr kconv  = get_kernel_conv_ptr(kernel_str);
    KernelFuncPtr kderiv = get_kernel_deriv_ptr(kernel_str);
    double denom = n * std::pow(h, 2*R_r+1);
    double threshold = 1.0 / denom;
    int o2 = 2*R_r;
    const int nt = kedd_threads(nthreads);
    double sumD1 = 0.0, sumD2 = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumD1,sumD2) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) {
            double u = (R_x[j] - R_x[i]) / h;
            sumD1 += 2.0 * kconv(u, R_r);
            if (std::fabs(u) > threshold)
                sumD2 += 2.0 * kderiv(u, o2);
        }
    double meanD1 = sumD1 / (n-1), meanD2 = sumD2 / (n-1);
    double RKr = A3_kMr(kernel_str.c_str(), R_r);
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double result_val = RKr / denom + (sign / denom) * (meanD1 - 2.0 * meanD2);
    return NumericVector::create(result_val);
}


/*
// SCV  : Smoothed Cross-Validation (Hall, Marron & Park 1992)
//        Analytical formula for Gaussian kernel and pilot, any derivative order r:
//        SCV(h) = R(K^(r))/(n h^{2r+1})
//               + (-1)^r / (n(n-1)) sum_{i!=j} [phi^(2r)_s1 - 2 phi^(2r)_s2 + phi^(2r)_s3](Xj - Xi),
//        phi^(2r)_s(d) = phi^(2r)(d/s) / s^{2r+1}.  Pilot bandwidth g supplied by caller.
*/

// [[Rcpp::export]]
NumericVector Cpp_h_scv(double R_h, double R_g,
                         CharacterVector R_kernel, int R_r,
                         NumericVector R_x, int nthreads) {
    std::string kernel_str = Rcpp::as<std::string>(R_kernel[0]);
    if (kernel_str != "gaussian")
        Rcpp::stop("h.scv currently supports only kernel = \"gaussian\".");
    int n = R_x.size();
    double h = R_h, g = R_g;
    /* convolution scales: T1 ~ N(0, 2(h²+g²)), T2 ~ N(0, h²+2g²), T3 ~ N(0, 2g²) */
    double s1 = std::sqrt(2.0 * (h*h + g*g));
    double s2 = std::sqrt(h*h + 2.0*g*g);
    double s3 = g * M_SQRT2;
    int    q  = 2 * R_r;
    double c1 = 1.0 / std::pow(s1, q + 1), c2 = 1.0 / std::pow(s2, q + 1), c3 = 1.0 / std::pow(s3, q + 1);
    const int nt = kedd_threads(nthreads);
    double sumB = 0.0;
    #pragma omp parallel for num_threads(nt) reduction(+:sumB) schedule(static)
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) {
            double d = R_x[j] - R_x[i];   /* phi^(2r) is even: d and -d give the same value */
            sumB += c1 * gaussian_kernel(d / s1, q)
                  - 2.0 * c2 * gaussian_kernel(d / s2, q)
                  + c3 * gaussian_kernel(d / s3, q);
        }
    double RKr   = A3_kMr(kernel_str.c_str(), R_r);
    double denom = n * std::pow(h, 2*R_r + 1);
    double sign  = ((R_r % 2) == 0) ? 1.0 : -1.0;
    double B_hat = sign * (2.0 / ((double)n * (n - 1.0))) * sumB;
    return NumericVector::create(RKr / denom + B_hat);
}


/*
// Cpp_plot_cv : evaluate CV objective function on a grid/matrix of h values
*/

// [[Rcpp::export]]
SEXP Cpp_plot_cv(CharacterVector method, SEXP h_sexp, CharacterVector kernel, int r, NumericVector x,
                 int nthreads = 0) {
    std::string method_str = Rcpp::as<std::string>(method[0]);

    /* SCV: compute pilot g from data, then evaluate on grid */
    if (method_str == "SCV") {
        int n = x.size();
        double mean_x = 0.0;
        for (int i = 0; i < n; ++i) mean_x += x[i];
        mean_x /= n;
        double var_x = 0.0;
        for (int i = 0; i < n; ++i) { double d = x[i] - mean_x; var_x += d*d; }
        double g = std::sqrt(var_x / (n - 1)) * 0.9266 * std::pow((double)n, -2.0/13.0);
        if (Rf_isMatrix(h_sexp)) {
            NumericMatrix h_mat(h_sexp);
            int nrows = h_mat.nrow(), ncols = h_mat.ncol();
            NumericMatrix res(nrows, ncols);
            for (int i = 0; i < nrows; ++i)
                for (int j = 0; j < ncols; ++j)
                    res(i,j) = Cpp_h_scv(h_mat(i,j), g, kernel, r, x, nthreads)[0];
            return wrap(res);
        } else {
            NumericVector h_vec(h_sexp);
            int nh = h_vec.size();
            NumericVector res(nh);
            for (int i = 0; i < nh; ++i)
                res[i] = Cpp_h_scv(h_vec[i], g, kernel, r, x, nthreads)[0];
            return wrap(res);
        }
    }

    typedef NumericVector (*BandwidthFuncPtr)(double, CharacterVector, int, NumericVector, int);
    BandwidthFuncPtr method_func = NULL;
    if      (method_str == "UCV")  method_func = &Cpp_h_ucv;
    else if (method_str == "BCV1") method_func = &Cpp_h_bcv_1;
    else if (method_str == "BCV2") method_func = &Cpp_h_bcv_2;
    else if (method_str == "CCV")  method_func = &Cpp_h_ccv;
    else if (method_str == "MCV")  method_func = &Cpp_h_mcv;
    else if (method_str == "TCV")  method_func = &Cpp_h_tcv;
    else stop("Method not supported.");
    if (Rf_isMatrix(h_sexp)) {
        NumericMatrix h_mat(h_sexp);
        int nrows = h_mat.nrow(), ncols = h_mat.ncol();
        NumericMatrix res(nrows, ncols);
        for (int i = 0; i < nrows; ++i)
            for (int j = 0; j < ncols; ++j)
                res(i, j) = (*method_func)(h_mat(i,j), kernel, r, x, nthreads)[0];
        return wrap(res);
    } else {
        NumericVector h_vec(h_sexp);
        int nh = h_vec.size();
        NumericVector res(nh);
        for (int i = 0; i < nh; ++i)
            res[i] = (*method_func)(h_vec[i], kernel, r, x, nthreads)[0];
        return wrap(res);
    }
}


/*
// Cpp_conv2conv : double convolution K^{(r1)} * K^{(r2)} via numerical integration
// Step d=0.0002 over [-10,10] (100001 points) — sufficient for all supported kernels
*/

// [[Rcpp::export]]
SEXP Cpp_conv2conv(SEXP t, int deriv_order1, int deriv_order2,
                   CharacterVector kernel1, CharacterVector kernel2, int nthreads) {
    std::string ker2_str = Rcpp::as<std::string>(kernel2[0]);
    double d = 0.0002;
    int m = 100001;
    const int nt = kedd_threads(nthreads);
    NumericVector x(m);
    #pragma omp parallel for num_threads(nt) schedule(guided)
    for (int i = 0; i < m; ++i) x[i] = -10.0 + i * d;
    NumericVector K1 = Cpp_kernel_conv(kernel1, x, deriv_order1, nthreads);
    KernelFuncPtr kfn2 = get_kernel_conv_ptr(ker2_str);
    NumericVector K2(m);   /* pre-allocated once, reused for each t value */
    auto eval_K2 = [&](double tv) {
        #pragma omp parallel for num_threads(nt) schedule(static)
        for (int k = 0; k < m; ++k) K2[k] = kfn2(tv - x[k], deriv_order2);
        double s = 0.0;
        for (int k = 0; k < m; ++k) s += K1[k] * K2[k];
        return s * d;
    };
    if (Rf_isMatrix(t)) {
        NumericMatrix t_mat(t);
        int nr = t_mat.nrow(), nc = t_mat.ncol();
        NumericMatrix z(nr, nc);
        for (int i = 0; i < nr; ++i)
            for (int j = 0; j < nc; ++j)
                z(i, j) = eval_K2(t_mat(i, j));
        return wrap(z);
    } else if (Rf_isNumeric(t)) {
        NumericVector t_vec(t);
        int len = t_vec.size();
        NumericVector z(len);
        for (int i = 0; i < len; ++i) z[i] = eval_K2(t_vec[i]);
        return wrap(z);
    }
    return R_NilValue;
}
