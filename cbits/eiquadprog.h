
#ifndef _HS_EIQUADPROG_H_
#define _HS_EIQUADPROG_H_

#ifdef __cplusplus
extern "C" {
#endif

char hs_is_solution (double);
double hs_solve_quadprog(double* G, double* g0, double* CE, double* ce0,  double* CI, double* ci0, double* x, int n, int p, int m);

#ifdef __cplusplus
}
#endif

#endif // _HS_EIQUADPROG_H_
