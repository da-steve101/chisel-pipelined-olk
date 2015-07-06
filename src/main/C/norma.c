#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define SEC_TO_NS (1000000000)

float * computeKernelVec(int features, int dictSize, float gamma, float * x, float ** dict, float * result) {
  int i, j;
  for ( i = 0; i < dictSize; i++) {
    float sum = 0;
    for ( j = 0; j < features; j++) {
      float tmp = x[j] - dict[i][j];
      sum += tmp*tmp;
    }
    sum *= -gamma;
    result[i] = expf(sum);
  }
  return result;
}

float getFt(int features, int dictSize, float gamma, float * x,
	    float ** dict, float * weights, float * tmp) {
  int i;
  computeKernelVec(features, dictSize, gamma, x, dict, tmp);
  float sum = 0;
  for ( i = 0; i < dictSize; i++)
    sum += weights[i]*tmp[i];
  return sum;
}

float * forgetWeights(int dictSize, float forget, float * weights){
  int i;
  for ( i = 0; i < dictSize; i++ )
    weights[i] *= forget;
  return weights;
}

int addToDict(int dictSize, int features, float * weights,
	      float ** dict, float * x, float newWeight, int idx){
  weights[idx] = newWeight;
  int i;
  for (i = 0; i < features; i++)
    dict[idx][i] = x[i];
  return (idx + 1) % dictSize;
}

int main(int argv, char ** args) {
  float gamma = 0.5;
  float forget = 0.99;
  int dictSize = 200;
  int features = 8;
  float nu = 0.5;
  float eta = 1 - forget;
  float rho_add = -eta*(1 - nu);
  float rho_notadd = eta*nu;
  float ** dict = (float**)malloc(sizeof(float*)*dictSize);
  int i, j;
  for ( i = 0; i < dictSize; i++ )
    dict[i] = (float*)malloc(sizeof(float)*features);
  float * tmp = (float*)malloc(sizeof(float)*dictSize);
  float * weights = (float*)malloc(sizeof(float)*dictSize);
  for ( i = 0; i < dictSize; i++ )
    weights[i] = 0;
  float * x = (float*)malloc(sizeof(float)*features);

  int noEx = 100000;

  float ** examples = (float**)malloc(sizeof(float*)*noEx);
  for ( i = 0; i < noEx; i++) {
    examples[i] = (float*)malloc(sizeof(float)*features);
    for ( j = 0; j < features; j++)
      examples[i][j] = (float)rand()/(float)(RAND_MAX);
  }

  // internal state
  int idx = 0;
  float rho = 0;
  float ft = 0;
  
  struct timespec * start = (struct timespec *)malloc(sizeof(struct timespec));
  struct timespec * stop = (struct timespec *)malloc(sizeof(struct timespec));
  
  // Start the timer
  clock_gettime(CLOCK_REALTIME, start);
  
  for ( i = 0; i < noEx; i++ ) {
    ft = getFt(features, dictSize, gamma, examples[i], dict, weights, tmp);
    // compute novelty update as its the fastest
    forgetWeights(dictSize, forget, weights);
    if (ft < rho) {
      rho = rho + rho_add;
      idx = addToDict(dictSize, features, weights, dict, x, eta, idx);
    } else
      rho = rho + rho_notadd;
  }

  // Stop the timer
  clock_gettime(CLOCK_REALTIME, stop);
  
  // print at end again so not optimized out
  printf("ft = %f\n", ft);
  long int totalTime = (stop->tv_sec*SEC_TO_NS + stop->tv_nsec) - (start->tv_sec*SEC_TO_NS + start->tv_nsec);
  printf("time = %lu ns\n", totalTime);
  printf("freq = %f Mhz\n", noEx*1000/((float)totalTime));
  
  // Free memory
  for ( i = 0; i < noEx; i++)
    free(examples[i]);
  free(examples);
  free(x);
  free(weights);
  free(tmp);
  for (i = 0; i < dictSize; i++)
    free(dict[i]);
  free(dict);
  return 0;
}
