
int openmp_matrix_multiply () ;

void main(){
  for (int i = 0; i < 10000; i ++) 
  {
  openmp_matrix_multiply();
  openmp_matrix_multiply();
  openmp_matrix_multiply();
  openmp_matrix_multiply();
  }
}

void task1(void* arg)
{

}
