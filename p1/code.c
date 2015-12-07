
int openmp_matrix_multiply () ;

double mycfun1 ( double inp)
{

  openmp_matrix_multiply();
    return inp + 5.5;
}

double mycfun2 ( double inp)
{
  openmp_matrix_multiply();
    return inp + 55;
}
